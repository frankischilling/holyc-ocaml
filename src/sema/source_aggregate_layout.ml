module Ast = Frontend.Ast

module No_query = struct
  type t = |

  let expression (query : t) =
    match query with
    | _ -> .

  let constant (query : t) =
    match query with
    | _ -> .

  let owns_table (query : t) _ =
    match query with
    | _ -> .
end

module Layout = Aggregate_layout_core.Make (No_query)

let ( let* ) = Result.bind

let place_member ~origin ~kind ~union_base ~current_size ~member_size =
  Layout.place_member ~origin
    ~kind:
      (match kind with
      | Ast.Class_aggregate -> Layout.Class
      | Ast.Union_aggregate -> Layout.Union)
    ~union_base ~current_size ~member_size
  |> Result.map_error Layout.error_to_string

let member_extent ~origin ~element_size ~counts =
  Layout.member_extent ~origin ~element_size ~counts
  |> Result.map_error Layout.error_to_string

let finish_size ~origin ~size ~negative_offset =
  Layout.finish_size ~origin ~size ~negative_offset
  |> Result.map_error Layout.error_to_string

let negative_offset ~origin ~previous ~position =
  Layout.negative_offset ~origin ~previous ~position
  |> Result.map_error Layout.error_to_string

let origin (location : Ast.location) =
  Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let map_result f values =
  let rec loop rev = function
    | [] -> Ok (List.rev rev)
    | item :: rest ->
        let* value = f item in
        loop (value :: rev) rest
  in
  loop [] values

module Members = Hashtbl.Make (struct
  type t = Ast.aggregate_member_declarator

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

module Offsets = Hashtbl.Make (struct
  type t = Ast.aggregate_offset_directive

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

let layout ?(members = fun _ _ -> None) ?(callbacks = fun _ -> None)
    ?initial_size ~offsets ~dimensions ~table ~namespace ~symbol
    (definition : Ast.aggregate_definition) =
  if Option.is_some definition.base <> Option.is_some initial_size then
    Error "retained aggregate bases require original selected layout metadata"
  else if definition.attached_declarators <> [] then
    Error "retained aggregate attached storage is not implemented"
  else
    let* () =
      match definition.backing with
      | None -> Ok ()
      | Some backing -> (
          (* A backing changes scalar interpretation, not member extent.
             Named backings need no scalar resolution in this layout adapter;
             their original selection is checked by the value consumers. *)
          match backing.backing_type_specifier with
          | Ast.Named_type_specifier _ ->
              Source_type_reference.pointer_depth backing.backing_pointer_layers
              |> Result.map ignore
          | _ ->
              Source_type_reference.builtin backing.backing_type_specifier
                backing.backing_pointer_layers
              |> Result.map ignore)
    in
    let selected_members = members in
    let offset_values = Offsets.create 8 in
    let rec facts path members =
      map_result
        (fun (index, member) ->
          let path = path @ [ index ] in
          match member with
          | Ast.Aggregate_member_declaration declaration ->
              map_result
                (fun ( declarator_index,
                       (member : Ast.aggregate_member_declarator) ) ->
                  if member.member_metadata <> [] then
                    Error
                      "retained aggregate member metadata requires original \
                       preparation"
                  else
                    let* type_, selected_size =
                      match member.member_function_pointer with
                      | None -> (
                          match
                            selected_members declaration.member_type_specifier
                              member
                          with
                          | Some (type_, size) -> Ok (type_, Some size)
                          | None ->
                              Result.map
                                (fun type_ -> (type_, None))
                                (Source_type_reference.builtin
                                   declaration.member_type_specifier
                                   member.member_pointer_layers))
                      | Some source -> (
                          match callbacks source with
                          | Some header ->
                              Result.map
                                (fun type_ -> (type_, None))
                                (Source_type_reference.callback_storage ~header
                                   source)
                          | None ->
                              Error
                                "retained callback layout lacks its original \
                                 completed header")
                    in
                    let* fact =
                      Member_collection.make_member
                        ~name:member.member_name.spelling
                        ~origin:(origin member.member_name.location)
                        ~member_path:path ~declarator_index
                    in
                    let* counts = dimensions member in
                    Ok (fact, type_, member, counts, selected_size))
                (List.mapi (fun i m -> (i, m)) declaration.member_declarators)
          | Ast.Anonymous_union_member union ->
              facts path union.anonymous_union_members
          | Ast.Aggregate_offset_directive directive ->
              let* value = offsets directive.aggregate_offset_expression in
              Offsets.add offset_values directive value;
              Ok []
          | Ast.Empty_aggregate_member _ -> Ok [])
        (List.mapi (fun i m -> (i, m)) members)
      |> Result.map List.concat
    in
    let* facts = facts [] definition.members in
    let* aggregate =
      Member_collection.make_aggregate ~symbol ~item_index:0
        (List.map (fun (fact, _, _, _, _) -> fact) facts)
    in
    let parent = Declaration_collection.namespace_scope namespace in
    let* collection = Member_collection.collect ~table ~parent [ aggregate ] in
    let collected = List.hd (Member_collection.aggregates collection) in
    let pairs = Members.create (List.length facts) in
    List.iter2
      (fun entry (_, type_, member, counts, selected_size) ->
        Members.add pairs member (entry, type_, counts, selected_size))
      (Member_collection.aggregate_entries collected)
      facts;
    let rec items path members =
      List.mapi
        (fun index member ->
          let path = path @ [ index ] in
          match member with
          | Ast.Empty_aggregate_member location ->
              [ Layout.Empty_member (origin location) ]
          | Ast.Aggregate_offset_directive directive ->
              let value = Offsets.find offset_values directive in
              [
                Layout.Offset_directive
                  (Layout.Integer_expression
                     {
                       value;
                       origin = origin directive.aggregate_offset_location;
                     });
              ]
          | Ast.Anonymous_union_member union ->
              [
                Layout.Anonymous_union
                  {
                    union_origin = origin union.anonymous_union_location;
                    union_items = items path union.anonymous_union_members;
                  };
              ]
          | Ast.Aggregate_member_declaration declaration ->
              List.mapi
                (fun declarator_index (member : Ast.aggregate_member_declarator)
                   ->
                  let entry, type_, counts, selected_size =
                    Members.find pairs member
                  in
                  let source_type = Type_reference.resolved_type type_ in
                  (* This adapter returns only source size metadata. An original
                     class extent becomes its byte count here; semantic member
                     types and runtime layouts retain the selected class. *)
                  let layout_type, dimensions =
                    match
                      ( selected_size,
                        Type.base source_type,
                        Type.pointer_depth source_type )
                    with
                    | Some size, Type.Aggregate _, 0 ->
                        let byte_type =
                          Type.make_primitive ~form:Type.Internal_storage
                            ~primitive:U8 ~pointer_depth:0
                          |> Result.get_ok
                        in
                        let class_extent =
                          {
                            Layout.dimension_origin =
                              origin member.member_declarator_location;
                            dimension_expression =
                              Some
                                (Layout.Integer_expression
                                   {
                                     value = size;
                                     origin =
                                       origin member.member_declarator_location;
                                   });
                          }
                        in
                        (byte_type, [ class_extent ])
                    | _ -> (source_type, [])
                  in
                  Layout.Field
                    {
                      member_symbol = Member_collection.entry_symbol entry;
                      member_path = path;
                      member_declarator_index = declarator_index;
                      member_origin = origin member.member_declarator_location;
                      member_type = layout_type;
                      member_is_function_pointer =
                        Option.is_some member.member_function_pointer;
                      member_dimensions =
                        dimensions
                        @ List.map2
                            (fun (dimension : Ast.array_dimension) count ->
                              {
                                Layout.dimension_origin =
                                  origin dimension.location;
                                dimension_expression =
                                  Some
                                    (Layout.Integer_expression
                                       {
                                         value = count;
                                         origin = origin dimension.location;
                                       });
                              })
                            member.member_array_dimensions counts;
                    })
                declaration.member_declarators)
        members
      |> List.concat
    in
    let input : Layout.aggregate_input =
      {
        aggregate_symbol = symbol;
        aggregate_scope = Member_collection.aggregate_scope collected;
        aggregate_kind =
          (match definition.aggregate_kind with
          | Class_aggregate -> Layout.Class
          | Union_aggregate -> Layout.Union);
        aggregate_item_index = 0;
        aggregate_origin = origin definition.location;
        aggregate_base = None;
        aggregate_items = items [] definition.members;
      }
    in
    (match initial_size with
      | None -> Layout.layout ~table ~parent [ input ]
      | Some initial_size ->
          Layout.layout_from_size ~table ~parent ~initial_size input)
    |> Result.map_error Layout.error_to_string
    |> Result.map (fun result -> (List.hd (Layout.layouts result)).size)
