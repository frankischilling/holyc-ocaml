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

let layout ~dimensions ~table ~namespace ~symbol
    (definition : Ast.aggregate_definition) =
  if Option.is_some definition.base then
    Error "retained aggregate bases require original selected layout metadata"
  else if definition.attached_declarators <> [] then
    Error "retained aggregate attached storage is not implemented"
  else
    let* () =
      match definition.backing with
      | None -> Ok ()
      | Some backing ->
          Source_type_reference.builtin backing.backing_type_specifier
            backing.backing_pointer_layers
          |> Result.map ignore
    in
    let rec facts path members =
      map_result
        (fun (index, member) ->
          let path = path @ [ index ] in
          match member with
          | Ast.Aggregate_member_declaration declaration ->
              map_result
                (fun ( declarator_index,
                       (member : Ast.aggregate_member_declarator) ) ->
                  if
                    Option.is_some member.member_function_pointer
                    || member.member_metadata <> []
                  then
                    Error
                      "retained aggregate callbacks and member metadata \
                       require original preparation"
                  else
                    let* type_ =
                      Source_type_reference.builtin
                        declaration.member_type_specifier
                        member.member_pointer_layers
                    in
                    let* fact =
                      Member_collection.make_member
                        ~name:member.member_name.spelling
                        ~origin:(origin member.member_name.location)
                        ~member_path:path ~declarator_index
                    in
                    let* counts = dimensions member in
                    Ok (fact, type_, member, counts))
                (List.mapi (fun i m -> (i, m)) declaration.member_declarators)
          | Ast.Anonymous_union_member union ->
              facts path union.anonymous_union_members
          | Ast.Aggregate_offset_directive _ ->
              Error
                "retained aggregate offsets require original expression \
                 preparation"
          | Ast.Empty_aggregate_member _ -> Ok [])
        (List.mapi (fun i m -> (i, m)) members)
      |> Result.map List.concat
    in
    let* facts = facts [] definition.members in
    let* aggregate =
      Member_collection.make_aggregate ~symbol ~item_index:0
        (List.map (fun (fact, _, _, _) -> fact) facts)
    in
    let parent = Declaration_collection.namespace_scope namespace in
    let* collection = Member_collection.collect ~table ~parent [ aggregate ] in
    let collected = List.hd (Member_collection.aggregates collection) in
    let pairs = Members.create (List.length facts) in
    List.iter2
      (fun entry (_, type_, member, counts) ->
        Members.add pairs member (entry, type_, counts))
      (Member_collection.aggregate_entries collected)
      facts;
    let rec items path members =
      List.mapi
        (fun index member ->
          let path = path @ [ index ] in
          match member with
          | Ast.Empty_aggregate_member location ->
              [ Layout.Empty_member (origin location) ]
          | Ast.Aggregate_offset_directive _ -> assert false
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
                  let entry, type_, counts = Members.find pairs member in
                  Layout.Field
                    {
                      member_symbol = Member_collection.entry_symbol entry;
                      member_path = path;
                      member_declarator_index = declarator_index;
                      member_origin = origin member.member_declarator_location;
                      member_type = Type_reference.resolved_type type_;
                      member_is_function_pointer = false;
                      member_dimensions =
                        List.map2
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
    Layout.layout ~table ~parent
      [
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
        };
      ]
    |> Result.map_error Layout.error_to_string
    |> Result.map (fun result -> (List.hd (Layout.layouts result)).size)
