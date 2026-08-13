let same_symbol left right =
  Sema.Symbol.Id.equal (Sema.Symbol.id left) (Sema.Symbol.id right)

let layout_kind = function
  | Sema.Aggregate_resolution.Class -> Sema.Aggregate_layout.Class
  | Sema.Aggregate_resolution.Union -> Sema.Aggregate_layout.Union

let header_base_symbol header =
  Sema.Aggregate_header_resolution.header_base header
  |> Option.map Sema.Aggregate_header_resolution.base_symbol

let layout_base_symbol (layout : Sema.Aggregate_layout.aggregate_layout) =
  Option.map
    (fun (base : Sema.Aggregate_layout.base_layout) -> base.symbol)
    layout.base

let same_optional_symbol left right =
  match (left, right) with
  | None, None -> true
  | Some left, Some right -> same_symbol left right
  | None, Some _ | Some _, None -> false

let validate_aggregate header aggregate
    (layout : Sema.Aggregate_layout.aggregate_layout) =
  let header_symbol = Sema.Aggregate_header_resolution.header_symbol header in
  let aggregate_symbol =
    Sema.Member_type_resolution.aggregate_symbol aggregate
  in
  if not (same_symbol header_symbol aggregate_symbol) then
    Error "aggregate member index header and member owner do not match"
  else if not (same_symbol header_symbol layout.symbol) then
    Error "aggregate member index header and layout owner do not match"
  else if
    Sema.Aggregate_header_resolution.header_item_index header
    <> Sema.Member_type_resolution.aggregate_item_index aggregate
    || Sema.Aggregate_header_resolution.header_item_index header
       <> layout.item_index
  then Error "aggregate member index inputs have different source positions"
  else if
    layout_kind (Sema.Aggregate_header_resolution.header_aggregate_kind header)
    <> layout.kind
  then Error "aggregate member index inputs have different aggregate kinds"
  else if
    not
      (same_optional_symbol
         (header_base_symbol header)
         (layout_base_symbol layout))
  then Error "aggregate member index inputs have different base classes"
  else Ok ()

let validate_member fact (layout : Sema.Aggregate_layout.member_layout) =
  let symbol = Sema.Member_type_resolution.member_symbol fact in
  if not (same_symbol symbol layout.symbol) then
    Error "aggregate member index member and layout identities do not match"
  else if Sema.Member_type_resolution.member_path fact <> layout.path then
    Error "aggregate member index member and layout paths do not match"
  else if
    Sema.Member_type_resolution.member_declarator_index fact
    <> layout.declarator_index
  then Error "aggregate member index declarator positions do not match"
  else if
    Sema.Member_type_resolution.member_declarator_origin fact <> layout.origin
  then Error "aggregate member index member and layout origins do not match"
  else Ok ()

let member_input fact layout =
  Result.map
    (fun () ->
      let reference = Sema.Member_type_resolution.member_type_reference fact in
      let member_is_function_pointer =
        match Sema.Member_type_resolution.member_declarator_kind fact with
        | Sema.Member_type_resolution.Object -> false
        | Sema.Member_type_resolution.Function_pointer _ -> true
      in
      {
        Sema.Aggregate_member_index.member_type =
          Sema.Member_type_resolution.type_reference_type reference;
        member_is_function_pointer;
        member_layout = layout;
      })
    (validate_member fact layout)

let member_inputs facts layouts =
  let rec loop inputs_rev facts layouts =
    match (facts, layouts) with
    | [], [] -> Ok (List.rev inputs_rev)
    | fact :: fact_rest, layout :: layout_rest ->
        Result.bind (member_input fact layout) (fun input ->
            loop (input :: inputs_rev) fact_rest layout_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "aggregate member index inputs contain different member counts"
  in
  loop [] facts layouts

let aggregate_input header aggregate layout =
  Result.bind (validate_aggregate header aggregate layout) (fun () ->
      Result.map
        (fun aggregate_members ->
          {
            Sema.Aggregate_member_index.aggregate_scope =
              Sema.Member_type_resolution.aggregate_scope aggregate;
            aggregate_layout = layout;
            aggregate_members;
          })
        (member_inputs
           (Sema.Member_type_resolution.aggregate_members aggregate)
           layout.Sema.Aggregate_layout.members))

let inputs headers aggregates layouts =
  let rec loop inputs_rev headers aggregates layouts =
    match (headers, aggregates, layouts) with
    | [], [], [] -> Ok (List.rev inputs_rev)
    | header :: header_rest, aggregate :: aggregate_rest, layout :: layout_rest
      ->
        Result.bind (aggregate_input header aggregate layout) (fun input ->
            loop (input :: inputs_rev) header_rest aggregate_rest layout_rest)
    | [], _, _ | _, [], _ | _, _, [] ->
        Error "aggregate member index inputs contain different aggregate counts"
  in
  loop [] headers aggregates layouts

let build ~table ~declarations ~headers ~members ~layouts =
  let headers = Sema.Aggregate_header_resolution.headers headers in
  let members = Sema.Member_type_resolution.aggregates members in
  let layouts = Sema.Aggregate_layout.layouts layouts in
  let parent = Sema.Declaration_collection.scope declarations in
  let result =
    if not (Sema.Symbol_table.owns_scope table parent) then
      Error "aggregate member index declarations belong to another symbol table"
    else if Sema.Symbol_table.scope_kind parent <> Sema.Symbol_table.Module then
      Error "aggregate member index needs a module scope"
    else
      Result.bind (inputs headers members layouts) (fun inputs ->
          Sema.Aggregate_member_index.build ~table ~parent inputs
          |> Result.map_error Sema.Aggregate_member_index.error_to_string)
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0010: " ^ message)
    result
