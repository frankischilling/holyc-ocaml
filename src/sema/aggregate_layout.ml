include Aggregate_layout_core.Make (Query_selection)

let reached_member ~table ~symbol field =
  let name = Compiler_record.aggregate_field_name field in
  let origin = Closed_numeric_expression.origin name.location in
  if
    (not (Symbol_table.owns_symbol table symbol))
    || Symbol.name symbol <> name.spelling
    || Symbol.origin symbol <> origin
    || not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Member)
  then Error "reached member layout has another original member identity"
  else
    let type_ = Compiler_record.aggregate_field_type field in
    let signedness =
      if Type.pointer_depth type_ > 0 then Unsigned
      else
        match Type.base type_ with
        | Type.Aggregate _ -> Not_applicable
        | Type.Primitive (_, primitive) -> (
            match (Primitive_type.info primitive).signedness with
            | Primitive_type.Signed -> Signed
            | Primitive_type.Unsigned -> Unsigned
            | Primitive_type.Not_applicable -> Not_applicable)
    in
    placed_member_layout ~symbol ~origin
      ~offset:(Compiler_record.aggregate_field_offset field)
      ~element_size:(Compiler_record.aggregate_field_placement_size field)
      ~dimensions:(Compiler_record.aggregate_field_counts field)
      ~signedness
    |> Result.map_error error_to_string

type original_member = {
  original_table : Symbol_table.t;
  original_parent : Symbol_table.scope;
  original_aggregate : Symbol.t;
  original_input : member_input;
  original_size : int64;
}

let original_member ~table ~parent ~aggregate ~type_specifier
    ~(declarator : Frontend.Ast.aggregate_member_declarator) input field =
  let origin = Closed_numeric_expression.origin in
  if
    (not (Symbol_table.owns_symbol table input.member_symbol))
    || (not
          (String.equal
             (Symbol.name input.member_symbol)
             declarator.member_name.spelling))
    || Symbol.origin input.member_symbol
       <> origin declarator.member_name.location
    || input.member_origin <> origin declarator.member_declarator_location
    || input.member_is_function_pointer
    || Type.pointer_depth input.member_type <> 0
    || (match Type.base input.member_type with
      | Type.Aggregate _ -> false
      | _ -> true)
    || List.map
         (fun dimension -> dimension.dimension_origin)
         input.member_dimensions
       <> List.map
            (fun (dimension : Frontend.Ast.array_dimension) ->
              origin dimension.location)
            declarator.member_array_dimensions
  then Error "original member layout differs from its source field"
  else
    Result.map
      (fun original_size ->
        {
          original_table = table;
          original_parent = parent;
          original_aggregate = aggregate;
          original_input = input;
          original_size;
        })
      (Compiler_record.aggregate_field_element_size ~table ~parent ~aggregate
         ~type_specifier ~declarator ~type_:input.member_type field)

let layout_core = layout

let layout ?(original_members = []) ~table ~parent inputs =
  let selected_sizes aggregate member =
    match
      List.find_opt
        (fun original ->
          original.original_input.member_symbol == member.member_symbol)
        original_members
    with
    | None -> Ok None
    | Some original ->
        if
          original.original_table != table
          || original.original_parent != parent
          || original.original_aggregate != aggregate.aggregate_symbol
          || original.original_input != member
        then Error "aggregate layout substituted its original member input"
        else Ok (Some original.original_size)
  in
  layout_core ~selected_sizes ~table ~parent inputs
