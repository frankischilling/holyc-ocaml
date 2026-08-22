type module_value =
  | Global_value of {
      global : Global_type_resolution.global;
      value : Function_call_resolution.identifier_value;
    }
  | Direct_function_value of {
      declaration : Function_resolution.resolved_declaration;
      value : Function_call_resolution.identifier_value;
    }
  | Aggregate_offset_base of Module_expression_binding.publication

type resolution =
  | Module_value of module_value
  | Outer_type_required of Outer_environment.binding

type leaf = {
  node : Top_level_expression_tree.expression_node;
  occurrence : Top_level_outer_expression_binding.occurrence;
  resolution : resolution;
}

type t = {
  table : Symbol_table.t;
  source_ : Top_level_expression_tree.t;
  leaves_ : leaf list;
}

type error_kind = Invalid_input of string
type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let invalid_input ?origin message =
  { code = "HCSEMA0056"; kind = Invalid_input message; origin }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message

let error_to_string error = error.code ^ ": " ^ error_message error
let owns_table result table = result.table == table
let source result = result.source_
let leaves result = result.leaves_
let leaf_node (leaf : leaf) = leaf.node
let leaf_occurrence (leaf : leaf) = leaf.occurrence
let leaf_resolution (leaf : leaf) = leaf.resolution

let module_value_type = function
  | Global_value { value; _ } | Direct_function_value { value; _ } ->
      Some (Function_call_resolution.identifier_value_type value)
  | Aggregate_offset_base _ -> None

let module_value_shape = function
  | Global_value { value; _ } | Direct_function_value { value; _ } ->
      Some (Function_call_resolution.identifier_value_shape value)
  | Aggregate_offset_base _ -> None

let module_value_array_rank = function
  | Global_value { value; _ } | Direct_function_value { value; _ } ->
      Some (Function_call_resolution.identifier_value_array_rank value)
  | Aggregate_offset_base _ -> None

let same_symbol left right = Symbol.Id.equal (Symbol.id left) (Symbol.id right)

let occurrence_publication occurrence =
  match Top_level_outer_expression_binding.occurrence_resolution occurrence with
  | Top_level_outer_expression_binding.Module_binding publication ->
      Some publication
  | Top_level_outer_expression_binding.Outer_binding _ -> None

let occurrence_outer_binding occurrence =
  match Top_level_outer_expression_binding.occurrence_resolution occurrence with
  | Top_level_outer_expression_binding.Module_binding _ -> None
  | Top_level_outer_expression_binding.Outer_binding binding -> Some binding

let same_publication left right =
  left == right
  || Module_expression_binding.publication_kind left
     = Module_expression_binding.publication_kind right
     && same_symbol
          (Module_expression_binding.publication_source_symbol left)
          (Module_expression_binding.publication_source_symbol right)
     && same_symbol
          (Module_expression_binding.publication_canonical_symbol left)
          (Module_expression_binding.publication_canonical_symbol right)
     && Module_expression_binding.publication_declaration_index left
        = Module_expression_binding.publication_declaration_index right
     && Module_expression_binding.publication_item_index left
        = Module_expression_binding.publication_item_index right
     && Module_expression_binding.publication_declarator_index left
        = Module_expression_binding.publication_declarator_index right

let function_matches_publication declaration publication =
  let site = Function_resolution.resolved_declaration_site declaration in
  let function_ = Function_resolution.declaration_site_function site in
  same_symbol
    (Function_type_resolution.function_symbol function_)
    (Module_expression_binding.publication_source_symbol publication)
  && same_symbol
       (Function_resolution.resolved_declaration_identity_symbol declaration)
       (Module_expression_binding.publication_canonical_symbol publication)

let global_matches_publication global publication =
  same_symbol
    (Global_type_resolution.global_symbol global)
    (Module_expression_binding.publication_source_symbol publication)
  && Global_type_resolution.global_item_index global
     = Module_expression_binding.publication_item_index publication
  && Global_type_resolution.global_declarator_index global
     = Module_expression_binding.publication_declarator_index publication

let module_resolution_is_valid publication = function
  | Global_value { global; value } ->
      Module_expression_binding.publication_kind publication
      = Module_expression_binding.Global_variable
      && global_matches_publication global publication
      && Function_call_resolution.identifier_value_shape value
         <> Function_call_resolution.Direct_function_value
  | Direct_function_value { declaration; value } ->
      Module_expression_binding.publication_kind publication
      = Module_expression_binding.Function
      && function_matches_publication declaration publication
      && Function_call_resolution.identifier_value_shape value
         = Function_call_resolution.Direct_function_value
      && Option.fold ~none:false
           ~some:(fun selected -> selected == declaration)
           (Function_call_resolution.identifier_value_function_declaration value)
  | Aggregate_offset_base selected ->
      Module_expression_binding.publication_kind publication
      = Module_expression_binding.Aggregate
      && same_publication selected publication

let resolution_is_valid occurrence = function
  | Module_value value -> (
      match occurrence_publication occurrence with
      | None -> false
      | Some publication -> module_resolution_is_valid publication value)
  | Outer_type_required binding -> (
      match occurrence_outer_binding occurrence with
      | None -> false
      | Some selected -> selected == binding)

let node_occurrence node =
  match
    node |> Top_level_expression_tree.expression_node_source
    |> Function_call_resolution.argument_expression_kind
  with
  | Function_call_resolution.Top_level_bound_identifier_expression identifier ->
      Some
        (Function_call_resolution.top_level_bound_identifier_occurrence
           identifier)
  | _ -> None

let make_leaf ~node ~occurrence ~resolution =
  match node_occurrence node with
  | None ->
      Error
        (invalid_input
           "top-level identifier classification received a nonidentifier node")
  | Some selected when selected != occurrence ->
      Error
        (invalid_input
           ~origin:
             (Top_level_outer_expression_binding.occurrence_origin occurrence)
           "top-level identifier node does not retain the supplied occurrence")
  | Some _ when not (resolution_is_valid occurrence resolution) ->
      Error
        (invalid_input
           ~origin:
             (Top_level_outer_expression_binding.occurrence_origin occurrence)
           "top-level identifier value does not match its binding")
  | Some _ -> Ok { node; occurrence; resolution }

let expected_nodes source =
  source |> Top_level_expression_tree.all_expression_nodes
  |> List.filter (fun node -> Option.is_some (node_occurrence node))

let validate_leaves expected leaves =
  let rec loop = function
    | [], [] -> Ok ()
    | expected :: expected_rest, actual :: actual_rest
      when expected == actual.node -> loop (expected_rest, actual_rest)
    | _ ->
        Error
          (invalid_input
             "top-level identifier classifications do not match the source tree")
  in
  loop (expected, leaves)

let type_is_owned table type_ =
  match Type.base type_ with
  | Type.Primitive _ -> true
  | Type.Aggregate symbol -> Symbol_table.owns_symbol table symbol

let value_type_is_owned table value =
  value |> Function_call_resolution.identifier_value_type |> type_is_owned table

let leaf_symbols_are_owned table leaves =
  List.for_all
    (fun leaf ->
      let occurrence_symbol_owned =
        match leaf.resolution with
        | Module_value (Global_value { global; value }) ->
            Symbol_table.owns_symbol table
              (Global_type_resolution.global_symbol global)
            && value_type_is_owned table value
        | Module_value (Direct_function_value { declaration; value }) ->
            Symbol_table.owns_symbol table
              (Function_resolution.resolved_declaration_identity_symbol
                 declaration)
            && value_type_is_owned table value
        | Module_value (Aggregate_offset_base publication) ->
            Symbol_table.owns_symbol table
              (Module_expression_binding.publication_canonical_symbol
                 publication)
        | Outer_type_required binding ->
            binding |> Outer_environment.binding_entry
            |> Outer_environment.entry_symbol
            |> Symbol_table.owns_symbol table
      in
      occurrence_symbol_owned)
    leaves

let create ~table ~source leaves =
  if not (Top_level_expression_tree.owns_table source table) then
    Error
      (invalid_input
         "top-level identifier source belongs to another symbol table")
  else if not (leaf_symbols_are_owned table leaves) then
    Error
      (invalid_input
         "top-level identifier classification contains a foreign symbol")
  else
    match validate_leaves (expected_nodes source) leaves with
    | Error _ as error -> error
    | Ok () -> Ok { table; source_ = source; leaves_ = leaves }
