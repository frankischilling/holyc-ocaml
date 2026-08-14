type resolution = Module_binding_environment.resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type global = {
  record : Global_resolution.global_record;
  point : Module_binding_environment.point;
}

type t = {
  module_environment : Module_binding_environment.t;
  source_globals : Global_resolution.t;
  globals : global list;
}

type cursor = Module_binding_environment.cursor

let same_symbol left right = Symbol.Id.equal (Symbol.id left) (Symbol.id right)
let global_data record = Global_resolution.global_record_global record
let global_symbol record = Global_resolution.global_record_symbol record

let global_item_index record =
  Global_type_resolution.global_item_index (global_data record)

let global_declarator_index record =
  Global_type_resolution.global_declarator_index (global_data record)

let global_mode_matches globals mode =
  match (Global_resolution.compilation_mode globals, mode) with
  | Global_resolution.Jit, Function_resolution.Jit
  | Global_resolution.Aot, Function_resolution.Aot -> true
  | Global_resolution.Jit, Function_resolution.Aot
  | Global_resolution.Aot, Function_resolution.Jit -> false

let ordered_after previous_item previous_declarator item_index declarator_index
    =
  item_index > previous_item
  || item_index = previous_item
     &&
     match (previous_declarator, declarator_index) with
     | None, Some _ -> true
     | Some left, Some right -> right > left
     | None, None | Some _, None -> false

let validate_global_publication table module_environment previous_index record =
  let symbol = global_symbol record in
  let item_index = global_item_index record in
  let declarator_index = global_declarator_index record in
  if not (Symbol_table.owns_symbol table symbol) then
    Error "global record belongs to another symbol table"
  else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Global_variable)
  then Error "global record symbol is not a global variable"
  else
    match Module_binding_environment.find_point module_environment symbol with
    | None -> Error "global record has no module publication"
    | Some point ->
        let publication = Module_binding_environment.point_publication point in
        let source =
          Module_expression_binding.publication_source_symbol publication
        in
        let canonical =
          Module_expression_binding.publication_canonical_symbol publication
        in
        let publication_index =
          Module_expression_binding.publication_declaration_index publication
        in
        if
          Module_expression_binding.publication_kind publication
          <> Module_expression_binding.Global_variable
        then Error "global publication has the wrong record kind"
        else if not (same_symbol source symbol && same_symbol canonical symbol)
        then Error "global publication has the wrong identity"
        else if
          Module_expression_binding.publication_item_index publication
          <> item_index
          || Module_expression_binding.publication_declarator_index publication
             <> declarator_index
        then Error "global publication has the wrong source position"
        else if publication_index <= previous_index then
          Error "global publications are not source ordered"
        else Ok (publication_index, point)

let pair_globals table module_environment records =
  let rec loop previous_item previous_declarator previous_publication paired_rev
      = function
    | [] -> Ok (List.rev paired_rev)
    | record :: rest -> (
        let item_index = global_item_index record in
        let declarator_index = global_declarator_index record in
        if
          not
            (ordered_after previous_item previous_declarator item_index
               declarator_index)
        then Error "global records are not source ordered"
        else
          match
            validate_global_publication table module_environment
              previous_publication record
          with
          | Error _ as error -> error
          | Ok (publication_index, point) ->
              loop item_index declarator_index publication_index
                ({ record; point } :: paired_rev)
                rest)
  in
  loop (-1) None (-1) [] records

let create ~table ~environment ~expressions ~globals:source_globals =
  if
    not
      (global_mode_matches source_globals
         (Module_expression_binding.compilation_mode expressions))
  then
    Error
      "global records and module expressions use different compilation modes"
  else
    match
      Module_binding_environment.create ~table ~environment ~expressions
    with
    | Error _ as error -> error
    | Ok module_environment -> (
        match
          pair_globals table module_environment
            (Global_resolution.records source_globals)
        with
        | Error _ as error -> error
        | Ok globals ->
            Ok { module_environment; source_globals; globals })

let table state = Module_binding_environment.table state.module_environment

let environment state =
  Module_binding_environment.environment state.module_environment

let expressions state =
  Module_binding_environment.expressions state.module_environment

let source_globals state = state.source_globals
let globals state = state.globals

let owns_table state table =
  Module_binding_environment.owns_table state.module_environment table

let global_record (global : global) = global.record

let global_publication (global : global) =
  Module_binding_environment.point_publication global.point

let initial_cursor state =
  Module_binding_environment.initial_cursor state.module_environment

let publish_before cursor global =
  Module_binding_environment.publish_before cursor global.point

let publish_through cursor global =
  Module_binding_environment.publish_through cursor global.point

let resolve = Module_binding_environment.resolve
