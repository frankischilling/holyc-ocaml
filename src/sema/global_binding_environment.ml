type resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type owner = unit ref

type global = {
  owner : owner;
  record : Global_resolution.global_record;
  publication : Module_expression_binding.publication;
}

module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)
module String_map = Map.Make (String)

type t = {
  owner : owner;
  table : Symbol_table.t;
  environment : Outer_environment.t;
  expressions : Module_expression_binding.t;
  source_globals : Global_resolution.t;
  globals : global list;
}

type cursor = {
  owner : owner;
  environment : Outer_environment.t;
  visible : Module_expression_binding.publication String_map.t;
  remaining : Module_expression_binding.publication list;
}

let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int
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

let publication_map table expressions =
  let rec collect expected_index previous_item seen by_symbol = function
    | [] -> Ok by_symbol
    | publication :: rest ->
        let source =
          Module_expression_binding.publication_source_symbol publication
        in
        let canonical =
          Module_expression_binding.publication_canonical_symbol publication
        in
        let source_number = symbol_number source in
        let declaration_index =
          Module_expression_binding.publication_declaration_index publication
        in
        let item_index =
          Module_expression_binding.publication_item_index publication
        in
        if declaration_index <> expected_index then
          Error "module publication declaration indexes are not contiguous"
        else if item_index < previous_item then
          Error "module publications do not follow source item order"
        else if Int_set.mem source_number seen then
          Error "module publication source symbol is repeated"
        else if
          not
            (Symbol_table.owns_symbol table source
            && Symbol_table.owns_symbol table canonical)
        then Error "module publication belongs to another symbol table"
        else
          collect (expected_index + 1) item_index
            (Int_set.add source_number seen)
            (Int_map.add source_number publication by_symbol)
            rest
  in
  collect 0 (-1) Int_set.empty Int_map.empty
    (Module_expression_binding.publications expressions)

let ordered_after previous_item previous_declarator item_index declarator_index
    =
  item_index > previous_item
  || item_index = previous_item
     &&
     match (previous_declarator, declarator_index) with
     | None, Some _ -> true
     | Some left, Some right -> right > left
     | None, None | Some _, None -> false

let validate_global_publication table publications previous_index record =
  let symbol = global_symbol record in
  let item_index = global_item_index record in
  let declarator_index = global_declarator_index record in
  let number = symbol_number symbol in
  if not (Symbol_table.owns_symbol table symbol) then
    Error "global record belongs to another symbol table"
  else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Global_variable)
  then Error "global record symbol is not a global variable"
  else
    match Int_map.find_opt number publications with
    | None -> Error "global record has no module publication"
    | Some publication ->
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
        else Ok (publication_index, publication)

let pair_globals owner table publications records =
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
            validate_global_publication table publications previous_publication
              record
          with
          | Error _ as error -> error
          | Ok (publication_index, publication) ->
              loop item_index declarator_index publication_index
                ({ owner; record; publication } :: paired_rev)
                rest)
  in
  loop (-1) None (-1) [] records

let create ~table ~environment ~expressions ~globals =
  if not (Module_expression_binding.owns_table expressions table) then
    Error "module expression bindings belong to another symbol table"
  else if not (Outer_environment.owns_table environment table) then
    Error "outer environment belongs to another symbol table"
  else if
    Module_expression_binding.compilation_mode expressions
    <> Outer_environment.compilation_mode environment
  then
    Error
      "module expression bindings and outer environment use different \
       compilation modes"
  else if
    not
      (global_mode_matches globals
         (Module_expression_binding.compilation_mode expressions))
  then
    Error
      "global records and module expressions use different compilation modes"
  else
    match publication_map table expressions with
    | Error _ as error -> error
    | Ok publications -> (
        let owner = ref () in
        match
          pair_globals owner table publications
            (Global_resolution.records globals)
        with
        | Error _ as error -> error
        | Ok paired ->
            Ok
              {
                owner;
                table;
                environment;
                expressions;
                source_globals = globals;
                globals = paired;
              })

let table (state : t) = state.table
let environment (state : t) = state.environment
let expressions (state : t) = state.expressions
let source_globals (state : t) = state.source_globals
let globals (state : t) = state.globals
let owns_table (state : t) table = state.table == table
let global_record (global : global) = global.record
let global_publication (global : global) = global.publication

let initial_cursor (state : t) =
  {
    owner = state.owner;
    environment = state.environment;
    visible = String_map.empty;
    remaining = Module_expression_binding.publications state.expressions;
  }

let add_publication visible publication =
  String_map.add
    (Module_expression_binding.publication_source_symbol publication
    |> Symbol.name)
    publication visible

let rec publish_while predicate visible = function
  | publication :: rest when predicate publication ->
      publish_while predicate (add_publication visible publication) rest
  | remaining -> (visible, remaining)

let advance comparison (cursor : cursor) (global : global) =
  if cursor.owner != global.owner then
    Error
      "global publication cursor and record belong to different environments"
  else
    let boundary =
      Module_expression_binding.publication_declaration_index global.publication
    in
    let visible, remaining =
      publish_while
        (fun publication ->
          comparison
            (Module_expression_binding.publication_declaration_index publication)
            boundary)
        cursor.visible cursor.remaining
    in
    Ok { cursor with visible; remaining }

let publish_before cursor global = advance ( < ) cursor global
let publish_through cursor global = advance ( <= ) cursor global

let resolve cursor name =
  match String_map.find_opt name cursor.visible with
  | Some publication -> Some (Module_binding publication)
  | None ->
      Outer_environment.find cursor.environment name
      |> Option.map (fun binding -> Outer_binding binding)
