type publication_kind = Aggregate | Function | Global_variable

type publication = {
  source_symbol : Symbol.t;
  canonical_symbol : Symbol.t;
  publication_kind : publication_kind;
  declaration_index : int;
  item_index : int;
  declarator_index : int option;
}

type resolution =
  | Local_binding of Function_binding_index.binding
  | Module_binding of publication
  | Outer_candidate

type occurrence = {
  source : Function_expression_binding.occurrence;
  resolution : resolution;
}

type resolved_function = {
  source : Function_expression_binding.resolved_function;
  occurrences : occurrence list;
}

module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)
module String_map = Map.Make (String)

type t = {
  table : Symbol_table.t;
  publications : publication list;
  functions : resolved_function list;
  by_symbol : resolved_function Int_map.t;
}

type error_kind =
  | Invalid_input of string
  | Missing_function_publication of { function_symbol : Symbol.t }

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let publications result = result.publications
let functions result = result.functions
let publication_kind publication = publication.publication_kind
let publication_source_symbol publication = publication.source_symbol
let publication_canonical_symbol publication = publication.canonical_symbol
let publication_declaration_index publication = publication.declaration_index
let publication_item_index publication = publication.item_index
let publication_declarator_index publication = publication.declarator_index

let function_symbol (function_ : resolved_function) =
  Function_expression_binding.function_symbol function_.source

let function_scope (function_ : resolved_function) =
  Function_expression_binding.function_scope function_.source

let function_item_index (function_ : resolved_function) =
  Function_expression_binding.function_item_index function_.source

let function_occurrences (function_ : resolved_function) = function_.occurrences
let occurrence_source (occurrence : occurrence) = occurrence.source

let occurrence_index (occurrence : occurrence) =
  Function_expression_binding.occurrence_index occurrence.source

let occurrence_name (occurrence : occurrence) =
  Function_expression_binding.occurrence_name occurrence.source

let occurrence_origin (occurrence : occurrence) =
  Function_expression_binding.occurrence_origin occurrence.source

let occurrence_resolution (occurrence : occurrence) = occurrence.resolution
let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int

let publication_kind_name = function
  | Aggregate -> "aggregate"
  | Function -> "function"
  | Global_variable -> "global-variable"

let invalid_input message =
  { code = "HCSEMA0020"; kind = Invalid_input message; origin = None }

let missing_function_publication function_symbol =
  {
    code = "HCSEMA0021";
    kind = Missing_function_publication { function_symbol };
    origin = Some (Symbol.origin function_symbol);
  }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message
  | Missing_function_publication { function_symbol } ->
      Printf.sprintf "function %S has no matching compilation-unit publication"
        (Symbol.name function_symbol)

let error_to_string error = error.code ^ ": " ^ error_message error

let symbol_kind_for_publication = function
  | Aggregate -> Symbol.Aggregate_type
  | Function -> Symbol.Function
  | Global_variable -> Symbol.Global_variable

let make_publication ~source_symbol ~canonical_symbol ~publication_kind
    ~declaration_index ~item_index ?declarator_index () =
  let expected_kind = symbol_kind_for_publication publication_kind in
  if declaration_index < 0 then
    Error "module publication declaration index cannot be negative"
  else if item_index < 0 then
    Error "module publication item index cannot be negative"
  else if
    match declarator_index with
    | Some index -> index < 0
    | None -> false
  then Error "module publication declarator index cannot be negative"
  else if not (Symbol.equal_kind (Symbol.kind source_symbol) expected_kind) then
    Error "module publication source symbol has the wrong kind"
  else if not (Symbol.equal_kind (Symbol.kind canonical_symbol) expected_kind)
  then Error "module publication canonical symbol has the wrong kind"
  else if
    not
      (String.equal (Symbol.name source_symbol) (Symbol.name canonical_symbol))
  then Error "module publication symbols have different spellings"
  else
    Ok
      {
        source_symbol;
        canonical_symbol;
        publication_kind;
        declaration_index;
        item_index;
        declarator_index;
      }

let same_scope left right =
  Symbol.Scope_id.equal
    (Symbol_table.scope_id left)
    (Symbol_table.scope_id right)

let symbol_in_scope symbol scope =
  Symbol.Scope_id.equal (Symbol.scope_id symbol) (Symbol_table.scope_id scope)

let validate_publication table parent expected_index previous_item seen
    publication =
  let source_number = symbol_number publication.source_symbol in
  if publication.declaration_index <> expected_index then
    Error
      (invalid_input "module publication declaration indexes are not contiguous")
  else if publication.item_index < previous_item then
    Error (invalid_input "module publications do not follow source item order")
  else if Int_set.mem source_number seen then
    Error (invalid_input "module publication source symbol is repeated")
  else if not (Symbol_table.owns_symbol table publication.source_symbol) then
    Error
      (invalid_input "module publication source belongs to another symbol table")
  else if not (Symbol_table.owns_symbol table publication.canonical_symbol) then
    Error
      (invalid_input
         "module publication identity belongs to another symbol table")
  else if not (symbol_in_scope publication.source_symbol parent) then
    Error (invalid_input "module publication source has the wrong module scope")
  else if not (symbol_in_scope publication.canonical_symbol parent) then
    Error
      (invalid_input "module publication identity has the wrong module scope")
  else
    let expected_kind =
      symbol_kind_for_publication publication.publication_kind
    in
    if
      not
        (Symbol.equal_kind (Symbol.kind publication.source_symbol) expected_kind
        && Symbol.equal_kind
             (Symbol.kind publication.canonical_symbol)
             expected_kind)
    then
      Error (invalid_input "module publication has an inconsistent symbol kind")
    else if
      not
        (String.equal
           (Symbol.name publication.source_symbol)
           (Symbol.name publication.canonical_symbol))
    then Error (invalid_input "module publication has inconsistent spellings")
    else Ok (publication.item_index, Int_set.add source_number seen)

let validate_publications table parent publications =
  let rec loop expected_index previous_item seen = function
    | [] -> Ok seen
    | publication :: rest -> (
        match
          validate_publication table parent expected_index previous_item seen
            publication
        with
        | Error _ as error -> error
        | Ok (item_index, seen) ->
            loop (expected_index + 1) item_index seen rest)
  in
  loop 0 (-1) Int_set.empty publications

let validate_local_binding table function_scope
    (binding : Function_binding_index.binding) =
  if not (Symbol_table.owns_symbol table binding.symbol) then
    Error
      (invalid_input
         "function expression binding belongs to another symbol table")
  else if not (symbol_in_scope binding.symbol function_scope) then
    Error
      (invalid_input "function expression binding has the wrong function scope")
  else Ok ()

let validate_occurrences table function_scope occurrences =
  let rec loop expected_index = function
    | [] -> Ok ()
    | occurrence :: rest -> (
        if
          Function_expression_binding.occurrence_index occurrence
          <> expected_index
        then
          Error
            (invalid_input
               "function expression occurrence indexes are not contiguous")
        else
          match
            Function_expression_binding.occurrence_resolution occurrence
          with
          | Function_expression_binding.Nonlocal_candidate ->
              loop (expected_index + 1) rest
          | Function_expression_binding.Function_binding binding -> (
              match validate_local_binding table function_scope binding with
              | Error _ as error -> error
              | Ok () -> loop (expected_index + 1) rest))
  in
  loop 0 occurrences

let validate_function table parent publication_symbols previous_item function_ =
  let symbol = Function_expression_binding.function_symbol function_ in
  let scope = Function_expression_binding.function_scope function_ in
  let item_index = Function_expression_binding.function_item_index function_ in
  if item_index <= previous_item then
    Error
      (invalid_input "function expression results do not follow source order")
  else if not (Symbol_table.owns_symbol table symbol) then
    Error
      (invalid_input "function expression owner belongs to another symbol table")
  else if not (Symbol_table.owns_scope table scope) then
    Error
      (invalid_input "function expression scope belongs to another symbol table")
  else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error (invalid_input "function expression owner is not a function")
  else if Symbol_table.scope_kind scope <> Symbol_table.Function then
    Error (invalid_input "function expression result has a nonfunction scope")
  else if not (symbol_in_scope symbol parent) then
    Error (invalid_input "function expression owner has the wrong module scope")
  else if
    match Symbol_table.parent scope with
    | Some scope_parent -> not (same_scope scope_parent parent)
    | None -> true
  then
    Error
      (invalid_input "function expression scope has the wrong module parent")
  else if not (Int_set.mem (symbol_number symbol) publication_symbols) then
    Error (missing_function_publication symbol)
  else
    match
      validate_occurrences table scope
        (Function_expression_binding.function_occurrences function_)
    with
    | Error _ as error -> error
    | Ok () -> Ok item_index

let validate_functions table parent publication_symbols expressions =
  let rec loop previous_item = function
    | [] -> Ok ()
    | function_ :: rest -> (
        match
          validate_function table parent publication_symbols previous_item
            function_
        with
        | Error _ as error -> error
        | Ok item_index -> loop item_index rest)
  in
  loop (-1) (Function_expression_binding.functions expressions)

let add_publication environment publication =
  String_map.add (Symbol.name publication.source_symbol) publication environment

let rec publish_through item_index environment publications =
  match publications with
  | publication :: rest when publication.item_index <= item_index ->
      publish_through item_index (add_publication environment publication) rest
  | _ -> (environment, publications)

let resolve_occurrence environment source =
  let resolution =
    match Function_expression_binding.occurrence_resolution source with
    | Function_expression_binding.Function_binding binding ->
        Local_binding binding
    | Function_expression_binding.Nonlocal_candidate -> (
        match
          String_map.find_opt
            (Function_expression_binding.occurrence_name source)
            environment
        with
        | Some publication -> Module_binding publication
        | None -> Outer_candidate)
  in
  { source; resolution }

let resolve_validated expressions publications =
  let rec loop environment remaining_publications functions_rev by_symbol =
    function
    | [] -> (List.rev functions_rev, by_symbol)
    | source :: rest ->
        let environment, remaining_publications =
          publish_through
            (Function_expression_binding.function_item_index source)
            environment remaining_publications
        in
        let function_ =
          {
            source;
            occurrences =
              Function_expression_binding.function_occurrences source
              |> List.map (resolve_occurrence environment);
          }
        in
        loop environment remaining_publications
          (function_ :: functions_rev)
          (Int_map.add
             (symbol_number
                (Function_expression_binding.function_symbol source))
             function_ by_symbol)
          rest
  in
  loop String_map.empty publications [] Int_map.empty
    (Function_expression_binding.functions expressions)

let resolve ~table ~parent ~expressions publications =
  if not (Symbol_table.owns_scope table parent) then
    Error
      (invalid_input "module expression parent belongs to another symbol table")
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error (invalid_input "module expression binding requires a module scope")
  else
    match validate_publications table parent publications with
    | Error _ as error -> error
    | Ok publication_symbols -> (
        match
          validate_functions table parent publication_symbols expressions
        with
        | Error _ as error -> error
        | Ok () ->
            let functions, by_symbol =
              resolve_validated expressions publications
            in
            Ok { table; publications; functions; by_symbol })

let find_function result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_symbol with
    | Some function_ when function_symbol function_ == symbol -> Some function_
    | Some _ | None -> None
