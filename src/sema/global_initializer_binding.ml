type resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type event = {
  name : string;
  origin : Symbol.origin;
  occurrence_index : int;
  initializer_path : int list;
}

type global_input = {
  record : Global_resolution.global_record;
  events : event list;
}

type occurrence = { source : event; resolution : resolution }

type resolved_global = {
  source : global_input;
  publication : Module_expression_binding.publication;
  occurrences : occurrence list;
}

module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)
module String_map = Map.Make (String)

type t = {
  table : Symbol_table.t;
  environment : Outer_environment.t;
  expressions : Module_expression_binding.t;
  source_globals : Global_resolution.t;
  globals : resolved_global list;
  by_symbol : resolved_global Int_map.t;
}

type error_kind =
  | Invalid_input of string
  | Unresolved_identifier of {
      global_symbol : Symbol.t;
      name : string;
      compilation_mode : Outer_environment.compilation_mode;
    }

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let invalid_input message =
  { code = "HCSEMA0025"; kind = Invalid_input message; origin = None }

let unresolved_identifier global_symbol event compilation_mode =
  {
    code = "HCSEMA0026";
    kind =
      Unresolved_identifier
        { global_symbol; name = event.name; compilation_mode };
    origin = Some event.origin;
  }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message
  | Unresolved_identifier { global_symbol; name; compilation_mode } ->
      Printf.sprintf
        "global initializer for %S uses ordinary identifier %S, which is \
         absent from the visible module records and the complete %s outer \
         table chain"
        (Symbol.name global_symbol)
        name
        (Outer_environment.compilation_mode_name compilation_mode)

let error_to_string error = error.code ^ ": " ^ error_message error
let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int
let global_data input = Global_resolution.global_record_global input.record

let global_symbol_of_input input =
  Global_resolution.global_record_symbol input.record

let global_item_of_input input =
  Global_type_resolution.global_item_index (global_data input)

let global_declarator_of_input input =
  Global_type_resolution.global_declarator_index (global_data input)

let global_initializer_of_input input =
  Global_type_resolution.global_initializer (global_data input)

let make_identifier ~name ~origin ~occurrence_index ~initializer_path =
  if String.equal name "" then
    Error "global initializer identifier cannot be empty"
  else if occurrence_index < 0 then
    Error "global initializer occurrence index cannot be negative"
  else if List.exists (fun index -> index < 0) initializer_path then
    Error "global initializer path cannot contain a negative index"
  else Ok { name; origin; occurrence_index; initializer_path }

let make_global ~record events = Ok { record; events }
let globals result = result.globals
let environment result = result.environment
let expressions result = result.expressions
let source_globals result = result.source_globals
let owns_table result table = result.table == table
let global_record (global : resolved_global) = global.source.record
let global_publication (global : resolved_global) = global.publication

let global_symbol (global : resolved_global) =
  global_symbol_of_input global.source

let global_item_index (global : resolved_global) =
  global_item_of_input global.source

let global_declarator_index (global : resolved_global) =
  global_declarator_of_input global.source

let global_initializer_origin (global : resolved_global) =
  global_initializer_of_input global.source
  |> Option.map Global_type_resolution.initializer_origin

let global_occurrences (global : resolved_global) = global.occurrences

let occurrence_index (occurrence : occurrence) =
  occurrence.source.occurrence_index

let occurrence_name (occurrence : occurrence) = occurrence.source.name
let occurrence_origin (occurrence : occurrence) = occurrence.source.origin

let occurrence_initializer_path (occurrence : occurrence) =
  occurrence.source.initializer_path

let occurrence_resolution (occurrence : occurrence) = occurrence.resolution
let same_symbol left right = Symbol.Id.equal (Symbol.id left) (Symbol.id right)

let validate_events input =
  let rec loop expected = function
    | [] -> Ok ()
    | event :: rest ->
        if event.occurrence_index <> expected then
          Error
            (invalid_input
               "global initializer occurrence indexes are not contiguous")
        else if String.equal event.name "" then
          Error (invalid_input "global initializer identifier is empty")
        else if List.exists (fun index -> index < 0) event.initializer_path then
          Error
            (invalid_input "global initializer path contains a negative index")
        else loop (expected + 1) rest
  in
  loop 0 input.events

let same_record left right =
  let left_global = Global_resolution.global_record_global left in
  let right_global = Global_resolution.global_record_global right in
  same_symbol
    (Global_resolution.global_record_symbol left)
    (Global_resolution.global_record_symbol right)
  && Global_type_resolution.global_item_index left_global
     = Global_type_resolution.global_item_index right_global
  && Global_type_resolution.global_declarator_index left_global
     = Global_type_resolution.global_declarator_index right_global
  && Global_resolution.global_record_kind left
     = Global_resolution.global_record_kind right

let validate_inputs table globals inputs =
  let rec pair previous_item previous_declarator = function
    | [], [] -> Ok ()
    | expected :: expected_rest, input :: input_rest -> (
        let symbol = global_symbol_of_input input in
        let item_index = global_item_of_input input in
        let declarator_index = global_declarator_of_input input in
        let ordered =
          item_index > previous_item
          || item_index = previous_item
             &&
             match (previous_declarator, declarator_index) with
             | None, Some _ -> true
             | Some left, Some right -> right > left
             | None, None | Some _, None -> false
        in
        if not (same_record expected input.record) then
          Error
            (invalid_input
               "global initializer inputs do not match the global records")
        else if not (Symbol_table.owns_symbol table symbol) then
          Error
            (invalid_input
               "global initializer symbol belongs to another symbol table")
        else if
          not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Global_variable)
        then
          Error
            (invalid_input "global initializer owner is not a global variable")
        else if not ordered then
          Error
            (invalid_input "global initializer inputs are not source ordered")
        else
          match validate_events input with
          | Error _ as error -> error
          | Ok () -> pair item_index declarator_index (expected_rest, input_rest)
        )
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input
             "global initializer input count does not match the global records")
  in
  pair (-1) None (Global_resolution.records globals, inputs)

let publication_map table expressions =
  let rec collect seen by_symbol previous_index = function
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
        if declaration_index <= previous_index then
          Error (invalid_input "module publications are not source ordered")
        else if Int_set.mem source_number seen then
          Error (invalid_input "module publication source symbol is repeated")
        else if
          not
            (Symbol_table.owns_symbol table source
            && Symbol_table.owns_symbol table canonical)
        then
          Error
            (invalid_input "module publication belongs to another symbol table")
        else
          collect
            (Int_set.add source_number seen)
            (Int_map.add source_number publication by_symbol)
            declaration_index rest
  in
  collect Int_set.empty Int_map.empty (-1)
    (Module_expression_binding.publications expressions)

let validate_global_publications publications inputs =
  let rec loop previous_index = function
    | [] -> Ok ()
    | input :: rest -> (
        let symbol = global_symbol_of_input input in
        let number = symbol_number symbol in
        match Int_map.find_opt number publications with
        | None ->
            Error
              (invalid_input
                 "global initializer owner has no module publication")
        | Some publication ->
            let source =
              Module_expression_binding.publication_source_symbol publication
            in
            let canonical =
              Module_expression_binding.publication_canonical_symbol publication
            in
            let publication_index =
              Module_expression_binding.publication_declaration_index
                publication
            in
            if
              Module_expression_binding.publication_kind publication
              <> Module_expression_binding.Global_variable
            then
              Error
                (invalid_input
                   "global initializer publication has the wrong record kind")
            else if
              not (same_symbol source symbol && same_symbol canonical symbol)
            then
              Error
                (invalid_input
                   "global initializer publication has the wrong identity")
            else if
              Module_expression_binding.publication_item_index publication
              <> global_item_of_input input
              || Module_expression_binding.publication_declarator_index
                   publication
                 <> global_declarator_of_input input
            then
              Error
                (invalid_input
                   "global initializer publication has the wrong source \
                    position")
            else if publication_index <= previous_index then
              Error
                (invalid_input
                   "global initializer publications are not source ordered")
            else loop publication_index rest)
  in
  loop (-1) inputs

let global_mode_matches globals mode =
  match (Global_resolution.compilation_mode globals, mode) with
  | Global_resolution.Jit, Function_resolution.Jit
  | Global_resolution.Aot, Function_resolution.Aot -> true
  | Global_resolution.Jit, Function_resolution.Aot
  | Global_resolution.Aot, Function_resolution.Jit -> false

let add_publication visible publication =
  String_map.add
    (Module_expression_binding.publication_source_symbol publication
    |> Symbol.name)
    publication visible

let rec publish_through declaration_index visible publications =
  match publications with
  | publication :: rest
    when Module_expression_binding.publication_declaration_index publication
         <= declaration_index ->
      publish_through declaration_index
        (add_publication visible publication)
        rest
  | _ -> (visible, publications)

let resolve_event environment visible global_symbol event =
  match String_map.find_opt event.name visible with
  | Some publication ->
      Ok { source = event; resolution = Module_binding publication }
  | None -> (
      match Outer_environment.find environment event.name with
      | Some binding ->
          Ok { source = event; resolution = Outer_binding binding }
      | None ->
          Error
            (unresolved_identifier global_symbol event
               (Outer_environment.compilation_mode environment)))

let resolve_events environment visible global_symbol events =
  let rec loop resolved_rev = function
    | [] -> Ok (List.rev resolved_rev)
    | event :: rest -> (
        match resolve_event environment visible global_symbol event with
        | Error _ as error -> error
        | Ok occurrence -> loop (occurrence :: resolved_rev) rest)
  in
  loop [] events

let resolve_inputs environment publications_by_symbol publications inputs =
  let rec loop visible remaining globals_rev by_symbol = function
    | [] -> Ok (List.rev globals_rev, by_symbol)
    | input :: rest -> (
        let symbol = global_symbol_of_input input in
        let publication =
          Int_map.find (symbol_number symbol) publications_by_symbol
        in
        let visible, remaining =
          publish_through
            (Module_expression_binding.publication_declaration_index publication)
            visible remaining
        in
        match resolve_events environment visible symbol input.events with
        | Error _ as error -> error
        | Ok occurrences ->
            let global = { source = input; publication; occurrences } in
            loop visible remaining (global :: globals_rev)
              (Int_map.add (symbol_number symbol) global by_symbol)
              rest)
  in
  loop String_map.empty publications [] Int_map.empty inputs

let resolve ~table ~environment ~expressions ~globals inputs =
  if not (Module_expression_binding.owns_table expressions table) then
    Error
      (invalid_input "module expression bindings belong to another symbol table")
  else if not (Outer_environment.owns_table environment table) then
    Error (invalid_input "outer environment belongs to another symbol table")
  else if
    Module_expression_binding.compilation_mode expressions
    <> Outer_environment.compilation_mode environment
  then
    Error
      (invalid_input
         "module expression bindings and outer environment use different \
          compilation modes")
  else if
    not
      (global_mode_matches globals
         (Module_expression_binding.compilation_mode expressions))
  then
    Error
      (invalid_input
         "global records and module expressions use different compilation modes")
  else
    match validate_inputs table globals inputs with
    | Error _ as error -> error
    | Ok () -> (
        match publication_map table expressions with
        | Error _ as error -> error
        | Ok publications_by_symbol -> (
            match
              validate_global_publications publications_by_symbol inputs
            with
            | Error _ as error -> error
            | Ok () -> (
                let publications =
                  Module_expression_binding.publications expressions
                in
                match
                  resolve_inputs environment publications_by_symbol publications
                    inputs
                with
                | Error _ as error -> error
                | Ok (resolved_globals, by_symbol) ->
                    Ok
                      {
                        table;
                        environment;
                        expressions;
                        source_globals = globals;
                        globals = resolved_globals;
                        by_symbol;
                      })))

let find_global result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_symbol with
    | Some global when global_symbol global == symbol -> Some global
    | Some _ | None -> None
