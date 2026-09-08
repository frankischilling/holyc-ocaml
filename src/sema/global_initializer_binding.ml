type resolution = Global_binding_environment.resolution =
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

let global_source (global : resolved_global) =
  Option.bind
    (global_initializer_of_input global.source)
    Global_type_resolution.initializer_source

let global_leaves global =
  match global_source global with
  | None -> []
  | Some source -> Initializer_source.leaves source

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
  let source =
    Option.bind
      (global_initializer_of_input input)
      Global_type_resolution.initializer_source
  in
  let manifest_matches =
    match source with
    | None ->
        Global_type_resolution.global_array_dimensions (global_data input) = []
        || Option.is_none (global_initializer_of_input input)
    | Some source ->
        let expected =
          Initializer_source.leaves source
          |> List.concat_map (fun leaf ->
              Initializer_source.leaf_identifiers leaf
              |> List.map (fun (name, origin) ->
                  (Initializer_source.leaf_path leaf, name, origin)))
        in
        let actual =
          List.map
            (fun event -> (event.initializer_path, event.name, event.origin))
            input.events
        in
        actual = expected
  in
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
  if not manifest_matches then
    Error
      (invalid_input
         "global initializer occurrences do not match the retained source \
          manifest")
  else loop 0 input.events

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

let validate_inputs table paired inputs =
  let rec pair = function
    | [], [] -> Ok ()
    | expected :: expected_rest, input :: input_rest -> (
        let record = Global_binding_environment.global_record expected in
        let symbol = global_symbol_of_input input in
        if not (same_record record input.record) then
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
        else
          match validate_events input with
          | Error _ as error -> error
          | Ok () -> pair (expected_rest, input_rest))
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input
             "global initializer input count does not match the global records")
  in
  pair (paired, inputs)

let resolve_event environment cursor global_symbol event =
  match Global_binding_environment.resolve cursor event.name with
  | Some resolution -> Ok { source = event; resolution }
  | None ->
      Error
        (unresolved_identifier global_symbol event
           (Outer_environment.compilation_mode environment))

let resolve_events environment cursor global_symbol events =
  let rec loop resolved_rev = function
    | [] -> Ok (List.rev resolved_rev)
    | event :: rest -> (
        match resolve_event environment cursor global_symbol event with
        | Error _ as error -> error
        | Ok occurrence -> loop (occurrence :: resolved_rev) rest)
  in
  loop [] events

let resolve_inputs binding_environment inputs =
  let environment =
    Global_binding_environment.environment binding_environment
  in
  let rec loop cursor globals_rev by_symbol paired inputs =
    match (paired, inputs) with
    | [], [] -> Ok (List.rev globals_rev, by_symbol)
    | paired_global :: paired_rest, input :: input_rest -> (
        let symbol = global_symbol_of_input input in
        match
          Global_binding_environment.publish_through cursor paired_global
        with
        | Error message -> Error (invalid_input message)
        | Ok cursor -> (
            match resolve_events environment cursor symbol input.events with
            | Error _ as error -> error
            | Ok occurrences ->
                let global =
                  {
                    source = input;
                    publication =
                      Global_binding_environment.global_publication
                        paired_global;
                    occurrences;
                  }
                in
                loop cursor (global :: globals_rev)
                  (Int_map.add (symbol_number symbol) global by_symbol)
                  paired_rest input_rest))
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input
             "global initializer input count changed during resolution")
  in
  loop
    (Global_binding_environment.initial_cursor binding_environment)
    [] Int_map.empty
    (Global_binding_environment.globals binding_environment)
    inputs

let resolve ~table ~environment ~expressions ~globals inputs =
  match
    Global_binding_environment.create ~table ~environment ~expressions ~globals
  with
  | Error message -> Error (invalid_input message)
  | Ok binding_environment -> (
      let paired = Global_binding_environment.globals binding_environment in
      match validate_inputs table paired inputs with
      | Error _ as error -> error
      | Ok () -> (
          match resolve_inputs binding_environment inputs with
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
                }))

let find_global result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_symbol with
    | Some global when global_symbol global == symbol -> Some global
    | Some _ | None -> None
