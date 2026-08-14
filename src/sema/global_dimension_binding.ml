type resolution = Global_binding_environment.resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type event = {
  name : string;
  origin : Symbol.origin;
  occurrence_index : int;
  dimension_index : int;
}

type dimension_input = {
  dimension : Global_type_resolution.array_dimension;
  events : event list;
}

type global_input = {
  record : Global_resolution.global_record;
  dimensions : dimension_input list;
}

type occurrence = { source : event; resolution : resolution }

type resolved_dimension = {
  source : dimension_input;
  occurrences : occurrence list;
}

type resolved_global = {
  source : global_input;
  publication : Module_expression_binding.publication;
  dimensions : resolved_dimension list;
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
      dimension_index : int;
      name : string;
      compilation_mode : Outer_environment.compilation_mode;
    }

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let invalid_input message =
  { code = "HCSEMA0027"; kind = Invalid_input message; origin = None }

let unresolved_identifier global_symbol event compilation_mode =
  {
    code = "HCSEMA0028";
    kind =
      Unresolved_identifier
        {
          global_symbol;
          dimension_index = event.dimension_index;
          name = event.name;
          compilation_mode;
        };
    origin = Some event.origin;
  }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message
  | Unresolved_identifier
      { global_symbol; dimension_index; name; compilation_mode } ->
      Printf.sprintf
        "global array dimension %d for %S uses ordinary identifier %S, which \
         is absent from the visible module records and the complete %s outer \
         table chain"
        dimension_index
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

let make_identifier ~name ~origin ~occurrence_index ~dimension_index =
  if String.equal name "" then
    Error "global array extent identifier cannot be empty"
  else if occurrence_index < 0 then
    Error "global array extent occurrence index cannot be negative"
  else if dimension_index < 0 then
    Error "global array extent dimension index cannot be negative"
  else Ok { name; origin; occurrence_index; dimension_index }

let make_dimension ~dimension events = Ok { dimension; events }
let make_global ~record dimensions = Ok { record; dimensions }
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

let global_dimensions (global : resolved_global) = global.dimensions

let dimension_source (dimension : resolved_dimension) =
  dimension.source.dimension

let dimension_index (dimension : resolved_dimension) =
  Global_type_resolution.array_dimension_index dimension.source.dimension

let dimension_origin (dimension : resolved_dimension) =
  Global_type_resolution.array_dimension_origin dimension.source.dimension

let dimension_opening_origin (dimension : resolved_dimension) =
  Global_type_resolution.array_dimension_opening_origin
    dimension.source.dimension

let dimension_expression_origin (dimension : resolved_dimension) =
  Global_type_resolution.array_dimension_expression_origin
    dimension.source.dimension

let dimension_closing_origin (dimension : resolved_dimension) =
  Global_type_resolution.array_dimension_closing_origin
    dimension.source.dimension

let dimension_occurrences (dimension : resolved_dimension) =
  dimension.occurrences

let occurrence_index (occurrence : occurrence) =
  occurrence.source.occurrence_index

let occurrence_dimension_index (occurrence : occurrence) =
  occurrence.source.dimension_index

let occurrence_name (occurrence : occurrence) = occurrence.source.name
let occurrence_origin (occurrence : occurrence) = occurrence.source.origin
let occurrence_resolution (occurrence : occurrence) = occurrence.resolution
let same_symbol left right = Symbol.Id.equal (Symbol.id left) (Symbol.id right)

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

let same_dimension left right =
  Global_type_resolution.array_dimension_index left
  = Global_type_resolution.array_dimension_index right
  && Global_type_resolution.array_dimension_origin left
     = Global_type_resolution.array_dimension_origin right
  && Global_type_resolution.array_dimension_opening_origin left
     = Global_type_resolution.array_dimension_opening_origin right
  && Global_type_resolution.array_dimension_expression_origin left
     = Global_type_resolution.array_dimension_expression_origin right
  && Global_type_resolution.array_dimension_closing_origin left
     = Global_type_resolution.array_dimension_closing_origin right

let validate_events expected_occurrence dimension =
  let dimension_index =
    Global_type_resolution.array_dimension_index dimension.dimension
  in
  if
    Option.is_none
      (Global_type_resolution.array_dimension_expression_origin
         dimension.dimension)
    && dimension.events <> []
  then
    Error (invalid_input "an empty global array dimension contains identifiers")
  else
    let rec loop expected = function
      | [] -> Ok expected
      | event :: rest ->
          if event.occurrence_index <> expected then
            Error
              (invalid_input
                 "global array extent occurrence indexes are not contiguous")
          else if event.dimension_index <> dimension_index then
            Error
              (invalid_input
                 "global array extent identifier has the wrong dimension index")
          else if String.equal event.name "" then
            Error (invalid_input "global array extent identifier is empty")
          else if expected = max_int then
            Error
              (invalid_input
                 "global array extent occurrence identity space is exhausted")
          else loop (expected + 1) rest
    in
    loop expected_occurrence dimension.events

let validate_dimensions semantic (input : global_input) =
  let rec pair expected_occurrence = function
    | [], [] -> Ok expected_occurrence
    | semantic :: semantic_rest, dimension :: input_rest -> (
        if not (same_dimension semantic dimension.dimension) then
          Error
            (invalid_input
               "global array extent dimensions do not match the global record")
        else
          match validate_events expected_occurrence dimension with
          | Error _ as error -> error
          | Ok next -> pair next (semantic_rest, input_rest))
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input
             "global array extent dimension count does not match the global \
              record")
  in
  pair 0 (semantic, input.dimensions) |> Result.map ignore

let validate_inputs table paired inputs =
  let rec pair = function
    | [], [] -> Ok ()
    | expected :: expected_rest, input :: input_rest -> (
        let record = Global_binding_environment.global_record expected in
        let symbol = global_symbol_of_input input in
        if not (same_record record input.record) then
          Error
            (invalid_input
               "global array extent inputs do not match the global records")
        else if not (Symbol_table.owns_symbol table symbol) then
          Error
            (invalid_input
               "global array extent symbol belongs to another symbol table")
        else if
          not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Global_variable)
        then
          Error
            (invalid_input "global array extent owner is not a global variable")
        else
          let semantic =
            global_data input |> Global_type_resolution.global_array_dimensions
          in
          match validate_dimensions semantic input with
          | Error _ as error -> error
          | Ok () -> pair (expected_rest, input_rest))
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input
             "global array extent input count does not match the global records")
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

let resolve_dimensions environment cursor global_symbol dimensions =
  let rec loop resolved_rev = function
    | [] -> Ok (List.rev resolved_rev)
    | dimension :: rest -> (
        match
          resolve_events environment cursor global_symbol dimension.events
        with
        | Error _ as error -> error
        | Ok occurrences ->
            loop ({ source = dimension; occurrences } :: resolved_rev) rest)
  in
  loop [] dimensions

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
          Global_binding_environment.publish_before cursor paired_global
        with
        | Error message -> Error (invalid_input message)
        | Ok before_cursor -> (
            match
              resolve_dimensions environment before_cursor symbol
                input.dimensions
            with
            | Error _ as error -> error
            | Ok dimensions -> (
                match
                  Global_binding_environment.publish_through before_cursor
                    paired_global
                with
                | Error message -> Error (invalid_input message)
                | Ok cursor ->
                    let global =
                      {
                        source = input;
                        publication =
                          Global_binding_environment.global_publication
                            paired_global;
                        dimensions;
                      }
                    in
                    loop cursor (global :: globals_rev)
                      (Int_map.add (symbol_number symbol) global by_symbol)
                      paired_rest input_rest)))
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input
             "global array extent input count changed during resolution")
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
