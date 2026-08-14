type resolution = Module_binding_environment.resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type event = {
  name : string;
  origin : Symbol.origin;
  occurrence_index : int;
  parameter_index : int;
}

type parameter_input = {
  parameter : Function_type_resolution.parameter;
  events : event list;
}

type function_input = {
  declaration : Function_resolution.resolved_declaration;
  parameters : parameter_input list;
}

type occurrence = { source : event; resolution : resolution }

type resolved_parameter = {
  source : parameter_input;
  occurrences : occurrence list;
}

type resolved_function = {
  source : function_input;
  point : Module_binding_environment.point;
  parameters : resolved_parameter list;
}

module Int_map = Map.Make (Int)

type t = {
  table : Symbol_table.t;
  environment : Outer_environment.t;
  expressions : Module_expression_binding.t;
  source_functions : Function_resolution.t;
  functions : resolved_function list;
  by_source_symbol : resolved_function Int_map.t;
}

type error_kind =
  | Invalid_input of string
  | Unresolved_identifier of {
      function_symbol : Symbol.t;
      parameter_index : int;
      name : string;
      compilation_mode : Outer_environment.compilation_mode;
    }

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let invalid_input message =
  { code = "HCSEMA0029"; kind = Invalid_input message; origin = None }

let unresolved_identifier function_symbol event compilation_mode =
  {
    code = "HCSEMA0030";
    kind =
      Unresolved_identifier
        {
          function_symbol;
          parameter_index = event.parameter_index;
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
      { function_symbol; parameter_index; name; compilation_mode } ->
      Printf.sprintf
        "default for parameter %d of function %S uses ordinary identifier %S, \
         which is absent from the visible module records and the complete %s \
         outer table chain"
        parameter_index
        (Symbol.name function_symbol)
        name
        (Outer_environment.compilation_mode_name compilation_mode)

let error_to_string error = error.code ^ ": " ^ error_message error
let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int
let same_symbol left right = Symbol.Id.equal (Symbol.id left) (Symbol.id right)

let function_of_declaration declaration =
  declaration |> Function_resolution.resolved_declaration_site
  |> Function_resolution.declaration_site_function

let source_symbol_of_input input =
  input.declaration |> function_of_declaration
  |> Function_type_resolution.function_symbol

let canonical_symbol_of_input input =
  Function_resolution.resolved_declaration_identity_symbol input.declaration

let item_index_of_input input =
  input.declaration |> function_of_declaration
  |> Function_type_resolution.function_item_index

let make_identifier ~name ~origin ~occurrence_index ~parameter_index =
  if String.equal name "" then Error "function default identifier cannot be empty"
  else if occurrence_index < 0 then
    Error "function default occurrence index cannot be negative"
  else if parameter_index < 0 then
    Error "function default parameter index cannot be negative"
  else Ok { name; origin; occurrence_index; parameter_index }

let make_parameter ~parameter events = Ok { parameter; events }
let make_function ~declaration parameters = Ok { declaration; parameters }
let functions result = result.functions
let environment result = result.environment
let expressions result = result.expressions
let source_functions result = result.source_functions
let compilation_mode result = Function_resolution.compilation_mode result.source_functions
let owns_table result table = result.table == table
let function_declaration (function_ : resolved_function) = function_.source.declaration

let function_publication (function_ : resolved_function) =
  Module_binding_environment.point_publication function_.point

let function_source_symbol (function_ : resolved_function) =
  source_symbol_of_input function_.source

let function_canonical_symbol (function_ : resolved_function) =
  canonical_symbol_of_input function_.source

let function_item_index (function_ : resolved_function) =
  item_index_of_input function_.source

let function_parameters (function_ : resolved_function) = function_.parameters
let parameter_source (parameter : resolved_parameter) = parameter.source.parameter

let parameter_index (parameter : resolved_parameter) =
  Function_type_resolution.parameter_index parameter.source.parameter

let parameter_default (parameter : resolved_parameter) =
  Function_type_resolution.parameter_default parameter.source.parameter

let parameter_occurrences (parameter : resolved_parameter) =
  parameter.occurrences

let occurrence_index (occurrence : occurrence) =
  occurrence.source.occurrence_index

let occurrence_parameter_index (occurrence : occurrence) =
  occurrence.source.parameter_index

let occurrence_name (occurrence : occurrence) = occurrence.source.name
let occurrence_origin (occurrence : occurrence) = occurrence.source.origin
let occurrence_resolution (occurrence : occurrence) = occurrence.resolution

let same_declaration left right =
  let left_site = Function_resolution.resolved_declaration_site left in
  let right_site = Function_resolution.resolved_declaration_site right in
  let left_function = Function_resolution.declaration_site_function left_site in
  let right_function = Function_resolution.declaration_site_function right_site in
  same_symbol
    (Function_type_resolution.function_symbol left_function)
    (Function_type_resolution.function_symbol right_function)
  && same_symbol
       (Function_resolution.resolved_declaration_identity_symbol left)
       (Function_resolution.resolved_declaration_identity_symbol right)
  && Function_type_resolution.function_item_index left_function
     = Function_type_resolution.function_item_index right_function
  && Function_resolution.declaration_site_kind left_site
     = Function_resolution.declaration_site_kind right_site
  && Function_resolution.declaration_site_state left_site
     = Function_resolution.declaration_site_state right_site

let validate_event expected_occurrence parameter_index event =
  if event.occurrence_index <> expected_occurrence then
    Error
      (invalid_input "function default occurrence indexes are not contiguous")
  else if event.parameter_index <> parameter_index then
    Error
      (invalid_input
         "function default identifier has the wrong parameter index")
  else if String.equal event.name "" then
    Error (invalid_input "function default identifier is empty")
  else if expected_occurrence = max_int then
    Error
      (invalid_input "function default occurrence identity space is exhausted")
  else Ok (expected_occurrence + 1)

let validate_events expected_occurrence parameter =
  let parameter_index =
    Function_type_resolution.parameter_index parameter.parameter
  in
  match
    ( Function_type_resolution.parameter_default parameter.parameter,
      parameter.events )
  with
  | None, _ :: _ ->
      Error (invalid_input "a parameter without a default contains identifiers")
  | Some (Function_type_resolution.Lastclass_default _), _ :: _ ->
      Error (invalid_input "a lastclass default contains ordinary identifiers")
  | None, [] | Some (Function_type_resolution.Lastclass_default _), [] ->
      Ok expected_occurrence
  | Some (Function_type_resolution.Expression_default _), events ->
      let rec loop expected = function
        | [] -> Ok expected
        | event :: rest -> (
            match validate_event expected parameter_index event with
            | Error _ as error -> error
            | Ok next -> loop next rest)
      in
      loop expected_occurrence events

let validate_parameters expected input =
  let rec pair expected_occurrence = function
    | [], [] -> Ok ()
    | expected :: expected_rest, parameter :: input_rest ->
        if expected != parameter.parameter then
          Error
            (invalid_input
               "function default parameters do not match the resolved header")
        else
          (match validate_events expected_occurrence parameter with
          | Error _ as error -> error
          | Ok next -> pair next (expected_rest, input_rest))
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input
             "function default parameter count does not match the resolved \
              header")
  in
  pair 0 (expected, input)

let validate_publication table module_environment input =
  let source_symbol = source_symbol_of_input input in
  let canonical_symbol = canonical_symbol_of_input input in
  let source_function = function_of_declaration input.declaration in
  let scope = Function_type_resolution.function_scope source_function in
  if
    not
      (Symbol_table.owns_symbol table source_symbol
      && Symbol_table.owns_symbol table canonical_symbol)
  then Error (invalid_input "function default owner belongs to another symbol table")
  else if not (Symbol_table.owns_scope table scope) then
    Error
      (invalid_input "function default scope belongs to another symbol table")
  else
    match Module_binding_environment.find_point module_environment source_symbol with
    | None ->
        Error
          (invalid_input "function default owner has no module publication")
    | Some point ->
        let publication = Module_binding_environment.point_publication point in
        if
          Module_expression_binding.publication_kind publication
          <> Module_expression_binding.Function
        then Error (invalid_input "function default publication is not a function")
        else if
          not
            (same_symbol
               (Module_expression_binding.publication_source_symbol publication)
               source_symbol
            && same_symbol
                 (Module_expression_binding.publication_canonical_symbol
                    publication)
                 canonical_symbol)
        then Error (invalid_input "function default publication has the wrong identity")
        else if
          Module_expression_binding.publication_item_index publication
          <> Function_type_resolution.function_item_index source_function
          || Module_expression_binding.publication_declarator_index publication
             <> None
        then
          Error
            (invalid_input
               "function default publication has the wrong source position")
        else Ok point

let validate_inputs table module_environment source_functions inputs =
  let expected = Function_resolution.declarations source_functions in
  let rec pair previous_publication paired_rev = function
    | [], [] -> Ok (List.rev paired_rev)
    | expected :: expected_rest, input :: input_rest ->
        if not (same_declaration expected input.declaration) then
          Error
            (invalid_input
               "function default inputs do not match function identity \
                resolution")
        else
          let expected_parameters =
            expected |> function_of_declaration
            |> Function_type_resolution.function_signature
            |> Function_type_resolution.signature_parameters
          in
          (match validate_parameters expected_parameters input.parameters with
          | Error _ as error -> error
          | Ok () -> (
              match validate_publication table module_environment input with
              | Error _ as error -> error
              | Ok point ->
                  let publication_index =
                    point |> Module_binding_environment.point_publication
                    |> Module_expression_binding.publication_declaration_index
                  in
                  if publication_index <= previous_publication then
                    Error
                      (invalid_input
                         "function default publications are not source ordered")
                  else
                    pair publication_index ((input, point) :: paired_rev)
                      (expected_rest, input_rest)))
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input
             "function default input count does not match function identity \
              resolution")
  in
  pair (-1) [] (expected, inputs)

let resolve_event environment cursor function_symbol event =
  match Module_binding_environment.resolve cursor event.name with
  | Some resolution -> Ok { source = event; resolution }
  | None ->
      Error
        (unresolved_identifier function_symbol event
           (Outer_environment.compilation_mode environment))

let resolve_parameters environment cursor function_symbol parameters =
  let rec resolve_events occurrences_rev = function
    | [] -> Ok (List.rev occurrences_rev)
    | event :: rest -> (
        match resolve_event environment cursor function_symbol event with
        | Error _ as error -> error
        | Ok occurrence -> resolve_events (occurrence :: occurrences_rev) rest)
  in
  let rec loop parameters_rev = function
    | [] -> Ok (List.rev parameters_rev)
    | source :: rest -> (
        match resolve_events [] source.events with
        | Error _ as error -> error
        | Ok occurrences ->
            loop ({ source; occurrences } :: parameters_rev) rest)
  in
  loop [] parameters

let resolve_inputs module_environment paired =
  let environment = Module_binding_environment.environment module_environment in
  let rec loop cursor functions_rev by_source_symbol = function
    | [] -> Ok (List.rev functions_rev, by_source_symbol)
    | (source, point) :: rest -> (
        match Module_binding_environment.publish_through cursor point with
        | Error message -> Error (invalid_input message)
        | Ok cursor ->
            let source_symbol = source_symbol_of_input source in
            (match
               resolve_parameters environment cursor source_symbol
                 source.parameters
             with
            | Error _ as error -> error
            | Ok parameters ->
                let function_ = { source; point; parameters } in
                loop cursor (function_ :: functions_rev)
                  (Int_map.add (symbol_number source_symbol) function_
                     by_source_symbol)
                  rest))
  in
  loop (Module_binding_environment.initial_cursor module_environment) []
    Int_map.empty paired

let resolve ~table ~environment ~expressions ~functions:source_functions inputs
    =
  if
    Function_resolution.compilation_mode source_functions
    <> Module_expression_binding.compilation_mode expressions
  then
    Error
      (invalid_input
         "function identities and module expressions use different compilation \
          modes")
  else
    match Module_binding_environment.create ~table ~environment ~expressions with
    | Error message -> Error (invalid_input message)
    | Ok module_environment -> (
        match
          validate_inputs table module_environment source_functions inputs
        with
        | Error _ as error -> error
        | Ok paired -> (
            match resolve_inputs module_environment paired with
            | Error _ as error -> error
            | Ok (functions, by_source_symbol) ->
                Ok
                  {
                    table;
                    environment;
                    expressions;
                    source_functions;
                    functions;
                    by_source_symbol;
                  }))

let find_function result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_source_symbol with
    | Some function_ when function_source_symbol function_ == symbol ->
        Some function_
    | Some _ | None -> None
