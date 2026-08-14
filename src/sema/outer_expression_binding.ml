type resolution =
  | Local_binding of Function_binding_index.binding
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type occurrence = {
  source : Module_expression_binding.occurrence;
  resolution : resolution;
}

type resolved_function = {
  source : Module_expression_binding.resolved_function;
  occurrences : occurrence list;
}

module Int_map = Map.Make (Int)

type t = {
  table : Symbol_table.t;
  environment : Outer_environment.t;
  source : Module_expression_binding.t;
  functions : resolved_function list;
  by_symbol : resolved_function Int_map.t;
}

type error_kind =
  | Invalid_input of string
  | Unresolved_identifier of {
      name : string;
      compilation_mode : Outer_environment.compilation_mode;
    }

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let invalid_input message =
  { code = "HCSEMA0023"; kind = Invalid_input message; origin = None }

let unresolved_identifier occurrence compilation_mode =
  {
    code = "HCSEMA0024";
    kind =
      Unresolved_identifier
        {
          name = Module_expression_binding.occurrence_name occurrence;
          compilation_mode;
        };
    origin = Some (Module_expression_binding.occurrence_origin occurrence);
  }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message
  | Unresolved_identifier { name; compilation_mode } ->
      Printf.sprintf
        "ordinary identifier %S is absent from the complete %s outer table \
         chain"
        name
        (Outer_environment.compilation_mode_name compilation_mode)

let error_to_string error = error.code ^ ": " ^ error_message error
let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int
let functions result = result.functions
let environment result = result.environment
let source result = result.source
let owns_table result table = result.table == table

let function_symbol (function_ : resolved_function) =
  Module_expression_binding.function_symbol function_.source

let function_scope (function_ : resolved_function) =
  Module_expression_binding.function_scope function_.source

let function_item_index (function_ : resolved_function) =
  Module_expression_binding.function_item_index function_.source

let function_occurrences (function_ : resolved_function) = function_.occurrences
let occurrence_source (occurrence : occurrence) = occurrence.source

let occurrence_index (occurrence : occurrence) =
  Module_expression_binding.occurrence_index occurrence.source

let occurrence_name (occurrence : occurrence) =
  Module_expression_binding.occurrence_name occurrence.source

let occurrence_origin (occurrence : occurrence) =
  Module_expression_binding.occurrence_origin occurrence.source

let occurrence_resolution (occurrence : occurrence) = occurrence.resolution

let resolve_occurrence environment source =
  match Module_expression_binding.occurrence_resolution source with
  | Module_expression_binding.Local_binding binding ->
      Ok { source; resolution = Local_binding binding }
  | Module_expression_binding.Module_binding publication ->
      Ok { source; resolution = Module_binding publication }
  | Module_expression_binding.Outer_candidate -> (
      match
        Outer_environment.find environment
          (Module_expression_binding.occurrence_name source)
      with
      | Some binding -> Ok { source; resolution = Outer_binding binding }
      | None ->
          Error
            (unresolved_identifier source
               (Outer_environment.compilation_mode environment)))

let resolve_occurrences environment occurrences =
  let rec loop resolved_rev = function
    | [] -> Ok (List.rev resolved_rev)
    | occurrence :: rest -> (
        match resolve_occurrence environment occurrence with
        | Error _ as error -> error
        | Ok resolved -> loop (resolved :: resolved_rev) rest)
  in
  loop [] occurrences

let resolve_functions environment functions =
  let rec loop functions_rev by_symbol = function
    | [] -> Ok (List.rev functions_rev, by_symbol)
    | source :: rest -> (
        match
          resolve_occurrences environment
            (Module_expression_binding.function_occurrences source)
        with
        | Error _ as error -> error
        | Ok occurrences ->
            let function_ = { source; occurrences } in
            loop
              (function_ :: functions_rev)
              (Int_map.add
                 (symbol_number
                    (Module_expression_binding.function_symbol source))
                 function_ by_symbol)
              rest)
  in
  loop [] Int_map.empty functions

let resolve ~table ~environment ~expressions =
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
  else
    match
      resolve_functions environment
        (Module_expression_binding.functions expressions)
    with
    | Error _ as error -> error
    | Ok (functions, by_symbol) ->
        Ok { table; environment; source = expressions; functions; by_symbol }

let find_function result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_symbol with
    | Some function_ when function_symbol function_ == symbol -> Some function_
    | Some _ | None -> None
