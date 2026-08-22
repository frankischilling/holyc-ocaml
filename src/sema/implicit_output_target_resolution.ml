module Int_map = Map.Make (Int)
module String_map = Map.Make (String)

type module_target = {
  publication : Module_expression_binding.publication;
  header : Function_type_resolution.resolved_function;
  declaration : Function_resolution.resolved_declaration;
  target_symbol : Symbol.t;
}

type target_binding =
  | Module_function of module_target
  | Outer_function of Outer_environment.binding

type output = {
  source_output : Function_call_expression_result.implicit_output_result;
  target_name : string;
  binding : target_binding;
}

type resolved_function = {
  source_function : Function_call_expression_result.resolved_function;
  outputs : output list;
}

type t = {
  table : Symbol_table.t;
  environment_ : Outer_environment.t;
  source_ : Function_call_expression_result.t;
  compilation_mode_ : Function_resolution.compilation_mode;
  functions_ : resolved_function list;
  by_symbol : resolved_function Int_map.t;
}

type error_kind =
  | Invalid_input of string
  | Missing_header of {
      target_name : string;
      output : Function_call_expression_result.implicit_output_result;
    }

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int
let same_symbol left right = Symbol.Id.equal (Symbol.id left) (Symbol.id right)

let invalid_input message =
  { code = "HCSEMA0047"; kind = Invalid_input message; origin = None }

let missing_header target_name output =
  let source = Function_call_expression_result.implicit_output_source output in
  {
    code = "HCSEMA0048";
    kind = Missing_header { target_name; output };
    origin =
      Some (Function_call_resolution.implicit_output_marker_origin source);
  }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message
  | Missing_header { target_name; _ } ->
      Printf.sprintf "implicit output requires a visible %s function header"
        target_name

let error_to_string error = error.code ^ ": " ^ error_message error
let owns_table result table = result.table == table
let environment result = result.environment_
let source result = result.source_
let compilation_mode result = result.compilation_mode_
let functions result = result.functions_
let function_source function_ = function_.source_function
let function_outputs function_ = function_.outputs
let output_source output = output.source_output
let output_target_name output = output.target_name
let output_binding output = output.binding
let module_publication target = target.publication
let module_header target = target.header
let module_declaration target = target.declaration
let module_target_symbol target = target.target_symbol

let target_binding_name = function
  | Module_function _ -> "module-function"
  | Outer_function binding ->
      binding |> Outer_environment.binding_table |> Outer_environment.table_kind
      |> Outer_environment.table_kind_name

let find_function result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_symbol with
    | Some function_
      when function_ |> function_source
           |> Function_call_expression_result.function_symbol
           |> same_symbol symbol -> Some function_
    | Some _ | None -> None

let target_name output =
  output |> Function_call_expression_result.implicit_output_source
  |> Function_call_resolution.implicit_output_target
  |> Function_call_resolution.implicit_output_target_name

let function_header_map table function_types =
  let rec loop map = function
    | [] -> Ok map
    | header :: rest ->
        let symbol = Function_type_resolution.function_symbol header in
        if not (Symbol_table.owns_symbol table symbol) then
          Error
            (invalid_input
               "implicit output function types belong to another symbol table")
        else loop (Int_map.add (symbol_number symbol) header map) rest
  in
  loop Int_map.empty (Function_type_resolution.functions function_types)

let function_declaration_map table functions =
  let rec loop map = function
    | [] -> Ok map
    | declaration :: rest ->
        let site = Function_resolution.resolved_declaration_site declaration in
        let header = Function_resolution.declaration_site_function site in
        let symbol = Function_type_resolution.function_symbol header in
        if not (Symbol_table.owns_symbol table symbol) then
          Error
            (invalid_input
               "implicit output function identities belong to another symbol \
                table")
        else loop (Int_map.add (symbol_number symbol) declaration map) rest
  in
  loop Int_map.empty (Function_resolution.declarations functions)

let add_visible_function visible publication =
  match Module_expression_binding.publication_kind publication with
  | Module_expression_binding.Function ->
      String_map.add
        (publication |> Module_expression_binding.publication_source_symbol
       |> Symbol.name)
        publication visible
  | Module_expression_binding.Aggregate
  | Module_expression_binding.Global_variable -> visible

let rec publish_through item_index visible = function
  | publication :: rest
    when Module_expression_binding.publication_item_index publication
         <= item_index ->
      publish_through item_index (add_visible_function visible publication) rest
  | publications -> (visible, publications)

let resolve_module_target headers declarations publication =
  let source_symbol =
    Module_expression_binding.publication_source_symbol publication
  in
  let target_symbol =
    Module_expression_binding.publication_canonical_symbol publication
  in
  let number = symbol_number source_symbol in
  match
    (Int_map.find_opt number headers, Int_map.find_opt number declarations)
  with
  | Some header, Some declaration
    when same_symbol
           (Function_resolution.resolved_declaration_identity_symbol declaration)
           target_symbol ->
      Ok (Module_function { publication; header; declaration; target_symbol })
  | Some _, Some _ ->
      Error
        (invalid_input
           "implicit output publication disagrees with function identity \
            resolution")
  | None, _ | _, None ->
      Error
        (invalid_input
           "implicit output publication has no active typed function header")

let validate_module_function_publications headers declarations
    module_expressions =
  let rec loop = function
    | [] -> Ok ()
    | publication :: rest -> (
        match Module_expression_binding.publication_kind publication with
        | Module_expression_binding.Aggregate
        | Module_expression_binding.Global_variable -> loop rest
        | Module_expression_binding.Function -> (
            match resolve_module_target headers declarations publication with
            | Error _ as error -> error
            | Ok _ -> loop rest))
  in
  loop (Module_expression_binding.publications module_expressions)

let resolve_output environment headers declarations visible source_output =
  let target_name = target_name source_output in
  match String_map.find_opt target_name visible with
  | Some publication -> (
      match resolve_module_target headers declarations publication with
      | Error _ as error -> error
      | Ok binding -> Ok { source_output; target_name; binding })
  | None -> (
      match
        Outer_environment.find_record environment ~name:target_name
          ~record_kind:Outer_environment.Function
      with
      | Some binding ->
          Ok { source_output; target_name; binding = Outer_function binding }
      | None -> Error (missing_header target_name source_output))

let resolve_outputs environment headers declarations visible source_function =
  let rec loop rev = function
    | [] -> Ok (List.rev rev)
    | output :: rest -> (
        match
          resolve_output environment headers declarations visible output
        with
        | Error _ as error -> error
        | Ok output -> loop (output :: rev) rest)
  in
  source_function |> Function_call_expression_result.function_implicit_outputs
  |> loop []

let validate_function_pair expected source =
  let expected_symbol = Module_expression_binding.function_symbol expected in
  let source_symbol = Function_call_expression_result.function_symbol source in
  if not (same_symbol expected_symbol source_symbol) then
    Error
      (invalid_input
         "implicit output functions do not match module expression bindings")
  else if
    Module_expression_binding.function_item_index expected
    <> Function_call_expression_result.function_item_index source
  then
    Error (invalid_input "implicit output function item positions do not match")
  else Ok ()

let resolve_functions environment headers declarations module_expressions
    expressions =
  let rec loop visible publications rev by_symbol expected sources =
    match (expected, sources) with
    | [], [] -> Ok (List.rev rev, by_symbol)
    | expected_function :: expected_rest, source_function :: source_rest -> (
        match validate_function_pair expected_function source_function with
        | Error _ as error -> error
        | Ok () -> (
            let item_index =
              Function_call_expression_result.function_item_index
                source_function
            in
            let visible, publications =
              publish_through item_index visible publications
            in
            match
              resolve_outputs environment headers declarations visible
                source_function
            with
            | Error _ as error -> error
            | Ok outputs ->
                let function_ = { source_function; outputs } in
                let symbol =
                  Function_call_expression_result.function_symbol
                    source_function
                in
                loop visible publications (function_ :: rev)
                  (Int_map.add (symbol_number symbol) function_ by_symbol)
                  expected_rest source_rest))
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input
             "implicit output expression results changed function shape")
  in
  loop String_map.empty
    (Module_expression_binding.publications module_expressions)
    [] Int_map.empty
    (Module_expression_binding.functions module_expressions)
    (Function_call_expression_result.functions expressions)

let resolve ~table ~environment ~module_expressions ~function_types ~functions
    ~expressions =
  let mode = Function_resolution.compilation_mode functions in
  if not (Outer_environment.owns_table environment table) then
    Error
      (invalid_input
         "implicit output environment belongs to another symbol table")
  else if not (Module_expression_binding.owns_table module_expressions table)
  then
    Error
      (invalid_input
         "implicit output module expressions belong to another symbol table")
  else if not (Function_call_expression_result.owns_table expressions table)
  then
    Error
      (invalid_input
         "implicit output expression results belong to another symbol table")
  else if Outer_environment.compilation_mode environment <> mode then
    Error
      (invalid_input
         "implicit output environment has the wrong compilation mode")
  else if Module_expression_binding.compilation_mode module_expressions <> mode
  then
    Error
      (invalid_input
         "implicit output module expressions have the wrong compilation mode")
  else if Function_call_expression_result.compilation_mode expressions <> mode
  then
    Error
      (invalid_input
         "implicit output expression results have the wrong compilation mode")
  else
    match function_header_map table function_types with
    | Error _ as error -> error
    | Ok headers -> (
        match function_declaration_map table functions with
        | Error _ as error -> error
        | Ok declarations -> (
            match
              validate_module_function_publications headers declarations
                module_expressions
            with
            | Error _ as error -> error
            | Ok () -> (
                match
                  resolve_functions environment headers declarations
                    module_expressions expressions
                with
                | Error _ as error -> error
                | Ok (functions_, by_symbol) ->
                    Ok
                      {
                        table;
                        environment_ = environment;
                        source_ = expressions;
                        compilation_mode_ = mode;
                        functions_;
                        by_symbol;
                      })))
