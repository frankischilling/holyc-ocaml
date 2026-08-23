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
  index : int;
  statement : Function_call_expression_result.top_level_statement_result;
  target : Function_call_resolution.implicit_output_target;
  fixed_source : Function_call_resolution.implicit_output_fixed_source;
  marker_origin : Symbol.origin;
  fixed_value : Function_call_expression_result.top_level_root_result;
  arguments : Function_call_expression_result.top_level_root_result list;
  target_name : string;
  binding : target_binding;
}

type t = {
  table : Symbol_table.t;
  environment_ : Outer_environment.t;
  source_ : Function_call_expression_result.top_level_t;
  compilation_mode_ : Function_resolution.compilation_mode;
  outputs_ : output list;
}

type error_kind =
  | Invalid_input of string
  | Missing_header of { target_name : string; output_index : int }

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

type source_output = {
  source_index : int;
  source_statement : Function_call_expression_result.top_level_statement_result;
  source_target : Function_call_resolution.implicit_output_target;
  source_fixed_source : Function_call_resolution.implicit_output_fixed_source;
  source_marker_origin : Symbol.origin;
  source_fixed_value : Function_call_expression_result.top_level_root_result;
  source_arguments : Function_call_expression_result.top_level_root_result list;
}

let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int
let same_symbol left right = Symbol.Id.equal (Symbol.id left) (Symbol.id right)

let invalid_input ?origin message =
  { code = "HCSEMA0058"; kind = Invalid_input message; origin }

let missing_header source target_name =
  {
    code = "HCSEMA0059";
    kind = Missing_header { target_name; output_index = source.source_index };
    origin = Some source.source_marker_origin;
  }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message
  | Missing_header { target_name; _ } ->
      Printf.sprintf
        "top-level implicit output requires a visible %s function header"
        target_name

let error_to_string error = error.code ^ ": " ^ error_message error
let owns_table result table = result.table == table
let environment result = result.environment_
let source result = result.source_
let compilation_mode result = result.compilation_mode_
let outputs result = result.outputs_
let output_index output = output.index
let output_statement output = output.statement
let output_target output = output.target
let output_fixed_source output = output.fixed_source
let output_marker_origin output = output.marker_origin
let output_fixed_value output = output.fixed_value
let output_arguments output = output.arguments
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

let function_header_map table function_types =
  let rec loop map = function
    | [] -> Ok map
    | header :: rest ->
        let symbol = Function_type_resolution.function_symbol header in
        if not (Symbol_table.owns_symbol table symbol) then
          Error
            (invalid_input
               "top-level implicit output function types belong to another \
                symbol table")
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
               "top-level implicit output function identities belong to \
                another symbol table")
        else loop (Int_map.add (symbol_number symbol) declaration map) rest
  in
  loop Int_map.empty (Function_resolution.declarations functions)

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
           "top-level implicit output publication disagrees with function \
            identity resolution")
  | None, _ | _, None ->
      Error
        (invalid_input
           "top-level implicit output publication has no active typed function \
            header")

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

let add_visible_function visible publication =
  match Module_expression_binding.publication_kind publication with
  | Module_expression_binding.Function ->
      String_map.add
        (publication |> Module_expression_binding.publication_source_symbol
       |> Symbol.name)
        publication visible
  | Module_expression_binding.Aggregate
  | Module_expression_binding.Global_variable -> visible

let rec publish_before item_index visible = function
  | publication :: rest
    when Module_expression_binding.publication_item_index publication
         < item_index ->
      publish_before item_index (add_visible_function visible publication) rest
  | publications -> (visible, publications)

let target_name target =
  Function_call_resolution.implicit_output_target_name target

let resolve_output environment headers declarations visible source =
  let name = target_name source.source_target in
  match String_map.find_opt name visible with
  | Some publication -> (
      match resolve_module_target headers declarations publication with
      | Error _ as error -> error
      | Ok binding ->
          Ok
            {
              index = source.source_index;
              statement = source.source_statement;
              target = source.source_target;
              fixed_source = source.source_fixed_source;
              marker_origin = source.source_marker_origin;
              fixed_value = source.source_fixed_value;
              arguments = source.source_arguments;
              target_name = name;
              binding;
            })
  | None -> (
      match
        Outer_environment.find_record environment ~name
          ~record_kind:Outer_environment.Function
      with
      | Some binding ->
          Ok
            {
              index = source.source_index;
              statement = source.source_statement;
              target = source.source_target;
              fixed_source = source.source_fixed_source;
              marker_origin = source.source_marker_origin;
              fixed_value = source.source_fixed_value;
              arguments = source.source_arguments;
              target_name = name;
              binding = Outer_function binding;
            }
      | None -> Error (missing_header source name))

let root_role root =
  root |> Function_call_expression_result.top_level_root_source
  |> Top_level_expression_tree.root_role

let collect_arguments output_index roots =
  let rec loop expected rev = function
    | root :: rest -> (
        match root_role root with
        | Top_level_expression_tree.Implicit_output_argument
            { output_index = actual_output; argument_index }
          when actual_output = output_index && argument_index = expected ->
            loop (expected + 1) (root :: rev) rest
        | Top_level_expression_tree.Implicit_output_argument _ ->
            Error
              (invalid_input
                 "top-level implicit output arguments are not contiguous")
        | _ -> Ok (List.rev rev, root :: rest))
    | [] -> Ok (List.rev rev, [])
  in
  loop 0 [] roots

let collect_statement_outputs expected_output statement =
  let rec loop expected rev = function
    | [] -> Ok (expected, List.rev rev)
    | root :: rest -> (
        match root_role root with
        | Top_level_expression_tree.Implicit_output_fixed
            { output_index; target; source; marker_origin } -> (
            if output_index <> expected then
              Error
                (invalid_input ~origin:marker_origin
                   "top-level implicit output indexes are not contiguous")
            else if expected = max_int then
              Error
                (invalid_input ~origin:marker_origin
                   "top-level implicit output identity space is exhausted")
            else
              match collect_arguments output_index rest with
              | Error _ as error -> error
              | Ok (arguments, rest) ->
                  loop (expected + 1)
                    ({
                       source_index = output_index;
                       source_statement = statement;
                       source_target = target;
                       source_fixed_source = source;
                       source_marker_origin = marker_origin;
                       source_fixed_value = root;
                       source_arguments = arguments;
                     }
                    :: rev)
                    rest)
        | Top_level_expression_tree.Implicit_output_argument _ ->
            Error
              (invalid_input
                 "top-level implicit output argument has no preceding fixed \
                  value")
        | Top_level_expression_tree.Expression_statement _
        | Top_level_expression_tree.Condition _
        | Top_level_expression_tree.Switch_selector _
        | Top_level_expression_tree.Switch_case_value _
        | Top_level_expression_tree.Local_array_dimension _
        | Top_level_expression_tree.Local_initializer _
        | Top_level_expression_tree.Return_value _ -> loop expected rev rest)
  in
  statement |> Function_call_expression_result.top_level_statement_roots
  |> loop expected_output []

let resolve_statements environment headers declarations module_expressions
    expressions =
  let rec loop visible publications next_output rev = function
    | [] -> Ok (List.rev rev)
    | statement :: rest -> (
        let item_index =
          statement
          |> Function_call_expression_result.top_level_statement_source
          |> Top_level_expression_tree.statement_source
          |> Top_level_outer_expression_binding.statement_item_index
        in
        let visible, publications =
          publish_before item_index visible publications
        in
        match collect_statement_outputs next_output statement with
        | Error _ as error -> error
        | Ok (next_output, source_outputs) -> (
            let rec resolve_outputs rev = function
              | [] -> Ok rev
              | source :: rest -> (
                  match
                    resolve_output environment headers declarations visible
                      source
                  with
                  | Error _ as error -> error
                  | Ok output -> resolve_outputs (output :: rev) rest)
            in
            match resolve_outputs rev source_outputs with
            | Error _ as error -> error
            | Ok rev -> loop visible publications next_output rev rest))
  in
  loop String_map.empty
    (Module_expression_binding.publications module_expressions)
    0 []
    (Function_call_expression_result.top_level_statements expressions)

let source_context expressions =
  let tree = Function_call_expression_result.top_level_source expressions in
  let outer = Top_level_expression_tree.source tree in
  let environment = Top_level_outer_expression_binding.environment outer in
  let module_expressions =
    outer |> Top_level_outer_expression_binding.source
    |> Top_level_expression_binding.module_expressions
  in
  (environment, module_expressions)

let resolve ~table ~function_types ~functions expressions =
  let environment, module_expressions = source_context expressions in
  let mode = Function_resolution.compilation_mode functions in
  if
    not (Function_call_expression_result.top_level_owns_table expressions table)
  then
    Error
      (invalid_input
         "top-level implicit output expressions belong to another symbol table")
  else if not (Outer_environment.owns_table environment table) then
    Error
      (invalid_input
         "top-level implicit output environment belongs to another symbol table")
  else if not (Module_expression_binding.owns_table module_expressions table)
  then
    Error
      (invalid_input
         "top-level implicit output publications belong to another symbol table")
  else if
    Function_call_expression_result.top_level_compilation_mode expressions
    <> mode
  then
    Error
      (invalid_input
         "top-level implicit output functions have the wrong compilation mode")
  else if Outer_environment.compilation_mode environment <> mode then
    Error
      (invalid_input
         "top-level implicit output environment has the wrong compilation mode")
  else if Module_expression_binding.compilation_mode module_expressions <> mode
  then
    Error
      (invalid_input
         "top-level implicit output publications have the wrong compilation \
          mode")
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
                  resolve_statements environment headers declarations
                    module_expressions expressions
                with
                | Error _ as error -> error
                | Ok outputs_ ->
                    Ok
                      {
                        table;
                        environment_ = environment;
                        source_ = expressions;
                        compilation_mode_ = mode;
                        outputs_;
                      })))
