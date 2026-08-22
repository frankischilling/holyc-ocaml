type actual_class = Function_call_expression_result.result_class =
  | Integer_result
  | F64_result
  | Unresolved_actual_class

type conversion =
  | No_conversion
  | Result_to_f64
  | Result_to_int
  | Unresolved_conversion

type default_materialization =
      Function_call_expression_result.declared_default_materialization =
  | Immediate_default
  | Aot_string_default

type provided_source =
  | Fixed_expression of Function_call_expression_result.expression_result
  | Following_argument of
      Function_call_expression_result.implicit_output_argument_result

type omission = {
  output : Implicit_output_target_resolution.output;
  parameter : Function_type_resolution.parameter;
  position : int;
}

type provided_binding = {
  source : provided_source;
  position : int;
  result : Function_call_expression_result.expression_result;
  target : Function_call_conversion_policy.target_class;
  actual : actual_class;
  conversion : conversion;
}

type default_binding = {
  parameter : Function_type_resolution.parameter;
  source : Function_type_resolution.parameter_default;
  omission : omission;
  target : Function_call_conversion_policy.target_class;
  materialization : default_materialization;
}

type fixed_path =
  | Provided_path of provided_binding
  | Defaulted_path of default_binding

type fixed_slot = {
  parameter : Function_type_resolution.parameter;
  path : fixed_path;
}

type bound_output = {
  source : Implicit_output_target_resolution.output;
  header : Function_type_resolution.resolved_function;
  fixed_slots : fixed_slot list;
  variadic_values : Function_call_expression_result.expression_result list;
}

type deferred_output = {
  source : Implicit_output_target_resolution.output;
  outer_binding : Outer_environment.binding;
}

type output_result =
  | Bound_output of bound_output
  | Deferred_outer_output of deferred_output

type resolved_function = {
  source : Implicit_output_target_resolution.resolved_function;
  outputs : output_result list;
}

module Int_map = Map.Make (Int)

type t = {
  table : Symbol_table.t;
  policies_ : Function_call_conversion_policy.t;
  targets_ : Implicit_output_target_resolution.t;
  compilation_mode_ : Function_resolution.compilation_mode;
  functions_ : resolved_function list;
  by_symbol : resolved_function Int_map.t;
}

type error_kind =
  | Invalid_input of string
  | Missing_required_parameter of omission
  | Extra_nonvariadic_argument of {
      output : Implicit_output_target_resolution.output;
      provided : provided_source;
      position : int;
      fixed_count : int;
    }

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int
let same_symbol left right = Symbol.Id.equal (Symbol.id left) (Symbol.id right)
let owns_table result table = result.table == table
let policies result = result.policies_
let targets result = result.targets_
let compilation_mode result = result.compilation_mode_
let functions result = result.functions_
let function_source (function_ : resolved_function) = function_.source
let function_outputs (function_ : resolved_function) = function_.outputs
let bound_source (output : bound_output) = output.source
let bound_header (output : bound_output) = output.header
let bound_fixed_slots (output : bound_output) = output.fixed_slots
let bound_variadic_values (output : bound_output) = output.variadic_values
let deferred_source (output : deferred_output) = output.source
let deferred_outer_binding (output : deferred_output) = output.outer_binding
let fixed_parameter (slot : fixed_slot) = slot.parameter
let fixed_path (slot : fixed_slot) = slot.path
let omission_output (omission : omission) = omission.output
let omission_parameter (omission : omission) = omission.parameter
let omission_position (omission : omission) = omission.position
let provided_source (binding : provided_binding) = binding.source
let provided_position (binding : provided_binding) = binding.position
let provided_result (binding : provided_binding) = binding.result
let provided_target (binding : provided_binding) = binding.target
let provided_actual (binding : provided_binding) = binding.actual
let provided_conversion (binding : provided_binding) = binding.conversion
let default_parameter (binding : default_binding) = binding.parameter
let default_source (binding : default_binding) = binding.source
let default_omission (binding : default_binding) = binding.omission
let default_target (binding : default_binding) = binding.target

let default_materialization (binding : default_binding) =
  binding.materialization

let output_source = function
  | Bound_output output -> output.source
  | Deferred_outer_output output -> output.source

let output_result_name = function
  | Bound_output _ -> "bound"
  | Deferred_outer_output _ -> "deferred-outer-header"

let actual_class_name = Function_call_expression_result.result_class_name

let conversion_name = function
  | No_conversion -> "none"
  | Result_to_f64 -> "ICF_RES_TO_F64"
  | Result_to_int -> "ICF_RES_TO_INT"
  | Unresolved_conversion -> "unresolved"

let default_materialization_name =
  Function_call_expression_result.declared_default_materialization_name

let fixed_path_name = function
  | Provided_path provided ->
      Printf.sprintf "provided:%s:%s"
        (actual_class_name provided.actual)
        (conversion_name provided.conversion)
  | Defaulted_path default ->
      "defaulted:" ^ default_materialization_name default.materialization

let invalid_input message =
  { code = "HCSEMA0049"; kind = Invalid_input message; origin = None }

let output_origin output =
  output |> Implicit_output_target_resolution.output_source
  |> Function_call_expression_result.implicit_output_source
  |> Function_call_resolution.implicit_output_origin

let missing_required_parameter omission =
  {
    code = "HCSEMA0050";
    kind = Missing_required_parameter omission;
    origin = Some (output_origin omission.output);
  }

let provided_origin = function
  | Fixed_expression result ->
      Function_call_expression_result.result_origin result
  | Following_argument argument ->
      argument |> Function_call_expression_result.implicit_output_argument_value
      |> Function_call_expression_result.result_origin

let extra_nonvariadic_argument output provided position fixed_count =
  {
    code = "HCSEMA0051";
    kind =
      Extra_nonvariadic_argument { output; provided; position; fixed_count };
    origin = Some (provided_origin provided);
  }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let parameter_display parameter =
  let position = Function_type_resolution.parameter_index parameter + 1 in
  match Function_type_resolution.parameter_name parameter with
  | Some name -> Printf.sprintf "argument %d (%s)" position name
  | None -> Printf.sprintf "argument %d" position

let error_message error =
  match error.kind with
  | Invalid_input message -> message
  | Missing_required_parameter omission ->
      Printf.sprintf "implicit output call to %S is missing required %s"
        (Implicit_output_target_resolution.output_target_name omission.output)
        (parameter_display omission.parameter)
  | Extra_nonvariadic_argument { output; position; fixed_count; _ } ->
      Printf.sprintf
        "implicit output call to %S provides argument %d, but its selected \
         header has %d fixed %s"
        (Implicit_output_target_resolution.output_target_name output)
        (position + 1) fixed_count
        (if fixed_count = 1 then "parameter" else "parameters")

let error_to_string error = error.code ^ ": " ^ error_message error

let find_function result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_symbol with
    | Some function_
      when function_ |> function_source
           |> Implicit_output_target_resolution.function_source
           |> Function_call_expression_result.function_symbol
           |> same_symbol symbol -> Some function_
    | Some _ | None -> None

let conversion target actual =
  match (target, actual) with
  | _, Unresolved_actual_class -> Unresolved_conversion
  | Function_call_conversion_policy.F64_result, Integer_result -> Result_to_f64
  | Function_call_conversion_policy.Integer_result, F64_result -> Result_to_int
  | Function_call_conversion_policy.F64_result, F64_result
  | Function_call_conversion_policy.Integer_result, Integer_result ->
      No_conversion

let result_of_source = function
  | Fixed_expression result -> result
  | Following_argument argument ->
      Function_call_expression_result.implicit_output_argument_value argument

let provided_values output =
  let source = Implicit_output_target_resolution.output_source output in
  let fixed =
    Function_call_expression_result.implicit_output_fixed_value source
  in
  let following =
    source |> Function_call_expression_result.implicit_output_arguments
    |> List.map (fun argument -> Following_argument argument)
  in
  Fixed_expression fixed :: following

let default_contains_string = function
  | Function_type_resolution.Expression_default { contains_string_literal; _ }
    -> contains_string_literal
  | Function_type_resolution.Lastclass_default _ -> true

let materialization mode default =
  match (mode, default_contains_string default) with
  | Function_resolution.Aot, true -> Aot_string_default
  | Function_resolution.Aot, false | Function_resolution.Jit, _ ->
      Immediate_default

let make_provided policies ~before_item_index position parameter source =
  let result = result_of_source source in
  let target =
    Function_call_conversion_policy.parameter_target_class policies
      ~before_item_index parameter
  in
  let actual = Function_call_expression_result.result_class result in
  {
    source;
    position;
    result;
    target;
    actual;
    conversion = conversion target actual;
  }

let make_default policies mode ~before_item_index output position parameter
    source =
  let omission = { output; parameter; position } in
  {
    parameter;
    source;
    omission;
    target =
      Function_call_conversion_policy.parameter_target_class policies
        ~before_item_index parameter;
    materialization = materialization mode source;
  }

let bind_header policies mode ~before_item_index output header =
  let signature = Function_type_resolution.function_signature header in
  let parameters = Function_type_resolution.signature_parameters signature in
  let values = provided_values output in
  let rec fixed position rev parameters values =
    match parameters with
    | parameter :: parameter_rest -> (
        match values with
        | value :: value_rest ->
            let provided =
              make_provided policies ~before_item_index position parameter value
            in
            fixed (position + 1)
              ({ parameter; path = Provided_path provided } :: rev)
              parameter_rest value_rest
        | [] -> (
            match Function_type_resolution.parameter_default parameter with
            | Some default ->
                let binding =
                  make_default policies mode ~before_item_index output position
                    parameter default
                in
                fixed (position + 1)
                  ({ parameter; path = Defaulted_path binding } :: rev)
                  parameter_rest []
            | None ->
                Error
                  (missing_required_parameter { output; parameter; position })))
    | [] -> Ok (List.rev rev, values)
  in
  match fixed 0 [] parameters values with
  | Error _ as error -> error
  | Ok (fixed_slots, extras) -> (
      let is_variadic =
        Option.is_some
          (Function_type_resolution.function_variadic_bindings header)
      in
      match (is_variadic, extras) with
      | false, provided :: _ ->
          Error
            (extra_nonvariadic_argument output provided (List.length parameters)
               (List.length parameters))
      | false, [] | true, [] ->
          Ok
            (Bound_output
               { source = output; header; fixed_slots; variadic_values = [] })
      | true, extras ->
          Ok
            (Bound_output
               {
                 source = output;
                 header;
                 fixed_slots;
                 variadic_values = List.map result_of_source extras;
               }))

let outer_header_map table headers =
  let rec loop map = function
    | [] -> Ok map
    | header :: rest ->
        let symbol = Function_type_resolution.function_symbol header in
        let number = symbol_number symbol in
        if not (Symbol_table.owns_symbol table symbol) then
          Error
            (invalid_input
               "implicit output outer header belongs to another symbol table")
        else if Int_map.mem number map then
          Error
            (invalid_input
               "implicit output outer headers repeat a function symbol")
        else loop (Int_map.add number header map) rest
  in
  loop Int_map.empty headers

let bind_output policies mode outer_headers ~before_item_index output =
  match Implicit_output_target_resolution.output_binding output with
  | Implicit_output_target_resolution.Module_function target ->
      bind_header policies mode ~before_item_index output
        (Implicit_output_target_resolution.module_header target)
  | Implicit_output_target_resolution.Outer_function outer_binding -> (
      let symbol =
        outer_binding |> Outer_environment.binding_entry
        |> Outer_environment.entry_symbol
      in
      match Int_map.find_opt (symbol_number symbol) outer_headers with
      | Some header
        when same_symbol symbol
               (Function_type_resolution.function_symbol header) ->
          bind_header policies mode ~before_item_index output header
      | Some _ ->
          Error
            (invalid_input
               "implicit output outer header does not match its resolved symbol")
      | None -> Ok (Deferred_outer_output { source = output; outer_binding }))

let map_result apply values =
  let rec loop rev = function
    | [] -> Ok (List.rev rev)
    | value :: rest -> (
        match apply value with
        | Error _ as error -> error
        | Ok mapped -> loop (mapped :: rev) rest)
  in
  loop [] values

let bind_function policies mode outer_headers source =
  let expression_function =
    Implicit_output_target_resolution.function_source source
  in
  let before_item_index =
    Function_call_expression_result.function_item_index expression_function
  in
  match
    source |> Implicit_output_target_resolution.function_outputs
    |> map_result (bind_output policies mode outer_headers ~before_item_index)
  with
  | Error _ as error -> error
  | Ok outputs -> Ok { source; outputs }

let bind ~table ~policies ?(outer_headers = []) targets =
  let expressions = Implicit_output_target_resolution.source targets in
  let mode = Implicit_output_target_resolution.compilation_mode targets in
  if not (Implicit_output_target_resolution.owns_table targets table) then
    Error
      (invalid_input "implicit output targets belong to another symbol table")
  else if not (Function_call_conversion_policy.owns_table policies table) then
    Error
      (invalid_input
         "implicit output conversion policies belong to another symbol table")
  else if
    not (Function_call_expression_result.owns_policies expressions policies)
  then
    Error
      (invalid_input
         "implicit output expressions belong to another conversion-policy \
          traversal")
  else if Function_call_conversion_policy.compilation_mode policies <> mode then
    Error
      (invalid_input
         "implicit output targets and conversion policies use different \
          compilation modes")
  else
    match outer_header_map table outer_headers with
    | Error _ as error -> error
    | Ok outer_headers -> (
        match
          targets |> Implicit_output_target_resolution.functions
          |> map_result (bind_function policies mode outer_headers)
        with
        | Error _ as error -> error
        | Ok functions ->
            let by_symbol =
              List.fold_left
                (fun map function_ ->
                  let symbol =
                    function_ |> function_source
                    |> Implicit_output_target_resolution.function_source
                    |> Function_call_expression_result.function_symbol
                  in
                  Int_map.add (symbol_number symbol) function_ map)
                Int_map.empty functions
            in
            Ok
              {
                table;
                policies_ = policies;
                targets_ = targets;
                compilation_mode_ = mode;
                functions_ = functions;
                by_symbol;
              })
