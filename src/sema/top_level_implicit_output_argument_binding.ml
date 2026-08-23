type actual_class = Function_call_expression_result.result_class =
  | Integer_result
  | F64_result
  | Unresolved_actual_class

type conversion = Implicit_output_argument_rules.conversion =
  | No_conversion
  | Result_to_f64
  | Result_to_int
  | Unresolved_conversion

type default_materialization =
      Function_call_expression_result.declared_default_materialization =
  | Immediate_default
  | Aot_string_default

type provided_source =
  | Fixed_value of Function_call_expression_result.top_level_root_result
  | Trailing_value of Function_call_expression_result.top_level_root_result

type omission = {
  output : Top_level_implicit_output_target_resolution.output;
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
  source : Top_level_implicit_output_target_resolution.output;
  header : Function_type_resolution.resolved_function;
  fixed_slots : fixed_slot list;
  variadic_roots : Function_call_expression_result.top_level_root_result list;
}

type deferred_output = {
  source : Top_level_implicit_output_target_resolution.output;
  outer_binding : Outer_environment.binding;
}

type output_result =
  | Bound_output of bound_output
  | Deferred_outer_output of deferred_output

type t = {
  table : Symbol_table.t;
  policies_ : Function_call_conversion_policy.t;
  targets_ : Top_level_implicit_output_target_resolution.t;
  compilation_mode_ : Function_resolution.compilation_mode;
  outputs_ : output_result list;
}

type error_kind =
  | Invalid_input of string
  | Missing_required_parameter of omission
  | Extra_nonvariadic_argument of {
      output : Top_level_implicit_output_target_resolution.output;
      provided : provided_source;
      position : int;
      fixed_count : int;
    }

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let owns_table result table = result.table == table
let policies result = result.policies_
let targets result = result.targets_
let compilation_mode result = result.compilation_mode_
let outputs result = result.outputs_
let bound_source (output : bound_output) = output.source
let bound_header (output : bound_output) = output.header
let bound_fixed_slots (output : bound_output) = output.fixed_slots
let bound_variadic_roots (output : bound_output) = output.variadic_roots
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
  { code = "HCSEMA0060"; kind = Invalid_input message; origin = None }

let output_origin output =
  Top_level_implicit_output_target_resolution.output_marker_origin output

let missing_required_parameter omission =
  {
    code = "HCSEMA0061";
    kind = Missing_required_parameter omission;
    origin = Some (output_origin omission.output);
  }

let root_of_source = function
  | Fixed_value root | Trailing_value root -> root

let result_of_source source =
  source |> root_of_source
  |> Function_call_expression_result.top_level_root_value

let provided_origin source =
  source |> result_of_source |> Function_call_expression_result.result_origin

let extra_nonvariadic_argument output provided position fixed_count =
  {
    code = "HCSEMA0062";
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
      Printf.sprintf
        "top-level implicit output call to %S is missing required %s"
        (Top_level_implicit_output_target_resolution.output_target_name
           omission.output)
        (parameter_display omission.parameter)
  | Extra_nonvariadic_argument { output; position; fixed_count; _ } ->
      Printf.sprintf
        "top-level implicit output call to %S provides argument %d, but its \
         selected header has %d fixed %s"
        (Top_level_implicit_output_target_resolution.output_target_name output)
        (position + 1) fixed_count
        (if fixed_count = 1 then "parameter" else "parameters")

let error_to_string error = error.code ^ ": " ^ error_message error

let provided_values output =
  let fixed =
    Top_level_implicit_output_target_resolution.output_fixed_value output
  in
  let following =
    output |> Top_level_implicit_output_target_resolution.output_arguments
    |> List.map (fun root -> Trailing_value root)
  in
  Fixed_value fixed :: following

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
    conversion = Implicit_output_argument_rules.conversion target actual;
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
    materialization = Implicit_output_argument_rules.materialization mode source;
  }

let bind_header policies mode ~before_item_index output header =
  match Implicit_output_argument_rules.plan header (provided_values output) with
  | Error
      (Implicit_output_argument_rules.Missing_required_parameter
         { parameter; position }) ->
      Error (missing_required_parameter { output; parameter; position })
  | Error
      (Implicit_output_argument_rules.Extra_nonvariadic_argument
         { provided; position; fixed_count }) ->
      Error (extra_nonvariadic_argument output provided position fixed_count)
  | Ok plan ->
      let fixed_slots =
        plan |> Implicit_output_argument_rules.fixed_slots
        |> List.map (fun slot ->
            let parameter =
              Implicit_output_argument_rules.fixed_parameter slot
            in
            let path =
              match Implicit_output_argument_rules.fixed_path slot with
              | Implicit_output_argument_rules.Provided provided ->
                  Provided_path
                    (make_provided policies ~before_item_index
                       (Implicit_output_argument_rules.provided_position
                          provided)
                       parameter
                       (Implicit_output_argument_rules.provided_value provided))
              | Implicit_output_argument_rules.Defaulted default ->
                  Defaulted_path
                    (make_default policies mode ~before_item_index output
                       (Implicit_output_argument_rules.default_position default)
                       parameter
                       (Implicit_output_argument_rules.default_source default))
            in
            { parameter; path })
      in
      Ok
        (Bound_output
           {
             source = output;
             header;
             fixed_slots;
             variadic_roots =
               plan |> Implicit_output_argument_rules.variadic_values
               |> List.map root_of_source;
           })

let before_item_index output =
  output |> Top_level_implicit_output_target_resolution.output_statement
  |> Function_call_expression_result.top_level_statement_source
  |> Top_level_expression_tree.statement_source
  |> Top_level_outer_expression_binding.statement_item_index

let bind_output policies mode outer_headers output =
  let before_item_index = before_item_index output in
  match Top_level_implicit_output_target_resolution.output_binding output with
  | Top_level_implicit_output_target_resolution.Module_function target ->
      bind_header policies mode ~before_item_index output
        (Top_level_implicit_output_target_resolution.module_header target)
  | Top_level_implicit_output_target_resolution.Outer_function binding -> (
      let symbol =
        binding |> Outer_environment.binding_entry
        |> Outer_environment.entry_symbol
      in
      match
        Implicit_output_argument_rules.find_outer_header outer_headers symbol
      with
      | Some header ->
          bind_header policies mode ~before_item_index output header
      | None ->
          Ok
            (Deferred_outer_output { source = output; outer_binding = binding })
      )

let map_result apply values =
  let rec loop rev = function
    | [] -> Ok (List.rev rev)
    | value :: rest -> (
        match apply value with
        | Error _ as error -> error
        | Ok mapped -> loop (mapped :: rev) rest)
  in
  loop [] values

let bind ~table ~policies ?(outer_headers = []) targets =
  let expressions =
    Top_level_implicit_output_target_resolution.source targets
  in
  let mode =
    Top_level_implicit_output_target_resolution.compilation_mode targets
  in
  if not (Top_level_implicit_output_target_resolution.owns_table targets table)
  then
    Error
      (invalid_input
         "top-level implicit output targets belong to another symbol table")
  else if not (Function_call_conversion_policy.owns_table policies table) then
    Error
      (invalid_input
         "top-level implicit output conversion policies belong to another \
          symbol table")
  else if
    not
      (Function_call_expression_result.top_level_owns_policies expressions
         policies)
  then
    Error
      (invalid_input
         "top-level implicit output expressions belong to another \
          conversion-policy traversal")
  else if Function_call_conversion_policy.compilation_mode policies <> mode then
    Error
      (invalid_input
         "top-level implicit output targets and conversion policies use \
          different compilation modes")
  else
    match
      Implicit_output_argument_rules.collect_outer_headers table outer_headers
    with
    | Error Implicit_output_argument_rules.Foreign_header ->
        Error
          (invalid_input
             "top-level implicit output outer header belongs to another symbol \
              table")
    | Error Implicit_output_argument_rules.Duplicate_header ->
        Error
          (invalid_input
             "top-level implicit output outer headers repeat a function symbol")
    | Ok outer_headers -> (
        match
          targets |> Top_level_implicit_output_target_resolution.outputs
          |> map_result (bind_output policies mode outer_headers)
        with
        | Error _ as error -> error
        | Ok outputs_ ->
            Ok
              {
                table;
                policies_ = policies;
                targets_ = targets;
                compilation_mode_ = mode;
                outputs_;
              })
