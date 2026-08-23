type conversion =
  | No_conversion
  | Result_to_f64
  | Result_to_int
  | Unresolved_conversion

type 'value provided = { value : 'value; position : int }

type defaulted = {
  source : Function_type_resolution.parameter_default;
  position : int;
}

type 'value fixed_path = Provided of 'value provided | Defaulted of defaulted

type 'value fixed_slot = {
  parameter : Function_type_resolution.parameter;
  path : 'value fixed_path;
}

type 'value plan = {
  fixed_slots : 'value fixed_slot list;
  variadic_values : 'value list;
}

module Int_map = Map.Make (Int)

type outer_headers = Function_type_resolution.resolved_function Int_map.t
type outer_header_error = Foreign_header | Duplicate_header

type 'value error =
  | Missing_required_parameter of {
      parameter : Function_type_resolution.parameter;
      position : int;
    }
  | Extra_nonvariadic_argument of {
      provided : 'value;
      position : int;
      fixed_count : int;
    }

let fixed_slots (plan : _ plan) = plan.fixed_slots
let variadic_values (plan : _ plan) = plan.variadic_values
let fixed_parameter (slot : _ fixed_slot) = slot.parameter
let fixed_path (slot : _ fixed_slot) = slot.path
let provided_value (provided : _ provided) = provided.value
let provided_position (provided : _ provided) = provided.position
let default_source (default : defaulted) = default.source
let default_position (default : defaulted) = default.position

let collect_outer_headers table headers =
  let rec loop map = function
    | [] -> Ok map
    | header :: rest ->
        let symbol = Function_type_resolution.function_symbol header in
        let number = Symbol.id symbol |> Symbol.Id.to_int in
        if not (Symbol_table.owns_symbol table symbol) then Error Foreign_header
        else if Int_map.mem number map then Error Duplicate_header
        else loop (Int_map.add number header map) rest
  in
  loop Int_map.empty headers

let find_outer_header headers symbol =
  let number = Symbol.id symbol |> Symbol.Id.to_int in
  match Int_map.find_opt number headers with
  | Some header
    when Symbol.Id.equal (Symbol.id symbol)
           (header |> Function_type_resolution.function_symbol |> Symbol.id) ->
      Some header
  | Some _ | None -> None

let plan header values =
  let signature = Function_type_resolution.function_signature header in
  let parameters = Function_type_resolution.signature_parameters signature in
  let fixed_count = List.length parameters in
  let rec fixed position rev parameters values =
    match parameters with
    | parameter :: parameter_rest -> (
        match values with
        | value :: value_rest ->
            fixed (position + 1)
              ({ parameter; path = Provided { value; position } } :: rev)
              parameter_rest value_rest
        | [] -> (
            match Function_type_resolution.parameter_default parameter with
            | Some source ->
                fixed (position + 1)
                  ({ parameter; path = Defaulted { source; position } } :: rev)
                  parameter_rest []
            | None -> Error (Missing_required_parameter { parameter; position })
            ))
    | [] -> Ok (List.rev rev, values)
  in
  match fixed 0 [] parameters values with
  | Error _ as error -> error
  | Ok (fixed_slots, extras) -> (
      let variadic =
        Option.is_some
          (Function_type_resolution.function_variadic_bindings header)
      in
      match (variadic, extras) with
      | false, provided :: _ ->
          Error
            (Extra_nonvariadic_argument
               { provided; position = fixed_count; fixed_count })
      | false, [] | true, _ -> Ok { fixed_slots; variadic_values = extras })

let conversion target actual =
  match (target, actual) with
  | _, Function_call_expression_result.Unresolved_actual_class ->
      Unresolved_conversion
  | ( Function_call_conversion_policy.F64_result,
      Function_call_expression_result.Integer_result ) -> Result_to_f64
  | ( Function_call_conversion_policy.Integer_result,
      Function_call_expression_result.F64_result ) -> Result_to_int
  | ( Function_call_conversion_policy.F64_result,
      Function_call_expression_result.F64_result )
  | ( Function_call_conversion_policy.Integer_result,
      Function_call_expression_result.Integer_result ) -> No_conversion

let default_contains_string = function
  | Function_type_resolution.Expression_default { contains_string_literal; _ }
    -> contains_string_literal
  | Function_type_resolution.Lastclass_default _ -> true

let materialization mode default =
  match (mode, default_contains_string default) with
  | Function_resolution.Aot, true ->
      Function_call_expression_result.Aot_string_default
  | Function_resolution.Aot, false | Function_resolution.Jit, _ ->
      Function_call_expression_result.Immediate_default
