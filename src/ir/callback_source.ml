module Typed = Sema.Function_call_expression_result
module Resolution = Sema.Function_call_resolution
module Tree = Sema.Top_level_expression_tree

type t =
  | Function of Typed.indirect_call
  | Global of Typed.top_level_global_callback_call
  | Outer of Typed.top_level_outer_callback_call
  | Indexed_global of Typed.top_level_indexed_global_callback_call

let function_resolution call =
  call |> Typed.indirect_source
  |> Sema.Function_call_conversion_policy.indirect_source

let top_source = function
  | Function _ -> None
  | Global call -> Some (Typed.top_level_global_callback_source call)
  | Outer call -> Some (Typed.top_level_outer_callback_source call)
  | Indexed_global call ->
      Some (Typed.top_level_indexed_global_callback_source call)

let callable = function
  | Function call -> Resolution.indirect_callable (function_resolution call)
  | Global call -> Typed.top_level_global_callback_callable call
  | Outer call -> Typed.top_level_outer_callback_callable call
  | Indexed_global call -> Typed.top_level_indexed_global_callback_callable call

let origin = function
  | Function call ->
      function_resolution call |> Resolution.indirect_source
      |> Resolution.call_origin
  | source ->
      Option.get (top_source source)
      |> Tree.call_source |> Resolution.call_origin

let callee = function
  | Function call -> Typed.indirect_callee_result call
  | Global call -> Some (Typed.top_level_global_callback_callee_result call)
  | Outer call -> Typed.top_level_outer_callback_callee_result call
  | Indexed_global call ->
      Some (Typed.top_level_indexed_global_callback_callee_result call)

let callee_source = function
  | Function call ->
      function_resolution call |> Resolution.indirect_source
      |> Resolution.call_callee_value
  | source -> Option.map Tree.call_callee_expression (top_source source)

let provided = function
  | Typed.Provided_result value -> Some value
  | Typed.Declared_default_result _ -> None

let top_fixed arguments =
  List.map
    (fun fixed ->
      ( Typed.top_level_fixed_source fixed |> Resolution.fixed_parameter,
        provided (Typed.top_level_fixed_path fixed) ))
    arguments

let fixed_arguments = function
  | Function call ->
      Typed.indirect_fixed_results call
      |> List.map (fun fixed ->
          ( fixed |> Typed.fixed_source
            |> Sema.Function_call_conversion_policy.fixed_source
            |> Resolution.fixed_parameter,
            provided (Typed.fixed_path fixed) ))
  | Global call ->
      top_fixed (Typed.top_level_global_callback_fixed_results call)
  | Outer call -> top_fixed (Typed.top_level_outer_callback_fixed_results call)
  | Indexed_global call ->
      top_fixed (Typed.top_level_indexed_global_callback_fixed_results call)

let variadic_arguments = function
  | Function call -> Typed.indirect_variadic_results call
  | Global call -> Typed.top_level_global_callback_variadic_results call
  | Outer call -> Typed.top_level_outer_callback_variadic_results call
  | Indexed_global call ->
      Typed.top_level_indexed_global_callback_variadic_results call

let matches_result source result =
  match source with
  | Function call -> (
      match Typed.result_call_resolution result with
      | Some (Resolution.Indirect_call original) ->
          original == function_resolution call
      | _ -> false)
  | source ->
      let id =
        match source with
        | Global call -> Typed.top_level_global_callback_result_id call
        | Outer call -> Typed.top_level_outer_callback_result_id call
        | Indexed_global call ->
            Typed.top_level_indexed_global_callback_result_id call
        | Function _ -> assert false
      in
      Typed.Id.equal id (Typed.result_id result)
      && Option.fold ~none:false
           ~some:(( == ) (Resolution.callable_pointer (callable source)))
           (Typed.result_callback_call_pointer result)
      && Tree.call_result_expression (Option.get (top_source source))
         == Typed.result_source result

let function_member source function_ =
  match source with
  | Function actual ->
      List.exists
        (function
          | Typed.Indirect_call_result expected -> actual == expected
          | _ -> false)
        (Typed.function_calls function_)
  | Global _ | Outer _ | Indexed_global _ -> false

let top_level_member source top_level =
  match source with
  | Function _ -> false
  | Global actual ->
      List.exists (( == ) actual)
        (Typed.top_level_global_callback_calls top_level)
  | Outer actual ->
      List.exists (( == ) actual)
        (Typed.top_level_outer_callback_calls top_level)
  | Indexed_global actual ->
      List.exists (( == ) actual)
        (Typed.top_level_indexed_global_callback_calls top_level)

let top_level_calls top_level =
  List.map
    (fun call -> Global call)
    (Typed.top_level_global_callback_calls top_level)
  @ List.map
      (fun call -> Outer call)
      (Typed.top_level_outer_callback_calls top_level)
  @ List.map
      (fun call -> Indexed_global call)
      (Typed.top_level_indexed_global_callback_calls top_level)
