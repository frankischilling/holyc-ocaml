module Id = struct
  type t = int

  let compare = Int.compare
  let equal = Int.equal
  let to_int value = value
end

type value_category =
  | Object_value
  | Address_value
  | Array_value
  | Callback_value
  | Function_value
  | Lvalue
  | Unavailable

type result_class = Integer_result | F64_result | Unresolved_actual_class

type expression_result = {
  id : Id.t;
  source : Function_call_resolution.argument_expression;
  origin : Symbol.origin;
  source_type : Type.t option;
  category : value_category;
  result_class : result_class;
}

type fixed_path =
  | Provided_result of expression_result
  | Declared_default_result

type fixed_result = {
  source : Function_call_conversion_policy.fixed_policy;
  path : fixed_path;
}

type direct_call = {
  source : Function_call_conversion_policy.direct_call;
  fixed_results : fixed_result list;
  variadic_arguments : Function_call_resolution.argument list;
}

type call_result =
  | Direct_call_result of direct_call
  | Deferred_call_result of Function_call_resolution.call_resolution

type resolved_function = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  calls : call_result list;
}

type t = {
  table : Symbol_table.t;
  policies : Function_call_conversion_policy.t;
  compilation_mode : Function_resolution.compilation_mode;
  functions : resolved_function list;
  all_results : expression_result list;
}

type error_kind = Invalid_input of string
type error = { code : string; kind : error_kind }
type expression_context = Value_context | Lvalue_context
type build_state = { next_id : int; results_rev : expression_result list }

let owns_table result table = result.table == table
let owns_policies result policies = result.policies == policies
let compilation_mode result = result.compilation_mode
let functions result = result.functions
let all_results result = result.all_results
let function_symbol (function_ : resolved_function) = function_.symbol
let function_scope (function_ : resolved_function) = function_.scope
let function_item_index (function_ : resolved_function) = function_.item_index
let function_calls (function_ : resolved_function) = function_.calls
let direct_source (call : direct_call) = call.source
let direct_fixed_results (call : direct_call) = call.fixed_results
let direct_variadic_arguments (call : direct_call) = call.variadic_arguments
let fixed_source (fixed : fixed_result) = fixed.source
let fixed_path (fixed : fixed_result) = fixed.path
let result_id (result : expression_result) = result.id
let result_source (result : expression_result) = result.source
let result_origin (result : expression_result) = result.origin
let result_type (result : expression_result) = result.source_type
let result_category (result : expression_result) = result.category
let result_class (result : expression_result) = result.result_class

let value_category_name = function
  | Object_value -> "object-value"
  | Address_value -> "address-value"
  | Array_value -> "array-value"
  | Callback_value -> "callback-value"
  | Function_value -> "function-value"
  | Lvalue -> "lvalue"
  | Unavailable -> "unavailable"

let result_class_name = function
  | Integer_result -> "integer-result"
  | F64_result -> "f64-result"
  | Unresolved_actual_class -> "unresolved"

let invalid_input message =
  let kind = Invalid_input message in
  { code = "HCSEMA0046"; kind }

let error_code error = error.code
let error_kind error = error.kind

let error_message error =
  match error.kind with
  | Invalid_input message -> message

let error_to_string error = error.code ^ ": " ^ error_message error

let primitive_type ?(form = Type.Internal_storage) primitive pointer_depth =
  match Type.make_primitive ~form ~primitive ~pointer_depth with
  | Ok type_ -> Some type_
  | Error _ -> None

let integer_type = primitive_type Primitive_type.I64 0
let float_type = primitive_type Primitive_type.F64 0
let string_type = primitive_type Primitive_type.U8 1

let type_is_owned table type_ =
  match Type.base type_ with
  | Type.Primitive _ -> true
  | Type.Aggregate symbol -> Symbol_table.owns_symbol table symbol

let forwarded_class policies ~before_item_index type_ =
  match
    Function_call_conversion_policy.forwarded_type_class policies
      ~before_item_index type_
  with
  | Function_call_conversion_policy.Integer_result -> Integer_result
  | Function_call_conversion_policy.F64_result -> F64_result

let allocate state =
  if state.next_id = max_int then
    Error (invalid_input "expression identity space is exhausted")
  else Ok (state.next_id, { state with next_id = state.next_id + 1 })

let record state result =
  (result, { state with results_rev = result :: state.results_rev })

let make_result state ~id ~source ~source_type ~category ~result_class =
  record state
    {
      id;
      source;
      origin = Function_call_resolution.argument_expression_origin source;
      source_type;
      category;
      result_class;
    }

let known_type table type_ =
  if type_is_owned table type_ then Ok type_
  else Error (invalid_input "expression type belongs to another symbol table")

let select_known_binary_type left right result_class =
  match result_class with
  | F64_result -> float_type
  | Unresolved_actual_class -> None
  | Integer_result -> (
      match (left.source_type, right.source_type) with
      | Some left_type, Some right_type -> (
          match (Type.base left_type, Type.base right_type) with
          | ( Type.Primitive (_, left_primitive),
              Type.Primitive (_, right_primitive) )
            when Type.pointer_depth left_type = 0
                 && Type.pointer_depth right_type = 0 ->
              let left_id = (Primitive_type.info left_primitive).raw_id in
              let right_id = (Primitive_type.info right_primitive).raw_id in
              if left_id >= right_id then Some left_type else Some right_type
          | _ -> None)
      | None, _ | _, None -> None)

let rec type_expression table policies ~before_item_index ~context state source
    =
  match allocate state with
  | Error _ as error -> error
  | Ok (id, state) -> (
      let finish ?(source_type = None) category result_class state =
        Ok (make_result state ~id ~source ~source_type ~category ~result_class)
      in
      match Function_call_resolution.argument_expression_kind source with
      | Function_call_resolution.Integer_literal
      | Function_call_resolution.Character_literal ->
          finish ~source_type:integer_type Object_value Integer_result state
      | Function_call_resolution.Float_literal ->
          finish ~source_type:float_type Object_value F64_result state
      | Function_call_resolution.String_literal ->
          finish ~source_type:string_type Address_value Integer_result state
      | Function_call_resolution.Parenthesized_expression grouped -> (
          match
            type_expression table policies ~before_item_index ~context state
              grouped
          with
          | Error _ as error -> error
          | Ok (grouped_result, state) ->
              finish ~source_type:grouped_result.source_type
                grouped_result.category grouped_result.result_class state)
      | Function_call_resolution.Prefix_expression prefix ->
          type_prefix table policies ~before_item_index ~context state id source
            prefix
      | Function_call_resolution.Postfix_expression postfix ->
          type_postfix table policies ~before_item_index state id source postfix
      | Function_call_resolution.Binary_expression binary ->
          type_binary table policies ~before_item_index state id source binary
      | Function_call_resolution.Postfix_cast_expression (operand, target) -> (
          match
            type_expression table policies ~before_item_index
              ~context:Value_context state operand
          with
          | Error _ as error -> error
          | Ok (_, state) -> (
              match known_type table (Type_reference.resolved_type target) with
              | Error _ as error -> error
              | Ok target_type ->
                  let category =
                    if Type.pointer_depth target_type > 0 then Address_value
                    else Object_value
                  in
                  finish ~source_type:(Some target_type) category
                    (forwarded_class policies ~before_item_index target_type)
                    state))
      | Function_call_resolution.Bound_identifier_expression identifier -> (
          match
            known_type table
              (Function_call_resolution.bound_identifier_type identifier)
          with
          | Error _ as error -> error
          | Ok source_type ->
              let category, result_class =
                match
                  Function_call_resolution.bound_identifier_shape identifier
                with
                | Function_call_resolution.Array_value ->
                    (Array_value, Integer_result)
                | Function_call_resolution.Function_pointer_value ->
                    (Callback_value, Integer_result)
                | Function_call_resolution.Object_value ->
                    ( (match context with
                      | Value_context -> Object_value
                      | Lvalue_context -> Lvalue),
                      forwarded_class policies ~before_item_index source_type )
              in
              finish ~source_type:(Some source_type) category result_class state
          )
      | Function_call_resolution.Unresolved_expression kind -> (
          match kind with
          | Function_call_resolution.Current_position_expression ->
              finish Unavailable Integer_result state
          | Function_call_resolution.Sizeof_expression
          | Function_call_resolution.Offset_expression
          | Function_call_resolution.Defined_expression ->
              finish ~source_type:integer_type Object_value Integer_result state
          | Function_call_resolution.Identifier_expression
          | Function_call_resolution.Postfix_cast_expression
          | Function_call_resolution.Call_expression
          | Function_call_resolution.Index_expression
          | Function_call_resolution.Member_expression ->
              finish Unavailable Unresolved_actual_class state))

and type_prefix table policies ~before_item_index ~context state id source
    prefix =
  let operator = Function_call_resolution.prefix_operator prefix in
  let operand_context =
    match operator with
    | Function_call_resolution.Address_of
    | Function_call_resolution.Pre_increment
    | Function_call_resolution.Pre_decrement -> Lvalue_context
    | Function_call_resolution.Unary_plus
    | Function_call_resolution.Unary_minus
    | Function_call_resolution.Logical_not
    | Function_call_resolution.Bitwise_not
    | Function_call_resolution.Dereference -> Value_context
  in
  match
    type_expression table policies ~before_item_index ~context:operand_context
      state
      (Function_call_resolution.prefix_operand prefix)
  with
  | Error _ as error -> error
  | Ok (operand, state) -> (
      let finish ?(source_type = None) category result_class =
        Ok (make_result state ~id ~source ~source_type ~category ~result_class)
      in
      match operator with
      | Function_call_resolution.Unary_plus
      | Function_call_resolution.Unary_minus
      | Function_call_resolution.Logical_not ->
          finish ~source_type:operand.source_type Object_value
            operand.result_class
      | Function_call_resolution.Bitwise_not ->
          finish ~source_type:integer_type Object_value Integer_result
      | Function_call_resolution.Address_of -> (
          match operand.source_type with
          | None -> finish Address_value Integer_result
          | Some source_type -> (
              match Type.pointer_to source_type with
              | Ok source_type ->
                  finish ~source_type:(Some source_type) Address_value
                    Integer_result
              | Error _ -> finish Address_value Integer_result))
      | Function_call_resolution.Pre_increment
      | Function_call_resolution.Pre_decrement ->
          finish ~source_type:operand.source_type Object_value
            operand.result_class
      | Function_call_resolution.Dereference -> (
          let value_category =
            match context with
            | Value_context -> Object_value
            | Lvalue_context -> Lvalue
          in
          match (operand.source_type, operand.category) with
          | None, _ -> finish Unavailable Unresolved_actual_class
          | Some source_type, Array_value ->
              finish ~source_type:(Some source_type) value_category
                (forwarded_class policies ~before_item_index source_type)
          | Some source_type, (Callback_value | Function_value) ->
              finish ~source_type:(Some source_type) Function_value
                Integer_result
          | Some source_type, _ ->
              let source_type =
                match Type.dereference source_type with
                | Ok source_type -> source_type
                | Error _ -> source_type
              in
              finish ~source_type:(Some source_type) value_category
                (forwarded_class policies ~before_item_index source_type)))

and type_postfix table policies ~before_item_index state id source postfix =
  match
    type_expression table policies ~before_item_index ~context:Lvalue_context
      state
      (Function_call_resolution.postfix_operand postfix)
  with
  | Error _ as error -> error
  | Ok (operand, state) ->
      Ok
        (make_result state ~id ~source ~source_type:operand.source_type
           ~category:Object_value ~result_class:operand.result_class)

and type_binary table policies ~before_item_index state id source binary =
  match
    type_expression table policies ~before_item_index ~context:Value_context
      state
      (Function_call_resolution.binary_left binary)
  with
  | Error _ as error -> error
  | Ok (left, state) -> (
      match
        type_expression table policies ~before_item_index ~context:Value_context
          state
          (Function_call_resolution.binary_right binary)
      with
      | Error _ as error -> error
      | Ok (right, state) ->
          let result_class, source_type =
            match Function_call_resolution.binary_operator binary with
            | Generated.Intermediate_codes.Ic_power -> (F64_result, float_type)
            | Generated.Intermediate_codes.Ic_equ_equ
            | Generated.Intermediate_codes.Ic_not_equ
            | Generated.Intermediate_codes.Ic_less
            | Generated.Intermediate_codes.Ic_greater_equ
            | Generated.Intermediate_codes.Ic_greater
            | Generated.Intermediate_codes.Ic_less_equ
            | Generated.Intermediate_codes.Ic_and_and
            | Generated.Intermediate_codes.Ic_or_or
            | Generated.Intermediate_codes.Ic_xor_xor ->
                (Integer_result, integer_type)
            | Generated.Intermediate_codes.Ic_shl
            | Generated.Intermediate_codes.Ic_shr
            | Generated.Intermediate_codes.Ic_mul
            | Generated.Intermediate_codes.Ic_div
            | Generated.Intermediate_codes.Ic_mod
            | Generated.Intermediate_codes.Ic_and
            | Generated.Intermediate_codes.Ic_or
            | Generated.Intermediate_codes.Ic_xor
            | Generated.Intermediate_codes.Ic_add
            | Generated.Intermediate_codes.Ic_sub ->
                let result_class =
                  match (left.result_class, right.result_class) with
                  | F64_result, _ | _, F64_result -> F64_result
                  | Integer_result, Integer_result -> Integer_result
                  | Integer_result, Unresolved_actual_class
                  | Unresolved_actual_class, Integer_result
                  | Unresolved_actual_class, Unresolved_actual_class ->
                      Unresolved_actual_class
                in
                (result_class, select_known_binary_type left right result_class)
            | _ -> (Unresolved_actual_class, None)
          in
          Ok
            (make_result state ~id ~source ~source_type ~category:Object_value
               ~result_class))

let map_state apply state values =
  let rec loop state rev = function
    | [] -> Ok (List.rev rev, state)
    | value :: rest -> (
        match apply state value with
        | Error _ as error -> error
        | Ok (result, state) -> loop state (result :: rev) rest)
  in
  loop state [] values

let type_fixed table policies ~before_item_index state source =
  match
    ( Function_call_conversion_policy.fixed_path source,
      source |> Function_call_conversion_policy.fixed_source
      |> Function_call_resolution.fixed_value )
  with
  | ( Function_call_conversion_policy.Declared_default,
      Function_call_resolution.Declared_default _ ) ->
      Ok ({ source; path = Declared_default_result }, state)
  | ( Function_call_conversion_policy.Provided_expression _,
      Function_call_resolution.Provided_argument argument ) -> (
      match Function_call_resolution.argument_expression argument with
      | None ->
          Error (invalid_input "provided fixed argument has no expression")
      | Some expression -> (
          match
            type_expression table policies ~before_item_index
              ~context:Value_context state expression
          with
          | Error _ as error -> error
          | Ok (result, state) ->
              Ok ({ source; path = Provided_result result }, state)))
  | ( Function_call_conversion_policy.Declared_default,
      Function_call_resolution.Provided_argument _ )
  | ( Function_call_conversion_policy.Provided_expression _,
      Function_call_resolution.Declared_default _ ) ->
      Error (invalid_input "fixed call policy has an inconsistent source path")

let type_call table policies ~before_item_index state = function
  | Function_call_conversion_policy.Direct_call_policy source -> (
      match
        source |> Function_call_conversion_policy.direct_fixed_policies
        |> map_state (type_fixed table policies ~before_item_index) state
      with
      | Error _ as error -> error
      | Ok (fixed_results, state) ->
          Ok
            ( Direct_call_result
                {
                  source;
                  fixed_results;
                  variadic_arguments =
                    Function_call_conversion_policy.direct_variadic_arguments
                      source;
                },
              state ))
  | Function_call_conversion_policy.Deferred_call_policy call ->
      Ok (Deferred_call_result call, state)

let type_function table policies state source =
  let item_index = Function_call_conversion_policy.function_item_index source in
  match
    source |> Function_call_conversion_policy.function_calls
    |> map_state (type_call table policies ~before_item_index:item_index) state
  with
  | Error _ as error -> error
  | Ok (calls, state) ->
      Ok
        ( {
            symbol = Function_call_conversion_policy.function_symbol source;
            scope = Function_call_conversion_policy.function_scope source;
            item_index;
            calls;
          },
          state )

let analyze ~table policies =
  if not (Function_call_conversion_policy.owns_table policies table) then
    Error
      (invalid_input "call conversion policies belong to another symbol table")
  else
    match
      policies |> Function_call_conversion_policy.functions
      |> map_state
           (type_function table policies)
           { next_id = 0; results_rev = [] }
    with
    | Error _ as error -> error
    | Ok (functions, state) ->
        Ok
          {
            table;
            policies;
            compilation_mode =
              Function_call_conversion_policy.compilation_mode policies;
            functions;
            all_results =
              List.sort
                (fun left right -> Id.compare left.id right.id)
                state.results_rev;
          }
