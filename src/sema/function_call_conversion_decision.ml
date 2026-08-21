type actual_class = Integer_result | F64_result | Unresolved_actual_class

type conversion =
  | No_conversion
  | Result_to_f64
  | Result_to_int
  | Unresolved_conversion

type provided_decision = {
  target : Function_call_conversion_policy.target_class;
  actual : actual_class;
  conversion : conversion;
}

type fixed_path = Provided_path of provided_decision | Declared_default_path

type fixed_decision = {
  source : Function_call_conversion_policy.fixed_policy;
  path : fixed_path;
}

type direct_call = {
  source : Function_call_conversion_policy.direct_call;
  fixed_decisions : fixed_decision list;
  variadic_arguments : Function_call_resolution.argument list;
}

type call_decision =
  | Direct_call_decision of direct_call
  | Deferred_call_decision of Function_call_resolution.call_resolution

type resolved_function = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  calls : call_decision list;
}

module Int_map = Map.Make (Int)

type t = {
  table : Symbol_table.t;
  compilation_mode : Function_resolution.compilation_mode;
  functions : resolved_function list;
  by_symbol : resolved_function Int_map.t;
}

type error_kind = Invalid_input of string
type error = { code : string; kind : error_kind }

let functions (result : t) = result.functions
let compilation_mode (result : t) = result.compilation_mode
let owns_table (result : t) table = result.table == table
let function_symbol (function_ : resolved_function) = function_.symbol
let function_scope (function_ : resolved_function) = function_.scope
let function_item_index (function_ : resolved_function) = function_.item_index
let function_calls (function_ : resolved_function) = function_.calls
let direct_source (call : direct_call) = call.source
let direct_fixed_decisions (call : direct_call) = call.fixed_decisions
let direct_variadic_arguments (call : direct_call) = call.variadic_arguments
let fixed_source (fixed : fixed_decision) = fixed.source
let fixed_path (fixed : fixed_decision) = fixed.path
let provided_target (provided : provided_decision) = provided.target
let provided_actual (provided : provided_decision) = provided.actual
let provided_conversion (provided : provided_decision) = provided.conversion
let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int

let actual_class_name = function
  | Integer_result -> "integer-result"
  | F64_result -> "f64-result"
  | Unresolved_actual_class -> "unresolved"

let conversion_name = function
  | No_conversion -> "none"
  | Result_to_f64 -> "ICF_RES_TO_F64"
  | Result_to_int -> "ICF_RES_TO_INT"
  | Unresolved_conversion -> "unresolved"

let fixed_path_name = function
  | Declared_default_path -> "declared-default"
  | Provided_path decision ->
      Printf.sprintf "provided:%s:%s"
        (actual_class_name decision.actual)
        (conversion_name decision.conversion)

let invalid_input message =
  { code = "HCSEMA0045"; kind = Invalid_input message }

let error_code error = error.code
let error_kind error = error.kind

let error_message error =
  match error.kind with
  | Invalid_input message -> message

let error_to_string error = error.code ^ ": " ^ error_message error
let same_symbol left right = Symbol.Id.equal (Symbol.id left) (Symbol.id right)

let rec source_actual_class policies ~before_item_index expression =
  match Function_call_resolution.argument_expression_kind expression with
  | Function_call_resolution.Integer_literal
  | Function_call_resolution.Character_literal
  | Function_call_resolution.String_literal -> Integer_result
  | Function_call_resolution.Float_literal -> F64_result
  | Function_call_resolution.Parenthesized_expression grouped ->
      source_actual_class policies ~before_item_index grouped
  | Function_call_resolution.Prefix_expression prefix -> (
      match Function_call_resolution.prefix_operator prefix with
      | Function_call_resolution.Unary_plus
      | Function_call_resolution.Unary_minus
      | Function_call_resolution.Logical_not ->
          source_actual_class policies ~before_item_index
            (Function_call_resolution.prefix_operand prefix)
      | Function_call_resolution.Address_of -> Integer_result
      | Function_call_resolution.Bitwise_not
      | Function_call_resolution.Dereference
      | Function_call_resolution.Pre_increment
      | Function_call_resolution.Pre_decrement -> Unresolved_actual_class)
  | Function_call_resolution.Postfix_expression postfix ->
      source_actual_class policies ~before_item_index
        (Function_call_resolution.postfix_operand postfix)
  | Function_call_resolution.Binary_expression binary -> (
      match Function_call_resolution.binary_operator binary with
      | Generated.Intermediate_codes.Ic_power -> F64_result
      | Generated.Intermediate_codes.Ic_equ_equ
      | Generated.Intermediate_codes.Ic_not_equ
      | Generated.Intermediate_codes.Ic_less
      | Generated.Intermediate_codes.Ic_greater_equ
      | Generated.Intermediate_codes.Ic_greater
      | Generated.Intermediate_codes.Ic_less_equ
      | Generated.Intermediate_codes.Ic_and_and
      | Generated.Intermediate_codes.Ic_or_or
      | Generated.Intermediate_codes.Ic_xor_xor -> Integer_result
      | Generated.Intermediate_codes.Ic_shl
      | Generated.Intermediate_codes.Ic_shr
      | Generated.Intermediate_codes.Ic_mul
      | Generated.Intermediate_codes.Ic_div
      | Generated.Intermediate_codes.Ic_mod
      | Generated.Intermediate_codes.Ic_and
      | Generated.Intermediate_codes.Ic_or
      | Generated.Intermediate_codes.Ic_xor
      | Generated.Intermediate_codes.Ic_add
      | Generated.Intermediate_codes.Ic_sub -> (
          match
            ( source_actual_class policies ~before_item_index
                (Function_call_resolution.binary_left binary),
              source_actual_class policies ~before_item_index
                (Function_call_resolution.binary_right binary) )
          with
          | F64_result, _ | _, F64_result -> F64_result
          | Integer_result, Integer_result -> Integer_result
          | Integer_result, Unresolved_actual_class
          | Unresolved_actual_class, Integer_result
          | Unresolved_actual_class, Unresolved_actual_class ->
              Unresolved_actual_class)
      | _ -> Unresolved_actual_class)
  | Function_call_resolution.Postfix_cast_expression (_, target) -> (
      let resolved = Type_reference.resolved_type target in
      match
        Function_call_conversion_policy.forwarded_type_class policies
          ~before_item_index resolved
      with
      | Function_call_conversion_policy.Integer_result -> Integer_result
      | Function_call_conversion_policy.F64_result -> F64_result)
  | Function_call_resolution.Unresolved_expression
      ( Function_call_resolution.Current_position_expression
      | Function_call_resolution.Sizeof_expression
      | Function_call_resolution.Offset_expression
      | Function_call_resolution.Defined_expression ) -> Integer_result
  | Function_call_resolution.Unresolved_expression
      ( Function_call_resolution.Identifier_expression
      | Function_call_resolution.Postfix_cast_expression
      | Function_call_resolution.Call_expression
      | Function_call_resolution.Index_expression
      | Function_call_resolution.Member_expression ) -> Unresolved_actual_class

let conversion target actual =
  match (target, actual) with
  | _, Unresolved_actual_class -> Unresolved_conversion
  | Function_call_conversion_policy.F64_result, Integer_result -> Result_to_f64
  | Function_call_conversion_policy.Integer_result, F64_result -> Result_to_int
  | Function_call_conversion_policy.F64_result, F64_result
  | Function_call_conversion_policy.Integer_result, Integer_result ->
      No_conversion

let fixed_decision policies ~before_item_index source =
  match
    ( Function_call_conversion_policy.fixed_path source,
      source |> Function_call_conversion_policy.fixed_source
      |> Function_call_resolution.fixed_value )
  with
  | ( Function_call_conversion_policy.Declared_default,
      Function_call_resolution.Declared_default _ ) ->
      Ok { source; path = Declared_default_path }
  | ( Function_call_conversion_policy.Provided_expression target,
      Function_call_resolution.Provided_argument argument ) -> (
      match Function_call_resolution.argument_expression argument with
      | None ->
          Error (invalid_input "provided fixed argument has no expression")
      | Some expression ->
          let actual =
            source_actual_class policies ~before_item_index expression
          in
          Ok
            {
              source;
              path =
                Provided_path
                  { target; actual; conversion = conversion target actual };
            })
  | ( Function_call_conversion_policy.Declared_default,
      Function_call_resolution.Provided_argument _ )
  | ( Function_call_conversion_policy.Provided_expression _,
      Function_call_resolution.Declared_default _ ) ->
      Error (invalid_input "fixed call policy has an inconsistent source path")

let map_result apply values =
  let rec loop rev = function
    | [] -> Ok (List.rev rev)
    | value :: rest -> (
        match apply value with
        | Error _ as error -> error
        | Ok result -> loop (result :: rev) rest)
  in
  loop [] values

let direct_call policies ~before_item_index source =
  match
    source |> Function_call_conversion_policy.direct_fixed_policies
    |> map_result (fixed_decision policies ~before_item_index)
  with
  | Error _ as error -> error
  | Ok fixed_decisions ->
      Ok
        {
          source;
          fixed_decisions;
          variadic_arguments =
            Function_call_conversion_policy.direct_variadic_arguments source;
        }

let call_decision policies ~before_item_index = function
  | Function_call_conversion_policy.Direct_call_policy call -> (
      match direct_call policies ~before_item_index call with
      | Error _ as error -> error
      | Ok call -> Ok (Direct_call_decision call))
  | Function_call_conversion_policy.Deferred_call_policy call ->
      Ok (Deferred_call_decision call)

let resolve_function policies source =
  let item_index = Function_call_conversion_policy.function_item_index source in
  match
    source |> Function_call_conversion_policy.function_calls
    |> map_result (call_decision policies ~before_item_index:item_index)
  with
  | Error _ as error -> error
  | Ok calls ->
      Ok
        {
          symbol = Function_call_conversion_policy.function_symbol source;
          scope = Function_call_conversion_policy.function_scope source;
          item_index;
          calls;
        }

let decide ~table policies =
  if not (Function_call_conversion_policy.owns_table policies table) then
    Error
      (invalid_input "call conversion policies belong to another symbol table")
  else
    match
      policies |> Function_call_conversion_policy.functions
      |> map_result (resolve_function policies)
    with
    | Error _ as error -> error
    | Ok functions ->
        let by_symbol =
          List.fold_left
            (fun map function_ ->
              Int_map.add (symbol_number function_.symbol) function_ map)
            Int_map.empty functions
        in
        Ok
          {
            table;
            compilation_mode =
              Function_call_conversion_policy.compilation_mode policies;
            functions;
            by_symbol;
          }

let find_function result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_symbol with
    | Some function_ when same_symbol function_.symbol symbol -> Some function_
    | Some _ | None -> None
