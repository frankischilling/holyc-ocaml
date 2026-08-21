type actual_class = Function_call_expression_result.result_class =
  | Integer_result
  | F64_result
  | Unresolved_actual_class

type conversion =
  | No_conversion
  | Result_to_f64
  | Result_to_int
  | Unresolved_conversion

type provided_decision = {
  target : Function_call_conversion_policy.target_class;
  actual_result : Function_call_expression_result.expression_result;
  actual : actual_class;
  conversion : conversion;
}

type fixed_path = Provided_path of provided_decision | Declared_default_path

type fixed_decision = {
  source : Function_call_conversion_policy.fixed_policy;
  path : fixed_path;
}

type variadic_decision = {
  actual_result : Function_call_expression_result.expression_result;
  actual : actual_class;
}

type direct_call = {
  source : Function_call_conversion_policy.direct_call;
  fixed_decisions : fixed_decision list;
  variadic_decisions : variadic_decision list;
}

type indirect_call = {
  source : Function_call_conversion_policy.indirect_call;
  fixed_decisions : fixed_decision list;
  variadic_decisions : variadic_decision list;
}

type call_decision =
  | Direct_call_decision of direct_call
  | Indirect_call_decision of indirect_call
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
let direct_variadic_decisions (call : direct_call) = call.variadic_decisions
let indirect_source (call : indirect_call) = call.source
let indirect_fixed_decisions (call : indirect_call) = call.fixed_decisions
let indirect_variadic_decisions (call : indirect_call) = call.variadic_decisions
let fixed_source (fixed : fixed_decision) = fixed.source
let fixed_path (fixed : fixed_decision) = fixed.path
let provided_target (provided : provided_decision) = provided.target

let provided_actual_result (provided : provided_decision) =
  provided.actual_result

let provided_actual (provided : provided_decision) = provided.actual
let provided_conversion (provided : provided_decision) = provided.conversion

let variadic_actual_result (variadic : variadic_decision) =
  variadic.actual_result

let variadic_actual (variadic : variadic_decision) = variadic.actual
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

let conversion target actual =
  match (target, actual) with
  | _, Unresolved_actual_class -> Unresolved_conversion
  | Function_call_conversion_policy.F64_result, Integer_result -> Result_to_f64
  | Function_call_conversion_policy.Integer_result, F64_result -> Result_to_int
  | Function_call_conversion_policy.F64_result, F64_result
  | Function_call_conversion_policy.Integer_result, Integer_result ->
      No_conversion

let fixed_decision source =
  let policy = Function_call_expression_result.fixed_source source in
  match
    ( Function_call_conversion_policy.fixed_path policy,
      Function_call_expression_result.fixed_path source )
  with
  | ( Function_call_conversion_policy.Declared_default,
      Function_call_expression_result.Declared_default_result ) ->
      Ok { source = policy; path = Declared_default_path }
  | ( Function_call_conversion_policy.Provided_expression target,
      Function_call_expression_result.Provided_result actual_result ) ->
      let actual = Function_call_expression_result.result_class actual_result in
      Ok
        {
          source = policy;
          path =
            Provided_path
              {
                target;
                actual_result;
                actual;
                conversion = conversion target actual;
              };
        }
  | ( Function_call_conversion_policy.Declared_default,
      Function_call_expression_result.Provided_result _ )
  | ( Function_call_conversion_policy.Provided_expression _,
      Function_call_expression_result.Declared_default_result ) ->
      Error (invalid_input "typed fixed call has an inconsistent source path")

let map_result apply values =
  let rec loop rev = function
    | [] -> Ok (List.rev rev)
    | value :: rest -> (
        match apply value with
        | Error _ as error -> error
        | Ok result -> loop (result :: rev) rest)
  in
  loop [] values

let direct_call (source : Function_call_expression_result.direct_call) :
    (direct_call, error) result =
  match
    source |> Function_call_expression_result.direct_fixed_results
    |> map_result fixed_decision
  with
  | Error _ as error -> error
  | Ok fixed_decisions ->
      let variadic_decisions =
        source |> Function_call_expression_result.direct_variadic_results
        |> List.map (fun actual_result ->
            {
              actual_result;
              actual =
                Function_call_expression_result.result_class actual_result;
            })
      in
      Ok
        {
          source = Function_call_expression_result.direct_source source;
          fixed_decisions;
          variadic_decisions;
        }

let indirect_call (source : Function_call_expression_result.indirect_call) :
    (indirect_call, error) result =
  match
    source |> Function_call_expression_result.indirect_fixed_results
    |> map_result fixed_decision
  with
  | Error _ as error -> error
  | Ok fixed_decisions ->
      let variadic_decisions =
        source |> Function_call_expression_result.indirect_variadic_results
        |> List.map (fun actual_result ->
            {
              actual_result;
              actual =
                Function_call_expression_result.result_class actual_result;
            })
      in
      Ok
        {
          source = Function_call_expression_result.indirect_source source;
          fixed_decisions;
          variadic_decisions;
        }

let call_decision = function
  | Function_call_expression_result.Direct_call_result call -> (
      match direct_call call with
      | Error _ as error -> error
      | Ok call -> Ok (Direct_call_decision call))
  | Function_call_expression_result.Indirect_call_result call -> (
      match indirect_call call with
      | Error _ as error -> error
      | Ok call -> Ok (Indirect_call_decision call))
  | Function_call_expression_result.Deferred_call_result call ->
      Ok (Deferred_call_decision call)

let resolve_function source =
  let item_index = Function_call_expression_result.function_item_index source in
  match
    source |> Function_call_expression_result.function_calls
    |> map_result call_decision
  with
  | Error _ as error -> error
  | Ok calls ->
      Ok
        {
          symbol = Function_call_expression_result.function_symbol source;
          scope = Function_call_expression_result.function_scope source;
          item_index;
          calls;
        }

let decide ~table ~policies expressions =
  if not (Function_call_expression_result.owns_table expressions table) then
    Error
      (invalid_input "typed call expressions belong to another symbol table")
  else if
    not (Function_call_expression_result.owns_policies expressions policies)
  then
    Error
      (invalid_input
         "typed call expressions belong to another conversion-policy traversal")
  else
    match
      expressions |> Function_call_expression_result.functions
      |> map_result resolve_function
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
              Function_call_expression_result.compilation_mode expressions;
            functions;
            by_symbol;
          }

let find_function result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_symbol with
    | Some function_ when same_symbol function_.symbol symbol -> Some function_
    | Some _ | None -> None
