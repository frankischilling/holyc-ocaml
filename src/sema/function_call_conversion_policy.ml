type target_class = Integer_result | F64_result
type fixed_path = Provided_expression of target_class | Declared_default

type fixed_policy = {
  source : Function_call_resolution.fixed_argument;
  path : fixed_path;
}

type direct_call = {
  source : Function_call_resolution.direct_call;
  fixed_policies : fixed_policy list;
  variadic_arguments : Function_call_resolution.argument list;
}

type indirect_call = {
  source : Function_call_resolution.indirect_call;
  fixed_policies : fixed_policy list;
  variadic_arguments : Function_call_resolution.argument list;
}

type call_policy =
  | Direct_call_policy of direct_call
  | Indirect_call_policy of indirect_call
  | Deferred_call_policy of Function_call_resolution.call_resolution

type resolved_function = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  return_type : Type_reference.t;
  conditions : Function_call_resolution.condition_input list;
  selectors : Function_call_resolution.selector_input list;
  returns : Function_call_resolution.return_input list;
  calls : call_policy list;
}

module Int_map = Map.Make (Int)

type t = {
  table : Symbol_table.t;
  compilation_mode : Function_resolution.compilation_mode;
  headers : Aggregate_header_resolution.header Int_map.t;
  functions : resolved_function list;
  by_symbol : resolved_function Int_map.t;
}

type error_kind =
  | Invalid_input of string
  | Aggregate_backing_cycle of Symbol.t list

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let functions (result : t) = result.functions
let compilation_mode (result : t) = result.compilation_mode
let owns_table (result : t) table = result.table == table
let function_symbol (function_ : resolved_function) = function_.symbol
let function_scope (function_ : resolved_function) = function_.scope
let function_item_index (function_ : resolved_function) = function_.item_index
let function_return_type (function_ : resolved_function) = function_.return_type
let function_conditions (function_ : resolved_function) = function_.conditions
let function_selectors (function_ : resolved_function) = function_.selectors
let function_returns (function_ : resolved_function) = function_.returns
let function_calls (function_ : resolved_function) = function_.calls
let direct_source (call : direct_call) = call.source
let direct_fixed_policies (call : direct_call) = call.fixed_policies
let direct_variadic_arguments (call : direct_call) = call.variadic_arguments
let indirect_source (call : indirect_call) = call.source
let indirect_fixed_policies (call : indirect_call) = call.fixed_policies
let indirect_variadic_arguments (call : indirect_call) = call.variadic_arguments
let fixed_source (fixed : fixed_policy) = fixed.source
let fixed_path (fixed : fixed_policy) = fixed.path
let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int

let target_class_name = function
  | Integer_result -> "integer-result"
  | F64_result -> "f64-result"

let fixed_path_name = function
  | Provided_expression target ->
      "provided-expression:" ^ target_class_name target
  | Declared_default -> "declared-default"

let invalid_input message =
  { code = "HCSEMA0043"; kind = Invalid_input message; origin = None }

let backing_cycle symbols origin =
  {
    code = "HCSEMA0044";
    kind = Aggregate_backing_cycle symbols;
    origin = Some origin;
  }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message
  | Aggregate_backing_cycle symbols ->
      let names = symbols |> List.map Symbol.name |> String.concat " -> " in
      "aggregate backing cycle in direct call target: " ^ names

let error_to_string error = error.code ^ ": " ^ error_message error
let same_symbol left right = Symbol.Id.equal (Symbol.id left) (Symbol.id right)

let same_scope left right =
  Symbol.Scope_id.equal
    (Symbol_table.scope_id left)
    (Symbol_table.scope_id right)

let symbol_in_scope symbol scope =
  Symbol.Scope_id.equal (Symbol.scope_id symbol) (Symbol_table.scope_id scope)

let validate_type table parent role type_ =
  match Type.base type_ with
  | Type.Primitive _ -> Ok ()
  | Type.Aggregate symbol ->
      if not (Symbol_table.owns_symbol table symbol) then
        Error (invalid_input (role ^ " belongs to another symbol table"))
      else if not (symbol_in_scope symbol parent) then
        Error (invalid_input (role ^ " has the wrong module scope"))
      else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type)
      then Error (invalid_input (role ^ " is not an aggregate symbol"))
      else Ok ()

let validate_header table parent previous_item seen header =
  let symbol = Aggregate_header_resolution.header_symbol header in
  let item_index = Aggregate_header_resolution.header_item_index header in
  let number = symbol_number symbol in
  if item_index <= previous_item then
    Error (invalid_input "aggregate headers do not follow source order")
  else if Int_map.mem number seen then
    Error (invalid_input "aggregate headers repeat a canonical definition")
  else if not (Symbol_table.owns_symbol table symbol) then
    Error (invalid_input "aggregate header belongs to another symbol table")
  else if not (symbol_in_scope symbol parent) then
    Error (invalid_input "aggregate header has the wrong module scope")
  else
    match Aggregate_header_resolution.header_backing header with
    | None -> Ok (item_index, Int_map.add number header seen)
    | Some backing -> (
        match
          validate_type table parent "aggregate backing target"
            (Aggregate_header_resolution.backing_type backing)
        with
        | Error _ as error -> error
        | Ok () -> Ok (item_index, Int_map.add number header seen))

let header_map table parent headers =
  let rec loop previous_item seen = function
    | [] -> Ok seen
    | header :: rest -> (
        match validate_header table parent previous_item seen header with
        | Error _ as error -> error
        | Ok (item_index, seen) -> loop item_index seen rest)
  in
  loop (-1) Int_map.empty (Aggregate_header_resolution.headers headers)

let backing_aggregate headers header =
  match Aggregate_header_resolution.header_backing header with
  | None -> None
  | Some backing -> (
      let type_ = Aggregate_header_resolution.backing_type backing in
      if Type.pointer_depth type_ <> 0 then None
      else
        match Type.base type_ with
        | Type.Primitive _ -> None
        | Type.Aggregate symbol ->
            Int_map.find_opt (symbol_number symbol) headers)

type visit = Visiting | Complete

let validate_backing_cycles headers =
  let rec visit states path header =
    let symbol = Aggregate_header_resolution.header_symbol header in
    let number = symbol_number symbol in
    match Int_map.find_opt number states with
    | Some Complete -> Ok states
    | Some Visiting ->
        Error
          (backing_cycle
             (List.rev (symbol :: path))
             (Aggregate_header_resolution.header_origin header))
    | None -> (
        let states = Int_map.add number Visiting states in
        match backing_aggregate headers header with
        | None -> Ok (Int_map.add number Complete states)
        | Some next -> (
            match visit states (symbol :: path) next with
            | Error _ as error -> error
            | Ok states -> Ok (Int_map.add number Complete states)))
  in
  let rec all states = function
    | [] -> Ok ()
    | header :: rest -> (
        match visit states [] header with
        | Error _ as error -> error
        | Ok states -> all states rest)
  in
  all Int_map.empty (headers |> Int_map.bindings |> List.map snd)

let validate_function table parent previous_item seen function_ =
  let symbol = Function_call_resolution.function_symbol function_ in
  let scope = Function_call_resolution.function_scope function_ in
  let item_index = Function_call_resolution.function_item_index function_ in
  let number = symbol_number symbol in
  if item_index <= previous_item then
    Error (invalid_input "direct call functions do not follow source order")
  else if Int_map.mem number seen then
    Error (invalid_input "direct call functions repeat a declaration symbol")
  else if not (Symbol_table.owns_symbol table symbol) then
    Error (invalid_input "direct call function belongs to another symbol table")
  else if not (Symbol_table.owns_scope table scope) then
    Error
      (invalid_input
         "direct call function scope belongs to another symbol table")
  else if not (symbol_in_scope symbol parent) then
    Error (invalid_input "direct call function has the wrong module scope")
  else if Symbol_table.scope_kind scope <> Symbol_table.Function then
    Error (invalid_input "direct call function has a nonfunction scope")
  else if
    match Symbol_table.parent scope with
    | Some owner -> not (same_scope owner parent)
    | None -> true
  then Error (invalid_input "direct call function has the wrong module parent")
  else Ok (item_index, Int_map.add number function_ seen)

let validate_functions table parent calls =
  let rec loop previous_item seen = function
    | [] -> Ok ()
    | function_ :: rest -> (
        match validate_function table parent previous_item seen function_ with
        | Error _ as error -> error
        | Ok (item_index, seen) -> loop item_index seen rest)
  in
  loop (-1) Int_map.empty (Function_call_resolution.functions calls)

let rec source_visible_type headers ~before_item_index type_ =
  if Type.pointer_depth type_ <> 0 then type_
  else
    match Type.base type_ with
    | Type.Primitive _ -> type_
    | Type.Aggregate symbol -> (
        match Int_map.find_opt (symbol_number symbol) headers with
        | None -> type_
        | Some header
          when Aggregate_header_resolution.header_item_index header
               >= before_item_index -> type_
        | Some header -> (
            match Aggregate_header_resolution.header_backing header with
            | None -> type_
            | Some backing ->
                source_visible_type headers ~before_item_index
                  (Aggregate_header_resolution.backing_type backing)))

let forwarded_target_class headers ~before_item_index type_ =
  let type_ = source_visible_type headers ~before_item_index type_ in
  if Type.pointer_depth type_ <> 0 then Integer_result
  else
    match Type.base type_ with
    | Type.Primitive (_, Primitive_type.F64) -> F64_result
    | Type.Primitive _ | Type.Aggregate _ -> Integer_result

let parameter_target_class headers ~before_item_index parameter =
  match Function_type_resolution.parameter_declarator_kind parameter with
  | Function_type_resolution.Function_pointer _ -> Integer_result
  | Function_type_resolution.Object ->
      parameter |> Function_type_resolution.parameter_type_reference
      |> Type_reference.resolved_type
      |> forwarded_target_class headers ~before_item_index

let fixed_policy headers ~before_item_index source =
  let path =
    match Function_call_resolution.fixed_value source with
    | Function_call_resolution.Declared_default _ -> Declared_default
    | Function_call_resolution.Provided_argument _ ->
        Provided_expression
          (source |> Function_call_resolution.fixed_parameter
          |> parameter_target_class headers ~before_item_index)
  in
  { source; path }

let direct_call headers ~before_item_index
    (source : Function_call_resolution.direct_call) : direct_call =
  {
    source;
    fixed_policies =
      source |> Function_call_resolution.direct_fixed_arguments
      |> List.map (fixed_policy headers ~before_item_index);
    variadic_arguments =
      Function_call_resolution.direct_variadic_arguments source;
  }

let indirect_call headers ~before_item_index
    (source : Function_call_resolution.indirect_call) : indirect_call =
  {
    source;
    fixed_policies =
      source |> Function_call_resolution.indirect_fixed_arguments
      |> List.map (fixed_policy headers ~before_item_index);
    variadic_arguments =
      Function_call_resolution.indirect_variadic_arguments source;
  }

let call_policy headers ~before_item_index = function
  | Function_call_resolution.Direct_call call ->
      Direct_call_policy (direct_call headers ~before_item_index call)
  | Function_call_resolution.Indirect_call call ->
      Indirect_call_policy (indirect_call headers ~before_item_index call)
  | Function_call_resolution.Deferred_call _ as call ->
      Deferred_call_policy call

let resolve_function headers source =
  let item_index = Function_call_resolution.function_item_index source in
  {
    symbol = Function_call_resolution.function_symbol source;
    scope = Function_call_resolution.function_scope source;
    item_index;
    return_type = Function_call_resolution.function_return_type source;
    conditions = Function_call_resolution.function_conditions source;
    selectors = Function_call_resolution.function_selectors source;
    returns = Function_call_resolution.function_returns source;
    calls =
      source |> Function_call_resolution.function_calls
      |> List.map (call_policy headers ~before_item_index:item_index);
  }

let analyze ~table ~parent ~headers ~calls =
  if not (Symbol_table.owns_scope table parent) then
    Error
      (invalid_input "call conversion parent belongs to another symbol table")
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error (invalid_input "call conversion analysis requires a module scope")
  else if not (Function_call_resolution.owns_table calls table) then
    Error (invalid_input "direct call results belong to another symbol table")
  else
    match header_map table parent headers with
    | Error _ as error -> error
    | Ok headers -> (
        match validate_backing_cycles headers with
        | Error _ as error -> error
        | Ok () -> (
            match validate_functions table parent calls with
            | Error _ as error -> error
            | Ok () ->
                let functions =
                  calls |> Function_call_resolution.functions
                  |> List.map (resolve_function headers)
                in
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
                      Function_call_resolution.compilation_mode calls;
                    headers;
                    functions;
                    by_symbol;
                  }))

let forwarded_type_class result ~before_item_index type_ =
  forwarded_target_class result.headers ~before_item_index type_

let forwarded_type result ~before_item_index type_ =
  source_visible_type result.headers ~before_item_index type_

let find_function result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_symbol with
    | Some function_ when same_symbol function_.symbol symbol -> Some function_
    | Some _ | None -> None
