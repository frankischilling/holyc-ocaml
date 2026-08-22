type binding_kind =
  | Named_parameter
  | Variadic_argc
  | Variadic_argv
  | Automatic_local
  | Static_local

type binding_input = {
  binding_symbol : Symbol.t;
  binding_kind : binding_kind;
  parameter_index : int option;
  local_declaration_index : int option;
  local_declarator_index : int option;
}

type function_input = {
  function_symbol : Symbol.t;
  function_scope : Symbol_table.scope;
  function_item_index : int;
  function_bindings : binding_input list;
}

type binding = {
  symbol : Symbol.t;
  kind : binding_kind;
  ordinal : int;
  parameter_index : int option;
  local_declaration_index : int option;
  local_declarator_index : int option;
}

module String_map = Map.Make (String)
module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)

type indexed_function = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  bindings : binding list;
  by_name : binding String_map.t;
}

type t = {
  table : Symbol_table.t;
  functions : indexed_function list;
  by_symbol : indexed_function Int_map.t;
}

type error_kind =
  | Invalid_input of string
  | Duplicate_binding of {
      function_symbol : Symbol.t;
      name : string;
      original : Symbol.t;
      duplicate : Symbol.t;
    }
  | Function_not_indexed of Symbol.t

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let functions result = result.functions
let function_symbol function_ = function_.symbol
let function_scope function_ = function_.scope
let function_item_index function_ = function_.item_index
let function_bindings function_ = function_.bindings
let binding_symbol (binding : binding) = binding.symbol
let binding_kind (binding : binding) = binding.kind
let binding_ordinal (binding : binding) = binding.ordinal
let binding_parameter_index (binding : binding) = binding.parameter_index

let binding_local_declaration_index (binding : binding) =
  binding.local_declaration_index

let binding_local_declarator_index (binding : binding) =
  binding.local_declarator_index

let binding_kind_name = function
  | Named_parameter -> "named-parameter"
  | Variadic_argc -> "variadic-argc"
  | Variadic_argv -> "variadic-argv"
  | Automatic_local -> "automatic-local"
  | Static_local -> "static-local"

let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int
let scope_number scope = Symbol_table.scope_id scope |> Symbol.Scope_id.to_int

let invalid_input message =
  { code = "HCSEMA0014"; kind = Invalid_input message; origin = None }

let duplicate_binding ~function_symbol ~name ~original ~duplicate =
  {
    code = "HCSEMA0015";
    kind = Duplicate_binding { function_symbol; name; original; duplicate };
    origin = Some (Symbol.origin duplicate);
  }

let function_not_indexed function_symbol =
  {
    code = "HCSEMA0016";
    kind = Function_not_indexed function_symbol;
    origin = Some (Symbol.origin function_symbol);
  }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message
  | Duplicate_binding { function_symbol; name; original; duplicate } ->
      Printf.sprintf
        "function %S declares %S more than once; symbol %d repeats symbol %d"
        (Symbol.name function_symbol)
        name (symbol_number duplicate) (symbol_number original)
  | Function_not_indexed function_symbol ->
      Printf.sprintf "function %S has no completed binding index"
        (Symbol.name function_symbol)

let error_to_string error = error.code ^ ": " ^ error_message error

let same_scope left right =
  Symbol.Scope_id.equal
    (Symbol_table.scope_id left)
    (Symbol_table.scope_id right)

let symbol_has_scope symbol scope =
  Symbol.Scope_id.equal (Symbol.scope_id symbol) (Symbol_table.scope_id scope)

let expected_symbol_kind = function
  | Named_parameter | Variadic_argc | Variadic_argv -> Symbol.Parameter
  | Automatic_local | Static_local -> Symbol.Local_variable

let valid_position (input : binding_input) =
  match input.binding_kind with
  | Named_parameter | Variadic_argc | Variadic_argv ->
      Option.is_some input.parameter_index
      && Option.is_none input.local_declaration_index
      && Option.is_none input.local_declarator_index
  | Automatic_local | Static_local ->
      Option.is_none input.parameter_index
      && Option.is_some input.local_declaration_index
      && Option.is_some input.local_declarator_index

type order_state =
  | Fixed_parameters of int
  | After_argc of int
  | After_argv
  | Local_bindings of int * int

let binding_position_error input message =
  Error
    (invalid_input
       (Printf.sprintf "function binding %S has %s"
          (Symbol.name input.binding_symbol)
          message))

let nonnegative_option = function
  | None -> true
  | Some value -> value >= 0

let validate_binding_order state (input : binding_input) =
  if
    not
      (nonnegative_option input.parameter_index
      && nonnegative_option input.local_declaration_index
      && nonnegative_option input.local_declarator_index)
  then binding_position_error input "a negative source position"
  else if not (valid_position input) then
    binding_position_error input "inconsistent parameter and local positions"
  else
    match (state, input.binding_kind) with
    | Fixed_parameters previous, Named_parameter ->
        let current = Option.get input.parameter_index in
        if current <= previous then
          binding_position_error input
            "a parameter position outside source order"
        else Ok (Fixed_parameters current)
    | Fixed_parameters previous, Variadic_argc ->
        let current = Option.get input.parameter_index in
        if current <= previous then
          binding_position_error input "an argc position outside source order"
        else Ok (After_argc current)
    | After_argc argc_index, Variadic_argv ->
        let current = Option.get input.parameter_index in
        if current <> argc_index + 1 then
          binding_position_error input
            "an argv position that does not follow argc"
        else Ok After_argv
    | (Fixed_parameters _ | After_argv), (Automatic_local | Static_local) ->
        Ok
          (Local_bindings
             ( Option.get input.local_declaration_index,
               Option.get input.local_declarator_index ))
    | ( Local_bindings (previous_declaration, previous_declarator),
        (Automatic_local | Static_local) ) ->
        let declaration = Option.get input.local_declaration_index in
        let declarator = Option.get input.local_declarator_index in
        if
          declaration < previous_declaration
          || declaration = previous_declaration
             && declarator <= previous_declarator
        then
          binding_position_error input "a local position outside source order"
        else Ok (Local_bindings (declaration, declarator))
    | After_argc _, _ ->
        binding_position_error input "a binding between variadic argc and argv"
    | After_argv, (Named_parameter | Variadic_argc | Variadic_argv)
    | Local_bindings _, (Named_parameter | Variadic_argc | Variadic_argv) ->
        binding_position_error input
          "a parameter after the local namespace began"
    | Fixed_parameters _, Variadic_argv ->
        binding_position_error input "variadic argv without a preceding argc"

let repeated_name_is_permitted = function
  | "pad" | "reserved" | "_anon_" -> true
  | _ -> false

let validate_binding table scope seen_symbols state input =
  let symbol = input.binding_symbol in
  let number = symbol_number symbol in
  if not (Symbol_table.owns_symbol table symbol) then
    Error (invalid_input "function binding belongs to another symbol table")
  else if not (symbol_has_scope symbol scope) then
    Error (invalid_input "function binding belongs to the wrong function scope")
  else if
    not
      (Symbol.equal_kind (Symbol.kind symbol)
         (expected_symbol_kind input.binding_kind))
  then
    Error (invalid_input "function binding has the wrong semantic symbol kind")
  else if Int_set.mem number seen_symbols then
    Error (invalid_input "function binding symbol is repeated")
  else
    match validate_binding_order state input with
    | Error _ as error -> error
    | Ok state -> Ok (Int_set.add number seen_symbols, state)

let validate_bindings table scope inputs =
  let rec loop seen state = function
    | [] ->
        if
          match state with
          | After_argc _ -> true
          | Fixed_parameters _ | After_argv | Local_bindings _ -> false
        then
          Error
            (invalid_input "function binding list ends after argc without argv")
        else Ok ()
    | input :: rest -> (
        match validate_binding table scope seen state input with
        | Error _ as error -> error
        | Ok (seen, state) -> loop seen state rest)
  in
  loop Int_set.empty (Fixed_parameters (-1)) inputs

let build_bindings function_symbol inputs =
  let rec loop ordinal bindings_rev (by_name : binding String_map.t) = function
    | [] -> Ok (List.rev bindings_rev, by_name)
    | input :: rest -> (
        let binding =
          {
            symbol = input.binding_symbol;
            kind = input.binding_kind;
            ordinal;
            parameter_index = input.parameter_index;
            local_declaration_index = input.local_declaration_index;
            local_declarator_index = input.local_declarator_index;
          }
        in
        let name = Symbol.name binding.symbol in
        match String_map.find_opt name by_name with
        | Some original when not (repeated_name_is_permitted name) ->
            Error
              (duplicate_binding ~function_symbol ~name
                 ~original:original.symbol ~duplicate:binding.symbol)
        | Some _ -> loop (ordinal + 1) (binding :: bindings_rev) by_name rest
        | None ->
            loop (ordinal + 1) (binding :: bindings_rev)
              (String_map.add name binding by_name)
              rest)
  in
  loop 0 [] String_map.empty inputs

let validate_function table parent previous_item seen_symbols seen_scopes
    (input : function_input) =
  let symbol = input.function_symbol in
  let scope = input.function_scope in
  let symbol_id = symbol_number symbol in
  let scope_id = scope_number scope in
  if input.function_item_index < 0 then
    Error
      (invalid_input "function binding index has a negative module position")
  else if input.function_item_index <= previous_item then
    Error
      (invalid_input
         "function binding indexes do not follow module source order")
  else if not (Symbol_table.owns_symbol table symbol) then
    Error (invalid_input "indexed function belongs to another symbol table")
  else if not (Symbol_table.owns_scope table scope) then
    Error
      (invalid_input "indexed function scope belongs to another symbol table")
  else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error (invalid_input "function binding index requires a function symbol")
  else if Symbol_table.scope_kind scope <> Symbol_table.Function then
    Error (invalid_input "function binding index requires a function scope")
  else if not (symbol_has_scope symbol parent) then
    Error
      (invalid_input "indexed function symbol does not belong to the module")
  else if
    match Symbol_table.parent scope with
    | Some scope_parent -> not (same_scope scope_parent parent)
    | None -> true
  then
    Error (invalid_input "indexed function scope does not belong to the module")
  else if Int_set.mem symbol_id seen_symbols then
    Error (invalid_input "indexed function symbol is repeated")
  else if Int_set.mem scope_id seen_scopes then
    Error (invalid_input "indexed function scope is repeated")
  else
    match validate_bindings table scope input.function_bindings with
    | Error _ as error -> error
    | Ok () ->
        Ok
          ( input.function_item_index,
            Int_set.add symbol_id seen_symbols,
            Int_set.add scope_id seen_scopes )

let validate table parent inputs =
  if not (Symbol_table.owns_scope table parent) then
    Error
      (invalid_input "function binding parent belongs to another symbol table")
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error (invalid_input "function binding indexes require a module scope")
  else
    let rec loop previous_item seen_symbols seen_scopes = function
      | [] -> Ok ()
      | input :: rest -> (
          match
            validate_function table parent previous_item seen_symbols
              seen_scopes input
          with
          | Error _ as error -> error
          | Ok (item, seen_symbols, seen_scopes) ->
              loop item seen_symbols seen_scopes rest)
    in
    loop (-1) Int_set.empty Int_set.empty inputs

let build_validated table inputs =
  let rec loop functions_rev by_symbol = function
    | [] -> Ok { table; functions = List.rev functions_rev; by_symbol }
    | input :: rest -> (
        match build_bindings input.function_symbol input.function_bindings with
        | Error _ as error -> error
        | Ok (bindings, by_name) ->
            let indexed =
              {
                symbol = input.function_symbol;
                scope = input.function_scope;
                item_index = input.function_item_index;
                bindings;
                by_name;
              }
            in
            loop (indexed :: functions_rev)
              (Int_map.add (symbol_number indexed.symbol) indexed by_symbol)
              rest)
  in
  loop [] Int_map.empty inputs

let build ~table ~parent inputs =
  match validate table parent inputs with
  | Error _ as error -> error
  | Ok () -> build_validated table inputs

let find_function result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_symbol with
    | Some indexed when indexed.symbol == symbol -> Some indexed
    | Some _ | None -> None

let lookup result ~function_ ~name =
  if String.equal name "" then
    Error (invalid_input "function binding lookup name cannot be empty")
  else if not (Symbol_table.owns_symbol result.table function_) then
    Error (invalid_input "function binding lookup uses a foreign symbol")
  else
    match find_function result function_ with
    | None -> Error (function_not_indexed function_)
    | Some indexed -> Ok (String_map.find_opt name indexed.by_name)
