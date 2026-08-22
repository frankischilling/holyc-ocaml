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

type binding = private {
  symbol : Symbol.t;
  kind : binding_kind;
  ordinal : int;
  parameter_index : int option;
  local_declaration_index : int option;
  local_declarator_index : int option;
}

type indexed_function
type t

type error_kind =
  | Invalid_input of string
  | Duplicate_binding of {
      function_symbol : Symbol.t;
      name : string;
      original : Symbol.t;
      duplicate : Symbol.t;
    }
  | Function_not_indexed of Symbol.t

type error

val build :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  function_input list ->
  (t, error) result
(** Validate each function namespace in source order. Ordinary repeated names
    are rejected. Repeated [pad], [reserved], and [_anon_] names remain in the
    binding list, while lookup selects their first source occurrence. *)

val functions : t -> indexed_function list
val find_function : t -> Symbol.t -> indexed_function option
val function_symbol : indexed_function -> Symbol.t
val function_scope : indexed_function -> Symbol_table.scope
val function_item_index : indexed_function -> int
val function_bindings : indexed_function -> binding list
val binding_symbol : binding -> Symbol.t
val binding_kind : binding -> binding_kind
val binding_ordinal : binding -> int
val binding_parameter_index : binding -> int option
val binding_local_declaration_index : binding -> int option
val binding_local_declarator_index : binding -> int option

val lookup :
  t -> function_:Symbol.t -> name:string -> (binding option, error) result
(** Look up a name in the completed function namespace without changing use
    counts. Publication timing belongs to the expression-binding pass. *)

val binding_kind_name : binding_kind -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
