type binding_kind =
  | Named_parameter
  | Variadic_argc
  | Variadic_argv
  | Automatic_local
  | Static_local

type variadic_parameter = Argc | Argv
type local_storage = Automatic | Static
type binding
type function_declaration
type entry
type collected_function
type t

val make_named_parameter :
  name:string ->
  origin:Symbol.origin ->
  parameter_index:int ->
  (binding, string) result

val make_variadic_parameter :
  variadic_parameter ->
  origin:Symbol.origin ->
  parameter_index:int ->
  (binding, string) result

val make_local :
  name:string ->
  origin:Symbol.origin ->
  storage:local_storage ->
  declaration_index:int ->
  declarator_index:int ->
  (binding, string) result

val make_function :
  symbol:Symbol.t ->
  item_index:int ->
  binding list ->
  (function_declaration, string) result

val collect :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  function_declaration list ->
  (t, string) result
(** Create one function scope per declaration and insert parameters followed by
    function-wide locals. Repeated names remain separate entries. *)

val functions : t -> collected_function list
val function_symbol : collected_function -> Symbol.t
val function_scope : collected_function -> Symbol_table.scope
val function_item_index : collected_function -> int
val function_entries : collected_function -> entry list
val entry_symbol : entry -> Symbol.t
val entry_kind : entry -> binding_kind
val entry_parameter_index : entry -> int option
val entry_local_declaration_index : entry -> int option
val entry_declarator_index : entry -> int option
val binding_kind_name : binding_kind -> string
