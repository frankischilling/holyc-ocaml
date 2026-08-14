type binding_input
type function_input
type warning_kind = Unused_variable | Unneeded_no_warn
type warning
type binding_analysis
type analyzed_function
type t
type error_kind = Invalid_input of string
type error

val make_binding_input :
  binding:Function_binding_index.binding ->
  initial_flag_mask:int64 ->
  binding_input

val make_function_input :
  symbol:Symbol.t ->
  scope:Symbol_table.scope ->
  item_index:int ->
  is_definition:bool ->
  binding_input list ->
  (function_input, string) result

val analyze :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  bindings:Function_binding_index.t ->
  expressions:Function_expression_binding.t ->
  compiler_option_mask:int64 ->
  function_input list ->
  (t, error) result
(** Reproduce the represented [CMemberLst.use_cnt] and [MLF_NO_UNUSED_WARN]
    rules without changing the symbol table. The compiler option mask applies to
    the complete function batch. *)

val compiler_option_mask : t -> int64
val functions : t -> analyzed_function list
val warnings : t -> warning list
val find_function : t -> Symbol.t -> analyzed_function option
val function_symbol : analyzed_function -> Symbol.t
val function_scope : analyzed_function -> Symbol_table.scope
val function_item_index : analyzed_function -> int
val function_is_definition : analyzed_function -> bool
val function_bindings : analyzed_function -> binding_analysis list
val function_warnings : analyzed_function -> warning list
val binding_source : binding_analysis -> Function_binding_index.binding
val binding_initial_flag_mask : binding_analysis -> int64
val binding_effective_flag_mask : binding_analysis -> int64
val binding_has_flag : binding_analysis -> Member_flag.t -> bool
val binding_ordinary_use_count : binding_analysis -> int
val binding_query_use_count : binding_analysis -> int
val binding_suppression_count : binding_analysis -> int
val binding_source_use_count : binding_analysis -> int
val binding_suppression_origins : binding_analysis -> Symbol.origin list
val warning_kind : warning -> warning_kind
val warning_code : warning -> string
val warning_function_symbol : warning -> Symbol.t
val warning_binding_symbol : warning -> Symbol.t
val warning_origin : warning -> Symbol.origin
val warning_message : warning -> string
val warning_kind_name : warning_kind -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
