type resolution =
  | Local_binding of Function_binding_index.binding
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type occurrence
type resolved_function
type t

type error_kind =
  | Invalid_input of string
  | Unresolved_identifier of {
      name : string;
      compilation_mode : Outer_environment.compilation_mode;
    }

type error

val resolve :
  table:Symbol_table.t ->
  environment:Outer_environment.t ->
  expressions:Module_expression_binding.t ->
  (t, error) result
(** Preserve local and compilation-unit bindings, then resolve each remaining
    ordinary identifier through the complete mode-specific outer table chain. An
    absent name is an error rather than an implicit import. *)

val functions : t -> resolved_function list
val environment : t -> Outer_environment.t
val source : t -> Module_expression_binding.t
val owns_table : t -> Symbol_table.t -> bool
val find_function : t -> Symbol.t -> resolved_function option
val function_symbol : resolved_function -> Symbol.t
val function_scope : resolved_function -> Symbol_table.scope
val function_item_index : resolved_function -> int
val function_occurrences : resolved_function -> occurrence list
val occurrence_source : occurrence -> Module_expression_binding.occurrence
val occurrence_index : occurrence -> int
val occurrence_name : occurrence -> string
val occurrence_origin : occurrence -> Symbol.origin
val occurrence_resolution : occurrence -> resolution
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
