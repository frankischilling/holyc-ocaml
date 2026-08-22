type resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type occurrence
type statement
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
  expressions:Top_level_expression_binding.t ->
  (t, error) result
(** Preserve source-visible module bindings, then resolve every remaining
    top-level candidate through the complete mode-specific outer table chain. *)

val owns_table : t -> Symbol_table.t -> bool
val environment : t -> Outer_environment.t
val source : t -> Top_level_expression_binding.t
val statements : t -> statement list
val all_occurrences : t -> occurrence list
val statement_source : statement -> Top_level_expression_binding.statement
val statement_index : statement -> int
val statement_item_index : statement -> int
val statement_origin : statement -> Symbol.origin
val statement_occurrences : statement -> occurrence list
val occurrence_source : occurrence -> Top_level_expression_binding.occurrence
val occurrence_index : occurrence -> int
val occurrence_name : occurrence -> string
val occurrence_origin : occurrence -> Symbol.origin
val occurrence_resolution : occurrence -> resolution
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
