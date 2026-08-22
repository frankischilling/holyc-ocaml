type event

val make_identifier :
  name:string -> origin:Symbol.origin -> (event, string) result

type input

val make_statement :
  statement_index:int ->
  item_index:int ->
  origin:Symbol.origin ->
  event list ->
  (input, string) result

type resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_candidate

type occurrence
type statement
type t
type error_kind = Invalid_input of string
type error

val resolve :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  module_expressions:Module_expression_binding.t ->
  input list ->
  (t, error) result
(** Replay the checked module publication stream through executable top-level
    statements. Names absent from the visible publication prefix remain outer
    candidates. *)

val owns_table : t -> Symbol_table.t -> bool
val owns_module_expressions : t -> Module_expression_binding.t -> bool
val module_expressions : t -> Module_expression_binding.t
val statements : t -> statement list
val all_occurrences : t -> occurrence list
val statement_source : statement -> input
val statement_index : statement -> int
val statement_item_index : statement -> int
val statement_origin : statement -> Symbol.origin
val statement_occurrences : statement -> occurrence list
val occurrence_index : occurrence -> int
val occurrence_name : occurrence -> string
val occurrence_origin : occurrence -> Symbol.origin
val occurrence_resolution : occurrence -> resolution
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
