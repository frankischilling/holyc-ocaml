type definition_kind =
  | Language_label
  | Assembly_global_label
  | Assembly_exported_global_label
  | Assembly_local_label

type occurrence_kind = Definition of definition_kind | Goto_reference
type occurrence
type function_labels
type resolved_occurrence
type label
type resolved_function
type t

val make_definition :
  name:string ->
  definition_kind:definition_kind ->
  origin:Symbol.origin ->
  occurrence_index:int ->
  (occurrence, string) result

val make_goto :
  name:string ->
  origin:Symbol.origin ->
  occurrence_index:int ->
  (occurrence, string) result

val make_function :
  symbol:Symbol.t ->
  scope:Symbol_table.scope ->
  item_index:int ->
  occurrence list ->
  (function_labels, string) result

val resolve : table:Symbol_table.t -> function_labels list -> (t, string) result
(** Validate every function and label before inserting one [Symbol.Label] per
    definition. Function-local assembly definitions share this namespace with
    language labels, as they do in [COCGoToLabelFind]. *)

val functions : t -> resolved_function list
val function_symbol : resolved_function -> Symbol.t
val function_scope : resolved_function -> Symbol_table.scope
val function_item_index : resolved_function -> int
val function_labels : resolved_function -> label list
val function_occurrences : resolved_function -> resolved_occurrence list
val label_symbol : label -> Symbol.t
val label_definition_kind : label -> definition_kind
val label_first_occurrence_index : label -> int
val label_goto_count : label -> int

val label_use_count : label -> int
(** Return the source-compatible use count. An assembly definition contributes
    one use because TempleOS suppresses its unused-label warning. *)

val occurrence_symbol : resolved_occurrence -> Symbol.t
val occurrence_kind : resolved_occurrence -> occurrence_kind
val occurrence_origin : resolved_occurrence -> Symbol.origin
val occurrence_index : resolved_occurrence -> int
val definition_kind_name : definition_kind -> string
val occurrence_kind_name : occurrence_kind -> string
