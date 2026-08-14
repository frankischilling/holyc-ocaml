type compilation_mode = Function_resolution.compilation_mode = Jit | Aot
type table_kind = Jit_task of int | Aot_parent of int | Assembler

type record_kind =
  | Aggregate
  | Function
  | Global_variable
  | Export_system_symbol

type entry
type table
type binding
type t
type error_kind = Invalid_input of string
type error

val make_entry :
  symbol:Symbol.t ->
  record_kind:record_kind ->
  entry_index:int ->
  (entry, error) result
(** Describe one source-ordered record in an outer hash table. Export-system
    records use the semantic assembler-symbol kind until address and loader
    metadata are modeled. *)

val make_table :
  table_kind:table_kind ->
  table_index:int ->
  entry list ->
  (table, error) result
(** Build one immutable table. Entries are supplied oldest to newest, matching
    source publication order. *)

val create :
  table:Symbol_table.t ->
  compilation_mode:compilation_mode ->
  table list ->
  (t, error) result
(** Validate a complete mode-specific table chain. JIT requires at least the
    current task and a final assembler table. AOT accepts zero or more enclosing
    compilations followed by one assembler table. *)

val find : t -> string -> binding option
val compilation_mode : t -> compilation_mode
val tables : t -> table list
val owns_table : t -> Symbol_table.t -> bool
val table_kind : table -> table_kind
val table_index : table -> int
val table_entries : table -> entry list
val entry_symbol : entry -> Symbol.t
val entry_record_kind : entry -> record_kind
val entry_index : entry -> int
val binding_table : binding -> table
val binding_entry : binding -> entry
val compilation_mode_name : compilation_mode -> string
val table_kind_name : table_kind -> string
val record_kind_name : record_kind -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
