type compilation_mode = Function_resolution.compilation_mode = Jit | Aot
type table_kind = Jit_task of int | Aot_parent of int | Assembler

type record_kind =
  | Aggregate
  | Function
  | Global_variable
  | Export_system_symbol

type global_declarator_kind =
  | Object_global
  | Function_pointer_global of Function_type_resolution.function_pointer

type global_metadata
type function_metadata
type entry
type table
type binding
type t
type error_kind = Invalid_input of string
type error

val make_global_metadata :
  type_reference:Type_reference.t ->
  declarator_kind:global_declarator_kind ->
  array_rank:int ->
  (global_metadata, error) result
(** Retain the checked fields needed from one source [CHashGlblVar]. The rank
    represents its linked dimension list; storage and runtime addresses remain
    outside this semantic snapshot. *)

val make_entry :
  symbol:Symbol.t ->
  record_kind:record_kind ->
  entry_index:int ->
  (entry, error) result
(** Describe one source-ordered record in an outer hash table. Export-system
    records use the semantic assembler-symbol kind until address and loader
    metadata are modeled. *)

val make_global_entry :
  symbol:Symbol.t ->
  entry_index:int ->
  global_metadata:global_metadata ->
  (entry, error) result
(** Build one global-variable record with its checked source type, callback, and
    array-rank metadata. *)

val make_function_metadata :
  records:Function_record_classification.t ->
  declaration:Function_resolution.resolved_declaration ->
  (function_metadata, error) result
(** Retain the exact declaration and its owning classification snapshot. *)

val make_function_entry :
  entry_index:int ->
  function_metadata:function_metadata ->
  (entry, error) result
(** Publish under the declaration's canonical identity symbol. *)

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

val find_record : t -> name:string -> record_kind:record_kind -> binding option
(** Find the newest matching name and record kind across the table chain. This
    mirrors the type-mask filtering performed by TempleOS [HashFind]. *)

val with_function_versions :
  t -> table:table -> function_metadata list -> (t * entry list, error) result
(** Extend an immutable environment with exact current or joined-ancestor
    versions of function records in the supplied existing table. The returned
    entries are available only through exact binding membership, never name
    lookup or [table_entries]. Each declaration can appear in history once;
    indexes follow the table's primary entries and any existing history. The
    version must belong to the original classification snapshot reached through
    the current record's exact retained predecessor chain. Reconstructed tables
    or classifications, unrelated declarations and foreign metadata fail. *)

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
val entry_global_metadata : entry -> global_metadata option
val entry_function_metadata : entry -> function_metadata option

val function_declaration :
  function_metadata -> Function_resolution.resolved_declaration

val function_classified_declaration :
  function_metadata -> Function_record_classification.classified_declaration

val global_type_reference : global_metadata -> Type_reference.t
val global_declarator_kind : global_metadata -> global_declarator_kind
val global_array_rank : global_metadata -> int
val binding_table : binding -> table
val binding_entry : binding -> entry
val binding_for_entry : t -> entry -> binding option

val owns_binding : t -> binding -> bool
(** Resolve and validate exact entry/table membership without repeating a name
    lookup. An equal reconstructed entry or table grants no ownership. *)

val compilation_mode_name : compilation_mode -> string
val table_kind_name : table_kind -> string
val record_kind_name : record_kind -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
