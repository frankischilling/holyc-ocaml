type compilation_mode = Jit | Aot
type storage = Code_heap | Data_heap
type binding_kind = Extern_binding | Import_binding | Intern_binding
type binding_target_kind = No_target | Symbol_target | Expression_target

type declaration_kind =
  | Definition
  | Extern
  | Alternate_extern
  | Import
  | Alternate_import
  | Intern

type state =
  | Defined
  | Unresolved_extern
  | Declared_extern
  | Bound_extern
  | Imported

type binding_target
type source_binding
type declaration
type global_record
type t

val no_binding_target : binding_target

val make_symbol_binding_target :
  name:string -> origin:Symbol.origin -> (binding_target, string) result

val make_expression_binding_target : origin:Symbol.origin -> binding_target

val make_source_binding :
  kind:binding_kind ->
  spelling:string ->
  origin:Symbol.origin ->
  target:binding_target ->
  (source_binding, string) result
(** Validate one of the exact source binding shapes accepted by the parser. *)

val make_declaration :
  ?compiler_option_mask:int64 ->
  global:Global_type_resolution.global ->
  ?binding:source_binding ->
  unit ->
  (declaration, string) result
(** Describe one checked global record without changing the symbol table. The
    option snapshot converts extern forms to their effective import kind and
    selects data-heap storage for definitions while retaining the source
    binding. *)

val resolve :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  compilation_mode:compilation_mode ->
  declaration list ->
  (t, string) result
(** Reconcile source-ordered global records. Every declaration remains a
    separate record; a later definition may attach an immediate alias edge to
    the newest prior record under the pinned JIT or AOT rule. *)

val compilation_mode : t -> compilation_mode
val records : t -> global_record list
val declaration_global : declaration -> Global_type_resolution.global
val declaration_storage : declaration -> storage
val declaration_binding : declaration -> source_binding option

(* Return the binding written in the source. *)
val declaration_source_kind : declaration -> declaration_kind

(* Return the binding used for record reconciliation. *)
val declaration_kind : declaration -> declaration_kind
val declaration_compiler_option_mask : declaration -> int64
val global_record_declaration : global_record -> declaration
val global_record_global : global_record -> Global_type_resolution.global
val global_record_symbol : global_record -> Symbol.t
val global_record_kind : global_record -> declaration_kind
val global_record_source_kind : global_record -> declaration_kind
val global_record_storage : global_record -> storage
val global_record_state : global_record -> state
val global_record_alias_target : global_record -> Symbol.t option
val source_binding_kind : source_binding -> binding_kind
val source_binding_spelling : source_binding -> string
val source_binding_origin : source_binding -> Symbol.origin
val source_binding_target : source_binding -> binding_target
val binding_target_kind : binding_target -> binding_target_kind
val binding_target_name : binding_target -> string option
val binding_target_origin : binding_target -> Symbol.origin option
val compilation_mode_name : compilation_mode -> string
val storage_name : storage -> string
val binding_kind_name : binding_kind -> string
val binding_target_kind_name : binding_target_kind -> string
val declaration_kind_name : declaration_kind -> string
val state_name : state -> string
