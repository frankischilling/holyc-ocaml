type t
type primitive_binding

val primitive_for :
  t -> Frontend.Symbol_visibility.entry -> primitive_binding option
(** Exact association recorded when this frontend entry and primitive semantic
    symbol were seeded. Names, IDs, origins and later entries grant no
    authority. Task views share associations; frontend forks bind the known
    entries to their own fresh semantic symbols. *)

val primitive_symbol : primitive_binding -> Sema.Symbol.t
val primitive_type : primitive_binding -> Common.Primitive_type.t
val primitive_record : primitive_binding -> Sema.Compiler_record.t
val create : unit -> t

val task_frontend : t -> t
(** Retain a private frontend publication owner sharing source files and
    semantic table identity with the session. Sibling task publications are
    hidden from lookups; the original root session can inspect all entries. *)

val fork_frontend : t -> t
(** Copy the frontend definition and symbol state while sharing source files.
    Semantic state starts fresh. *)

val sources : t -> Common.Source_manager.t
val definitions : t -> Frontend.Definition.Environment.t
val symbols : t -> Frontend.Symbol_visibility.Environment.t
val semantic_symbols : t -> Sema.Symbol_table.t
val add_source : t -> path:string -> contents:string -> Common.Source_file.t

val load_source :
  ?max_bytes:int ->
  ?display_path:string ->
  t ->
  path:string ->
  (Common.Source_file.t, string) result
