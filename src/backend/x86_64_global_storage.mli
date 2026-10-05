type error = { code : string; message : string; span : Common.Span.t option }
type slot
type t
type task_layout
type task_snapshot

val hard_max_task_layout_work : int
(** Cumulative retained binding and declared storage visits admitted by one task
    layout. Map lookup preserves exact identities without nested list scans. *)

val hard_max_global_bytes : int
val hard_max_arena_bytes : int
val validate_global_limit : max_global_bytes:int -> (unit, error list) result

val create :
  functions:Ir.Integer_interpreter.function_definition list ->
  max_global_bytes:int ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  (t, error list) result
(** Seal integer and original callback globals and statics for one exact
    compiled bundle. Fixed arrays retain their checked shape, full object extent
    and per-element initialization state. Statics require their unique supplied
    function and exact frame/location. Declaration initial values require
    [create_prepared] and original native preparation proof. Retained task
    storage and foreign owners are rejected. *)

val create_prepared :
  functions:Ir.Integer_interpreter.function_definition list ->
  initializers:Driver.Native_global_initializers.t ->
  max_global_bytes:int ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  (t, error list) result

(** Seal original prepared global/static values to the same exact bundle and
    restore their declared-width bytes and per-element initialization flags in
    each image. *)

val create_task_layout :
  ?max_layout_work:int ->
  ?max_literal_bytes:int ->
  max_global_bytes:int ->
  unit ->
  (task_layout, error list) result
(** Create a bounded append-only integer layout. Its first original fragment
    binds the layout to that retained task; later snapshots require the same
    catalog and exact retained references and storage objects. *)

val claim_task_arena : task_layout -> (unit, string) result
(** Internal native ownership admission. Each layout has one storage arena for
    its lifetime; release does not authorize a replacement or a copied arena. *)

val task_layout_work : task_layout -> int

val create_task_snapshot :
  task_layout ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  (task_snapshot, error list) result
(** Append previously unseen original scalar globals or fixed integer arrays and
    their initialization flags without moving earlier offsets. Arrays retain the
    original checked dimensions, strides and full object extent, with one
    reversed eight-byte initialization flag per element. This allocates
    immutable layout metadata, not runtime values, and grants no source
    execution authority. *)

val task_snapshot_matches_layout : task_snapshot -> task_layout -> bool
val task_snapshot_arena_image : task_snapshot -> string
val task_snapshot_arena_bytes : task_snapshot -> int
val task_snapshot_global_bytes : task_snapshot -> int
val task_snapshot_literal_bytes : task_snapshot -> int
val task_snapshot_literals : task_snapshot -> X86_64_literal_storage.t

val task_snapshot_initializations_since :
  task_snapshot -> arena_prefix_bytes:int -> (int * string) list

val append_task_literals :
  task_snapshot ->
  sources:X86_64_literal_storage.source list ->
  work:int ->
  (task_snapshot, error list) result
(** Append original sealed literal regions after this exact current snapshot.
    Literal bytes, canonical reference tables and graph visits are cumulative.
    Earlier global, literal and table offsets remain unchanged. *)

val task_snapshot_storage : task_snapshot -> t

val task_snapshot_matches :
  task_snapshot ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  bool

val globals : t -> Ir.Integer_globals.t
val entry : t -> Ir.X87_stack.t
val global_bytes : t -> int
val arena_bytes : t -> int

val image : t -> string
(** A fresh copy of the sealed initial object bytes and private flags. *)

val is_empty : t -> bool

val find_symbol : t -> Sema.Symbol.t -> slot option
(** Lookup requires the exact symbol object retained by the sealed storage slot.
*)

val find_symbol_from_source :
  t -> source_globals:Ir.Integer_globals.t -> Sema.Symbol.t -> slot option
(** Resolve a historical source symbol through an exact task storage view from
    the same catalog. The source view and persistent slot must name the same
    physical storage object. *)

val find_retained : t -> Ir.Retained_global.t -> slot option
(** Resolve only an original reference present in this fragment's exact retained
    snapshot and bound to the same append-only storage object. *)

val find_retained_from_source :
  t ->
  source_globals:Ir.Integer_globals.t ->
  Ir.Retained_global.t ->
  slot option
(** Resolve a historical retained reference through its exact original task
    storage view. The source view must belong to the same task catalog as this
    append-only layout, and both the retained reference and physical storage
    object must match the slot originally admitted to the layout. *)

val source_slot : slot -> Ir.Integer_globals.storage_slot
val owns_address : slot -> Ir.Runtime_call_context.owner -> bool
val symbol : slot -> Sema.Symbol.t
val type_ : slot -> Sema.Type.t
val callback : slot -> Sema.Function_type_resolution.function_pointer option
val code_owner_offset : slot -> int option

(* Private image-local ownership words, one per callback element. *)
val scalar : slot -> Ir.Integer_scalar_storage.t
val dimensions : slot -> int64 list
val strides : slot -> int64 list
val element_count : slot -> int
val extent_bytes : slot -> int
val data_offset : slot -> int
val flag_offset : slot -> int
val initially_initialized : slot -> bool
