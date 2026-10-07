type error = { code : string; message : string; span : Common.Span.t option }
type slot
type t
type task_layout
type task_snapshot
type static_reservation
type static_copy

val prepare_static_copy :
  task_layout ->
  Driver.Integer_task.Native_static_copy.request ->
  admitted_arena_bytes:int ->
  (static_copy, string) result

val check_static_copy :
  static_copy ->
  layout:task_layout ->
  request:Driver.Integer_task.Native_static_copy.request ->
  (unit, string) result

val static_copy_payload : static_copy -> int * int * int * string
(** Checked arena prefix, byte destination, first initialization flag and a
    detached original payload. Flags descend by eight bytes per element. This
    plan grants no entry or write authority; the host must claim its original
    live request while holding the exact arena lease. *)

val reserve_static :
  task_layout ->
  Driver.Integer_task.Native_static_allocation.request ->
  (static_reservation, error list) result

val check_static_reservation :
  static_reservation ->
  layout:task_layout ->
  request:Driver.Integer_task.Native_static_allocation.request ->
  (unit, string) result

val static_reservation_arena_bytes : static_reservation -> int

val static_reservation_initializations_since :
  static_reservation -> arena_prefix_bytes:int -> (int * string) list
(** Original live private integer or one-star callback allocation, with padded
    data and inaccessible padding separate from the checked object extent.
    Callback cells retain their original anonymous header and reserve a private
    eight-byte owner lane per element in addition to initialization flags. No
    entry graph, address or initial values are fabricated. Pending static
    storage grants no function address access. The host must still claim its
    live request before arena admission. *)

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
  ?functions:Ir.Integer_interpreter.function_definition list ->
  task_layout ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  (task_snapshot, error list) result
(** Append original scalar globals, fixed integer arrays and one-star callback
    word storage without moving earlier offsets. Arrays retain their checked
    dimensions, strides and extent, with one reversed eight-byte initialization
    flag per element. Callback cells also reserve one eight-byte
    executable-owner lane per element. Header completion may refresh the
    callback pointer only within the same physical original source object. Data
    extent charges the logical limit; flags and owner lanes charge the arena
    bound. This allocates layout metadata, not runtime values, and grants no
    source entry or executable ownership. *)

type code_owner

val task_code_owners : task_snapshot -> code_owner list
val code_owner_link : code_owner -> Ir.Retained_function.t

val code_owner_definition :
  code_owner -> Ir.Integer_interpreter.function_definition

val code_owner_id : code_owner -> int
val code_owner_address : code_owner -> int
val code_owner_target : code_owner -> int

val append_task_code_owners :
  task_snapshot ->
  (Ir.Retained_function.t * Ir.Integer_interpreter.function_definition) list ->
  (task_snapshot, error list) result
(** Reserve immutable original-body identities and two private native address
    cells per owner. No runtime address or executable authority is published. *)

type provider_code_owner

val task_provider_code_owners : task_snapshot -> provider_code_owner list

val provider_code_owner_binding :
  provider_code_owner -> Ir.Integer_interpreter.native_slot_address_binding

val provider_code_owner_id : provider_code_owner -> int
val provider_code_owner_address : provider_code_owner -> int
val provider_code_owner_target : provider_code_owner -> int

val append_task_provider_code_owners :
  task_snapshot ->
  Ir.Integer_interpreter.native_slot_address_binding list ->
  (task_snapshot, error list) result
(** Reserve distinct PutChars entry owners from current original task bindings.
    Entries retain the original extern declaration after a joined source body
    replaces its slot. Reservation grants no machine entry or call admission. *)

val task_snapshot_matches_layout : task_snapshot -> task_layout -> bool

type undefined_code_owner

val task_undefined_code_owner : task_snapshot -> undefined_code_owner option
val undefined_code_owner_id : undefined_code_owner -> int
val undefined_code_owner_address : undefined_code_owner -> int
val undefined_code_owner_target : undefined_code_owner -> int

type function_slot

val task_function_slots : task_snapshot -> function_slot list

val function_slot_binding :
  function_slot -> Ir.Integer_interpreter.native_slot_address_binding

val function_slot_address : function_slot -> int
val function_slot_owner_offset : function_slot -> int

val find_function_slot :
  task_snapshot ->
  Ir.Runtime_call_context.function_slot_address ->
  function_slot option

val append_task_function_slots :
  task_snapshot ->
  Ir.Integer_interpreter.native_slot_address_binding list ->
  (task_snapshot, error list) result
(** Reserve private mutable logical function slots from original task bindings.
    The shared undefined entry and source slot words charge the arena and layout
    limits. Allocation supplies no machine address or new source admission. *)

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

val owns_address :
  ?source_globals:Ir.Integer_globals.t ->
  slot ->
  Ir.Runtime_call_context.owner ->
  bool

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
