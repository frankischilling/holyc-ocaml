type error = { code : string; message : string; span : Common.Span.t option }
type slot
type t

val hard_max_global_bytes : int
val hard_max_arena_bytes : int
val validate_global_limit : max_global_bytes:int -> (unit, error list) result

val create :
  functions:Ir.Integer_interpreter.function_definition list ->
  max_global_bytes:int ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  (t, error list) result
(** Seal ordinary integer globals and statics for one exact compiled bundle.
    Fixed arrays retain their checked shape, full object extent and per-element
    initialization state. Statics require their unique supplied function and
    exact frame/location. Declaration initial values require [create_prepared]
    and original native preparation proof. Retained task storage and foreign
    owners are rejected. *)

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

val globals : t -> Ir.Integer_globals.t
val entry : t -> Ir.X87_stack.t
val global_bytes : t -> int

val image : t -> string
(** A fresh copy of the sealed initial object bytes and private flags. *)

val is_empty : t -> bool

val find_symbol : t -> Sema.Symbol.t -> slot option
(** Lookup requires the exact symbol object retained by the sealed storage slot.
*)

val source_slot : slot -> Ir.Integer_globals.storage_slot
val owns_address : slot -> Ir.Runtime_call_context.owner -> bool
val symbol : slot -> Sema.Symbol.t
val type_ : slot -> Sema.Type.t
val scalar : slot -> Ir.Integer_scalar_storage.t
val dimensions : slot -> int64 list
val strides : slot -> int64 list
val element_count : slot -> int
val extent_bytes : slot -> int
val data_offset : slot -> int
val flag_offset : slot -> int
val initially_initialized : slot -> bool
