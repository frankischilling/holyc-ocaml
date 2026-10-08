type t
type data

val word : int64 -> t
val word_bits : t -> int64 option

val data :
  source:Sema.Function_call_expression_result.expression_result ->
  type_:Sema.Type.t ->
  (t, string) result
(** Create an immutable descriptor for a checked data result. It grants no
    memory authority; the owning interpreter or native arena must retain the
    actual evaluated address under this descriptor's identity. *)

val data_source : t -> data option

val data_expression :
  data -> Sema.Function_call_expression_result.expression_result

val data_type : data -> Sema.Type.t
val same_data : data -> data -> bool

val with_native_data_comparison :
  owner:unit ref -> offset:int -> live:(unit -> bool) -> t -> (t, string) result
(** The native capture producer attaches the actual arena-relative address read
    under its original lease. The immutable value keeps the same descriptor
    identity and gains no scalar word, address or memory authority. *)

val compare_native_data : data -> data -> (bool, string) result option
(** Compare captured offsets only while both original arenas remain live. *)

val with_string_default : bytes:string -> t -> (t, string) result
(** Attach the actual terminated copy made by the owning default evaluator. The
    descriptor identity stays unchanged; bytes grant no memory authority. *)

val compare_string_defaults : t -> t -> bool option
(** Compare STR_DFT_AVAILABLE flags and saved bytes through the first NUL. If
    neither default carries the string flag, return None. *)

val accepts_callback_expression :
  Sema.Function_call_expression_result.expression_result -> bool

val callback :
  source:Sema.Function_call_expression_result.expression_result ->
  link:Retained_function.t ->
  (t, string) result
(** Preserve the checked expression and selected original function identity.
    This value contains no executable address or native entry permission. *)

val callback_source :
  t ->
  (Retained_function.t * Sema.Function_call_expression_result.expression_result)
  option

val same : t -> t -> bool

val undefined_callback :
  source:Sema.Function_call_expression_result.expression_result ->
  (t, string) result

val undefined_callback_source :
  t -> Sema.Function_call_expression_result.expression_result option
(** Original declaration-time placeholder capture. It has no body link, machine
    PC, or permission to execute a source function. *)
