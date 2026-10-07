type t = Internal_binding_fragment_program.t Native_scalar_capture.t
(** A successful native scalar result from an original internal-binding entry.
    Only the native execution bridge produces this value. It retains its source
    program and live arena across GC and image release. *)

val consume :
  t ->
  program:Internal_binding_fragment_program.t ->
  work:int ->
  (int64, string) result
(** Require the original program and actual execution work before consuming the
    capture once. Equal source metadata and a repeated completion do not pass.
*)
