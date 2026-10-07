type t

external consume_raw : t -> Internal_binding_fragment_program.t * int -> int64
  = "holyc_native_consume_internal_binding_capture"

let consume value ~program ~work =
  try Ok (consume_raw value (program, work))
  with Failure message | Invalid_argument message -> Error message
