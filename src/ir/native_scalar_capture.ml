type 'program t

external consume_raw : 'program t -> 'program * int -> int64
  = "holyc_native_consume_internal_binding_capture"

let consume value ~program ~work =
  try Ok (consume_raw value (program, work))
  with Failure message | Invalid_argument message -> Error message
