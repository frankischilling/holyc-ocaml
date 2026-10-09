type 'target t

external consume_raw :
  'target t -> 'target -> Native_source_suspension.t option -> string
  = "holyc_native_consume_generation_capture"

external bounds_raw : 'target t -> 'target -> int * int
  = "holyc_native_generation_capture_bounds"

let bounds value ~target =
  try Ok (bounds_raw value target)
  with Failure message | Invalid_argument message -> Error message

let consume ?scope value ~target =
  try Ok (consume_raw value target scope)
  with Failure message | Invalid_argument message -> Error message
