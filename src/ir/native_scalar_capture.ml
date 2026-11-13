type 'program t

external consume_raw :
  'program t -> 'program * int -> Native_source_suspension.t option -> int64
  = "holyc_native_consume_internal_binding_capture"

let consume ?scope value ~program ~work =
  try Ok (consume_raw value (program, work) scope)
  with Failure message | Invalid_argument message -> Error message

external consume_value_raw :
  'program t ->
  'program * int ->
  Native_source_suspension.t option ->
  int64 option = "holyc_native_consume_initializer_value_capture"

let consume_value ?scope value ~program ~work =
  try Ok (consume_value_raw value (program, work) scope)
  with Failure message | Invalid_argument message -> Error message
