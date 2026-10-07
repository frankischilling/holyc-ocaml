type 'target t

external consume_raw : 'target t -> 'target -> string
  = "holyc_native_consume_generation_capture"

let consume value ~target =
  try Ok (consume_raw value target)
  with Failure message | Invalid_argument message -> Error message
