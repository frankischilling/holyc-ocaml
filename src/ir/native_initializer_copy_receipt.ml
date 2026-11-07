type 'copy t

external consume_raw : 'copy t -> 'copy * int -> bool
  = "holyc_native_consume_initializer_copy"

let consume receipt ~copy ~bytes =
  try
    if consume_raw receipt (copy, bytes) then Ok ()
    else Error "native initializer copy did not match its original completion"
  with Failure message | Invalid_argument message -> Error message
