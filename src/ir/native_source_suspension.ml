type t

external check_raw : t -> unit = "holyc_native_source_suspension_check"

external open_raw : 'bridge -> t * string
  = "holyc_native_source_suspension_open"

external limits_raw : t -> int * int * int * int
  = "holyc_native_source_suspension_limits"

external owns_generation_raw : t -> 'target -> bool
  = "holyc_native_source_suspension_owns_generation"

let check scope =
  try Ok (check_raw scope)
  with Failure message | Invalid_argument message -> Error message

let limits scope =
  try Ok (limits_raw scope)
  with Failure message | Invalid_argument message -> Error message

let owns_generation scope target =
  try Ok (owns_generation_raw scope target)
  with Failure message | Invalid_argument message -> Error message
