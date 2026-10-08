type t

type request =
  | Execute_source of string
  | Read_option of int64
  | Write_option of int64 * bool
[@@warning "-37"]
(* The private C bridge constructs these variants from actual machine values. *)

external check_raw : t -> unit = "holyc_native_source_suspension_check"

external open_raw : 'bridge -> t * request
  = "holyc_native_source_suspension_open"

external owns_request_raw : t -> request -> bool
  = "holyc_native_source_suspension_owns_request"

let owns_request scope request =
  try Ok (owns_request_raw scope request)
  with Failure message | Invalid_argument message -> Error message

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
