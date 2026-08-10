type t = int

let of_int value =
  if value < 0 then Error "source ID must be nonnegative" else Ok value

let to_int value = value
let compare = Int.compare
let equal = Int.equal
let pp formatter value = Format.pp_print_int formatter value
