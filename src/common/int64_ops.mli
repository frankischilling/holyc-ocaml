val mul_add : base:int64 -> int64 -> digit:int -> int64
val shift_add : bits:int -> int64 -> digit:int -> int64
val of_digits : base:int -> digit:(char -> int option) -> string -> int64 option
