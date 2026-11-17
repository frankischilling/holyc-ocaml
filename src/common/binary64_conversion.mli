val of_signed_word : int64 -> int64
(** [of_signed_word word] converts the complete, materialized 64-bit signed
    integer [word] to IEEE 754 binary64 bits, rounding to nearest with ties to
    even, as TempleOS's FILD/FSTP conversion does after a default expression
    completes. The high bit of a HolyC U64 word is treated as a signed bit. *)

val to_signed_word : int64 -> int64
(** [to_signed_word bits] converts materialized IEEE 754 binary64 [bits] to a
    signed 64-bit integer by truncating toward zero. Values outside the signed
    range, infinity and NaNs produce the x87 masked-invalid integer-indefinite
    value [Int64.min_int], as FISTTP does. *)
