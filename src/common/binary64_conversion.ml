(* TempleOS PrsVar converts a completed integer or F64 expression into its
   declared default type. The original BackFA conversion uses signed FILD with
   FSTP, or FLD with truncating FISTTP. These helpers operate on the completed
   64-bit word; they do not evaluate an expression or preserve extended x87
   intermediates. *)

let fraction_mask = 0x000f_ffff_ffff_ffffL
let implicit_bit = 0x0010_0000_0000_0000L
let carry_bit = 0x0020_0000_0000_0000L

let of_signed_word word =
  if word = 0L then 0L
  else
    let negative = Int64.compare word 0L < 0 in
    (* Negating min_int retains bit 63. Logical shifts treat it as unsigned. *)
    let magnitude = if negative then Int64.neg word else word in
    let rec highest_bit position =
      if Int64.shift_right_logical magnitude position <> 0L then position
      else highest_bit (position - 1)
    in
    let exponent = highest_bit 63 in
    let significand =
      if exponent <= 52 then Int64.shift_left magnitude (52 - exponent)
      else
        let discarded_bits = exponent - 52 in
        let retained = Int64.shift_right_logical magnitude discarded_bits in
        let discarded =
          Int64.logand magnitude
            (Int64.pred (Int64.shift_left 1L discarded_bits))
        in
        let halfway = Int64.shift_left 1L (discarded_bits - 1) in
        if
          Int64.compare discarded halfway > 0
          || (discarded = halfway && Int64.logand retained 1L <> 0L)
        then Int64.succ retained
        else retained
    in
    let exponent, significand =
      if Int64.logand significand carry_bit <> 0L then
        (exponent + 1, Int64.shift_right_logical significand 1)
      else (exponent, significand)
    in
    let sign = if negative then Int64.min_int else 0L in
    Int64.logor sign
      (Int64.logor
         (Int64.shift_left (Int64.of_int (exponent + 1023)) 52)
         (Int64.logand significand fraction_mask))

let to_signed_word bits =
  let exponent =
    Int64.to_int (Int64.logand (Int64.shift_right_logical bits 52) 0x7ffL)
  in
  if exponent >= 1086 then
    (* FISTTP with masked invalid returns the integer-indefinite bit pattern;
       the valid -2^63 has these same bits. This also covers NaN and infinity. *)
    Int64.min_int
  else if exponent < 1023 then 0L
  else
    let significand =
      Int64.logor implicit_bit (Int64.logand bits fraction_mask)
    in
    let shift = exponent - 1023 - 52 in
    let magnitude =
      if shift >= 0 then Int64.shift_left significand shift
      else Int64.shift_right_logical significand (-shift)
    in
    if Int64.logand bits Int64.min_int <> 0L then Int64.neg magnitude
    else magnitude
