module Conversion = Holyc_lib__Common.Binary64_conversion

(* Fixed bit patterns from signed x87 FILD/FSTP and FLD/FISTTP semantics. No
   host-float round trip is used to construct expected words. *)
let check_vectors convert vectors =
  List.iter
    (fun (label, input, expected) ->
      Alcotest.(check int64) label expected (convert input))
    vectors

let integer_to_binary64 () =
  check_vectors Conversion.of_signed_word
    [
      ("zero", 0L, 0L);
      ("positive one", 1L, 0x3ff0_0000_0000_0000L);
      ("negative one / signed U64 high bits", -1L, 0xbff0_0000_0000_0000L);
      ("positive 42", 42L, 0x4045_0000_0000_0000L);
      ("negative 42", -42L, 0xc045_0000_0000_0000L);
      ("2^53 - 1 exact", 9007199254740991L, 0x433f_ffff_ffff_ffffL);
      ("2^53 exact", 9007199254740992L, 0x4340_0000_0000_0000L);
      ("2^53 + 1 tie down", 9007199254740993L, 0x4340_0000_0000_0000L);
      ("2^53 + 2 exact", 9007199254740994L, 0x4340_0000_0000_0001L);
      ("2^53 + 3 tie up", 9007199254740995L, 0x4340_0000_0000_0002L);
      ("2^53 + 5 tie down", 9007199254740997L, 0x4340_0000_0000_0002L);
      ("2^53 + 7 tie up", 9007199254740999L, 0x4340_0000_0000_0004L);
      ("negative tie down", -9007199254740993L, 0xc340_0000_0000_0000L);
      ("negative tie up", -9007199254740995L, 0xc340_0000_0000_0002L);
      ("signed minimum", Int64.min_int, 0xc3e0_0000_0000_0000L);
      ("signed maximum rounds upward", Int64.max_int, 0x43e0_0000_0000_0000L);
      ( "negative maximum rounds downward",
        Int64.neg Int64.max_int,
        0xc3e0_0000_0000_0000L );
    ]

let binary64_to_integer () =
  check_vectors Conversion.to_signed_word
    [
      ("positive zero", 0L, 0L);
      ("negative zero", 0x8000_0000_0000_0000L, 0L);
      ("smallest positive subnormal", 1L, 0L);
      ("smallest negative subnormal", 0x8000_0000_0000_0001L, 0L);
      ("positive one", 0x3ff0_0000_0000_0000L, 1L);
      ("negative one", 0xbff0_0000_0000_0000L, -1L);
      ("positive fractional truncation", 0x400e_0000_0000_0000L, 3L);
      ("negative fractional truncation", 0xc00e_0000_0000_0000L, -3L);
      ("positive one-half truncation", 0x3fe0_0000_0000_0000L, 0L);
      ("negative one-half truncation", 0xbfe0_0000_0000_0000L, 0L);
      ("2^53 exact", 0x4340_0000_0000_0000L, 9007199254740992L);
      ( "largest float below signed limit",
        0x43df_ffff_ffff_ffffL,
        Int64.sub Int64.max_int 1023L );
      ("negative signed minimum valid", 0xc3e0_0000_0000_0000L, Int64.min_int);
      ("positive signed limit invalid", 0x43e0_0000_0000_0000L, Int64.min_int);
      ("negative overflow invalid", 0xc3e0_0000_0000_0001L, Int64.min_int);
      ("positive overflow invalid", 0x43e0_0000_0000_0001L, Int64.min_int);
      ("positive infinity invalid", 0x7ff0_0000_0000_0000L, Int64.min_int);
      ("negative infinity invalid", 0xfff0_0000_0000_0000L, Int64.min_int);
      ("quiet NaN invalid", 0x7ff8_0000_0000_0001L, Int64.min_int);
      ("signaling NaN invalid", 0x7ff0_0000_0000_0001L, Int64.min_int);
      ("negative NaN invalid", 0xfff8_0000_0000_0001L, Int64.min_int);
    ]

let () =
  Alcotest.run "TempleOS scalar default conversions"
    [
      ( "materialized integer to binary64",
        [
          Alcotest.test_case "FILD and nearest-even FSTP" `Quick
            integer_to_binary64;
        ] );
      ( "materialized binary64 to integer",
        [
          Alcotest.test_case "FLD and truncating FISTTP" `Quick
            binary64_to_integer;
        ] );
    ]
