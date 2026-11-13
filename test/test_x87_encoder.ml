module E = Holyc_lib.X86_64_encoder

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let hex bytes =
  String.to_seq bytes
  |> Seq.map (fun byte -> Printf.sprintf "%02x" (Char.code byte))
  |> List.of_seq |> String.concat ""

let check_cases cases =
  List.iter
    (fun (name, instruction, bytes) ->
      Alcotest.(check string) name bytes (hex (E.encode instruction));
      Alcotest.(check int)
        (name ^ " size")
        (String.length bytes / 2)
        (E.size instruction))
    cases;
  let instructions = List.map (fun (_, instruction, _) -> instruction) cases in
  let expected =
    String.concat "" (List.map (fun (_, _, bytes) -> bytes) cases)
  in
  let limit = String.length expected / 2 in
  Alcotest.(check string)
    "exact batch limit" expected
    (E.encode_all ~max_code_bytes:limit instructions |> checked |> hex);
  Alcotest.(check bool)
    "one byte too small" true
    (Result.is_error (E.encode_all ~max_code_bytes:(limit - 1) instructions))

(* Literal oracle: pinned OpCodes.DD:1158-1253, Asm.HC:530-566 (disp32
   ModR/M), Asm.HC:580 onward (+I stack index). BackFA.HC:480-512 chooses
   reverse memory forms after swapping operands and pop forms for two built-in
   constants. Expectations do not read generated opcode facts. *)
let memory () =
  let open E in
  let f = X87_frame (frame_slot ~offset:(-8) |> checked) in
  let s = X87_stack (stack_slot ~offset:24 |> checked) in
  check_cases
    [
      ("FILD frame", Fild64 f, "dfadf8ffffff");
      ("FISTTP frame", Fisttp64 f, "dd8df8ffffff");
      ("FILD stack", Fild64 s, "dfac2418000000");
      ("FISTTP stack", Fisttp64 s, "dd8c2418000000");
      ("FLD frame", Fld64 f, "dd85f8ffffff");
      ("FSTP frame", Fstp64 f, "dd9df8ffffff");
      ("FLD stack", Fld64 s, "dd842418000000");
      ("FSTP stack", Fstp64 s, "dd9c2418000000");
      ("FADD frame", Fbinary64 (Fadd, f), "dc85f8ffffff");
      ("FMUL frame", Fbinary64 (Fmul, f), "dc8df8ffffff");
      ("FSUB frame", Fbinary64 (Fsub, f), "dca5f8ffffff");
      ("FSUBR frame", Fbinary64 (Fsubr, f), "dcadf8ffffff");
      ("FDIV frame", Fbinary64 (Fdiv, f), "dcb5f8ffffff");
      ("FDIVR frame", Fbinary64 (Fdivr, f), "dcbdf8ffffff");
      ("FADD stack", Fbinary64 (Fadd, s), "dc842418000000");
      ("FMUL stack", Fbinary64 (Fmul, s), "dc8c2418000000");
      ("FSUB stack", Fbinary64 (Fsub, s), "dca42418000000");
      ("FSUBR stack", Fbinary64 (Fsubr, s), "dcac2418000000");
      ("FDIV stack", Fbinary64 (Fdiv, s), "dcb42418000000");
      ("FDIVR stack", Fbinary64 (Fdivr, s), "dcbc2418000000");
    ]

let registers () =
  let open E in
  let st1 = x87_register ~index:1 |> checked in
  [
    (Fadd, "d8c1", "dcc1", "dec1");
    (Fmul, "d8c9", "dcc9", "dec9");
    (Fsub, "d8e1", "dce9", "dee9");
    (Fsubr, "d8e9", "dce1", "dee1");
    (Fdiv, "d8f1", "dcf9", "def9");
    (Fdivr, "d8f9", "dcf1", "def1");
  ]
  |> List.concat_map (fun (op, st0, sti, pop) ->
      [
        ("ST0 destination", Fbinary_st0 (op, st1), st0);
        ("STi destination", Fbinary_sti (op, st1), sti);
        ("STi destination and pop", Fbinary_pop (op, st1), pop);
      ])
  |> check_cases

let constants () =
  let open E in
  check_cases
    [
      ("FLDZ", Fld_constant Fzero, "d9ee");
      ("FLD1", Fld_constant Fone, "d9e8");
      ("FLDPI", Fld_constant Fpi, "d9eb");
      ("FLDL2T", Fld_constant Flog2_10, "d9e9");
      ("FLDL2E", Fld_constant Flog2_e, "d9ea");
      ("FLDLG2", Fld_constant Flog10_2, "d9ec");
      ("FLDLN2", Fld_constant Floge_2, "d9ed");
      ("FCHS", Fchs, "d9e0");
      ("FCOMIP ST1", Fcomip (x87_register ~index:1 |> checked), "dff1");
      ("FCOMIP ST0", Fcomip (x87_register ~index:0 |> checked), "dff0");
      ("FCOMIP ST7", Fcomip (x87_register ~index:7 |> checked), "dff7");
      ("FFREE ST0", Ffree (x87_register ~index:0 |> checked), "ddc0");
      ("FFREE ST7", Ffree (x87_register ~index:7 |> checked), "ddc7");
      ("FINCSTP", Fincstp, "d9f7");
    ]

let bounds () =
  let open E in
  List.iter
    (fun index ->
      Alcotest.(check bool)
        "bad ST index" true
        (Result.is_error (x87_register ~index)))
    [ -1; 8; max_int; min_int ];
  List.iter
    (fun offset ->
      Alcotest.(check bool)
        "bad stack offset" true
        (Result.is_error (stack_slot ~offset)))
    [ -8; 1; 524280 ];
  List.iter
    (fun offset ->
      Alcotest.(check bool)
        "bad frame offset" true
        (Result.is_error (frame_slot ~offset)))
    [ -1; 0x80000000; -0x80000008 ];
  let unaligned = X87_frame (scalar_frame_slot ~offset:1 |> checked) in
  List.iter
    (fun instruction ->
      Alcotest.check_raises "unaligned x87 frame rejected by size"
        (Invalid_argument "x87 m64 access requires an aligned frame slot")
        (fun () -> ignore (size instruction));
      Alcotest.(check bool)
        "unaligned x87 batch rejected" true
        (Result.is_error (encode_all ~max_code_bytes:100 [ instruction ])))
    [
      Fild64 unaligned;
      Fisttp64 unaligned;
      Fld64 unaligned;
      Fstp64 unaligned;
      Fbinary64 (Fsubr, unaligned);
    ];
  check_cases
    [
      ( "FILD minimum frame",
        Fild64 (X87_frame (frame_slot ~offset:(-0x80000000) |> checked)),
        "dfad00000080" );
      ( "FISTTP maximum frame",
        Fisttp64 (X87_frame (frame_slot ~offset:0x7ffffff8 |> checked)),
        "dd8df8ffff7f" );
      ( "FILD zero stack",
        Fild64 (X87_stack (stack_slot ~offset:0 |> checked)),
        "dfac2400000000" );
      ( "FISTTP maximum stack",
        Fisttp64 (X87_stack (stack_slot ~offset:524272 |> checked)),
        "dd8c24f0ff0700" );
      ( "minimum frame",
        Fld64 (X87_frame (frame_slot ~offset:(-0x80000000) |> checked)),
        "dd8500000080" );
      ( "maximum frame",
        Fstp64 (X87_frame (frame_slot ~offset:0x7ffffff8 |> checked)),
        "dd9df8ffff7f" );
      ( "zero stack",
        Fld64 (X87_stack (stack_slot ~offset:0 |> checked)),
        "dd842400000000" );
      ( "maximum stack",
        Fstp64 (X87_stack (stack_slot ~offset:524272 |> checked)),
        "dd9c24f0ff0700" );
      ("ST0", Fbinary_st0 (Fsub, x87_register ~index:0 |> checked), "d8e0");
      ("ST7", Fbinary_pop (Fdivr, x87_register ~index:7 |> checked), "def7");
    ];
  Alcotest.(check bool)
    "temporary bank unchanged" true
    (E.registers = [ Rax; Rcx; Rdx; R8; R9; R10; R11 ]);
  check_cases
    [
      ( "negative zero bit transport remains integer MOV",
        Mov_imm64 (Rax, Int64.min_int),
        "48b80000000000000080" );
      ( "NaN payload transport remains integer MOV",
        Mov_imm64 (Rax, 0x7ff8000000001234L),
        "48b8341200000000f87f" );
    ]

let () =
  Alcotest.run "checked x87 encoder"
    [
      ( "pinned source",
        [
          Alcotest.test_case "m64 frame and stack" `Quick memory;
          Alcotest.test_case "register orientation" `Quick registers;
          Alcotest.test_case "arithmetic constants" `Quick constants;
          Alcotest.test_case "bounds and bit transport" `Quick bounds;
        ] );
    ]
