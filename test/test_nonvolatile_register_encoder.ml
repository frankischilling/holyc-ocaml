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
    (fun (label, instruction, expected) ->
      Alcotest.(check string) label expected (hex (E.encode instruction));
      Alcotest.(check int)
        (label ^ " exact size")
        (String.length expected / 2)
        (E.size instruction))
    cases;
  let instructions = List.map (fun (_, instruction, _) -> instruction) cases in
  let expected =
    String.concat "" (List.map (fun (_, _, bytes) -> bytes) cases)
  in
  let bytes = String.length expected / 2 in
  Alcotest.(check string)
    "batch encoding at its exact byte limit" expected
    (E.encode_all ~max_code_bytes:bytes instructions |> checked |> hex);
  Alcotest.(check bool)
    "batch rejects one byte below its exact limit" true
    (Result.is_error (E.encode_all ~max_code_bytes:(bytes - 1) instructions))

(* OpCodes.DD:93-96 assigns R12-R15 their numbers. Literal expectations below
   follow the pinned MOV/MOVSX/MOVZX/MOVSXD forms and Asm.HC:580-638 REX.R/B
   and ModR/M rules, independently of the encoder's generated-table lookup. *)
let temporary_bank () =
  Alcotest.(check bool)
    "nonvolatile variables never join the temporary allocation bank" true
    (E.registers = E.[ Rax; Rcx; Rdx; R8; R9; R10; R11 ])

let moves () =
  let open E in
  check_cases
    [
      ("MOV R12,imm64", Mov_imm64 (R12, 42L), "49bc2a00000000000000");
      ("MOV R13,imm64", Mov_imm64 (R13, -1L), "49bdffffffffffffffff");
      ("MOV R14,imm64", Mov_imm64 (R14, Int64.min_int), "49be0000000000000080");
      ( "MOV R15,imm64",
        Mov_imm64 (R15, 0x0123456789abcdefL),
        "49bfefcdab8967452301" );
      ("MOV R12,RAX", Mov (R12, Rax), "4989c4");
      ("MOV RAX,R12", Mov (Rax, R12), "4c89e0");
      ("MOV R13,RCX", Mov (R13, Rcx), "4989cd");
      ("MOV RCX,R13", Mov (Rcx, R13), "4c89e9");
      ("MOV R14,RDX", Mov (R14, Rdx), "4989d6");
      ("MOV RDX,R14", Mov (Rdx, R14), "4c89f2");
      ("MOV R15,RAX", Mov (R15, Rax), "4989c7");
      ("MOV RAX,R15", Mov (Rax, R15), "4c89f8");
      ("MOV R12,R15", Mov (R12, R15), "4d89fc");
      ("MOV R15,R12", Mov (R15, R12), "4d89e7");
      ("MOV R13,R14", Mov (R13, R14), "4d89f5");
      ("MOV R14,R13", Mov (R14, R13), "4d89ee");
    ]

let fixed_storage () =
  let open E in
  let stack = stack_slot ~offset:24 |> checked in
  let frame = frame_slot ~offset:(-8) |> checked in
  let arena = arena_slot ~offset:32 |> checked in
  [
    (R12, "R12", "a4", "a5", "a1");
    (R13, "R13", "ac", "ad", "a9");
    (R14, "R14", "b4", "b5", "b1");
    (R15, "R15", "bc", "bd", "b9");
  ]
  |> List.concat_map (fun (register, name, rsp, rbp, r9) ->
      [
        ( "load stack " ^ name,
          Load_stack (register, stack),
          "4c8b" ^ rsp ^ "2418000000" );
        ( "store stack " ^ name,
          Store_stack (stack, register),
          "4c89" ^ rsp ^ "2418000000" );
        ( "address stack " ^ name,
          Address_stack (register, stack),
          "4c8d" ^ rsp ^ "2418000000" );
        ( "load frame " ^ name,
          Load_frame (register, frame),
          "4c8b" ^ rbp ^ "f8ffffff" );
        ( "store frame " ^ name,
          Store_frame (frame, register),
          "4c89" ^ rbp ^ "f8ffffff" );
        ( "address frame " ^ name,
          Address_frame (register, frame),
          "4c8d" ^ rbp ^ "f8ffffff" );
        ( "load arena " ^ name,
          Load_arena (register, arena),
          "4d8b" ^ r9 ^ "20000000" );
        ( "store arena " ^ name,
          Store_arena (arena, register),
          "4d89" ^ r9 ^ "20000000" );
        ( "address arena " ^ name,
          Address_arena (register, arena),
          "4d8d" ^ r9 ^ "20000000" );
      ])
  |> check_cases

let narrow_frames () =
  let open E in
  let slot = scalar_frame_slot ~offset:(-3) |> checked in
  [
    (R12, "R12", "a5");
    (R13, "R13", "ad");
    (R14, "R14", "b5");
    (R15, "R15", "bd");
  ]
  |> List.concat_map (fun (register, name, modrm) ->
      let suffix = modrm ^ "fdffffff" in
      [
        ( "load signed byte " ^ name,
          Load_frame_narrow (register, slot, Frame8, Sign_extend),
          "4c0fbe" ^ suffix );
        ( "load unsigned byte " ^ name,
          Load_frame_narrow (register, slot, Frame8, Zero_extend),
          "4c0fb6" ^ suffix );
        ( "load signed word " ^ name,
          Load_frame_narrow (register, slot, Frame16, Sign_extend),
          "4c0fbf" ^ suffix );
        ( "load unsigned word " ^ name,
          Load_frame_narrow (register, slot, Frame16, Zero_extend),
          "4c0fb7" ^ suffix );
        ( "load signed dword " ^ name,
          Load_frame_narrow (register, slot, Frame32, Sign_extend),
          "4c63" ^ suffix );
        ( "load unsigned dword " ^ name,
          Load_frame_narrow (register, slot, Frame32, Zero_extend),
          "448b" ^ suffix );
        ( "store byte " ^ name,
          Store_frame_narrow (slot, Frame8, register),
          "4488" ^ suffix );
        ( "store word " ^ name,
          Store_frame_narrow (slot, Frame16, register),
          "664489" ^ suffix );
        ( "store dword " ^ name,
          Store_frame_narrow (slot, Frame32, register),
          "4489" ^ suffix );
      ])
  |> check_cases

let indirect_and_context () =
  let open E in
  let arena = arena_slot ~offset:3 |> checked in
  check_cases
    [
      ("R12 base needs SIB", Load_indirect (R13, R12, 8), "4d8bac2408000000");
      ("R13 base retains disp32", Load_indirect (R14, R13, 0), "4d8bb500000000");
      ("R14 base", Load_indirect (R15, R14, 16), "4d8bbe10000000");
      ("R15 base", Load_indirect (R12, R15, 32), "4d8ba720000000");
      ("store through R12", Store_indirect (R12, R15), "4d89bc2400000000");
      ( "store through R13",
        Store_indirect_offset (R13, 24, R12),
        "4d89a518000000" );
      ( "signed byte through R12",
        Load_indirect_narrow (R13, R12, Frame8, Sign_extend),
        "4d0fbeac2400000000" );
      ( "unsigned word through R13",
        Load_indirect_narrow (R14, R13, Frame16, Zero_extend),
        "4d0fb7b500000000" );
      ( "unsigned dword through R14",
        Load_indirect_narrow (R15, R14, Frame32, Zero_extend),
        "458bbe00000000" );
      ( "store byte through R12",
        Store_indirect_narrow (R12, Frame8, R13),
        "4588ac2400000000" );
      ( "store word through R13",
        Store_indirect_narrow (R13, Frame16, R14),
        "664589b500000000" );
      ( "store dword through R15",
        Store_indirect_narrow (R15, Frame32, R12),
        "4589a700000000" );
      ( "arena signed byte",
        Load_arena_narrow (R12, arena, Frame8, Sign_extend),
        "4d0fbea103000000" );
      ( "arena unsigned word",
        Load_arena_narrow (R13, arena, Frame16, Zero_extend),
        "4d0fb7a903000000" );
      ( "arena unsigned dword",
        Load_arena_narrow (R14, arena, Frame32, Zero_extend),
        "458bb103000000" );
      ( "arena byte store",
        Store_arena_narrow (arena, Frame8, R15),
        "4588b903000000" );
      ("context disp8 load", Load_context (R12, 120), "4d8b6378");
      ("context disp8 store", Store_context (120, R13), "4d896b78");
      ("context disp32 load", Load_context (R14, 128), "4d8bb380000000");
      ("context disp32 store", Store_context (128, R15), "4d89bb80000000");
      ("RIP-relative R15", Address_code_relative (R15, -7L), "4c8d3df9ffffff");
      ( "locked byte ADD through R12",
        Lock_binary_indirect (Add, R12, Atomic8, R13),
        "f04500ac2400000000" );
      ( "locked word SUB through R13",
        Lock_binary_indirect (Sub, R13, Atomic16, R14),
        "f0664529b500000000" );
      ( "locked dword XOR through R14",
        Lock_binary_indirect (Xor, R14, Atomic32, R15),
        "f04531be00000000" );
      ( "locked qword CMPXCHG through R15",
        Lock_cmpxchg_indirect (R15, Atomic64, R12),
        "f04d0fb1a700000000" );
    ]

let arithmetic_and_predicates () =
  let open E in
  check_cases
    [
      ("NEG R12", Unary (Neg, R12), "49f7dc");
      ("NOT R13", Unary (Not, R13), "49f7d5");
      ("ADD R14,R15", Binary (Add, R14, R15), "4d01fe");
      ("SUB R15,R12", Binary (Sub, R15, R12), "4d29e7");
      ("AND R12,R13", Binary (And, R12, R13), "4d21ec");
      ("OR R13,R14", Binary (Or, R13, R14), "4d09f5");
      ("XOR R14,R15", Binary (Xor, R14, R15), "4d31fe");
      ("IMUL R12,RAX", Binary (Imul, R12, Rax), "4c0fafe0");
      ("IMUL RAX,R12", Binary (Imul, Rax, R12), "490fafc4");
      ("BSF R13,R14", Bsf (R13, R14), "4d0fbcee");
      ("BSR R15,R12", Bsr (R15, R12), "4d0fbdfc");
      ("BT R12,R13", Bit (Bt, R12, R13), "4d0fa3ec");
      ("BTS R13,R14", Bit (Bts, R13, R14), "4d0fabf5");
      ("BTR R14,R15", Bit (Btr, R14, R15), "4d0fb3fe");
      ("BTC R15,R12", Bit (Btc, R15, R12), "4d0fbbe7");
      ("SHL R12,CL", Shift_cl (Shl, R12), "49d3e4");
      ("SHR R13,1", Shift_immediate (Shr, R13, 1L), "49d1ed");
      ("SAR R14,63", Shift_immediate (Sar, R14, 63L), "49c1fe3f");
      ("CMP R15,imm8", Cmp_imm8 (R15, -1), "4983ffff");
      ("DEC R12", Dec R12, "49ffcc");
      ("CMP R13,R14", Cmp (R13, R14), "4d39f5");
      ("TEST R15", Test R15, "4d85ff");
      ("SETE R12b", Setcc (E, R12), "410f94c4");
      ("SETNE R13b", Setcc (NE, R13), "410f95c5");
      ("SETL R14b", Setcc (L, R14), "410f9cc6");
      ("SETA R15b", Setcc (A, R15), "410f97c7");
      ("MOVZX RAX,R12b", Movzx8 (Rax, R12), "490fb6c4");
      ("MOVZX R13,AL", Movzx8 (R13, Rax), "4c0fb6e8");
      ("MOVZX R14,R15b", Movzx8 (R14, R15), "4d0fb6f7");
      ("MOVZX R15,R14b", Movzx8 (R15, R14), "4d0fb6fe");
    ]

let () =
  Alcotest.run "nonvolatile-register-encoder"
    [
      ( "encoding",
        [
          Alcotest.test_case "temporary bank is unchanged" `Quick temporary_bank;
          Alcotest.test_case "moves in both directions" `Quick moves;
          Alcotest.test_case "stack, frame and arena storage" `Quick
            fixed_storage;
          Alcotest.test_case "all narrow frame forms" `Quick narrow_frames;
          Alcotest.test_case "indirect, context and atomic forms" `Quick
            indirect_and_context;
          Alcotest.test_case "arithmetic and byte predicates" `Quick
            arithmetic_and_predicates;
        ] );
    ]
