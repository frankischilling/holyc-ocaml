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

(* OpCodes.DD:87-96 assigns RSI/RDI/R12-R15 their numbers; its RSIu8/RDIu8
   aliases at lines 44-45 force REX via Asm.HC:623-640. Literal expectations
   below follow the pinned MOV/MOVSX/MOVZX/MOVSXD forms and Asm.HC:580-640
   REX.R/B and ModR/M rules, independently of the generated-table lookup. *)
let temporary_bank () =
  Alcotest.(check bool)
    "physical variables never join the temporary allocation bank" true
    (E.registers = E.[ Rax; Rcx; Rdx; R8; R9; R10; R11 ])

let moves () =
  let open E in
  check_cases
    [
      ("MOV RSI,imm64", Mov_imm64 (Rsi, 42L), "48be2a00000000000000");
      ("MOV RDI,imm64", Mov_imm64 (Rdi, -1L), "48bfffffffffffffffff");
      ("MOV RSI,RAX", Mov (Rsi, Rax), "4889c6");
      ("MOV RAX,RSI", Mov (Rax, Rsi), "4889f0");
      ("MOV RDI,RCX", Mov (Rdi, Rcx), "4889cf");
      ("MOV RCX,RDI", Mov (Rcx, Rdi), "4889f9");
      ("MOV RSI,RDI", Mov (Rsi, Rdi), "4889fe");
      ("MOV RDI,RSI", Mov (Rdi, Rsi), "4889f7");
      ("MOV RSI,R12", Mov (Rsi, R12), "4c89e6");
      ("MOV R12,RSI", Mov (R12, Rsi), "4989f4");
      ("MOV RDI,R15", Mov (Rdi, R15), "4c89ff");
      ("MOV R15,RDI", Mov (R15, Rdi), "4989ff");
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
    (Rsi, "RSI", "48", "49", "b4", "b5", "b1");
    (Rdi, "RDI", "48", "49", "bc", "bd", "b9");
    (R12, "R12", "4c", "4d", "a4", "a5", "a1");
    (R13, "R13", "4c", "4d", "ac", "ad", "a9");
    (R14, "R14", "4c", "4d", "b4", "b5", "b1");
    (R15, "R15", "4c", "4d", "bc", "bd", "b9");
  ]
  |> List.concat_map (fun (register, name, rex, arena_rex, rsp, rbp, r9) ->
      [
        ( "load stack " ^ name,
          Load_stack (register, stack),
          rex ^ "8b" ^ rsp ^ "2418000000" );
        ( "store stack " ^ name,
          Store_stack (stack, register),
          rex ^ "89" ^ rsp ^ "2418000000" );
        ( "address stack " ^ name,
          Address_stack (register, stack),
          rex ^ "8d" ^ rsp ^ "2418000000" );
        ( "load frame " ^ name,
          Load_frame (register, frame),
          rex ^ "8b" ^ rbp ^ "f8ffffff" );
        ( "store frame " ^ name,
          Store_frame (frame, register),
          rex ^ "89" ^ rbp ^ "f8ffffff" );
        ( "address frame " ^ name,
          Address_frame (register, frame),
          rex ^ "8d" ^ rbp ^ "f8ffffff" );
        ( "load arena " ^ name,
          Load_arena (register, arena),
          arena_rex ^ "8b" ^ r9 ^ "20000000" );
        ( "store arena " ^ name,
          Store_arena (arena, register),
          arena_rex ^ "89" ^ r9 ^ "20000000" );
        ( "address arena " ^ name,
          Address_arena (register, arena),
          arena_rex ^ "8d" ^ r9 ^ "20000000" );
      ])
  |> check_cases

let narrow_frames () =
  let open E in
  let slot = scalar_frame_slot ~offset:(-3) |> checked in
  [
    (Rsi, "RSI", "48", "40", "b5");
    (Rdi, "RDI", "48", "40", "bd");
    (R12, "R12", "4c", "44", "a5");
    (R13, "R13", "4c", "44", "ad");
    (R14, "R14", "4c", "44", "b5");
    (R15, "R15", "4c", "44", "bd");
  ]
  |> List.concat_map (fun (register, name, wide_rex, narrow_rex, modrm) ->
      let suffix = modrm ^ "fdffffff" in
      [
        ( "load signed byte " ^ name,
          Load_frame_narrow (register, slot, Frame8, Sign_extend),
          wide_rex ^ "0fbe" ^ suffix );
        ( "load unsigned byte " ^ name,
          Load_frame_narrow (register, slot, Frame8, Zero_extend),
          wide_rex ^ "0fb6" ^ suffix );
        ( "load signed word " ^ name,
          Load_frame_narrow (register, slot, Frame16, Sign_extend),
          wide_rex ^ "0fbf" ^ suffix );
        ( "load unsigned word " ^ name,
          Load_frame_narrow (register, slot, Frame16, Zero_extend),
          wide_rex ^ "0fb7" ^ suffix );
        ( "load signed dword " ^ name,
          Load_frame_narrow (register, slot, Frame32, Sign_extend),
          wide_rex ^ "63" ^ suffix );
        ( "load unsigned dword " ^ name,
          Load_frame_narrow (register, slot, Frame32, Zero_extend),
          narrow_rex ^ "8b" ^ suffix );
        ( "store byte " ^ name,
          Store_frame_narrow (slot, Frame8, register),
          narrow_rex ^ "88" ^ suffix );
        ( "store word " ^ name,
          Store_frame_narrow (slot, Frame16, register),
          "66" ^ narrow_rex ^ "89" ^ suffix );
        ( "store dword " ^ name,
          Store_frame_narrow (slot, Frame32, register),
          narrow_rex ^ "89" ^ suffix );
      ])
  |> check_cases

let indirect_and_context () =
  let open E in
  let arena = arena_slot ~offset:3 |> checked in
  check_cases
    [
      ("load RSI through RDI", Load_indirect (Rsi, Rdi, 8), "488bb708000000");
      ("load RDI through RSI", Load_indirect (Rdi, Rsi, 0), "488bbe00000000");
      ("load R12 through RSI", Load_indirect (R12, Rsi, 16), "4c8ba610000000");
      ("load RSI through R12", Load_indirect (Rsi, R12, 8), "498bb42408000000");
      ("store RDI through RSI", Store_indirect (Rsi, Rdi), "4889be00000000");
      ( "store RSI through RDI",
        Store_indirect_offset (Rdi, 24, Rsi),
        "4889b718000000" );
      ( "signed byte through RDI into RSI",
        Load_indirect_narrow (Rsi, Rdi, Frame8, Sign_extend),
        "480fbeb700000000" );
      ( "unsigned byte through RSI into RDI",
        Load_indirect_narrow (Rdi, Rsi, Frame8, Zero_extend),
        "480fb6be00000000" );
      ( "unsigned word through RDI into RSI",
        Load_indirect_narrow (Rsi, Rdi, Frame16, Zero_extend),
        "480fb7b700000000" );
      ( "unsigned dword through RSI into RDI",
        Load_indirect_narrow (Rdi, Rsi, Frame32, Zero_extend),
        "408bbe00000000" );
      ( "store SIL through RAX",
        Store_indirect_narrow (Rax, Frame8, Rsi),
        "4088b000000000" );
      ( "store DIL through RAX",
        Store_indirect_narrow (Rax, Frame8, Rdi),
        "4088b800000000" );
      ( "store SIL through RDI",
        Store_indirect_narrow (Rdi, Frame8, Rsi),
        "4088b700000000" );
      ( "store DIL through RSI",
        Store_indirect_narrow (Rsi, Frame8, Rdi),
        "4088be00000000" );
      ( "store SI through RDI",
        Store_indirect_narrow (Rdi, Frame16, Rsi),
        "664089b700000000" );
      ( "store EDI through RSI",
        Store_indirect_narrow (Rsi, Frame32, Rdi),
        "4089be00000000" );
      ( "arena signed byte into RSI",
        Load_arena_narrow (Rsi, arena, Frame8, Sign_extend),
        "490fbeb103000000" );
      ( "arena unsigned word into RDI",
        Load_arena_narrow (Rdi, arena, Frame16, Zero_extend),
        "490fb7b903000000" );
      ( "arena unsigned dword into RSI",
        Load_arena_narrow (Rsi, arena, Frame32, Zero_extend),
        "418bb103000000" );
      ( "arena SIL store",
        Store_arena_narrow (arena, Frame8, Rsi),
        "4188b103000000" );
      ( "arena DIL store",
        Store_arena_narrow (arena, Frame8, Rdi),
        "4188b903000000" );
      ("context disp8 load RSI", Load_context (Rsi, 120), "498b7378");
      ("context disp8 store RDI", Store_context (120, Rdi), "49897b78");
      ("context disp32 load RDI", Load_context (Rdi, 128), "498bbb80000000");
      ("context disp32 store RSI", Store_context (128, Rsi), "4989b380000000");
      ("RIP-relative RSI", Address_code_relative (Rsi, -7L), "488d35f9ffffff");
      ("RIP-relative RDI", Address_code_relative (Rdi, -7L), "488d3df9ffffff");
      ( "locked byte ADD with SIL through RAX",
        Lock_binary_indirect (Add, Rax, Atomic8, Rsi),
        "f04000b000000000" );
      ( "locked byte XOR with DIL through RSI",
        Lock_binary_indirect (Xor, Rsi, Atomic8, Rdi),
        "f04030be00000000" );
      ( "locked byte CMPXCHG with SIL through RDI",
        Lock_cmpxchg_indirect (Rdi, Atomic8, Rsi),
        "f0400fb0b700000000" );
      ( "locked byte CMPXCHG with DIL through RAX",
        Lock_cmpxchg_indirect (Rax, Atomic8, Rdi),
        "f0400fb0b800000000" );
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
      ("NEG RSI", Unary (Neg, Rsi), "48f7de");
      ("NOT RDI", Unary (Not, Rdi), "48f7d7");
      ("ADD RSI,RDI", Binary (Add, Rsi, Rdi), "4801fe");
      ("SUB RDI,RSI", Binary (Sub, Rdi, Rsi), "4829f7");
      ("AND RSI,RDI", Binary (And, Rsi, Rdi), "4821fe");
      ("OR RDI,RSI", Binary (Or, Rdi, Rsi), "4809f7");
      ("XOR RSI,RDI", Binary (Xor, Rsi, Rdi), "4831fe");
      ("IMUL RSI,RDI", Binary (Imul, Rsi, Rdi), "480faff7");
      ("IMUL RDI,RSI", Binary (Imul, Rdi, Rsi), "480faffe");
      ("BSF RSI,RDI", Bsf (Rsi, Rdi), "480fbcf7");
      ("BSR RDI,RSI", Bsr (Rdi, Rsi), "480fbdfe");
      ("BT RSI,RDI", Bit (Bt, Rsi, Rdi), "480fa3fe");
      ("BTS RDI,RSI", Bit (Bts, Rdi, Rsi), "480fabf7");
      ("BTR RSI,RDI", Bit (Btr, Rsi, Rdi), "480fb3fe");
      ("BTC RDI,RSI", Bit (Btc, Rdi, Rsi), "480fbbf7");
      ("SHL RSI,CL", Shift_cl (Shl, Rsi), "48d3e6");
      ("SHR RDI,1", Shift_immediate (Shr, Rdi, 1L), "48d1ef");
      ("SAR RSI,63", Shift_immediate (Sar, Rsi, 63L), "48c1fe3f");
      ("CMP RDI,imm8", Cmp_imm8 (Rdi, -1), "4883ffff");
      ("DEC RSI", Dec Rsi, "48ffce");
      ("CMP RSI,RDI", Cmp (Rsi, Rdi), "4839fe");
      ("TEST RDI", Test Rdi, "4885ff");
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

let low_byte_registers () =
  let open E in
  let predicates =
    [
      (E, "SETE", "94");
      (NE, "SETNE", "95");
      (L, "SETL", "9c");
      (GE, "SETGE", "9d");
      (G, "SETG", "9f");
      (LE, "SETLE", "9e");
      (B, "SETB", "92");
      (AE, "SETAE", "93");
      (A, "SETA", "97");
      (BE, "SETBE", "96");
    ]
    |> List.concat_map (fun (condition, name, opcode) ->
        [
          (name ^ " SIL", Setcc (condition, Rsi), "400f" ^ opcode ^ "c6");
          (name ^ " DIL", Setcc (condition, Rdi), "400f" ^ opcode ^ "c7");
        ])
  in
  check_cases
    (predicates
    @ [
        ("SETE AL retains no REX", Setcc (E, Rax), "0f94c0");
        ("SETNE CL retains no REX", Setcc (NE, Rcx), "0f95c1");
        ("SETL DL retains no REX", Setcc (L, Rdx), "0f9cc2");
        ("MOVZX RAX,SIL", Movzx8 (Rax, Rsi), "480fb6c6");
        ("MOVZX RAX,DIL", Movzx8 (Rax, Rdi), "480fb6c7");
        ("MOVZX RSI,AL", Movzx8 (Rsi, Rax), "480fb6f0");
        ("MOVZX RDI,AL", Movzx8 (Rdi, Rax), "480fb6f8");
        ("MOVZX RSI,SIL", Movzx8 (Rsi, Rsi), "480fb6f6");
        ("MOVZX RDI,DIL", Movzx8 (Rdi, Rdi), "480fb6ff");
        ("MOVZX RSI,DIL", Movzx8 (Rsi, Rdi), "480fb6f7");
        ("MOVZX RDI,SIL", Movzx8 (Rdi, Rsi), "480fb6fe");
        ("MOVZX RSI,R12b", Movzx8 (Rsi, R12), "490fb6f4");
        ("MOVZX R12,SIL", Movzx8 (R12, Rsi), "4c0fb6e6");
        ("MOVZX RDI,R15b", Movzx8 (Rdi, R15), "490fb6ff");
        ("MOVZX R15,DIL", Movzx8 (R15, Rdi), "4c0fb6ff");
      ])

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
          Alcotest.test_case "SIL and DIL force REX" `Quick low_byte_registers;
        ] );
    ]
