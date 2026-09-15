open Holyc_lib
module Native = X86_64_expression
module Encoder = X86_64_encoder
module Sequence = Ir_instruction_sequence
module Graph = Ir_block_graph
module X87 = Ir_x87_stack
module Opcode = Ir_opcode
module Type = Semantic_type

let require_ok show = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (show errors)

let diagnostic_errors errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let native_errors errors =
  errors
  |> List.map (fun (error : Native.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let sequence_error (error : Sequence.error) = error.code ^ ": " ^ error.message

let source_graph ?(mode = Preprocessor.Jit) contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-expression.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  lower_integer_expression session ~config ~source
  |> require_ok diagnostic_errors

let compile ?(max_ir_instructions = 10000) ?(max_code_bytes = 1048576) graph =
  Native.compile ~max_ir_instructions ~max_code_bytes graph

let image graph = compile graph |> require_ok native_errors

let reject ?code label = function
  | Ok _ -> Alcotest.failf "%s unexpectedly produced an image" label
  | Error [] -> Alcotest.failf "%s returned no diagnostic" label
  | Error errors ->
      List.iter
        (fun (error : Native.error) ->
          Alcotest.(check bool)
            (label ^ " diagnostic has a message")
            true (error.message <> ""))
        errors;
      Option.iter
        (fun expected ->
          Alcotest.(check bool)
            (label ^ " diagnostic " ^ expected)
            true
            (List.exists
               (fun (error : Native.error) -> error.code = expected)
               errors))
        code;
      errors

let instruction_id id =
  Sequence.Instruction_id.of_int id |> require_ok sequence_error

let value_id id = Sequence.Value_id.of_int id |> require_ok sequence_error
let block_id id = Sequence.Block_id.of_int id |> require_ok sequence_error
let fixture_source = Source_id.of_int 642 |> require_ok Fun.id

let fixture_span id =
  Span.unsafe_make ~source:fixture_source ~start:(2 * id) ~stop:((2 * id) + 1)

let primitive ?(form = Type.Internal_storage) ?(pointer_depth = 0) primitive =
  Type.make_primitive ~form ~primitive ~pointer_depth |> require_ok Fun.id

let i64 = primitive Primitive_type.I64
let u64 = primitive Primitive_type.U64

let description ?(operands = []) ?result ?target_type ?payload ?(flags = 0L) id
    opcode : Sequence.description =
  {
    instruction_id = instruction_id id;
    opcode;
    operands = List.map value_id operands;
    result = Option.map (fun id -> { Sequence.value_id = value_id id }) result;
    target_type;
    payload;
    flags;
    span = Some (fixture_span id);
  }

let imm ?(type_ = i64) id bits =
  description ~result:id ~target_type:type_ ~payload:(Sequence.Integer bits) id
    Opcode.Ic_imm_i64

let unary ?(type_ = i64) id opcode operand =
  description ~operands:[ operand ] ~result:id ~target_type:type_ id opcode

let binary ?(type_ = i64) id opcode left right =
  description ~operands:[ left; right ] ~result:id ~target_type:type_ id opcode

let return_value ?(type_ = i64) id operand =
  description ~operands:[ operand ] ~target_type:type_ id Opcode.Ic_return_val

let ret id = description id Opcode.Ic_ret

let block id instructions : Graph.block_description =
  { block_id = block_id id; instructions }

let graph ?(entry = 0) blocks =
  Graph.create ~entry:(block_id entry) blocks
  |> require_ok (fun errors ->
      errors
      |> List.map (fun (error : Graph.error) ->
          error.code ^ ": " ^ error.message)
      |> String.concat "; ")
  |> X87.verify
  |> require_ok (fun errors ->
      errors
      |> List.map (fun (error : X87.error) -> error.code ^ ": " ^ error.message)
      |> String.concat "; ")

let single instructions = graph [ block 0 instructions ]

let returned ?(producer_type = i64) ?(return_type = i64) bits =
  single
    [
      imm ~type_:producer_type 0 bits;
      return_value ~type_:return_type 1 0;
      ret 2;
    ]

let descriptions checked =
  checked |> X87.graph |> Graph.blocks
  |> List.concat_map (fun block ->
      Graph.instructions block |> Sequence.instructions
      |> List.map Sequence.description)

let replace_instruction id replacement checked =
  descriptions checked
  |> List.map (fun (item : Sequence.description) ->
      if Sequence.Instruction_id.to_int item.instruction_id = id then
        replacement item
      else item)
  |> single

let type_name = function
  | Native.I64 -> "I64"
  | Native.U64 -> "U64"

let hex bytes =
  String.to_seq bytes
  |> Seq.map (fun byte -> Printf.sprintf "%02x" (Char.code byte))
  |> List.of_seq |> String.concat ""

(* This checks instruction boundaries and operands, never evaluates code. *)
let decoded_mnemonics code =
  let length = String.length code in
  let byte offset =
    if offset >= length then Alcotest.fail "truncated native instruction";
    Char.code code.[offset]
  in
  let register number =
    if not (List.mem number [ 0; 1; 2; 8; 9; 10; 11 ]) then
      Alcotest.failf "native code uses nonvolatile or stack register %d" number
  in
  let rec decode offset reversed =
    if offset = length then List.rev reversed
    else if byte offset = 0xc3 then (
      Alcotest.(check int)
        "RET is the last machine instruction" length (offset + 1);
      List.rev ("ret" :: reversed))
    else
      let rex = byte offset in
      if not (List.mem rex [ 0x48; 0x49; 0x4c; 0x4d ]) then
        Alcotest.failf "missing 64-bit register-only REX at byte %d" offset;
      let opcode = byte (offset + 1) in
      if opcode >= 0xb8 && opcode <= 0xbf then (
        Alcotest.(check int) "immediate has no REX.R extension" 0 (rex land 4);
        register (opcode - 0xb8 + if rex land 1 = 0 then 0 else 8);
        ignore (byte (offset + 9));
        decode (offset + 10) ("mov-imm64" :: reversed))
      else
        let modrm_offset = offset + if opcode = 0x0f then 3 else 2 in
        let modrm = byte modrm_offset in
        Alcotest.(check int) "ModRM uses registers, never memory" 3 (modrm lsr 6);
        let rm = (modrm land 7) + if rex land 1 = 0 then 0 else 8 in
        register rm;
        let reg = ((modrm lsr 3) land 7) + if rex land 4 = 0 then 0 else 8 in
        let mnemonic =
          match opcode with
          | 0xf7 -> (
              Alcotest.(check int)
                "unary group has no REX.R extension" 0 (rex land 4);
              match (modrm lsr 3) land 7 with
              | 2 -> "not"
              | 3 -> "neg"
              | _ -> Alcotest.fail "unexpected unary opcode extension")
          | 0x0f ->
              register reg;
              Alcotest.(check int)
                "two-byte opcode is IMUL" 0xaf
                (byte (offset + 2));
              "imul"
          | opcode -> (
              register reg;
              match opcode with
              | 0x89 | 0x8b -> "mov"
              | 0x01 | 0x03 -> "add"
              | 0x29 | 0x2b -> "sub"
              | 0x21 | 0x23 -> "and"
              | 0x09 | 0x0b -> "or"
              | 0x31 | 0x33 -> "xor"
              | _ -> Alcotest.failf "unsupported emitted opcode 0x%02x" opcode)
        in
        decode (modrm_offset + 1) (mnemonic :: reversed)
  in
  Alcotest.(check bool) "machine image is nonempty" true (length > 0);
  Alcotest.(check int) "image ends in RET" 0xc3 (byte (length - 1));
  decode 0 []

let inspect_image checked compiled =
  let decoded = Native.code compiled |> decoded_mnemonics in
  Alcotest.(check int)
    "all IR instructions are counted"
    (List.length (descriptions checked))
    (Native.ir_instructions compiled);
  Alcotest.(check int)
    "machine count matches decoded instructions" (List.length decoded)
    (Native.machine_instructions compiled);
  Alcotest.(check bool)
    "register peak fits the volatile set" true
    (Native.register_peak compiled >= 1 && Native.register_peak compiled <= 7)

let high_register_shared_graph () =
  (* The first two subtractions must reuse R11 and R10 respectively: every
     other register still owns an operand needed later. The duplicated input
     to IMUL then has its final use while all seven registers are occupied. *)
  single
    [
      imm 0 0x0000000100000000L;
      imm 1 11L;
      imm 2 13L;
      imm 3 17L;
      imm 4 19L;
      imm 5 23L;
      imm 6 29L;
      binary 7 Opcode.Ic_sub 0 6;
      binary 8 Opcode.Ic_sub 7 5;
      binary 9 Opcode.Ic_mul 7 7;
      binary 10 Opcode.Ic_sub 8 9;
      binary 11 Opcode.Ic_sub 0 10;
      binary 12 Opcode.Ic_add 11 1;
      binary 13 Opcode.Ic_add 12 2;
      binary 14 Opcode.Ic_add 13 3;
      binary 15 Opcode.Ic_add 14 4;
      return_value 16 15;
      ret 17;
    ]

let shared_cases () =
  [
    ( "left operand remains live across subtraction",
      single
        [
          imm 0 100L;
          imm 1 7L;
          binary 2 Opcode.Ic_sub 0 1;
          binary 3 Opcode.Ic_sub 0 2;
          return_value 4 3;
          ret 5;
        ] );
    ( "right operand remains live across subtraction",
      single
        [
          imm 0 100L;
          imm 1 7L;
          binary 2 Opcode.Ic_sub 0 1;
          binary 3 Opcode.Ic_sub 1 2;
          return_value 4 3;
          ret 5;
        ] );
    ( "both operands remain live",
      single
        [
          imm 0 100L;
          imm 1 7L;
          binary 2 Opcode.Ic_sub 0 1;
          binary 3 Opcode.Ic_add 0 1;
          binary 4 Opcode.Ic_xor 2 3;
          return_value 5 4;
          ret 6;
        ] );
    ( "duplicate operand has a later use",
      single
        [
          imm 0 17L;
          binary 1 Opcode.Ic_mul 0 0;
          binary 2 Opcode.Ic_sub 0 1;
          return_value 3 2;
          ret 4;
        ] );
    ( "duplicate operand dies in one instruction",
      single [ imm 0 17L; binary 1 Opcode.Ic_sub 0 0; return_value 2 1; ret 3 ]
    );
    ( "unary operation preserves a shared input",
      single
        [
          imm 0 17L;
          unary 1 Opcode.Ic_unary_minus 0;
          binary 2 Opcode.Ic_sub 0 1;
          return_value 3 2;
          ret 4;
        ] );
    ( "return moves a still-live temporary to RAX",
      single
        [
          imm 0 100L;
          imm 1 7L;
          binary 2 Opcode.Ic_sub 0 1;
          unary 3 Opcode.Ic_unary_minus 0;
          return_value 4 2;
          ret 5;
        ] );
    ( "seven-register sharing reuses right operands in R11 and R10",
      high_register_shared_graph () );
  ]

let class_cases () =
  let complement_tail tail =
    single (imm ~type_:u64 0 Int64.min_int :: unary 1 Opcode.Ic_com 0 :: tail)
  in
  [
    ( "unsigned immediate",
      returned ~producer_type:u64 ~return_type:u64 (-1L),
      Native.U64 );
    ( "complement returns its declared I64",
      complement_tail [ return_value 2 1; ret 3 ],
      Native.I64 );
    ( "complement forwards U64 computation into addition",
      complement_tail
        [
          imm 2 7L;
          binary ~type_:u64 3 Opcode.Ic_add 1 2;
          return_value ~type_:u64 4 3;
          ret 5;
        ],
      Native.U64 );
    ( "nested complement retains U64 computation",
      complement_tail
        [
          unary 2 Opcode.Ic_com 1;
          imm 3 7L;
          binary ~type_:u64 4 Opcode.Ic_xor 2 3;
          return_value ~type_:u64 5 4;
          ret 6;
        ],
      Native.U64 );
    ( "negation uses the operand declared type",
      complement_tail
        [
          unary 2 Opcode.Ic_unary_minus 1;
          imm 3 7L;
          binary 4 Opcode.Ic_add 2 3;
          return_value 5 4;
          ret 6;
        ],
      Native.I64 );
    ( "unsigned negation produces I64",
      single
        [
          imm ~type_:u64 0 Int64.min_int;
          unary 1 Opcode.Ic_unary_minus 0;
          return_value 2 1;
          ret 3;
        ],
      Native.I64 );
  ]

let pressure_graph count =
  let definitions =
    List.init count (fun id -> imm id (Int64.of_int (id + 1)))
  in
  let rec reduce next accumulator = function
    | [] -> [ return_value next accumulator; ret (next + 1) ]
    | operand :: rest ->
        binary next Opcode.Ic_add accumulator operand
        :: reduce (next + 1) next rest
  in
  single
    (definitions @ reduce count 0 (List.init (count - 1) (fun id -> id + 1)))

let encoder_extended_register_bytes () =
  (* Literal bytes are independent of the encoder and generated tables. The
     forms come from pinned OpCodes.DD; Asm.HC:580-638 supplies REX.R/B and
     ModRM placement. IMUL places its destination in the opposite field to
     MOV and the other binary forms, so both orientations matter. *)
  let cases =
    let open Encoder in
    [
      ( "MOV R8,imm64",
        Mov_imm64 (R8, 0x0123456789abcdefL),
        "49b8efcdab8967452301" );
      ("MOV R9,imm64", Mov_imm64 (R9, Int64.min_int), "49b90000000000000080");
      ("MOV R10,imm64", Mov_imm64 (R10, -1L), "49baffffffffffffffff");
      ( "MOV R11,imm64",
        Mov_imm64 (R11, 0x0000000100000001L),
        "49bb0100000001000000" );
      ("MOV R8,RAX", Mov (R8, Rax), "4989c0");
      ("MOV RAX,R8", Mov (Rax, R8), "4c89c0");
      ("MOV R9,RDX", Mov (R9, Rdx), "4989d1");
      ("MOV RDX,R9", Mov (Rdx, R9), "4c89ca");
      ("MOV R10,R11", Mov (R10, R11), "4d89da");
      ("MOV R11,R10", Mov (R11, R10), "4d89d3");
      ("NEG R8", Unary (Neg, R8), "49f7d8");
      ("NEG R9", Unary (Neg, R9), "49f7d9");
      ("NEG R10", Unary (Neg, R10), "49f7da");
      ("NEG R11", Unary (Neg, R11), "49f7db");
      ("NOT R8", Unary (Not, R8), "49f7d0");
      ("NOT R9", Unary (Not, R9), "49f7d1");
      ("NOT R10", Unary (Not, R10), "49f7d2");
      ("NOT R11", Unary (Not, R11), "49f7d3");
      ("ADD R8,RAX", Binary (Add, R8, Rax), "4901c0");
      ("ADD RAX,R8", Binary (Add, Rax, R8), "4c01c0");
      ("ADD R10,R11", Binary (Add, R10, R11), "4d01da");
      ("SUB R8,RAX", Binary (Sub, R8, Rax), "4929c0");
      ("SUB RAX,R8", Binary (Sub, Rax, R8), "4c29c0");
      ("SUB R11,R10", Binary (Sub, R11, R10), "4d29d3");
      ("IMUL R8,RAX", Binary (Imul, R8, Rax), "4c0fafc0");
      ("IMUL RAX,R8", Binary (Imul, Rax, R8), "490fafc0");
      ("IMUL R10,R11", Binary (Imul, R10, R11), "4d0fafd3");
      ("IMUL R11,R10", Binary (Imul, R11, R10), "4d0fafda");
      ("AND R9,RCX", Binary (And, R9, Rcx), "4921c9");
      ("AND RCX,R9", Binary (And, Rcx, R9), "4c21c9");
      ("AND R10,R11", Binary (And, R10, R11), "4d21da");
      ("OR R8,RDX", Binary (Or, R8, Rdx), "4909d0");
      ("OR RDX,R8", Binary (Or, Rdx, R8), "4c09c2");
      ("OR R11,R10", Binary (Or, R11, R10), "4d09d3");
      ("XOR R9,RDX", Binary (Xor, R9, Rdx), "4931d1");
      ("XOR RDX,R9", Binary (Xor, Rdx, R9), "4c31ca");
      ("XOR R10,R11", Binary (Xor, R10, R11), "4d31da");
    ]
  in
  List.iter
    (fun (label, instruction, expected) ->
      Alcotest.(check string) label expected (hex (Encoder.encode instruction));
      Alcotest.(check int)
        (label ^ " exact byte count")
        (String.length expected / 2)
        (Encoder.size instruction))
    cases;
  let instructions =
    List.map (fun (_, instruction, _) -> instruction) cases @ [ Encoder.Ret ]
  in
  let expected =
    String.concat "" (List.map (fun (_, _, bytes) -> bytes) cases) ^ "c3"
  in
  let bytes = String.length expected / 2 in
  let encoded =
    Encoder.encode_all ~max_code_bytes:bytes instructions |> require_ok Fun.id
  in
  Alcotest.(check string)
    "batch encoding preserves every field at its exact bound" expected
    (hex encoded);
  match Encoder.encode_all ~max_code_bytes:(bytes - 1) instructions with
  | Error message ->
      Alcotest.(check bool)
        "batch byte exhaustion has a diagnostic" true (message <> "")
  | Ok _ ->
      Alcotest.fail "batch encoding accepted one byte below its exact bound"

let high_register_shared_bytes () =
  let checked = high_register_shared_graph () in
  let compiled = image checked in
  inspect_image checked compiled;
  (* Fixed physical operands also check preservation of every still-live input.
     shared_cases exposes this same graph to the explicit native/VM comparison. *)
  let expected =
    String.concat ""
      [
        "48b80000000001000000";
        "48b90b00000000000000";
        "48ba0d00000000000000";
        "49b81100000000000000";
        "49b91300000000000000";
        "49ba1700000000000000";
        "49bb1d00000000000000";
        "4929c3";
        "49f7db";
        "4d29da";
        "49f7da";
        "4d0fafdb";
        "4d29da";
        "4c29d0";
        "4801c8";
        "4801d0";
        "4c01c0";
        "4c01c8";
        "c3";
      ]
  in
  Alcotest.(check string)
    "right-only reuse preserves order and shared operands" expected
    (hex (Native.code compiled));
  Alcotest.(check int)
    "both high-register reuses occur at full pressure" 7
    (Native.register_peak compiled);
  Alcotest.(check int)
    "shared DAG instruction count" 18
    (Native.ir_instructions compiled);
  Alcotest.(check int)
    "two subtraction repairs need two extra NEG instructions" 19
    (Native.machine_instructions compiled);
  Alcotest.(check int)
    "shared DAG needs no spill or return move" 105
    (String.length (Native.code compiled))

let sparse_identities () =
  let dense = high_register_shared_graph () in
  let remap_value operand =
    let id = Sequence.Value_id.to_int operand in
    value_id (if id land 1 = 0 then max_int - id else 4096 * id)
  in
  (* Remap after constructing dense descriptions so source spans remain small
     and valid. Instruction order, instruction IDs and value IDs are distinct. *)
  let instructions =
    descriptions dense
    |> List.map (fun (item : Sequence.description) ->
        {
          item with
          instruction_id =
            instruction_id
              (max_int
              - (2 * Sequence.Instruction_id.to_int item.instruction_id));
          operands = List.map remap_value item.operands;
          result =
            Option.map
              (fun (result : Sequence.value_definition) ->
                { Sequence.value_id = remap_value result.value_id })
              item.result;
        })
  in
  let checked = graph ~entry:max_int [ block max_int instructions ] in
  let compiled =
    compile ~max_ir_instructions:18 ~max_code_bytes:105 checked
    |> require_ok native_errors
  in
  inspect_image checked compiled;
  Alcotest.(check string)
    "sparse identities do not affect emitted code"
    (Native.code (image dense))
    (Native.code compiled);
  Alcotest.(check int)
    "sparse identities do not affect register pressure" 7
    (Native.register_peak compiled);
  let errors =
    compile ~max_ir_instructions:17 ~max_code_bytes:105 checked
    |> reject ~code:"HCBACK0001" "IR limit counts nodes rather than sparse IDs"
  in
  Alcotest.(check bool)
    "sparse IDs retain the original terminal source span" true
    (List.exists
       (fun (error : Native.error) -> error.span = Some (fixture_span 17))
       errors)

let actual_ir_limit () =
  let instruction_count = 100000 in
  let negations = instruction_count - 3 in
  (* Tail-build one fixture and reuse it for the rejected bound. Avoid a huge
     decoded/hex string: inspect the fixed-size instructions directly below. *)
  let rec prepend id suffix =
    if id = 0 then imm 0 0x0000000100000001L :: suffix
    else prepend (id - 1) (unary id Opcode.Ic_unary_minus (id - 1) :: suffix)
  in
  let checked =
    single
      (prepend negations
         [
           return_value (instruction_count - 2) negations;
           ret (instruction_count - 1);
         ])
  in
  let bytes = 11 + (3 * negations) in
  let compiled =
    compile ~max_ir_instructions:instruction_count ~max_code_bytes:bytes checked
    |> require_ok native_errors
  in
  let code = Native.code compiled in
  Alcotest.(check int)
    "the actual hard-limit graph is fully counted" instruction_count
    (Native.ir_instructions compiled);
  Alcotest.(check int)
    "every negation emits one instruction" (instruction_count - 1)
    (Native.machine_instructions compiled);
  Alcotest.(check int)
    "the long chain reuses one register" 1
    (Native.register_peak compiled);
  Alcotest.(check int)
    "the actual hard-limit image fits its exact byte quota" bytes
    (String.length code);
  Alcotest.(check string)
    "the initial immediate retains its upper 32 bits" "48b80100000001000000"
    (hex (String.sub code 0 10));
  for index = 0 to negations - 1 do
    let offset = 10 + (3 * index) in
    if
      code.[offset] <> '\x48'
      || code.[offset + 1] <> '\xf7'
      || code.[offset + 2] <> '\xd8'
    then Alcotest.failf "long-chain instruction %d is not NEG RAX" (index + 1)
  done;
  Alcotest.(check int)
    "the long chain ends with RET" 0xc3
    (Char.code code.[bytes - 1]);
  let errors =
    compile ~max_ir_instructions:(instruction_count - 1) ~max_code_bytes:bytes
      checked
    |> reject ~code:"HCBACK0001" "actual 100000 nodes exceed a 99999-node quota"
  in
  Alcotest.(check bool)
    "the rejected graph identifies its first excess node" true
    (List.exists
       (fun (error : Native.error) ->
         error.span = Some (fixture_span (instruction_count - 1)))
       errors)

let multiply_bytes () =
  let checked = source_graph "6*7;" in
  let compiled = image checked in
  inspect_image checked compiled;
  Alcotest.(check string)
    "source expression emits two inputs and IMUL"
    "48b8060000000000000048b90700000000000000480fafc1c3"
    (hex (Native.code compiled));
  Alcotest.(check (list string))
    "multiply is executed by an IMUL instruction"
    [ "mov-imm64"; "mov-imm64"; "imul"; "ret" ]
    (decoded_mnemonics (Native.code compiled));
  Alcotest.(check int)
    "canonical image length" 25
    (String.length (Native.code compiled));
  Alcotest.(check int)
    "two live source operands" 2
    (Native.register_peak compiled);
  Alcotest.(check string)
    "declared result type" "I64"
    (type_name (Native.value_type compiled));
  let repeated = image (source_graph "6*7;") in
  Alcotest.(check string)
    "fresh sessions produce identical bytes" (Native.code compiled)
    (Native.code repeated)

let supported_source () =
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          let checked = source_graph ~mode text in
          inspect_image checked (image checked))
        [
          "-42;";
          "~0x8000000000000000;";
          "(2+3)*(11-4);";
          "(15&6)|(8^3);";
          "0xFFFFFFFFFFFFFFFF+1;";
          "(~0x8000000000000000)+2;";
          "#define ANSWER (6*7)\nANSWER;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ];
  List.iter
    (fun (_, checked) -> inspect_image checked (image checked))
    (shared_cases ());
  let left_live = List.hd (shared_cases ()) |> snd |> image |> Native.code in
  Alcotest.(check (list string))
    "reusing the right register preserves left-minus-right order"
    [ "mov-imm64"; "mov-imm64"; "sub"; "neg"; "sub"; "ret" ]
    (decoded_mnemonics left_live);
  List.iter
    (fun (label, checked, expected_type) ->
      let compiled = image checked in
      inspect_image checked compiled;
      Alcotest.(check string)
        label (type_name expected_type)
        (type_name (Native.value_type compiled)))
    (class_cases ())

let limits () =
  let checked = source_graph "6*7;" in
  let compiled = image checked in
  let ir = Native.ir_instructions compiled in
  let bytes = String.length (Native.code compiled) in
  let exact =
    compile ~max_ir_instructions:ir ~max_code_bytes:bytes checked
    |> require_ok native_errors
  in
  Alcotest.(check string)
    "exact independent bounds preserve code" (Native.code compiled)
    (Native.code exact);
  ignore
    (compile ~max_ir_instructions:(ir - 1) ~max_code_bytes:bytes checked
    |> reject ~code:"HCBACK0001" "one instruction below the required IR bound");
  ignore
    (compile ~max_ir_instructions:ir ~max_code_bytes:(bytes - 1) checked
    |> reject ~code:"HCBACK0005" "one byte below the complete image bound");
  List.iter
    (fun (max_ir_instructions, max_code_bytes) ->
      let errors =
        compile ~max_ir_instructions ~max_code_bytes checked
        |> reject ~code:"HCBACK0001" "invalid configuration"
      in
      Alcotest.(check bool)
        "configuration errors do not fabricate source spans" true
        (List.for_all (fun (error : Native.error) -> error.span = None) errors))
    [
      (0, bytes); (-1, bytes); (ir, 0); (ir, -1); (100001, bytes); (ir, 16777217);
    ];
  ignore
    (compile ~max_ir_instructions:100000
       ~max_code_bytes:(min 16777216 Sys.max_string_length)
       checked
    |> require_ok native_errors);
  Alcotest.(check string)
    "budget failure does not poison another compilation" (Native.code compiled)
    (Native.code (image checked))

let pressure () =
  let checked = pressure_graph 7 in
  let compiled = image checked in
  inspect_image checked compiled;
  Alcotest.(check int)
    "all seven volatile registers are available" 7
    (Native.register_peak compiled);
  ignore
    (compile (pressure_graph 8)
    |> reject ~code:"HCBACK0004" "eighth simultaneously live value");
  Alcotest.(check string)
    "failed allocation leaves subsequent output unchanged"
    (Native.code compiled)
    (Native.code (image checked))

let immutable_code () =
  let compiled = image (source_graph "6*7;") in
  let first = Native.code compiled and second = Native.code compiled in
  let expected = Bytes.of_string second |> Bytes.to_string in
  Bytes.fill (Bytes.unsafe_of_string first) 0 (String.length first) '\000';
  Alcotest.(check string)
    "existing getter result is independent" expected second;
  Alcotest.(check string)
    "compiled image survives caller mutation" expected (Native.code compiled);
  Bytes.set (Bytes.unsafe_of_string second) 0 '\255';
  Alcotest.(check string)
    "every getter returns independently owned bytes" expected
    (Native.code compiled)

let unsupported_source () =
  List.iter
    (fun text ->
      let checked = source_graph text in
      let errors = compile checked |> reject ~code:"HCBACK0002" text in
      Alcotest.(check bool)
        "unsupported source preserves a source span" true
        (List.exists
           (fun (error : Native.error) -> Option.is_some error.span)
           errors))
    [ "1/0;"; "7%3;"; "1<<2;"; "1==2;"; "!0;"; "1&&0;"; "1.0;" ];
  List.iter
    (fun type_ ->
      ignore
        (returned ~producer_type:type_ ~return_type:type_ 1L
        |> compile
        |> reject ~code:"HCBACK0002" "unsupported scalar domain"))
    [
      primitive ~form:Type.Public_spelling Primitive_type.I64;
      primitive ~form:Type.Public_spelling Primitive_type.U64;
      primitive Primitive_type.I8;
      primitive Primitive_type.U8;
      primitive ~pointer_depth:1 Primitive_type.I64;
    ];
  let public_return =
    returned
      ~return_type:(primitive ~form:Type.Public_spelling Primitive_type.I64)
      1L
  in
  ignore
    (compile public_return
    |> reject ~code:"HCBACK0002" "public return boundary is outside this gate")

let unsupported_graphs () =
  List.iter
    (fun opcode ->
      let info = Opcode.info opcode in
      let operands =
        match info.argument_count with
        | Opcode.Zero | Opcode.Variable -> []
        | Opcode.One -> [ 0 ]
        | Opcode.Two -> [ 0; 1 ]
      in
      let result, target_type =
        if info.result_count = 0 then (None, None) else (Some 2, Some i64)
      in
      let checked =
        single
          [
            imm 0 40L;
            imm 1 2L;
            description ~operands ?result ?target_type 2 opcode;
            return_value 3 0;
            ret 4;
          ]
      in
      ignore
        (compile checked
        |> reject ~code:"HCBACK0002" (Opcode.to_source_name opcode)))
    [
      Opcode.Ic_deref;
      Opcode.Ic_addr;
      Opcode.Ic_assign;
      Opcode.Ic_call;
      Opcode.Ic_call_start;
      Opcode.Ic_call_end;
      Opcode.Ic_enter;
      Opcode.Ic_leave;
      Opcode.Ic_label;
    ];
  let base = descriptions (returned 42L) in
  let unreachable = graph [ block 0 base; block 1 [ ret 3 ] ] in
  ignore
    (compile unreachable |> reject ~code:"HCBACK0002" "extra unreachable block");
  let jump =
    description ~payload:(Sequence.Block (block_id 0)) 0 Opcode.Ic_jmp
  in
  ignore
    (compile (single [ jump ])
    |> reject ~code:"HCBACK0002" "single-block self edge")

let complete_preflight () =
  let dead_division =
    single
      [
        imm 0 1L;
        imm 1 0L;
        binary 2 Opcode.Ic_div 0 1;
        imm 3 42L;
        return_value 4 3;
        ret 5;
      ]
  in
  let errors =
    compile ~max_code_bytes:1 dead_division
    |> reject ~code:"HCBACK0002" "unused unsupported producer precedes emission"
  in
  Alcotest.(check bool)
    "complete preflight retains the rejected source node" true
    (List.exists
       (fun (error : Native.error) -> error.span = Some (fixture_span 2))
       errors);
  let flagged =
    returned 42L
    |> replace_instruction 0 (fun item -> { item with flags = 0x200L })
  in
  ignore
    (compile flagged |> reject ~code:"HCBACK0002" "known but nonzero IR flags");
  ignore
    (compile ~max_ir_instructions:0 dead_division
    |> reject ~code:"HCBACK0001" "configuration precedes unsupported IR")

let malformed_graphs () =
  let checked = source_graph "6*7;" in
  let malformed =
    [
      ( "missing integer payload",
        replace_instruction 0 (fun item -> { item with payload = None }) checked
      );
      ( "floating payload on integer opcode",
        replace_instruction 0
          (fun item -> { item with payload = Some (Sequence.Float_bits 0L) })
          checked );
      ( "extra arithmetic payload",
        replace_instruction 2
          (fun item -> { item with payload = Some (Sequence.Integer 0L) })
          checked );
      ( "extra return payload",
        replace_instruction 4
          (fun item -> { item with payload = Some (Sequence.Integer 0L) })
          checked );
      ( "missing return type",
        replace_instruction 3
          (fun item -> { item with target_type = None })
          checked );
      ("missing return value", single [ imm 0 42L; ret 1 ]);
      ( "return value before another producer",
        single [ imm 0 42L; return_value 1 0; imm 2 7L; ret 3 ] );
      ( "two return-value preparations",
        single [ imm 0 42L; return_value 1 0; return_value 2 0; ret 3 ] );
    ]
  in
  List.iter
    (fun (label, graph) ->
      ignore (compile graph |> reject ~code:"HCBACK0003" label))
    malformed

let invalid_type_relationships () =
  let malformed =
    [
      ("return differs from declared result", returned ~return_type:u64 42L);
      ( "complement declares U64",
        single
          [
            imm ~type_:u64 0 (-1L);
            unary ~type_:u64 1 Opcode.Ic_com 0;
            return_value ~type_:u64 2 1;
            ret 3;
          ] );
      ( "unsigned negation incorrectly remains U64",
        single
          [
            imm ~type_:u64 0 (-1L);
            unary ~type_:u64 1 Opcode.Ic_unary_minus 0;
            return_value ~type_:u64 2 1;
            ret 3;
          ] );
      ( "mixed arithmetic incorrectly declares I64",
        single
          [
            imm ~type_:u64 0 (-1L);
            imm 1 7L;
            binary 2 Opcode.Ic_add 0 1;
            return_value 3 2;
            ret 4;
          ] );
      ( "signed arithmetic incorrectly declares U64",
        single
          [
            imm 0 6L;
            imm 1 7L;
            binary ~type_:u64 2 Opcode.Ic_mul 0 1;
            return_value ~type_:u64 3 2;
            ret 4;
          ] );
      ( "complement computation class is lost downstream",
        single
          [
            imm ~type_:u64 0 Int64.min_int;
            unary 1 Opcode.Ic_com 0;
            imm 2 7L;
            binary 3 Opcode.Ic_add 1 2;
            return_value 4 3;
            ret 5;
          ] );
    ]
  in
  List.iter
    (fun (label, graph) ->
      ignore (compile graph |> reject ~code:"HCBACK0003" label))
    malformed

let tests =
  [
    Alcotest.test_case "extended REX and ModRM orientations have exact bytes"
      `Quick encoder_extended_register_bytes;
    Alcotest.test_case
      "high-register sharing preserves subtraction and duplicates" `Quick
      high_register_shared_bytes;
    Alcotest.test_case
      "sparse near-max_int identities preserve bounded emission" `Quick
      sparse_identities;
    Alcotest.test_case
      "actual 100000-instruction graph accepts only its full quota" `Slow
      actual_ir_limit;
    Alcotest.test_case "public source emits deterministic 64-bit multiply bytes"
      `Quick multiply_bytes;
    Alcotest.test_case
      "supported source, shared values and distinct type classes" `Quick
      supported_source;
    Alcotest.test_case "exact IR and machine-code budgets" `Quick limits;
    Alcotest.test_case "seven-register pressure boundary" `Quick pressure;
    Alcotest.test_case "compiled bytes have no mutable getter aliases" `Quick
      immutable_code;
    Alcotest.test_case "unsupported source and scalar domains" `Quick
      unsupported_source;
    Alcotest.test_case "memory, calls, frames and control flow reject" `Quick
      unsupported_graphs;
    Alcotest.test_case "preflight checks dead producers before emission" `Quick
      complete_preflight;
    Alcotest.test_case "constructor-valid malformed return harnesses reject"
      `Quick malformed_graphs;
    Alcotest.test_case "declared and computational type relationships reject"
      `Quick invalid_type_relationships;
  ]
