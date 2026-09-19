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

let source_inputs ?(mode = Preprocessor.Jit) contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-expression.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let source_graph ?mode contents =
  let session, config, source = source_inputs ?mode contents in
  lower_integer_expression session ~config ~source
  |> require_ok diagnostic_errors

let source_image ?mode ?status_abi contents =
  let session, config, source = source_inputs ?mode contents in
  Native_expression.compile ?status_abi session ~config ~source
  |> require_ok diagnostic_errors

let compile ?(max_ir_instructions = 10000) ?(max_code_bytes = 1048576)
    ?(max_stack_bytes = 4088) ?status_abi graph =
  Native.compile ~max_ir_instructions ~max_code_bytes ~max_stack_bytes
    ?status_abi graph

let image ?status_abi graph =
  compile ?status_abi graph |> require_ok native_errors

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

let word_view ?(type_ = u64) id operand =
  description ~operands:[ operand ] ~result:id ~target_type:type_
    ~payload:(Sequence.Integer 0L) id Opcode.Ic_holyc_typecast

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

let find_index predicate values =
  let rec loop index = function
    | [] -> None
    | value :: rest ->
        if predicate value then Some index else loop (index + 1) rest
  in
  loop 0 values

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
  let setcc = function
    | 0x92 -> "setb"
    | 0x93 -> "setae"
    | 0x94 -> "sete"
    | 0x95 -> "setne"
    | 0x96 -> "setbe"
    | 0x97 -> "seta"
    | 0x9c -> "setl"
    | 0x9d -> "setge"
    | 0x9e -> "setle"
    | 0x9f -> "setg"
    | opcode -> Alcotest.failf "unsupported SETcc opcode 0x%02x" opcode
  in
  let rec decode offset reversed =
    if offset = length then List.rev reversed
    else if byte offset = 0xc3 then (
      Alcotest.(check int)
        "RET is the last machine instruction" length (offset + 1);
      List.rev ("ret" :: reversed))
    else if byte offset = 0x33 && byte (offset + 1) = 0xd2 then
      decode (offset + 2) ("zero-edx" :: reversed)
    else if byte offset = 0xe9 then (
      ignore (byte (offset + 4));
      decode (offset + 5) ("jmp" :: reversed))
    else if byte offset = 0x0f && List.mem (byte (offset + 1)) [ 0x84; 0x85 ]
    then (
      let mnemonic = if byte (offset + 1) = 0x84 then "je" else "jne" in
      ignore (byte (offset + 5));
      decode (offset + 6) (mnemonic :: reversed))
    else if
      byte offset = 0x49
      && byte (offset + 1) = 0x89
      && List.mem (byte (offset + 2)) [ 0xcb; 0xfb ]
    then
      decode (offset + 3)
        ((if byte (offset + 2) = 0xcb then "capture-status-win"
          else "capture-status-sysv")
        :: reversed)
    else if
      byte offset = 0x49
      && byte (offset + 1) = 0xc7
      && byte (offset + 2) = 0x43
      && List.mem (byte (offset + 3)) [ 0x00; 0x08 ]
    then (
      ignore (byte (offset + 7));
      decode (offset + 8)
        ((if byte (offset + 3) = 0 then "store-status-kind"
          else "store-status-site")
        :: reversed))
    else if byte offset = 0x48 && byte (offset + 1) = 0x99 then
      decode (offset + 2) ("cqo" :: reversed)
    else if byte offset = 0x0f || byte offset = 0x41 then (
      (* After the explicitly decoded status/branch forms above, this 0F/41
         path is SETcc. Low registers need no prefix; extended byte registers
         need REX.B alone, never a high-byte register. *)
      let extended = byte offset = 0x41 in
      let opcode_offset = offset + if extended then 1 else 0 in
      Alcotest.(check int)
        "byte-result opcode starts with 0F" 0x0f (byte opcode_offset);
      let mnemonic = setcc (byte (opcode_offset + 1)) in
      let modrm = byte (opcode_offset + 2) in
      Alcotest.(check int) "SETcc addresses a register" 3 (modrm lsr 6);
      Alcotest.(check int) "SETcc retains the /0 field" 0 ((modrm lsr 3) land 7);
      register ((modrm land 7) + if extended then 8 else 0);
      decode (opcode_offset + 3) (mnemonic :: reversed))
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
      else if opcode = 0x81 then (
        Alcotest.(check int)
          "stack adjustment uses RSP without an extension" 0x48 rex;
        let modrm = byte (offset + 2) in
        let mnemonic =
          match modrm with
          | 0xec -> "alloc-stack"
          | 0xc4 -> "free-stack"
          | _ -> Alcotest.failf "unexpected stack-adjust ModRM 0x%02x" modrm
        in
        ignore (byte (offset + 6));
        decode (offset + 7) (mnemonic :: reversed))
      else if opcode = 0x83 then (
        let modrm = byte (offset + 2) in
        Alcotest.(check int) "CMP imm8 uses a register" 3 (modrm lsr 6);
        Alcotest.(check int)
          "CMP imm8 retains the /7 extension" 7
          ((modrm lsr 3) land 7);
        register ((modrm land 7) + if rex land 1 = 0 then 0 else 8);
        ignore (byte (offset + 3));
        decode (offset + 4) ("cmp-imm8" :: reversed))
      else
        let modrm_offset = offset + if opcode = 0x0f then 3 else 2 in
        let modrm = byte modrm_offset in
        let reg = ((modrm lsr 3) land 7) + if rex land 4 = 0 then 0 else 8 in
        if
          (opcode = 0x8b || opcode = 0x89)
          && modrm lsr 6 = 2
          && modrm land 7 = 4
        then (
          Alcotest.(check int)
            "stack spill uses the canonical RSP SIB" 0x24
            (byte (modrm_offset + 1));
          Alcotest.(check int)
            "stack spill has no REX.B extension" 0 (rex land 1);
          register reg;
          ignore (byte (modrm_offset + 5));
          decode (modrm_offset + 6)
            ((if opcode = 0x8b then "load-stack" else "store-stack") :: reversed))
        else (
          Alcotest.(check int)
            "ModRM uses registers, never memory" 3 (modrm lsr 6);
          let rm = (modrm land 7) + if rex land 1 = 0 then 0 else 8 in
          register rm;
          let mnemonic =
            match opcode with
            | 0xf7 -> (
                Alcotest.(check int)
                  "unary group has no REX.R extension" 0 (rex land 4);
                match (modrm lsr 3) land 7 with
                | 2 -> "not"
                | 3 -> "neg"
                | 6 -> "div"
                | 7 -> "idiv"
                | _ -> Alcotest.fail "unexpected F7 opcode extension")
            | 0x0f -> (
                register reg;
                match byte (offset + 2) with
                | 0xaf -> "imul"
                | 0xb6 -> "movzx8"
                | opcode ->
                    Alcotest.failf "unsupported two-byte opcode 0x%02x" opcode)
            | 0xd3 -> (
                Alcotest.(check int)
                  "shift group has no REX.R extension" 0 (rex land 4);
                match (modrm lsr 3) land 7 with
                | 4 -> "shl"
                | 5 -> "shr"
                | 7 -> "sar"
                | slash ->
                    Alcotest.failf "unsupported D3 slash extension /%d" slash)
            | opcode -> (
                register reg;
                match opcode with
                | 0x89 | 0x8b -> "mov"
                | 0x01 | 0x03 -> "add"
                | 0x29 | 0x2b -> "sub"
                | 0x21 | 0x23 -> "and"
                | 0x09 | 0x0b -> "or"
                | 0x31 | 0x33 -> "xor"
                | 0x39 | 0x3b -> "cmp"
                | 0x85 ->
                    Alcotest.(check int)
                      "TEST reads the same register twice" rm reg;
                    "test"
                | _ -> Alcotest.failf "unsupported emitted opcode 0x%02x" opcode
                )
          in
          decode (modrm_offset + 1) (mnemonic :: reversed))
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

let pressure_source count =
  let rec expression value =
    if value = count then string_of_int value
    else Printf.sprintf "%d+(%s)" value (expression (value + 1))
  in
  expression 1 ^ ";"

let add_tail next accumulator operands =
  let rec loop next accumulator = function
    | [] -> (next, accumulator, [])
    | operand :: remaining ->
        let instruction = binary next Opcode.Ic_add accumulator operand in
        let final_next, final_value, suffix = loop (next + 1) next remaining in
        (final_next, final_value, instruction :: suffix)
  in
  loop next accumulator operands

(* These expected values are stated independently of native execution. The
   count cases freeze x86/VM low-six-bit masking, while the right-shift cases
   distinguish arithmetic I64 from logical U64 computation classes. *)
let shift_source_cases =
  [
    ("count 0", "1<<0;", Native.I64, 1L, [ "shl" ]);
    ("count 1", "1<<1;", Native.I64, 2L, [ "shl" ]);
    ("count 31", "1<<31;", Native.I64, 0x0000000080000000L, [ "shl" ]);
    ("count 32", "1<<32;", Native.I64, 0x0000000100000000L, [ "shl" ]);
    ("count 63", "1<<63;", Native.I64, Int64.min_int, [ "shl" ]);
    ("count 64 masks to zero", "1<<64;", Native.I64, 1L, [ "shl" ]);
    ("count 65 masks to one", "1<<65;", Native.I64, 2L, [ "shl" ]);
    ("count 127 masks to 63", "1<<127;", Native.I64, Int64.min_int, [ "shl" ]);
    ("count -1 masks to 63", "1<<(-1);", Native.I64, Int64.min_int, [ "shl" ]);
    ( "minimum count masks to zero",
      "1<<(-9223372036854775807-1);",
      Native.I64,
      1L,
      [ "shl" ] );
    ( "unsigned high bit shifts logically",
      "0x8000000000000000>>63;",
      Native.U64,
      1L,
      [ "shr" ] );
    ( "signed high bit shifts arithmetically",
      "0x8000000000000000(I64i)>>63;",
      Native.I64,
      -1L,
      [ "sar" ] );
    ( "unsigned count promotes signed left to logical shift",
      "0x8000000000000000(I64i)>>1(U64i);",
      Native.U64,
      0x4000000000000000L,
      [ "shr" ] );
    ( "unsigned left keeps logical shift with signed count",
      "0x8000000000000000>>1(I64i);",
      Native.U64,
      0x4000000000000000L,
      [ "shr" ] );
    ( "left shift wraps at 64 bits",
      "0x8000000000000000<<1;",
      Native.U64,
      0L,
      [ "shl" ] );
    ( "maintained mixed shift fixture",
      "40+(1<<65)+(0x8000000000000000>>63)+(0x8000000000000000(I64i)>>63);",
      Native.U64,
      42L,
      [ "shl"; "shr"; "sar" ] );
  ]

let shift_shared_cases () =
  [
    ( "shift preserves a shared left operand",
      single
        [
          imm 0 3L;
          imm 1 1L;
          binary 2 Opcode.Ic_shl 0 1;
          binary 3 Opcode.Ic_add 0 2;
          return_value 4 3;
          ret 5;
        ],
      Native.I64,
      9L );
    ( "shift preserves a shared count operand",
      single
        [
          imm 0 3L;
          imm 1 1L;
          binary 2 Opcode.Ic_shl 0 1;
          binary 3 Opcode.Ic_add 1 2;
          return_value 4 3;
          ret 5;
        ],
      Native.I64,
      7L );
    ( "duplicate value supplies both left and count",
      single [ imm 0 65L; binary 1 Opcode.Ic_shl 0 0; return_value 2 1; ret 3 ],
      Native.I64,
      130L );
    ( "count is already in RCX",
      single
        [
          imm 0 3L;
          imm 1 1L;
          binary 2 Opcode.Ic_shl 0 1;
          return_value 3 2;
          ret 4;
        ],
      Native.I64,
      6L );
    ( "left starts in RCX while count starts elsewhere",
      single
        [
          imm 0 1L;
          imm 1 3L;
          binary 2 Opcode.Ic_shl 1 0;
          return_value 3 2;
          ret 4;
        ],
      Native.I64,
      6L );
    ( "unrelated live RCX owner survives count setup",
      single
        [
          imm 0 3L;
          imm 1 40L;
          imm 2 1L;
          binary 3 Opcode.Ic_shl 0 2;
          binary 4 Opcode.Ic_add 1 3;
          return_value 5 4;
          ret 6;
        ],
      Native.I64,
      46L );
    ( "unsigned count controls mixed right-shift class",
      single
        [
          imm 0 Int64.min_int;
          imm ~type_:u64 1 1L;
          binary ~type_:u64 2 Opcode.Ic_shr 0 1;
          return_value ~type_:u64 3 2;
          ret 4;
        ],
      Native.U64,
      0x4000000000000000L );
    ( "unsigned left controls mixed right-shift class",
      single
        [
          imm ~type_:u64 0 Int64.min_int;
          imm 1 1L;
          binary ~type_:u64 2 Opcode.Ic_shr 0 1;
          return_value ~type_:u64 3 2;
          ret 4;
        ],
      Native.U64,
      0x4000000000000000L );
    ( "COM forwarded U64 class selects logical right shift",
      single
        [
          imm ~type_:u64 0 0L;
          unary 1 Opcode.Ic_com 0;
          imm 2 1L;
          binary ~type_:u64 3 Opcode.Ic_shr 1 2;
          return_value ~type_:u64 4 3;
          ret 5;
        ],
      Native.U64,
      Int64.max_int );
  ]

let shift_pressure_graph ~count_in_rcx =
  let definitions = List.init 7 (fun id -> imm id (Int64.of_int (id + 1))) in
  let count = if count_in_rcx then 1 else 2 in
  let survivors =
    if count_in_rcx then [ 2; 3; 4; 5; 6 ] else [ 1; 3; 4; 5; 6 ]
  in
  let next, final_value, tail = add_tail 8 7 survivors in
  single
    (definitions
    @ [ binary 7 Opcode.Ic_shl 0 count ]
    @ tail
    @ [ return_value next final_value; ret (next + 1) ])

(* These are literal source expectations, independent of the native backend and
   the interpreter. Unsigned rows use OCaml's public unsigned arithmetic only in
   the execution matrix below; keeping representative constants here makes the
   source/API contract reviewable without consulting either implementation. *)
let divmod_source_cases =
  [
    ("signed quotient", "84/2;", Native.I64, 42L, [ "idiv" ]);
    ("signed remainder", "85%2;", Native.I64, 1L, [ "idiv" ]);
    ( "negative quotient truncates toward zero",
      "-7/3;",
      Native.I64,
      -2L,
      [ "idiv" ] );
    ("negative remainder follows dividend", "-7%3;", Native.I64, -1L, [ "idiv" ]);
    ( "unsigned quotient keeps all high bits",
      "0xFFFFFFFFFFFFFFFF/3;",
      Native.U64,
      6148914691236517205L,
      [ "div" ] );
    ( "unsigned remainder keeps all high bits",
      "0xFFFFFFFFFFFFFFFF%0x8000000000000000;",
      Native.U64,
      Int64.max_int,
      [ "div" ] );
    ( "mixed class selects unsigned division",
      "-1/0x8000000000000000;",
      Native.U64,
      1L,
      [ "div" ] );
    ( "signed high bit divides arithmetically",
      "0x8000000000000000(I64i)/2;",
      Native.I64,
      -4611686018427387904L,
      [ "idiv" ] );
    ( "masked shift composes with division",
      "(1<<65)+(84/2)-2;",
      Native.I64,
      42L,
      [ "idiv" ] );
    ( "maintained mixed divmod fixture",
      "84/2+85%2+0xFFFFFFFFFFFFFFFF/0xFFFFFFFFFFFFFFFF+0xFFFFFFFFFFFFFFFF%2-3;",
      Native.U64,
      42L,
      [ "idiv"; "idiv"; "div"; "div" ] );
  ]

let divmod_shared_cases () =
  [
    ( "division preserves a shared dividend",
      single
        [
          imm 0 84L;
          imm 1 2L;
          binary 2 Opcode.Ic_div 0 1;
          binary 3 Opcode.Ic_add 0 2;
          return_value 4 3;
          ret 5;
        ],
      Native.I64,
      126L );
    ( "division preserves a shared divisor",
      single
        [
          imm 0 84L;
          imm 1 2L;
          binary 2 Opcode.Ic_div 0 1;
          binary 3 Opcode.Ic_add 1 2;
          return_value 4 3;
          ret 5;
        ],
      Native.I64,
      44L );
    ( "duplicate operands produce quotient one",
      single [ imm 0 42L; binary 1 Opcode.Ic_div 0 0; return_value 2 1; ret 3 ],
      Native.I64,
      1L );
    ( "duplicate operands produce remainder zero",
      single [ imm 0 42L; binary 1 Opcode.Ic_mod 0 0; return_value 2 1; ret 3 ],
      Native.I64,
      0L );
    ( "RCX dividend and RAX divisor use the RDX cycle scratch",
      single
        [
          imm 0 2L;
          imm 1 84L;
          binary 2 Opcode.Ic_div 1 0;
          return_value 3 2;
          ret 4;
        ],
      Native.I64,
      42L );
    ( "unrelated live RAX value survives fixed dividend transport",
      single
        [
          imm 0 40L;
          imm 1 84L;
          imm 2 2L;
          binary 3 Opcode.Ic_div 1 2;
          binary 4 Opcode.Ic_add 0 3;
          return_value 5 4;
          ret 6;
        ],
      Native.I64,
      82L );
    ( "unrelated live RDX value survives signed extension",
      single
        [
          imm 0 84L;
          imm 1 2L;
          imm 2 40L;
          binary 3 Opcode.Ic_div 0 1;
          binary 4 Opcode.Ic_add 2 3;
          return_value 5 4;
          ret 6;
        ],
      Native.I64,
      82L );
    ( "remainder result in RDX preserves an unrelated live RAX value",
      single
        [
          imm 0 40L;
          imm 1 85L;
          imm 2 2L;
          binary 3 Opcode.Ic_mod 1 2;
          binary 4 Opcode.Ic_add 0 3;
          return_value 5 4;
          ret 6;
        ],
      Native.I64,
      41L );
    ( "mixed U64 divisor selects unsigned quotient",
      single
        [
          imm 0 (-1L);
          imm ~type_:u64 1 Int64.min_int;
          binary ~type_:u64 2 Opcode.Ic_div 0 1;
          return_value ~type_:u64 3 2;
          ret 4;
        ],
      Native.U64,
      1L );
    ( "COM forwarded U64 class selects unsigned remainder",
      single
        [
          imm ~type_:u64 0 0L;
          unary 1 Opcode.Ic_com 0;
          imm 2 3L;
          binary ~type_:u64 3 Opcode.Ic_mod 1 2;
          return_value ~type_:u64 4 3;
          ret 5;
        ],
      Native.U64,
      0L );
  ]

let divmod_pressure_graph () =
  (* All seven values are live after the final immediate, so reserved R11 forces
     one slot. The longest-lived dividend is selected for that slot. Three dead
     reductions then release every unrelated owner before DIV needs RAX/RCX/RDX,
     isolating the one-spill operand transport from extra fixed-register pressure. *)
  single
    [
      imm 0 84L;
      imm 1 2L;
      imm 2 10L;
      imm 3 11L;
      imm 4 12L;
      imm 5 13L;
      imm 6 14L;
      binary 7 Opcode.Ic_add 4 5;
      binary 8 Opcode.Ic_add 2 3;
      unary 9 Opcode.Ic_unary_minus 6;
      binary 10 Opcode.Ic_div 0 1;
      return_value 11 10;
      ret 12;
    ]

let spill_values count =
  List.init count (fun id ->
      imm id
        (if id = 0 then 10L else if id = 1 then 3L else Int64.of_int (id * 10)))

let two_spilled_inputs_graph opcode =
  let definitions = spill_values 9 in
  let _, sum, prefix = add_tail 9 2 [ 3; 4; 5; 6; 7; 8 ] in
  single
    (definitions @ prefix
    @ [
        binary 15 opcode 0 1;
        binary 16 Opcode.Ic_add sum 15;
        return_value 17 16;
        ret 18;
      ])

let one_spilled_input_graph producer =
  let definitions = spill_values 8 in
  let _, sum, prefix = add_tail 8 1 [ 2; 3; 4; 5; 6; 7 ] in
  single
    (definitions @ prefix
    @ [
        producer 14; binary 15 Opcode.Ic_add sum 14; return_value 16 15; ret 17;
      ])

let spill_semantic_cases () =
  [
    ( "noncommutative operation with both inputs dying",
      two_spilled_inputs_graph Opcode.Ic_sub,
      Native.I64,
      357L );
    ( "shared input remains available after a spilled subtraction",
      (let definitions = spill_values 8 in
       let _, sum, prefix = add_tail 8 2 [ 3; 4; 5; 6; 7 ] in
       single
         (definitions @ prefix
         @ [
             binary 13 Opcode.Ic_sub 0 1;
             binary 14 Opcode.Ic_add 13 0;
             binary 15 Opcode.Ic_add 14 sum;
             return_value 16 15;
             ret 17;
           ])),
      Native.I64,
      287L );
    ( "duplicate value survives spill selection",
      one_spilled_input_graph (fun id -> binary id Opcode.Ic_mul 0 0),
      Native.I64,
      373L );
    ( "unary input reloads with all bits intact",
      one_spilled_input_graph (fun id -> unary id Opcode.Ic_unary_minus 0),
      Native.I64,
      263L );
    ( "comparison inputs retain ordering across spills",
      two_spilled_inputs_graph Opcode.Ic_less,
      Native.I64,
      350L );
    ( "logical NOT reloads a complete spilled word",
      one_spilled_input_graph (fun id -> unary id Opcode.Ic_not 0),
      Native.I64,
      273L );
    ( "binary logical scratch coexists with spilled live values",
      two_spilled_inputs_graph Opcode.Ic_and_and,
      Native.I64,
      351L );
    ( "shift left reloads both spilled operands",
      two_spilled_inputs_graph Opcode.Ic_shl,
      Native.I64,
      430L );
    ( "arithmetic shift right reloads both spilled operands",
      two_spilled_inputs_graph Opcode.Ic_shr,
      Native.I64,
      351L );
    ( "signed division reloads both spilled operands",
      two_spilled_inputs_graph Opcode.Ic_div,
      Native.I64,
      353L );
    ( "signed remainder reloads both spilled operands",
      two_spilled_inputs_graph Opcode.Ic_mod,
      Native.I64,
      351L );
  ]

let spill_slot_reuse_graph () =
  let first_definitions =
    List.init 8 (fun id -> imm id (Int64.of_int (id + 1)))
  in
  let _, _, first_tail = add_tail 8 0 [ 1; 2; 3; 4; 5; 6; 7 ] in
  let second_definitions =
    List.init 8 (fun index ->
        let id = 15 + index in
        imm id (Int64.of_int (10 + index)))
  in
  let next, final_value, second_tail =
    add_tail 23 15 [ 16; 17; 18; 19; 20; 21; 22 ]
  in
  single
    (first_definitions @ first_tail @ second_definitions @ second_tail
    @ [ return_value next final_value; ret (next + 1) ])

let high_register_predicate_graph () =
  (* RAX through R9 remain live. The first comparison can reuse only R11,
     the second only R10, and the third consumes R11 twice. NOT then reuses
     R10. Each Boolean must discard all stale upper bits in that register. *)
  single
    [
      imm 0 Int64.min_int;
      imm 1 11L;
      imm 2 13L;
      imm 3 17L;
      imm 4 19L;
      imm 5 23L;
      imm 6 Int64.max_int;
      binary 7 Opcode.Ic_less 0 6;
      binary 8 Opcode.Ic_greater 7 5;
      binary 9 Opcode.Ic_equ_equ 7 7;
      unary 10 Opcode.Ic_not 8;
      binary 11 Opcode.Ic_add 9 10;
      binary 12 Opcode.Ic_add 11 0;
      binary 13 Opcode.Ic_add 12 1;
      binary 14 Opcode.Ic_add 13 2;
      binary 15 Opcode.Ic_add 14 3;
      binary 16 Opcode.Ic_add 15 4;
      return_value 17 16;
      ret 18;
    ]

let predicate_shared_cases () =
  [
    ( "comparison reuses its left input while the right remains live",
      single
        [
          imm 0 Int64.min_int;
          imm 1 7L;
          binary 2 Opcode.Ic_less 0 1;
          binary 3 Opcode.Ic_add 1 2;
          return_value 4 3;
          ret 5;
        ],
      8L );
    ( "comparison reuses its right input without reversing CMP",
      single
        [
          imm 0 Int64.min_int;
          imm 1 7L;
          binary 2 Opcode.Ic_less 0 1;
          binary 3 Opcode.Ic_add 0 2;
          return_value 4 3;
          ret 5;
        ],
      Int64.add Int64.min_int 1L );
    ( "comparison needs a fresh destination while both inputs remain live",
      single
        [
          imm 0 0x1234567800000100L;
          imm 1 0x1234567800000101L;
          binary 2 Opcode.Ic_less 0 1;
          binary 3 Opcode.Ic_sub 1 0;
          binary 4 Opcode.Ic_add 2 3;
          return_value 5 4;
          ret 6;
        ],
      2L );
    ( "fresh comparison destination clears an expired register's upper bits",
      single
        [
          imm 0 Int64.min_int;
          imm 1 20L;
          imm 2 21L;
          unary 3 Opcode.Ic_unary_minus 0;
          binary 4 Opcode.Ic_less 1 2;
          binary 5 Opcode.Ic_add 1 2;
          binary 6 Opcode.Ic_add 4 5;
          return_value 7 6;
          ret 8;
        ],
      42L );
    ( "duplicate comparison inputs die together",
      single
        [
          imm 0 Int64.min_int;
          binary 1 Opcode.Ic_less_equ 0 0;
          return_value 2 1;
          ret 3;
        ],
      1L );
    ( "duplicate comparison inputs retain their later use",
      single
        [
          imm 0 Int64.min_int;
          binary 1 Opcode.Ic_equ_equ 0 0;
          binary 2 Opcode.Ic_sub 1 0;
          return_value 3 2;
          ret 4;
        ],
      Int64.add Int64.min_int 1L );
    ( "logical NOT tests all input bits and preserves a shared input",
      single
        [
          imm 0 0x1234000000000000L;
          unary 1 Opcode.Ic_not 0;
          binary 2 Opcode.Ic_sub 0 1;
          return_value 3 2;
          ret 4;
        ],
      0x1234000000000000L );
    ( "logical NOT preserves a shared zero input",
      single
        [
          imm 0 0L;
          unary 1 Opcode.Ic_not 0;
          binary 2 Opcode.Ic_sub 0 1;
          return_value 3 2;
          ret 4;
        ],
      -1L );
    ( "predicates reuse R11 and R10 at full pressure",
      high_register_predicate_graph (),
      Int64.add Int64.min_int 62L );
  ]

let predicate_pressure_graph ~logical_not count =
  let definitions =
    List.init count (fun id -> imm id (Int64.of_int (id + 1)))
  in
  let predicate =
    if logical_not then unary count Opcode.Ic_not 0
    else binary count Opcode.Ic_less 0 1
  in
  let rec reduce next accumulator = function
    | [] -> [ return_value next accumulator; ret (next + 1) ]
    | operand :: rest ->
        binary next Opcode.Ic_add accumulator operand
        :: reduce (next + 1) next rest
  in
  single
    (definitions
    @ (predicate :: reduce (count + 1) count (List.init count Fun.id)))

let predicate_class_cases () =
  [
    ( "comparison of U64 words returns independent I64",
      single
        [
          imm ~type_:u64 0 Int64.min_int;
          imm ~type_:u64 1 (-1L);
          binary 2 Opcode.Ic_less 0 1;
          return_value 3 2;
          ret 4;
        ],
      Native.I64 );
    ( "COM forwards U64 into comparison despite its I64 declaration",
      single
        [
          imm ~type_:u64 0 Int64.min_int;
          unary 1 Opcode.Ic_com 0;
          imm 2 (-1L);
          binary 3 Opcode.Ic_less 1 2;
          return_value 4 3;
          ret 5;
        ],
      Native.I64 );
    ( "comparison output resets downstream arithmetic to I64",
      single
        [
          imm ~type_:u64 0 Int64.min_int;
          imm 1 0L;
          binary 2 Opcode.Ic_greater 0 1;
          imm 3 (-2L);
          binary 4 Opcode.Ic_add 2 3;
          imm 5 0L;
          binary 6 Opcode.Ic_less 4 5;
          return_value 7 6;
          ret 8;
        ],
      Native.I64 );
    ( "logical NOT retains U64",
      single
        [
          imm ~type_:u64 0 Int64.min_int;
          unary ~type_:u64 1 Opcode.Ic_not 0;
          return_value ~type_:u64 2 1;
          ret 3;
        ],
      Native.U64 );
    ( "logical NOT uses COM's forwarded U64 rather than its declared I64",
      single
        [
          imm ~type_:u64 0 (-1L);
          unary 1 Opcode.Ic_com 0;
          unary ~type_:u64 2 Opcode.Ic_not 1;
          imm 3 (-1L);
          binary ~type_:u64 4 Opcode.Ic_add 2 3;
          return_value ~type_:u64 5 4;
          ret 6;
        ],
      Native.U64 );
    ( "logical NOT of comparison output is I64",
      single
        [
          imm ~type_:u64 0 Int64.min_int;
          imm 1 0L;
          binary 2 Opcode.Ic_less 0 1;
          unary 3 Opcode.Ic_not 2;
          return_value 4 3;
          ret 5;
        ],
      Native.I64 );
  ]

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

let encoder_shift_bytes () =
  (* D3 /4, /5 and /7 are fixed qword CL-count forms. These literals are
     independent of the encoder tables and cover REX.B for every value register. *)
  let cases =
    let open Encoder in
    [
      ("SHL RAX,CL", Shift_cl (Shl, Rax), "48d3e0");
      ("SHL RCX,CL", Shift_cl (Shl, Rcx), "48d3e1");
      ("SHL RDX,CL", Shift_cl (Shl, Rdx), "48d3e2");
      ("SHL R8,CL", Shift_cl (Shl, R8), "49d3e0");
      ("SHL R9,CL", Shift_cl (Shl, R9), "49d3e1");
      ("SHL R10,CL", Shift_cl (Shl, R10), "49d3e2");
      ("SHL R11,CL", Shift_cl (Shl, R11), "49d3e3");
      ("SHR RAX,CL", Shift_cl (Shr, Rax), "48d3e8");
      ("SHR RCX,CL", Shift_cl (Shr, Rcx), "48d3e9");
      ("SHR RDX,CL", Shift_cl (Shr, Rdx), "48d3ea");
      ("SHR R8,CL", Shift_cl (Shr, R8), "49d3e8");
      ("SHR R9,CL", Shift_cl (Shr, R9), "49d3e9");
      ("SHR R10,CL", Shift_cl (Shr, R10), "49d3ea");
      ("SHR R11,CL", Shift_cl (Shr, R11), "49d3eb");
      ("SAR RAX,CL", Shift_cl (Sar, Rax), "48d3f8");
      ("SAR RCX,CL", Shift_cl (Sar, Rcx), "48d3f9");
      ("SAR RDX,CL", Shift_cl (Sar, Rdx), "48d3fa");
      ("SAR R8,CL", Shift_cl (Sar, R8), "49d3f8");
      ("SAR R9,CL", Shift_cl (Sar, R9), "49d3f9");
      ("SAR R10,CL", Shift_cl (Sar, R10), "49d3fa");
      ("SAR R11,CL", Shift_cl (Sar, R11), "49d3fb");
    ]
  in
  List.iter
    (fun (label, instruction, expected) ->
      Alcotest.(check string) label expected (hex (Encoder.encode instruction));
      Alcotest.(check int)
        (label ^ " exact byte count")
        3 (Encoder.size instruction))
    cases;
  let instructions =
    List.map (fun (_, instruction, _) -> instruction) cases @ [ Encoder.Ret ]
  in
  let expected =
    String.concat "" (List.map (fun (_, _, bytes) -> bytes) cases) ^ "c3"
  in
  let bytes = String.length expected / 2 in
  Alcotest.(check string)
    "all CL shifts fit the exact aggregate byte quota" expected
    (Encoder.encode_all ~max_code_bytes:bytes instructions
    |> require_ok Fun.id |> hex);
  match Encoder.encode_all ~max_code_bytes:(bytes - 1) instructions with
  | Error message ->
      Alcotest.(check bool)
        "shift batch exhaustion has a diagnostic" true (message <> "")
  | Ok _ -> Alcotest.fail "shift batch accepted one byte below its exact quota"

let encoder_divmod_status_bytes () =
  (* Literal encodings pin the private status ABI and the only dangerous x86
     arithmetic forms. No generated opcode table or native execution is used to
     produce these expectations. *)
  let cases =
    let open Encoder in
    [
      ("capture Windows status pointer", Capture_status Windows_x64, "4989cb");
      ("capture System V status pointer", Capture_status System_v_x64, "4989fb");
      ("zero RDX", Zero_edx, "33d2");
      ("sign extend RAX", Cqo, "4899");
      ("unsigned DIV RCX", Div_rcx, "48f7f1");
      ("signed IDIV RCX", Idiv_rcx, "48f7f9");
      ("compare RAX with zero", Cmp_imm8 (Rax, 0), "4883f800");
      ("compare RCX with minus one", Cmp_imm8 (Rcx, -1), "4883f9ff");
      ("compare R11 with one", Cmp_imm8 (R11, 1), "4983fb01");
      ("jump rel32 zero", Jump 0L, "e900000000");
      ("JE rel32 minus one", Jump_equal (-1L), "0f84ffffffff");
      ("JNE rel32 one", Jump_not_equal 1L, "0f8501000000");
      ("JB rel32 zero", Jump_below 0L, "0f8200000000");
      ("JB rel32 minimum", Jump_below (-0x80000000L), "0f8200000080");
      ("JB rel32 maximum", Jump_below 0x7fffffffL, "0f82ffffff7f");
      ("store zero-divide kind", Store_status_kind 1, "49c7430001000000");
      ("store overflow kind", Store_status_kind 2, "49c7430002000000");
      ("store first site", Store_status_site 1, "49c7430801000000");
      ("store last site", Store_status_site 100000, "49c74308a0860100");
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
  Alcotest.(check string)
    "division/status instructions fit their exact aggregate quota" expected
    (Encoder.encode_all ~max_code_bytes:bytes instructions
    |> require_ok Fun.id |> hex);
  (match Encoder.encode_all ~max_code_bytes:(bytes - 1) instructions with
  | Error message ->
      Alcotest.(check bool)
        "division/status batch exhaustion has a diagnostic" true (message <> "")
  | Ok _ -> Alcotest.fail "division/status batch accepted one byte below quota");
  let invalid label instruction =
    match
      try Some (Encoder.size instruction) with Invalid_argument _ -> None
    with
    | None -> ()
    | Some _ -> Alcotest.fail (label ^ " unexpectedly encoded")
  in
  let open Encoder in
  invalid "CMP imm8 below signed byte" (Cmp_imm8 (Rax, -129));
  invalid "CMP imm8 above signed byte" (Cmp_imm8 (Rax, 128));
  invalid "jump below rel32" (Jump (-0x80000001L));
  invalid "jump above rel32" (Jump 0x80000000L);
  invalid "JB below rel32" (Jump_below (-0x80000001L));
  invalid "JB above rel32" (Jump_below 0x80000000L);
  invalid "status kind zero" (Store_status_kind 0);
  invalid "status kind three" (Store_status_kind 3);
  invalid "status site zero" (Store_status_site 0);
  invalid "status site above hard IR bound" (Store_status_site 100001)

let encoder_predicate_bytes () =
  (* Opcode bytes and ModRM fields are literal expectations from pinned
     OpCodes.DD:376,461,893,981-994, independent of the generated tables. *)
  let cases =
    let open Encoder in
    let full_width =
      [
        ("CMP RAX,RCX", Cmp (Rax, Rcx), "4839c8");
        ("CMP RCX,RAX", Cmp (Rcx, Rax), "4839c1");
        ("CMP R8,RAX", Cmp (R8, Rax), "4939c0");
        ("CMP RAX,R8", Cmp (Rax, R8), "4c39c0");
        ("CMP R10,R11", Cmp (R10, R11), "4d39da");
        ("CMP R11,R10", Cmp (R11, R10), "4d39d3");
        ("CMP R11,R11", Cmp (R11, R11), "4d39db");
        ("TEST RAX", Test Rax, "4885c0");
        ("TEST RCX", Test Rcx, "4885c9");
        ("TEST RDX", Test Rdx, "4885d2");
        ("TEST R8", Test R8, "4d85c0");
        ("TEST R9", Test R9, "4d85c9");
        ("TEST R10", Test R10, "4d85d2");
        ("TEST R11", Test R11, "4d85db");
        ("MOVZX RAX,AL", Movzx8 (Rax, Rax), "480fb6c0");
        ("MOVZX RCX,CL", Movzx8 (Rcx, Rcx), "480fb6c9");
        ("MOVZX RDX,DL", Movzx8 (Rdx, Rdx), "480fb6d2");
        ("MOVZX R8,R8B", Movzx8 (R8, R8), "4d0fb6c0");
        ("MOVZX R9,R9B", Movzx8 (R9, R9), "4d0fb6c9");
        ("MOVZX R10,R10B", Movzx8 (R10, R10), "4d0fb6d2");
        ("MOVZX R11,R11B", Movzx8 (R11, R11), "4d0fb6db");
        ("MOVZX R8,AL", Movzx8 (R8, Rax), "4c0fb6c0");
        ("MOVZX RAX,R8B", Movzx8 (Rax, R8), "490fb6c0");
        ("MOVZX R10,R11B", Movzx8 (R10, R11), "4d0fb6d3");
        ("MOVZX R11,R10B", Movzx8 (R11, R10), "4d0fb6da");
      ]
    in
    let conditions =
      [
        (E, "E", "94");
        (NE, "NE", "95");
        (L, "L", "9c");
        (GE, "GE", "9d");
        (G, "G", "9f");
        (LE, "LE", "9e");
        (B, "B", "92");
        (AE, "AE", "93");
        (A, "A", "97");
        (BE, "BE", "96");
      ]
    in
    let destinations =
      [
        (Rax, "AL", "", "c0");
        (Rcx, "CL", "", "c1");
        (Rdx, "DL", "", "c2");
        (R8, "R8B", "41", "c0");
        (R9, "R9B", "41", "c1");
        (R10, "R10B", "41", "c2");
        (R11, "R11B", "41", "c3");
      ]
    in
    full_width
    @ List.concat_map
        (fun (condition, name, opcode) ->
          List.map
            (fun (destination, register, prefix, modrm) ->
              ( "SET" ^ name ^ " " ^ register,
                Setcc (condition, destination),
                prefix ^ "0f" ^ opcode ^ modrm ))
            destinations)
        conditions
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
    "mixed-width predicate instructions fit their exact bound" expected
    (hex encoded);
  Alcotest.(check int)
    "decoder retains all predicate instruction boundaries"
    (List.length instructions)
    (List.length (decoded_mnemonics encoded));
  match Encoder.encode_all ~max_code_bytes:(bytes - 1) instructions with
  | Error message ->
      Alcotest.(check bool)
        "predicate batch exhaustion has a diagnostic" true (message <> "")
  | Ok _ -> Alcotest.fail "predicate batch accepted an insufficient byte quota"

let encoder_stack_bytes () =
  let slot0 = Encoder.stack_slot ~offset:0 |> require_ok Fun.id in
  let slot8 = Encoder.stack_slot ~offset:8 |> require_ok Fun.id in
  let slot4080 = Encoder.stack_slot ~offset:4080 |> require_ok Fun.id in
  let frame8 = Encoder.stack_frame ~bytes:8 |> require_ok Fun.id in
  let frame4088 = Encoder.stack_frame ~bytes:4088 |> require_ok Fun.id in
  let cases =
    let open Encoder in
    [
      ("load RAX from slot zero", Load_stack (Rax, slot0), "488b842400000000");
      ("store RAX to slot zero", Store_stack (slot0, Rax), "4889842400000000");
      ("load RDX from slot eight", Load_stack (Rdx, slot8), "488b942408000000");
      ("store RDX to slot eight", Store_stack (slot8, Rdx), "4889942408000000");
      ("load R11 from last slot", Load_stack (R11, slot4080), "4c8b9c24f00f0000");
      ("store R11 to last slot", Store_stack (slot4080, R11), "4c899c24f00f0000");
      ("allocate eight-byte frame", Alloc_stack frame8, "4881ec08000000");
      ("free eight-byte frame", Free_stack frame8, "4881c408000000");
      ("allocate maximum frame", Alloc_stack frame4088, "4881ecf80f0000");
      ("free maximum frame", Free_stack frame4088, "4881c4f80f0000");
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
  let byte_count = String.length expected / 2 in
  Alcotest.(check string)
    "stack instructions batch at the exact byte quota" expected
    (Encoder.encode_all ~max_code_bytes:byte_count instructions
    |> require_ok Fun.id |> hex);
  (match Encoder.encode_all ~max_code_bytes:(byte_count - 1) instructions with
  | Error message ->
      Alcotest.(check bool)
        "stack batch exhaustion has a diagnostic" true (message <> "")
  | Ok _ -> Alcotest.fail "stack batch accepted one byte below its exact quota");
  List.iter
    (fun offset ->
      match Encoder.stack_slot ~offset with
      | Error message ->
          Alcotest.(check bool)
            (Printf.sprintf "invalid slot %d has a diagnostic" offset)
            true (message <> "")
      | Ok _ ->
          Alcotest.failf "invalid stack slot offset %d was accepted" offset)
    [ -8; -1; 1; 4088; 4096 ];
  List.iter
    (fun bytes ->
      match Encoder.stack_frame ~bytes with
      | Error message ->
          Alcotest.(check bool)
            (Printf.sprintf "invalid frame %d has a diagnostic" bytes)
            true (message <> "")
      | Ok _ -> Alcotest.failf "invalid stack frame %d was accepted" bytes)
    [ -8; 0; 7; 16; 4080; 4089; 4096 ]

let encoder_narrow_frame_bytes () =
  (* OpCodes.DD's MOV/MOVSX/MOVZX/MOVSXD forms and Asm.HC's REX/ModRM
     placement give these independent bytes. RBP uses mod=10 and disp32;
     byte/word reads extend to 64 bits, while a dword MOV clears upper bits. *)
  let slot offset = Encoder.scalar_frame_slot ~offset |> require_ok Fun.id in
  let cases =
    let open Encoder in
    [
      ( "signed byte local into RAX",
        Load_frame_narrow (Rax, slot (-1), Frame8, Sign_extend),
        "480fbe85ffffffff" );
      ( "unsigned byte local into RDX",
        Load_frame_narrow (Rdx, slot (-1), Frame8, Zero_extend),
        "480fb695ffffffff" );
      ( "signed word local into R8",
        Load_frame_narrow (R8, slot (-2), Frame16, Sign_extend),
        "4c0fbf85feffffff" );
      ( "unaligned unsigned word into R11",
        Load_frame_narrow (R11, slot (-3), Frame16, Zero_extend),
        "4c0fb79dfdffffff" );
      ( "signed dword local into RCX",
        Load_frame_narrow (Rcx, slot (-4), Frame32, Sign_extend),
        "48638dfcffffff" );
      ( "unsigned dword local into R9",
        Load_frame_narrow (R9, slot (-4), Frame32, Zero_extend),
        "448b8dfcffffff" );
      ( "signed byte parameter at RBP+16",
        Load_frame_narrow (Rax, slot 16, Frame8, Sign_extend),
        "480fbe8510000000" );
      ( "unsigned word parameter at RBP+24",
        Load_frame_narrow (R10, slot 24, Frame16, Zero_extend),
        "4c0fb79518000000" );
      ( "store low byte from RCX",
        Store_frame_narrow (slot (-1), Frame8, Rcx),
        "40888dffffffff" );
      ( "store low byte from R10",
        Store_frame_narrow (slot (-1), Frame8, R10),
        "448895ffffffff" );
      ( "store low word from RAX",
        Store_frame_narrow (slot (-2), Frame16, Rax),
        "66408985feffffff" );
      ( "store low word from R11",
        Store_frame_narrow (slot (-2), Frame16, R11),
        "6644899dfeffffff" );
      ( "store low dword from RCX",
        Store_frame_narrow (slot (-4), Frame32, Rcx),
        "40898dfcffffff" );
      ( "store low dword from R8",
        Store_frame_narrow (slot (-4), Frame32, R8),
        "448985fcffffff" );
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
  let instructions = List.map (fun (_, instruction, _) -> instruction) cases in
  let expected =
    String.concat "" (List.map (fun (_, _, bytes) -> bytes) cases)
  in
  let bytes = String.length expected / 2 in
  Alcotest.(check string)
    "mixed scalar widths consume their exact code quota" expected
    (Encoder.encode_all ~max_code_bytes:bytes instructions
    |> require_ok Fun.id |> hex);
  Alcotest.(check bool)
    "mixed scalar widths reject one byte below their code quota" true
    (Encoder.encode_all ~max_code_bytes:(bytes - 1) instructions
    |> Result.is_error);
  List.iter
    (fun instruction ->
      Alcotest.(check bool)
        "unaligned scalar slots do not bypass qword frame validation" true
        (Encoder.encode_all ~max_code_bytes:32 [ instruction ]
        |> Result.is_error))
    [
      Encoder.Load_frame (Encoder.Rax, slot (-1));
      Encoder.Store_frame (slot (-2), Encoder.Rcx);
    ];
  if Sys.int_size > 32 then (
    List.iter
      (fun offset ->
        Alcotest.(check bool)
          "scalar frame offset outside signed disp32 rejects" true
          (Encoder.scalar_frame_slot ~offset:(Int64.to_int offset)
          |> Result.is_error))
      [ -2147483649L; 2147483648L ];
    let edge = slot (Int64.to_int (-2147483648L)) in
    Alcotest.(check string)
      "negative signed disp32 edge retains every displacement bit"
      "4c0fbe9d00000080"
      (Encoder.encode
         (Encoder.Load_frame_narrow
            (Encoder.R11, edge, Encoder.Frame8, Encoder.Sign_extend))
      |> hex))

let encoder_arena_bytes () =
  (* Literal bytes come from the same pinned MOV/MOVSX/MOVZX/MOVSXD forms as the
     frame tests, with R9 fixed as the ModR/M base. REX.B is therefore always
     present; REX.R varies only with the value register. *)
  let slot offset = Encoder.arena_slot ~offset |> require_ok Fun.id in
  let cases =
    let open Encoder in
    [
      ("qword arena load into RAX", Load_arena (Rax, slot 0), "498b8100000000");
      ( "unaligned qword arena store from RCX",
        Store_arena (slot 1, Rcx),
        "49898901000000" );
      ( "extended qword arena load into R8",
        Load_arena (R8, slot 0x12345678),
        "4d8b8178563412" );
      ( "extended qword arena store from R10",
        Store_arena (slot 3, R10),
        "4d899103000000" );
      ( "signed arena byte into RAX",
        Load_arena_narrow (Rax, slot 1, Frame8, Sign_extend),
        "490fbe8101000000" );
      ( "unsigned arena byte into RDX",
        Load_arena_narrow (Rdx, slot 1, Frame8, Zero_extend),
        "490fb69101000000" );
      ( "signed arena word into R8",
        Load_arena_narrow (R8, slot 2, Frame16, Sign_extend),
        "4d0fbf8102000000" );
      ( "unsigned arena word into R11",
        Load_arena_narrow (R11, slot 3, Frame16, Zero_extend),
        "4d0fb79903000000" );
      ( "signed arena dword into RCX",
        Load_arena_narrow (Rcx, slot 4, Frame32, Sign_extend),
        "49638904000000" );
      ( "unsigned arena dword into R8",
        Load_arena_narrow (R8, slot 4, Frame32, Zero_extend),
        "458b8104000000" );
      ( "store arena byte from RCX",
        Store_arena_narrow (slot 1, Frame8, Rcx),
        "41888901000000" );
      ( "store arena byte from R10",
        Store_arena_narrow (slot 1, Frame8, R10),
        "45889101000000" );
      ( "store arena word from RAX",
        Store_arena_narrow (slot 2, Frame16, Rax),
        "6641898102000000" );
      ( "store arena word from R11",
        Store_arena_narrow (slot 2, Frame16, R11),
        "6645899902000000" );
      ( "store arena dword from RCX",
        Store_arena_narrow (slot 4, Frame32, Rcx),
        "41898904000000" );
      ( "store arena dword from R8",
        Store_arena_narrow (slot 4, Frame32, R8),
        "45898104000000" );
      ( "load immutable arena pointer from context",
        Load_context (R9, 72),
        "4d8b4b48" );
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
  let instructions = List.map (fun (_, instruction, _) -> instruction) cases in
  let expected =
    String.concat "" (List.map (fun (_, _, bytes) -> bytes) cases)
  in
  let bytes = String.length expected / 2 in
  Alcotest.(check string)
    "arena forms fit their exact aggregate code quota" expected
    (Encoder.encode_all ~max_code_bytes:bytes instructions
    |> require_ok Fun.id |> hex);
  Alcotest.(check bool)
    "arena forms reject one byte below their code quota" true
    (Encoder.encode_all ~max_code_bytes:(bytes - 1) instructions
    |> Result.is_error);
  Alcotest.(check bool)
    "negative arena displacement rejects" true
    (Encoder.arena_slot ~offset:(-1) |> Result.is_error);
  let invalid_write =
    try
      ignore (Encoder.size (Encoder.Store_context (72, Encoder.Rax)));
      false
    with Invalid_argument _ -> true
  in
  Alcotest.(check bool)
    "arena pointer context word is not writable by generated code" true
    invalid_write;
  if Sys.int_size > 32 then (
    Alcotest.(check bool)
      "arena displacement above signed disp32 rejects" true
      (Encoder.arena_slot ~offset:(Int64.to_int 2147483648L) |> Result.is_error);
    let edge =
      Encoder.arena_slot ~offset:(Int64.to_int 2147483647L) |> require_ok Fun.id
    in
    Alcotest.(check string)
      "maximum arena displacement retains every bit" "490fbe81ffffff7f"
      (Encoder.encode
         (Encoder.Load_arena_narrow
            (Encoder.Rax, edge, Encoder.Frame8, Encoder.Sign_extend))
      |> hex))

let predicate_bytes () =
  let comparisons =
    [
      ("==", "94");
      ("!=", "95");
      ("<", "9c");
      (">=", "9d");
      (">", "9f");
      ("<=", "9e");
    ]
  in
  let signed =
    List.map
      (fun (operator, condition) ->
        ( "1" ^ operator ^ "2;",
          "48b8010000000000000048b902000000000000004839c80f" ^ condition
          ^ "c0480fb6c0c3",
          5,
          6,
          2,
          Native.I64 ))
      comparisons
  in
  let unsigned =
    List.map
      (fun (operator, condition) ->
        ( "0x8000000000000000" ^ operator ^ "0;",
          "48b8000000000000008048b900000000000000004839c80f" ^ condition
          ^ "c0480fb6c0c3",
          5,
          6,
          2,
          Native.I64 ))
      [ ("<", "92"); (">=", "93"); (">", "97"); ("<=", "96") ]
  in
  let cases =
    signed @ unsigned
    @ [
        ( "!0;",
          "48b800000000000000004885c00f94c0480fb6c0c3",
          4,
          5,
          1,
          Native.I64 );
        ( "!0x8000000000000000;",
          "48b800000000000000804885c00f94c0480fb6c0c3",
          4,
          5,
          1,
          Native.U64 );
      ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected, ir, machine, peak, expected_type) ->
          let checked = source_graph ~mode source in
          let compiled = image checked in
          inspect_image checked compiled;
          Alcotest.(check string)
            (source ^ " exact machine code")
            expected
            (hex (Native.code compiled));
          Alcotest.(check int)
            (source ^ " IR count") ir
            (Native.ir_instructions compiled);
          Alcotest.(check int)
            (source ^ " machine count")
            machine
            (Native.machine_instructions compiled);
          Alcotest.(check int)
            (source ^ " register peak")
            peak
            (Native.register_peak compiled);
          Alcotest.(check string)
            (source ^ " declared result")
            (type_name expected_type)
            (type_name (Native.value_type compiled)))
        cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let predicate_source_types () =
  let cases =
    [
      ("(~0x8000000000000000)<-1;", Native.I64, [ "setb" ]);
      ("-1>(~0x8000000000000000);", Native.I64, [ "seta" ]);
      ("((0x8000000000000000>0)-2)<0;", Native.I64, [ "seta"; "setl" ]);
      ("(0x8000000000000000>0)+41;", Native.I64, [ "seta" ]);
      ("!(~0x8000000000000000);", Native.U64, [ "sete" ]);
      ("!!0x8000000000000000;", Native.U64, [ "sete"; "sete" ]);
      ("(!0x8000000000000000)+(-1);", Native.U64, [ "sete" ]);
      ("!(0x8000000000000000<0);", Native.I64, [ "setb"; "sete" ]);
      ("!(0-1);", Native.I64, [ "sete" ]);
      ("#define HIGH 0x8000000000000000\n!(~HIGH);", Native.U64, [ "sete" ]);
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected_type, conditions) ->
          let checked = source_graph ~mode source in
          let compiled = image checked in
          inspect_image checked compiled;
          Alcotest.(check string)
            (source ^ " result class") (type_name expected_type)
            (type_name (Native.value_type compiled));
          Alcotest.(check (list string))
            (source ^ " selected conditions")
            conditions
            (decoded_mnemonics (Native.code compiled)
            |> List.filter (String.starts_with ~prefix:"set")))
        cases)
    [ Preprocessor.Jit; Preprocessor.Aot ];
  List.iter
    (fun (label, checked, expected_type) ->
      let compiled = image checked in
      inspect_image checked compiled;
      Alcotest.(check string)
        label (type_name expected_type)
        (type_name (Native.value_type compiled)))
    (predicate_class_cases ())

let predicate_shared_allocation () =
  List.iter
    (fun (_, checked, _) -> inspect_image checked (image checked))
    (predicate_shared_cases ());
  let checked = high_register_predicate_graph () in
  let compiled = image checked in
  let expected =
    String.concat ""
      [
        "48b80000000000000080";
        "48b90b00000000000000";
        "48ba0d00000000000000";
        "49b81100000000000000";
        "49b91300000000000000";
        "49ba1700000000000000";
        "49bbffffffffffffff7f";
        "4c39d8410f9cc34d0fb6db";
        "4d39d3410f9fc24d0fb6d2";
        "4d39db410f94c34d0fb6db";
        "4d85d2410f94c24d0fb6d2";
        "4d01da";
        "4c01d0";
        "4801c8";
        "4801d0";
        "4c01c0";
        "4c01c8";
        "c3";
      ]
  in
  Alcotest.(check string)
    "R10/R11 predicates read original operands before SETcc and MOVZX" expected
    (hex (Native.code compiled));
  Alcotest.(check int)
    "predicate DAG uses all seven volatile registers" 7
    (Native.register_peak compiled);
  Alcotest.(check int)
    "predicate DAG IR count" 19
    (Native.ir_instructions compiled);
  Alcotest.(check int)
    "predicate DAG machine count" 26
    (Native.machine_instructions compiled);
  Alcotest.(check int)
    "predicate DAG code bytes" 133
    (String.length (Native.code compiled))

let predicate_limits () =
  List.iter
    (fun (label, checked, ir, bytes) ->
      let exact =
        compile ~max_ir_instructions:ir ~max_code_bytes:bytes checked
        |> require_ok native_errors
      in
      Alcotest.(check int)
        (label ^ " exact byte limit")
        bytes
        (String.length (Native.code exact));
      Alcotest.(check int)
        (label ^ " exact IR limit")
        ir
        (Native.ir_instructions exact);
      ignore
        (compile ~max_ir_instructions:(ir - 1) ~max_code_bytes:bytes checked
        |> reject ~code:"HCBACK0001" (label ^ " one fewer IR instruction"));
      ignore
        (compile ~max_ir_instructions:ir ~max_code_bytes:(bytes - 1) checked
        |> reject ~code:"HCBACK0005" (label ^ " one fewer encoded byte"));
      Alcotest.(check string)
        (label ^ " failure leaves compilation reusable")
        (Native.code exact)
        (Native.code (image checked)))
    [
      ("comparison", source_graph "1<2;", 5, 31);
      ("logical NOT", source_graph "!0x8000000000000000;", 4, 21);
      ("extended-register DAG", high_register_predicate_graph (), 19, 133);
    ];
  List.iter
    (fun logical_not ->
      let label = if logical_not then "logical NOT" else "comparison" in
      let checked = predicate_pressure_graph ~logical_not 6 in
      let compiled = image checked in
      inspect_image checked compiled;
      Alcotest.(check int)
        (label ^ " fresh result uses the seventh register")
        7
        (Native.register_peak compiled);
      let errors =
        compile ~max_stack_bytes:0 (predicate_pressure_graph ~logical_not 7)
        |> reject ~code:"HCBACK0004"
             (label ^ " fresh result needs an eighth register")
      in
      Alcotest.(check bool)
        (label ^ " pressure failure is at the predicate")
        true
        (List.exists
           (fun (error : Native.error) -> error.span = Some (fixture_span 7))
           errors))
    [ false; true ]

let predicate_malformed () =
  let comparisons =
    [
      Opcode.Ic_equ_equ;
      Opcode.Ic_not_equ;
      Opcode.Ic_less;
      Opcode.Ic_greater_equ;
      Opcode.Ic_greater;
      Opcode.Ic_less_equ;
    ]
  in
  List.iter
    (fun opcode ->
      let checked =
        single
          [
            imm ~type_:u64 0 Int64.min_int;
            imm 1 0L;
            binary ~type_:u64 2 opcode 0 1;
            return_value ~type_:u64 3 2;
            ret 4;
          ]
      in
      let errors =
        compile checked
        |> reject ~code:"HCBACK0003"
             (Opcode.to_source_name opcode ^ " declares U64")
      in
      Alcotest.(check bool)
        "wrong comparison type identifies the producer" true
        (List.exists
           (fun (error : Native.error) -> error.span = Some (fixture_span 2))
           errors))
    comparisons;
  let malformed =
    [
      ( "NOT on U64 incorrectly declares I64",
        single
          [
            imm ~type_:u64 0 0L;
            unary 1 Opcode.Ic_not 0;
            return_value 2 1;
            ret 3;
          ] );
      ( "NOT on I64 incorrectly declares U64",
        single
          [
            imm 0 0L;
            unary ~type_:u64 1 Opcode.Ic_not 0;
            return_value ~type_:u64 2 1;
            ret 3;
          ] );
      ( "NOT loses COM's forwarded U64",
        single
          [
            imm ~type_:u64 0 (-1L);
            unary 1 Opcode.Ic_com 0;
            unary 2 Opcode.Ic_not 1;
            return_value 3 2;
            ret 4;
          ] );
      ( "comparison input class leaks into later arithmetic",
        single
          [
            imm ~type_:u64 0 Int64.min_int;
            imm 1 0L;
            binary 2 Opcode.Ic_greater 0 1;
            imm 3 41L;
            binary ~type_:u64 4 Opcode.Ic_add 2 3;
            return_value ~type_:u64 5 4;
            ret 6;
          ] );
      ( "NOT of a comparison incorrectly retains its unsigned input class",
        single
          [
            imm ~type_:u64 0 Int64.min_int;
            imm 1 0L;
            binary 2 Opcode.Ic_less 0 1;
            unary ~type_:u64 3 Opcode.Ic_not 2;
            return_value ~type_:u64 4 3;
            ret 5;
          ] );
      ( "unused comparison has an extra payload",
        single
          [
            imm 0 1L;
            imm 1 2L;
            description ~operands:[ 0; 1 ] ~result:2 ~target_type:i64
              ~payload:(Sequence.Integer 7L) 2 Opcode.Ic_less;
            return_value 3 0;
            ret 4;
          ] );
      ( "unused NOT has an extra payload",
        single
          [
            imm 0 1L;
            description ~operands:[ 0 ] ~result:1 ~target_type:i64
              ~payload:(Sequence.Integer 7L) 1 Opcode.Ic_not;
            return_value 2 0;
            ret 3;
          ] );
    ]
  in
  List.iter
    (fun (label, checked) ->
      ignore
        (compile ~max_code_bytes:1 checked |> reject ~code:"HCBACK0003" label))
    malformed

let predicate_rejections () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let checked = source_graph ~mode source in
          let errors = compile checked |> reject ~code:"HCBACK0002" source in
          Alcotest.(check bool)
            (source ^ " retains its source diagnostic")
            true
            (List.exists
               (fun (error : Native.error) -> Option.is_some error.span)
               errors))
        [
          "42(I64);";
          "0x8000000000000000(U64);";
          "(42)(I64i);";
          "(0x8000000000000000)(U64i);";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

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
  Alcotest.(check int)
    "no-pressure source has no stack frame" 0
    (Native.frame_bytes compiled);
  Alcotest.(check string)
    "no-pressure source has no Windows unwind record" ""
    (Native.windows_unwind_info compiled);
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

let shift_source_types_and_bytes () =
  List.iter
    (fun mode ->
      List.iter
        (fun (_, source, expected_type, _, expected_shifts) ->
          let checked = source_graph ~mode source in
          let compiled = source_image ~mode source in
          inspect_image checked compiled;
          Alcotest.(check string)
            (source ^ " result class") (type_name expected_type)
            (type_name (Native.value_type compiled));
          Alcotest.(check (list string))
            (source ^ " shift instruction selection")
            expected_shifts
            (decoded_mnemonics (Native.code compiled)
            |> List.filter (fun mnemonic ->
                List.mem mnemonic [ "shl"; "shr"; "sar" ]));
          Alcotest.(check string)
            (source ^ " driver and graph agree")
            (Native.code (image checked))
            (Native.code compiled))
        shift_source_cases)
    [ Preprocessor.Jit; Preprocessor.Aot ];
  List.iter
    (fun mode ->
      let checked = source_graph ~mode "1<<1;" in
      let compiled = source_image ~mode "1<<1;" in
      Alcotest.(check string)
        "simple shift keeps count in RCX and shifts RAX in place"
        "48b8010000000000000048b9010000000000000048d3e0c3"
        (hex (Native.code compiled));
      Alcotest.(check (list int))
        "simple shift exact IR, machine, register and frame counts"
        [ 5; 4; 2; 0 ]
        [
          Native.ir_instructions compiled;
          Native.machine_instructions compiled;
          Native.register_peak compiled;
          Native.frame_bytes compiled;
        ];
      inspect_image checked compiled)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let shift_shared_allocation () =
  List.iter
    (fun (_, checked, expected_type, _) ->
      let compiled = image checked in
      inspect_image checked compiled;
      Alcotest.(check string)
        "shared shift keeps its declared promoted class"
        (type_name expected_type)
        (type_name (Native.value_type compiled));
      Alcotest.(check int)
        "shared shift needs no spill frame" 0
        (Native.frame_bytes compiled))
    (shift_shared_cases ());
  let left_in_rcx =
    single
      [
        imm 0 1L; imm 1 3L; binary 2 Opcode.Ic_shl 1 0; return_value 3 2; ret 4;
      ]
  in
  let compiled = image left_in_rcx in
  inspect_image left_in_rcx compiled;
  Alcotest.(check string)
    "dying left leaves RCX before CL receives its distinct count"
    "48b8010000000000000048b903000000000000004889ca4889c148d3e24889d0c3"
    (hex (Native.code compiled));
  Alcotest.(check (list string))
    "left-in-RCX shift uses RDX as the non-RCX destination"
    [ "mov-imm64"; "mov-imm64"; "mov"; "mov"; "shl"; "mov"; "ret" ]
    (decoded_mnemonics (Native.code compiled));
  let unrelated_live_rcx =
    single
      [
        imm 0 3L;
        imm 1 40L;
        imm 2 1L;
        binary 3 Opcode.Ic_shl 0 2;
        binary 4 Opcode.Ic_add 1 3;
        return_value 5 4;
        ret 6;
      ]
  in
  let compiled = image unrelated_live_rcx in
  inspect_image unrelated_live_rcx compiled;
  Alcotest.(check int)
    "free extended register preserves unrelated live RCX without spilling" 0
    (Native.frame_bytes compiled);
  Alcotest.(check bool)
    "RCX preservation relocates the unrelated owner before the shift" true
    (match decoded_mnemonics (Native.code compiled) with
    | "mov-imm64" :: "mov-imm64" :: "mov-imm64" :: "mov" :: "mov" :: "shl" :: _
      -> true
    | _ -> false)

let shift_limits_and_pressure () =
  let simple = source_graph "1<<65;" in
  let exact =
    compile ~max_ir_instructions:5 ~max_code_bytes:24 ~max_stack_bytes:0 simple
    |> require_ok native_errors
  in
  Alcotest.(check (list int))
    "simple shift fits exact public resources" [ 5; 24; 0 ]
    [
      Native.ir_instructions exact;
      String.length (Native.code exact);
      Native.frame_bytes exact;
    ];
  ignore
    (compile ~max_ir_instructions:4 ~max_code_bytes:24 ~max_stack_bytes:0 simple
    |> reject ~code:"HCBACK0001" "shift one below exact IR quota");
  ignore
    (compile ~max_ir_instructions:5 ~max_code_bytes:23 ~max_stack_bytes:0 simple
    |> reject ~code:"HCBACK0005" "shift one below exact code quota");
  let no_spill = shift_pressure_graph ~count_in_rcx:true in
  let compiled =
    compile ~max_ir_instructions:15 ~max_code_bytes:89 ~max_stack_bytes:0
      no_spill
    |> require_ok native_errors
  in
  inspect_image no_spill compiled;
  Alcotest.(check (list int))
    "seven live values need no spill when the count already owns RCX"
    [ 7; 0; 15; 14; 89 ]
    [
      Native.register_peak compiled;
      Native.frame_bytes compiled;
      Native.ir_instructions compiled;
      Native.machine_instructions compiled;
      String.length (Native.code compiled);
    ];
  let one_spill = shift_pressure_graph ~count_in_rcx:false in
  let compiled =
    compile ~max_ir_instructions:15 ~max_code_bytes:122 ~max_stack_bytes:8
      one_spill
    |> require_ok native_errors
  in
  inspect_image one_spill compiled;
  Alcotest.(check (list int))
    "seven live values spill exactly the unrelated RCX owner"
    [ 7; 8; 15; 19; 122 ]
    [
      Native.register_peak compiled;
      Native.frame_bytes compiled;
      Native.ir_instructions compiled;
      Native.machine_instructions compiled;
      String.length (Native.code compiled);
    ];
  let mnemonics = decoded_mnemonics (Native.code compiled) in
  Alcotest.(check bool)
    "shift pressure stores the RCX owner" true
    (List.mem "store-stack" mnemonics);
  Alcotest.(check bool)
    "shift pressure reloads the preserved owner" true
    (List.mem "load-stack" mnemonics);
  ignore
    (compile ~max_ir_instructions:14 ~max_code_bytes:122 ~max_stack_bytes:8
       one_spill
    |> reject ~code:"HCBACK0001" "spilled shift one below exact IR quota");
  ignore
    (compile ~max_ir_instructions:15 ~max_code_bytes:121 ~max_stack_bytes:8
       one_spill
    |> reject ~code:"HCBACK0005" "spilled shift one below exact code quota");
  ignore
    (compile ~max_ir_instructions:15 ~max_code_bytes:122 ~max_stack_bytes:7
       one_spill
    |> reject ~code:"HCBACK0004" "spilled shift one below exact stack quota")

let divmod_source_types_and_guards () =
  let default_abi =
    if Sys.os_type = "Win32" then Native.Windows_x64 else Native.System_v_x64
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (_, source, expected_type, _, expected_divides) ->
          let checked = source_graph ~mode source in
          let compiled = source_image ~mode source in
          inspect_image checked compiled;
          Alcotest.(check string)
            (source ^ " result class") (type_name expected_type)
            (type_name (Native.value_type compiled));
          Alcotest.(check bool)
            (source ^ " declares the host private-status ABI")
            true
            (Native.status_abi compiled = Some default_abi);
          let mnemonics = decoded_mnemonics (Native.code compiled) in
          Alcotest.(check (list string))
            (source ^ " selects signed or unsigned hardware division")
            expected_divides
            (List.filter
               (fun mnemonic -> List.mem mnemonic [ "div"; "idiv" ])
               mnemonics);
          Alcotest.(check string)
            (source ^ " driver and graph agree")
            (Native.code (image checked))
            (Native.code compiled);
          let first_divide =
            find_index
              (fun mnemonic -> List.mem mnemonic [ "div"; "idiv" ])
              mnemonics
            |> Option.get
          in
          let first_guard =
            find_index (String.equal "test") mnemonics |> Option.get
          in
          Alcotest.(check bool)
            (source ^ " guards precede the dangerous instruction")
            true
            (first_guard < first_divide))
        divmod_source_cases)
    [ Preprocessor.Jit; Preprocessor.Aot ];
  let signed = source_image ~status_abi:Native.Windows_x64 "84/2;" in
  let signed_mnemonics = decoded_mnemonics (Native.code signed) in
  Alcotest.(check string)
    "signed DIV guards branch around both fault blocks into one epilogue"
    (String.concat ""
       [
         "4989cb";
         "48b85400000000000000";
         "48b90200000000000000";
         "4885c9";
         "0f8427000000";
         "48ba0000000000000080";
         "4839d0";
         "0f850a000000";
         "4883f9ff";
         "0f841f000000";
         "4899";
         "48f7f9";
         "e92a000000";
         "49c7430803000000";
         "49c7430001000000";
         "e915000000";
         "49c7430803000000";
         "49c7430002000000";
         "e900000000";
         "c3";
       ])
    (hex (Native.code signed));
  Alcotest.(check bool)
    "signed division sign-extends only after its guards" true
    (match
       ( find_index (String.equal "cqo") signed_mnemonics,
         find_index (String.equal "idiv") signed_mnemonics )
     with
    | Some cqo, Some divide -> cqo < divide
    | _ -> false);
  let unsigned =
    source_image ~status_abi:Native.Windows_x64 "0xFFFFFFFFFFFFFFFF/3;"
  in
  let unsigned_mnemonics = decoded_mnemonics (Native.code unsigned) in
  Alcotest.(check string)
    "unsigned DIV guards zero then zero-extends RDX before hardware division"
    (String.concat ""
       [
         "4989cb";
         "48b8ffffffffffffffff";
         "48b90300000000000000";
         "4885c9";
         "0f840a000000";
         "33d2";
         "48f7f1";
         "e915000000";
         "49c7430803000000";
         "49c7430001000000";
         "e900000000";
         "c3";
       ])
    (hex (Native.code unsigned));
  Alcotest.(check bool)
    "unsigned division zeroes the high dividend before DIV" true
    (match
       ( find_index (String.equal "zero-edx") unsigned_mnemonics,
         find_index (String.equal "div") unsigned_mnemonics )
     with
    | Some zero, Some divide -> zero < divide
    | _ -> false)

let divmod_shared_allocation () =
  List.iter
    (fun (label, checked, expected_type, _) ->
      let compiled = image checked in
      inspect_image checked compiled;
      Alcotest.(check string)
        (label ^ " result class") (type_name expected_type)
        (type_name (Native.value_type compiled));
      Alcotest.(check int)
        (label ^ " uses free-register transport")
        0
        (Native.frame_bytes compiled))
    (divmod_shared_cases ());
  let cycle =
    single
      [
        imm 0 2L; imm 1 84L; binary 2 Opcode.Ic_div 1 0; return_value 3 2; ret 4;
      ]
  in
  let cycle_image = image ~status_abi:Native.Windows_x64 cycle in
  Alcotest.(check bool)
    "RCX/RAX operand cycle is repaired through RDX before any guard" true
    (match decoded_mnemonics (Native.code cycle_image) with
    | "capture-status-win" :: "mov-imm64" :: "mov-imm64" :: "mov" :: "mov"
      :: "mov" :: "test" :: _ -> true
    | _ -> false);
  Alcotest.(check int)
    "RCX/RAX cycle needs no spill frame" 0
    (Native.frame_bytes cycle_image);
  Alcotest.(check int)
    "RCX/RAX cycle counts all fixed temporaries and the status pointer" 4
    (Native.register_peak cycle_image);
  List.iter
    (fun opcode ->
      let duplicate =
        single [ imm 0 42L; binary 1 opcode 0 0; return_value 2 1; ret 3 ]
      in
      List.iter
        (fun abi ->
          let compiled =
            compile ~status_abi:abi ~max_stack_bytes:0 duplicate
            |> require_ok native_errors
          in
          inspect_image duplicate compiled;
          Alcotest.(check int)
            "duplicate division counts RAX, RCX, RDX and private R11" 4
            (Native.register_peak compiled))
        [ Native.Windows_x64; Native.System_v_x64 ])
    [ Opcode.Ic_div; Opcode.Ic_mod ];
  let pressured = divmod_pressure_graph () in
  let compiled = image pressured in
  inspect_image pressured compiled;
  Alcotest.(check int)
    "reserved R11 turns the seventh live value into one spill slot" 8
    (Native.frame_bytes compiled);
  Alcotest.(check int)
    "fault-capable pressure counts R11 plus six value registers" 7
    (Native.register_peak compiled);
  let mnemonics = decoded_mnemonics (Native.code compiled) in
  Alcotest.(check bool)
    "division pressure stores one live value" true
    (List.mem "store-stack" mnemonics);
  Alcotest.(check bool)
    "division pressure later reloads the spilled value" true
    (List.mem "load-stack" mnemonics)

let divmod_status_metadata () =
  let make ?(left_type = i64) ?(right_type = i64) ?(result_type = i64) opcode
      left right =
    single
      [
        imm ~type_:left_type 0 left;
        imm ~type_:right_type 1 right;
        binary ~type_:result_type 2 opcode 0 1;
        return_value ~type_:result_type 3 2;
        ret 4;
      ]
  in
  let cases =
    [
      ( "divide by zero",
        make Opcode.Ic_div 7L 0L,
        1L,
        Native.Division_by_zero,
        Native.Divide,
        "HCNATIVE0004" );
      ( "remainder by zero",
        make Opcode.Ic_mod 7L 0L,
        1L,
        Native.Division_by_zero,
        Native.Remainder,
        "HCNATIVE0004" );
      ( "signed divide overflow",
        make Opcode.Ic_div Int64.min_int (-1L),
        2L,
        Native.Signed_division_overflow,
        Native.Divide,
        "HCNATIVE0005" );
      ( "signed remainder overflow",
        make Opcode.Ic_mod Int64.min_int (-1L),
        2L,
        Native.Signed_division_overflow,
        Native.Remainder,
        "HCNATIVE0005" );
    ]
  in
  List.iter
    (fun (label, graph, kind_value, expected_kind, expected_operation, code) ->
      List.iter
        (fun abi ->
          let compiled = image ~status_abi:abi graph in
          Alcotest.(check bool)
            (label ^ " retains explicit status ABI")
            true
            (Native.status_abi compiled = Some abi);
          (match Native.decode_runtime_status compiled ~kind:0L ~site:0L with
          | Ok None -> ()
          | Ok (Some _) | Error _ ->
              Alcotest.fail (label ^ " rejected clean 0/0 status"));
          let fault =
            Native.decode_runtime_status compiled ~kind:kind_value ~site:3L
            |> require_ok Fun.id |> Option.get
          in
          Alcotest.(check bool)
            (label ^ " fault kind") true
            (fault.kind = expected_kind);
          Alcotest.(check bool)
            (label ^ " operation") true
            (fault.operation = expected_operation);
          Alcotest.(check int)
            (label ^ " sparse instruction id")
            2 fault.instruction_id;
          Alcotest.(check int) (label ^ " zero-based position") 2 fault.position;
          Alcotest.(check bool)
            (label ^ " original source span")
            true
            (fault.span = Some (fixture_span 2));
          let error = Native.arithmetic_fault_error fault in
          Alcotest.(check string) (label ^ " diagnostic code") code error.code;
          Alcotest.(check bool)
            (label ^ " diagnostic preserves span")
            true
            (error.span = Some (fixture_span 2)))
        [ Native.Windows_x64; Native.System_v_x64 ])
    cases;
  let sparse =
    make Opcode.Ic_div 7L 0L |> descriptions
    |> List.mapi (fun position (item : Sequence.description) ->
        { item with instruction_id = instruction_id (100 + (17 * position)) })
    |> single |> image
  in
  let sparse_fault =
    Native.decode_runtime_status sparse ~kind:1L ~site:3L
    |> require_ok Fun.id |> Option.get
  in
  Alcotest.(check int)
    "runtime status site is the dense position plus one, not the sparse IR id"
    134 sparse_fault.instruction_id;
  Alcotest.(check int)
    "sparse fault still reports zero-based dense position" 2
    sparse_fault.position;
  Alcotest.(check bool)
    "sparse instruction id is not accepted as a status site" true
    (Result.is_error (Native.decode_runtime_status sparse ~kind:1L ~site:134L));
  let unsigned =
    make ~left_type:u64 ~right_type:u64 ~result_type:u64 Opcode.Ic_div
      Int64.min_int (-1L)
    |> image ~status_abi:Native.System_v_x64
  in
  let malformed =
    [
      (0L, 3L);
      (1L, 0L);
      (3L, 3L);
      (-1L, 3L);
      (1L, -1L);
      (1L, 4L);
      (1L, 100001L);
      (2L, 3L);
    ]
  in
  List.iter
    (fun (kind, site) ->
      Alcotest.(check bool)
        (Printf.sprintf "malformed runtime status kind=%Ld site=%Ld rejects"
           kind site)
        true
        (Result.is_error (Native.decode_runtime_status unsigned ~kind ~site)))
    malformed;
  let guarded =
    image ~status_abi:Native.Windows_x64 (divmod_pressure_graph ())
  in
  let expected_code = Native.code guarded in
  let expected_unwind = Native.windows_unwind_info guarded in
  let exported_code = Native.code guarded in
  let exported_unwind = Native.windows_unwind_info guarded in
  Bytes.fill
    (Bytes.unsafe_of_string exported_code)
    0
    (String.length exported_code)
    '\000';
  Bytes.fill
    (Bytes.unsafe_of_string exported_unwind)
    0
    (String.length exported_unwind)
    '\255';
  Alcotest.(check string)
    "fault image code export is caller-immutable" expected_code
    (Native.code guarded);
  Alcotest.(check string)
    "fault image unwind export is caller-immutable" expected_unwind
    (Native.windows_unwind_info guarded);
  let decoded =
    Native.decode_runtime_status guarded ~kind:1L ~site:11L
    |> require_ok Fun.id |> Option.get
  in
  Alcotest.(check int)
    "immutable image still decodes original fault site" 10 decoded.position;
  Alcotest.(check bool)
    "immutable image retains status ABI" true
    (Native.status_abi guarded = Some Native.Windows_x64);
  let plain = returned 42L in
  let baseline = image plain in
  (match Native.decode_runtime_status baseline ~kind:0L ~site:0L with
  | Ok None -> ()
  | Ok (Some _) | Error _ -> Alcotest.fail "plain image rejected clean status");
  Alcotest.(check bool)
    "plain image rejects fabricated arithmetic status" true
    (Result.is_error (Native.decode_runtime_status baseline ~kind:1L ~site:3L));
  List.iter
    (fun abi ->
      let overridden = image ~status_abi:abi plain in
      Alcotest.(check bool)
        "plain image ignores an explicit status ABI" true
        (Native.status_abi overridden = None);
      Alcotest.(check string)
        "plain image bytes remain unchanged" (Native.code baseline)
        (Native.code overridden))
    [ Native.Windows_x64; Native.System_v_x64 ]

let divmod_limits () =
  let simple = source_graph "84/2;" in
  let baseline = image simple in
  let ir = Native.ir_instructions baseline in
  let bytes = String.length (Native.code baseline) in
  Alcotest.(check int) "simple division keeps the five-node IR" 5 ir;
  Alcotest.(check int)
    "simple signed division counts capture, guards, two fault blocks and \
     epilogue"
    20
    (Native.machine_instructions baseline);
  Alcotest.(check int)
    "simple signed division exact guarded image bytes" 114 bytes;
  Alcotest.(check int)
    "simple signed division peak is RAX/RCX/RDX plus private R11" 4
    (Native.register_peak baseline);
  Alcotest.(check int)
    "simple division is register-only" 0
    (Native.frame_bytes baseline);
  let mnemonics = decoded_mnemonics (Native.code baseline) in
  Alcotest.(check bool)
    "simple division includes both fault stores" true
    (List.mem "store-status-kind" mnemonics
    && List.mem "store-status-site" mnemonics);
  let exact =
    compile ~max_ir_instructions:ir ~max_code_bytes:bytes ~max_stack_bytes:0
      simple
    |> require_ok native_errors
  in
  Alcotest.(check string)
    "exact division quotas preserve the image" (Native.code baseline)
    (Native.code exact);
  ignore
    (compile ~max_ir_instructions:(ir - 1) ~max_code_bytes:bytes
       ~max_stack_bytes:0 simple
    |> reject ~code:"HCBACK0001" "division one below exact IR quota");
  ignore
    (compile ~max_ir_instructions:ir ~max_code_bytes:(bytes - 1)
       ~max_stack_bytes:0 simple
    |> reject ~code:"HCBACK0005" "division one below exact code quota");
  let pressured = divmod_pressure_graph () in
  let pressured_baseline = image pressured in
  let pressured_ir = Native.ir_instructions pressured_baseline in
  let pressured_bytes = String.length (Native.code pressured_baseline) in
  Alcotest.(check int)
    "division pressure needs exactly one eight-byte slot" 8
    (Native.frame_bytes pressured_baseline);
  let windows_pressure = image ~status_abi:Native.Windows_x64 pressured in
  let prologue =
    decoded_mnemonics (Native.code windows_pressure)
    |> List.filter (fun mnemonic ->
        mnemonic = "alloc-stack"
        || String.starts_with ~prefix:"capture-status" mnemonic)
  in
  Alcotest.(check bool)
    "Windows spill prologue allocates seven bytes before capturing status" true
    (match prologue with
    | "alloc-stack" :: "capture-status-win" :: _ -> true
    | _ -> false);
  let windows_code = Native.code windows_pressure in
  Alcotest.(check string)
    "Windows status capture starts immediately after the seven-byte SUB \
     prologue"
    "4989cb"
    (hex (String.sub windows_code 7 3));
  let windows_mnemonics = decoded_mnemonics windows_code in
  Alcotest.(check bool)
    "all guarded exits share the frame release and terminal RET" true
    (match List.rev windows_mnemonics with
    | "ret" :: "free-stack" :: _ ->
        List.length (List.filter (String.equal "ret") windows_mnemonics) = 1
    | _ -> false);
  Alcotest.(check string)
    "fault-capable spill keeps the established unwind prologue offset seven"
    "0107010007020000"
    (hex (Native.windows_unwind_info windows_pressure));
  let exact =
    compile ~max_ir_instructions:pressured_ir ~max_code_bytes:pressured_bytes
      ~max_stack_bytes:8 pressured
    |> require_ok native_errors
  in
  Alcotest.(check string)
    "exact guarded spill quotas preserve the image"
    (Native.code pressured_baseline)
    (Native.code exact);
  ignore
    (compile ~max_ir_instructions:(pressured_ir - 1)
       ~max_code_bytes:pressured_bytes ~max_stack_bytes:8 pressured
    |> reject ~code:"HCBACK0001" "guarded spill one below IR quota");
  ignore
    (compile ~max_ir_instructions:pressured_ir
       ~max_code_bytes:(pressured_bytes - 1) ~max_stack_bytes:8 pressured
    |> reject ~code:"HCBACK0005" "guarded spill one below code quota");
  ignore
    (compile ~max_ir_instructions:pressured_ir ~max_code_bytes:pressured_bytes
       ~max_stack_bytes:7 pressured
    |> reject ~code:"HCBACK0004" "guarded spill one below stack quota")

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
    (compile ~max_stack_bytes:0 (pressure_graph 8)
    |> reject ~code:"HCBACK0004" "eighth simultaneously live value");
  Alcotest.(check string)
    "failed allocation leaves subsequent output unchanged"
    (Native.code compiled)
    (Native.code (image checked))

let spill_frames_and_unwind () =
  let cases =
    [
      (7, 0, "");
      (8, 8, "0107010007020000");
      (9, 24, "0107010007220000");
      (10, 24, "0107010007220000");
      (24, 136, "0107020007011100");
    ]
  in
  List.iter
    (fun (live_values, expected_frame, expected_unwind) ->
      let checked = pressure_graph live_values in
      let compiled = image checked in
      inspect_image checked compiled;
      Alcotest.(check int)
        (Printf.sprintf "%d live values frame size" live_values)
        expected_frame
        (Native.frame_bytes compiled);
      Alcotest.(check string)
        (Printf.sprintf "%d live values Windows unwind bytes" live_values)
        expected_unwind
        (hex (Native.windows_unwind_info compiled));
      let mnemonics = decoded_mnemonics (Native.code compiled) in
      if expected_frame = 0 then (
        Alcotest.(check bool)
          "frame-zero image has no stack allocation" false
          (List.mem "alloc-stack" mnemonics);
        Alcotest.(check bool)
          "frame-zero image has no stack release" false
          (List.mem "free-stack" mnemonics))
      else (
        Alcotest.(check string)
          "spilled image allocates its frame before value code" "alloc-stack"
          (List.hd mnemonics);
        match List.rev mnemonics with
        | "ret" :: "free-stack" :: _ -> ()
        | _ ->
            Alcotest.fail
              "spilled image must release its frame immediately before RET"))
    cases;
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected_frame) ->
          let checked = source_graph ~mode source in
          let compiled = source_image ~mode source in
          inspect_image checked compiled;
          Alcotest.(check int)
            (source ^ " source pressure frame")
            expected_frame
            (Native.frame_bytes compiled))
        [ (pressure_source 7, 0); (pressure_source 8, 8) ])
    [ Preprocessor.Jit; Preprocessor.Aot ];
  let public_fixture = "1+(2+(3+(4+(5+(6+(7+14))))));" in
  List.iter
    (fun mode ->
      let checked = source_graph ~mode public_fixture in
      let compiled = source_image ~mode public_fixture in
      inspect_image checked compiled;
      Alcotest.(check int)
        "public spill fixture IR count" 17
        (Native.ir_instructions compiled);
      Alcotest.(check int)
        "public spill fixture frame" 8
        (Native.frame_bytes compiled);
      Alcotest.(check int)
        "public spill fixture machine instructions" 20
        (Native.machine_instructions compiled);
      Alcotest.(check int)
        "public spill fixture code bytes" 132
        (String.length (Native.code compiled)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let spill_limits_and_hard_bound () =
  Alcotest.(check int)
    "public hard stack bound" 4088 Native.hard_max_stack_bytes;
  List.iter
    (fun max_stack_bytes ->
      Native.validate_stack_limit ~max_stack_bytes |> require_ok native_errors)
    [ 0; 1; 7; 8; 4087; 4088 ];
  List.iter
    (fun max_stack_bytes ->
      match Native.validate_stack_limit ~max_stack_bytes with
      | Ok () ->
          Alcotest.failf "invalid stack limit %d was accepted" max_stack_bytes
      | Error errors ->
          Alcotest.(check bool)
            "invalid stack configuration uses HCBACK0001" true
            (List.exists
               (fun (error : Native.error) ->
                 error.code = "HCBACK0001" && error.span = None)
               errors))
    [ -1; 4089; max_int ];
  let checked = pressure_graph 8 in
  let baseline =
    compile ~max_stack_bytes:8 checked |> require_ok native_errors
  in
  let ir = Native.ir_instructions baseline in
  let bytes = String.length (Native.code baseline) in
  Alcotest.(check int)
    "one spill needs the minimum eight-byte frame" 8
    (Native.frame_bytes baseline);
  let exact =
    compile ~max_ir_instructions:ir ~max_code_bytes:bytes ~max_stack_bytes:8
      checked
    |> require_ok native_errors
  in
  Alcotest.(check string)
    "all three exact quotas preserve the spill image" (Native.code baseline)
    (Native.code exact);
  ignore
    (compile ~max_ir_instructions:(ir - 1) ~max_code_bytes:bytes
       ~max_stack_bytes:8 checked
    |> reject ~code:"HCBACK0001" "spill image one below exact IR quota");
  ignore
    (compile ~max_ir_instructions:ir ~max_code_bytes:(bytes - 1)
       ~max_stack_bytes:8 checked
    |> reject ~code:"HCBACK0005" "spill image one below exact code quota");
  ignore
    (compile ~max_ir_instructions:ir ~max_code_bytes:bytes ~max_stack_bytes:7
       checked
    |> reject ~code:"HCBACK0004" "spill image one below exact frame quota");
  ignore
    (compile ~max_stack_bytes:0 checked
    |> reject ~code:"HCBACK0004" "zero stack quota retains no-spill policy");
  let maximum =
    compile ~max_stack_bytes:4088 (pressure_graph 518)
    |> require_ok native_errors
  in
  Alcotest.(check int)
    "511 spill slots use the maximum admitted frame" 4088
    (Native.frame_bytes maximum);
  Alcotest.(check string)
    "maximum frame uses large-allocation unwind encoding" "010702000701ff01"
    (hex (Native.windows_unwind_info maximum));
  ignore
    (compile ~max_stack_bytes:4088 (pressure_graph 519)
    |> reject ~code:"HCBACK0004" "512 spill slots exceed the hard frame bound")

let spill_lifetimes_and_preflight () =
  List.iter
    (fun (label, checked, _, _) ->
      let compiled = image checked in
      inspect_image checked compiled;
      Alcotest.(check bool)
        (label ^ " uses a bounded spill frame")
        true
        (Native.frame_bytes compiled > 0 && Native.frame_bytes compiled <= 4088);
      let mnemonics = decoded_mnemonics (Native.code compiled) in
      Alcotest.(check bool)
        (label ^ " emits at least one spill store")
        true
        (List.mem "store-stack" mnemonics);
      Alcotest.(check bool)
        (label ^ " reloads the spilled value used by the operation")
        true
        (List.mem "load-stack" mnemonics))
    (spill_semantic_cases ());
  let reused = spill_slot_reuse_graph () in
  let reused_image = image reused in
  inspect_image reused reused_image;
  Alcotest.(check int)
    "disjoint pressure phases reuse the same spill slot" 8
    (Native.frame_bytes reused_image);
  let dead_unsupported =
    let definitions = List.init 8 (fun id -> imm id (Int64.of_int (id + 1))) in
    let _, final_value, tail = add_tail 9 0 [ 1; 2; 3; 4; 5; 6; 7 ] in
    single
      (definitions
      @ [ binary 8 Opcode.Ic_power 0 1 ]
      @ tail
      @ [ return_value 16 final_value; ret 17 ])
  in
  let errors =
    compile ~max_code_bytes:1 ~max_stack_bytes:0 dead_unsupported
    |> reject ~code:"HCBACK0002"
         "dead unsupported IR wins before spill planning and byte emission"
  in
  Alcotest.(check bool)
    "dead unsupported node keeps its own source span" true
    (List.exists
       (fun (error : Native.error) -> error.span = Some (fixture_span 8))
       errors)

let immutable_spill_metadata () =
  let compiled = image (pressure_graph 8) in
  let code = Native.code compiled in
  let unwind = Native.windows_unwind_info compiled in
  let expected_code = Bytes.of_string code |> Bytes.to_string in
  let expected_unwind = Bytes.of_string unwind |> Bytes.to_string in
  Alcotest.(check int)
    "immutable spill fixture frame" 8
    (Native.frame_bytes compiled);
  Bytes.fill (Bytes.unsafe_of_string code) 0 (String.length code) '\000';
  Bytes.fill (Bytes.unsafe_of_string unwind) 0 (String.length unwind) '\000';
  Alcotest.(check string)
    "spill code getter returns a fresh copy" expected_code
    (Native.code compiled);
  Alcotest.(check string)
    "unwind getter returns a fresh copy" expected_unwind
    (Native.windows_unwind_info compiled);
  let second = Native.windows_unwind_info compiled in
  Bytes.set (Bytes.unsafe_of_string second) 0 '\255';
  Alcotest.(check string)
    "unwind metadata has no caller-mutable alias" expected_unwind
    (Native.windows_unwind_info compiled)

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
    [ "2`3;"; "0&&(2`3);"; "1||(2`3);"; "1.0;" ];
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
  let dead_unsupported =
    single
      [
        imm 0 1L;
        imm 1 2L;
        binary 2 Opcode.Ic_power 0 1;
        imm 3 42L;
        return_value 4 3;
        ret 5;
      ]
  in
  let errors =
    compile ~max_code_bytes:1 dead_unsupported
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
    (compile ~max_ir_instructions:0 dead_unsupported
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

let shift_malformed () =
  let error_at label code position checked =
    let errors = compile ~max_code_bytes:1 checked |> reject ~code label in
    Alcotest.(check bool)
      (label ^ " precedes byte planning at its own source")
      true
      (List.exists
         (fun (error : Native.error) ->
           error.span = Some (fixture_span position))
         errors)
  in
  List.iter
    (fun opcode ->
      let checked =
        single
          [
            imm 0 Int64.min_int;
            imm 1 1L;
            binary 2 opcode 0 1;
            return_value 3 2;
            ret 4;
          ]
      in
      error_at
        (Opcode.to_source_name opcode ^ " rejects nonzero flags")
        "HCBACK0002" 2
        (replace_instruction 2 (fun d -> { d with flags = 0x200L }) checked);
      error_at
        (Opcode.to_source_name opcode ^ " rejects an arithmetic payload")
        "HCBACK0003" 2
        (replace_instruction 2
           (fun d -> { d with payload = Some (Sequence.Integer 1L) })
           checked);
      error_at
        (Opcode.to_source_name opcode ^ " cannot promote I64 operands to U64")
        "HCBACK0003" 2
        (replace_instruction 2
           (fun d -> { d with target_type = Some u64 })
           checked);
      error_at
        (Opcode.to_source_name opcode ^ " mixed U64 count must promote result")
        "HCBACK0003" 2
        (single
           [
             imm 0 Int64.min_int;
             imm ~type_:u64 1 1L;
             binary 2 opcode 0 1;
             return_value 3 2;
             ret 4;
           ]))
    [ Opcode.Ic_shl; Opcode.Ic_shr ];
  List.iter
    (fun (label, type_) ->
      List.iter
        (fun opcode ->
          error_at
            (Opcode.to_source_name opcode ^ " rejects " ^ label)
            "HCBACK0002" 2
            (single
               [
                 imm 0 1L;
                 imm 1 1L;
                 binary ~type_ 2 opcode 0 1;
                 return_value ~type_ 3 2;
                 ret 4;
               ]))
        [ Opcode.Ic_shl; Opcode.Ic_shr ])
    [
      ("public I64", primitive ~form:Type.Public_spelling Primitive_type.I64);
      ("internal I8", primitive Primitive_type.I8);
      ("I64 pointer", primitive ~pointer_depth:1 Primitive_type.I64);
    ];
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let checked = source_graph ~mode source in
          let errors = compile checked |> reject ~code:"HCBACK0002" source in
          Alcotest.(check bool)
            (source ^ " rejects F64 at its original source span")
            true
            (List.exists
               (fun (error : Native.error) -> Option.is_some error.span)
               errors))
        [ "1.0<<2.0;"; "1.0>>2.0;" ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let divmod_malformed () =
  let error_at label code position checked =
    let errors = compile ~max_code_bytes:1 checked |> reject ~code label in
    Alcotest.(check bool)
      (label ^ " precedes byte planning at its own source")
      true
      (List.exists
         (fun (error : Native.error) ->
           error.span = Some (fixture_span position))
         errors)
  in
  List.iter
    (fun opcode ->
      let checked =
        single
          [ imm 0 84L; imm 1 2L; binary 2 opcode 0 1; return_value 3 2; ret 4 ]
      in
      error_at
        (Opcode.to_source_name opcode ^ " rejects nonzero flags")
        "HCBACK0002" 2
        (replace_instruction 2 (fun d -> { d with flags = 0x200L }) checked);
      error_at
        (Opcode.to_source_name opcode ^ " rejects an arithmetic payload")
        "HCBACK0003" 2
        (replace_instruction 2
           (fun d -> { d with payload = Some (Sequence.Integer 1L) })
           checked);
      error_at
        (Opcode.to_source_name opcode ^ " cannot promote I64 operands to U64")
        "HCBACK0003" 2
        (replace_instruction 2
           (fun d -> { d with target_type = Some u64 })
           checked);
      error_at
        (Opcode.to_source_name opcode ^ " mixed U64 divisor must promote result")
        "HCBACK0003" 2
        (single
           [
             imm 0 84L;
             imm ~type_:u64 1 2L;
             binary 2 opcode 0 1;
             return_value 3 2;
             ret 4;
           ]))
    [ Opcode.Ic_div; Opcode.Ic_mod ];
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let checked = source_graph ~mode source in
          let errors = compile checked |> reject ~code:"HCBACK0002" source in
          Alcotest.(check bool)
            (source ^ " rejects F64 at its original source span")
            true
            (List.exists
               (fun (error : Native.error) -> Option.is_some error.span)
               errors))
        [ "1.0/2.0;"; "1.0%2.0;" ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

(* The two expected truth columns describe nonzero/nonzero and nonzero/zero.
   They are literal truth-table expectations, independent of the emitter. *)
let logical_operations =
  [
    (Opcode.Ic_and_and, "&&", 1L, 0L);
    (Opcode.Ic_or_or, "||", 1L, 1L);
    (Opcode.Ic_xor_xor, "^^", 0L, 1L);
  ]

let high_register_logical_graph opcode =
  (* Five later uses keep RAX through R9 intact. The right input in R10 is
     the first reusable register, so it must be tested before being replaced.
     The next immediate must be able to reuse the scratch register, R11. *)
  single
    [
      imm 0 11L;
      imm 1 13L;
      imm 2 17L;
      imm 3 19L;
      imm 4 23L;
      imm 5 0x0000000100000000L;
      imm 6 0L;
      binary 7 opcode 6 5;
      imm 8 31L;
      binary 9 Opcode.Ic_add 7 8;
      binary 10 Opcode.Ic_add 9 0;
      binary 11 Opcode.Ic_add 10 1;
      binary 12 Opcode.Ic_add 11 2;
      binary 13 Opcode.Ic_add 12 3;
      binary 14 Opcode.Ic_add 13 4;
      return_value 15 14;
      ret 16;
    ]

let consumed_scratch_logical_graph opcode =
  (* RAX has expired, RCX dies in the logical, and RDX remains live. After
     testing RCX into RAX, the second normalization may overwrite RCX. *)
  single
    [
      imm 0 Int64.min_int;
      imm 1 2L;
      imm 2 0x100000000L;
      unary 3 Opcode.Ic_unary_minus 0;
      binary 4 opcode 1 2;
      binary 5 Opcode.Ic_add 2 4;
      return_value 6 5;
      ret 7;
    ]

let logical_shared_cases () =
  let high = 0x0000000100000000L in
  List.concat_map
    (fun (opcode, operator, both_true, one_true) ->
      List.map
        (fun (label, graph, expected) ->
          (operator ^ " " ^ label, graph, expected))
        [
          ( "reuses the left input and preserves the right",
            single
              [
                imm 0 0L;
                imm 1 high;
                binary 2 opcode 0 1;
                binary 3 Opcode.Ic_add 1 2;
                return_value 4 3;
                ret 5;
              ],
            Int64.add high one_true );
          ( "tests a dying right input before overwriting it",
            single
              [
                imm 0 high;
                imm 1 0L;
                binary 2 opcode 0 1;
                binary 3 Opcode.Ic_add 0 2;
                return_value 4 3;
                ret 5;
              ],
            Int64.add high one_true );
          ( "preserves both shared inputs with different nonzero bits",
            single
              [
                imm 0 high;
                imm 1 256L;
                binary 2 opcode 0 1;
                binary 3 Opcode.Ic_sub 1 0;
                binary 4 Opcode.Ic_add 2 3;
                return_value 5 4;
                ret 6;
              ],
            Int64.add (Int64.sub 256L high) both_true );
          ( "clears an expired destination and reuses a dying scratch input",
            single
              [
                imm 0 Int64.min_int;
                imm 1 high;
                imm 2 0L;
                unary 3 Opcode.Ic_unary_minus 0;
                binary 4 opcode 1 2;
                binary 5 Opcode.Ic_add 1 4;
                return_value 6 5;
                ret 7;
              ],
            Int64.add high one_true );
          ( "duplicate inputs die together",
            single [ imm 0 high; binary 1 opcode 0 0; return_value 2 1; ret 3 ],
            both_true );
          ( "scratch overwrites the first consumed input",
            consumed_scratch_logical_graph opcode,
            Int64.add high both_true );
          ( "duplicate inputs retain their original later value",
            single
              [
                imm 0 high;
                binary 1 opcode 0 0;
                binary 2 Opcode.Ic_sub 0 1;
                return_value 3 2;
                ret 4;
              ],
            Int64.sub high both_true );
          ( "unused logical result preserves the returned input",
            single
              [
                imm 0 high;
                imm 1 0L;
                binary 2 opcode 0 1;
                return_value 3 0;
                ret 4;
              ],
            high );
          ( "reuses R10 and releases R11 at full pressure",
            high_register_logical_graph opcode,
            Int64.add 114L one_true );
        ])
    logical_operations

let logical_pressure_graph ?(duplicate = false) opcode count survivors =
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
    (definitions
    @ binary count opcode 0 (if duplicate then 0 else 1)
      :: reduce (count + 1) count survivors)

let word_view_shared_cases () =
  [
    ( "a live signed source and its unsigned view preserve every bit",
      single
        [
          imm 0 Int64.min_int;
          word_view 1 0;
          binary ~type_:u64 2 Opcode.Ic_add 0 1;
          return_value ~type_:u64 3 2;
          ret 4;
        ],
      Native.U64,
      0L );
    ( "signed view changes ordering without changing the unsigned source",
      single
        [
          imm ~type_:u64 0 Int64.min_int;
          word_view ~type_:i64 1 0;
          imm 2 0L;
          binary 3 Opcode.Ic_less 1 2;
          binary 4 Opcode.Ic_less 0 2;
          binary 5 Opcode.Ic_sub 3 4;
          return_value 6 5;
          ret 7;
        ],
      Native.I64,
      1L );
    ( "I64 view resets COM's forwarded U64 before comparison",
      single
        [
          imm ~type_:u64 0 0L;
          unary 1 Opcode.Ic_com 0;
          word_view ~type_:i64 2 1;
          imm 3 0L;
          binary 4 Opcode.Ic_less 2 3;
          return_value 5 4;
          ret 6;
        ],
      Native.I64,
      1L );
    ( "I64 view resets COM's forwarded U64 before NOT and arithmetic",
      single
        [
          imm ~type_:u64 0 0L;
          unary 1 Opcode.Ic_com 0;
          word_view ~type_:i64 2 1;
          unary 3 Opcode.Ic_not 2;
          imm 4 (-1L);
          binary 5 Opcode.Ic_add 3 4;
          return_value 6 5;
          ret 7;
        ],
      Native.I64,
      -1L );
    ( "U64 view determines later NOT and arithmetic classes",
      single
        [
          imm 0 0L;
          word_view 1 0;
          unary ~type_:u64 2 Opcode.Ic_not 1;
          imm 3 (-2L);
          binary ~type_:u64 4 Opcode.Ic_add 2 3;
          return_value ~type_:u64 5 4;
          ret 6;
        ],
      Native.U64,
      -1L );
  ]

let word_view_pressure_graph count =
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
    (definitions
    @ word_view ~type_:i64 count 0
      :: reduce (count + 1) count (List.init count Fun.id))

(* Comparing native with the VM alone cannot detect a shared lowering defect.
   Keep independently stated source results for signedness and grouping. *)
let native_chain_sources =
  [
    ("2==2==2;", 1L);
    ("3<2<1;", 0L);
    ("5>4>3>2;", 1L);
    ("2<=2<=3;", 1L);
    ("3>=3>=2;", 1L);
    ("1!=2!=3;", 1L);
    ("1<2==2<3;", 0L);
    ("(3<2)<1;", 1L);
    ("1<(2<3);", 0L);
    ("1<(2*3)<7;", 1L);
    ("0xFFFFFFFFFFFFFFFF>0>-1;", 0L);
    ("0xFFFFFFFFFFFFFFFF>0==0>-1;", 0L);
    ("0xFFFFFFFFFFFFFFFF>0<-1;", 1L);
    ("(~0x8000000000000000)>0>-1;", 0L);
    ("(~0x8000000000000000)<-1<0;", 0L);
    ("0<(~0x8000000000000000)<-1<0;", 0L);
    ("((~0x8000000000000000)<-1)>0;", 1L);
    ("(2==2==2)+41;", 42L);
    ("0||(2==2==2);", 1L);
    ("(2==2==2)^^(3<2<1);", 1L);
  ]

let logical_source_cases () =
  [
    ("2&&4;", 1L);
    ("2||4;", 1L);
    ("2^^4;", 0L);
    ("0||0x100;", 1L);
    ("0^^0x100000000;", 1L);
    ("0x8000000000000000&&0xFFFFFFFFFFFFFFFF;", 1L);
    ("0x8000000000000000^^0xFFFFFFFFFFFFFFFF;", 0L);
    ("((0x8000000000000000&&1)-2)<0;", 1L);
    ("!(0x8000000000000000&&0);", 1L);
    ("1<2<3;", 1L);
    ("(~0x8000000000000000)>0<-1;", 1L);
    ("0<(~0x8000000000000000)>0;", 1L);
    ("0<1<(~0x8000000000000000)>0>-1;", 0L);
    ("((~0x8000000000000000)>0)>-1;", 1L);
    ("1==(2<3)==1;", 1L);
    ("(1==2<3)==1;", 1L);
  ]
  @ native_chain_sources

let word_view_cases () =
  let bits = Int64.logor Int64.min_int 0x100000001L in
  let types = [ ("I64", i64, Native.I64); ("U64", u64, Native.U64) ] in
  List.concat_map
    (fun (source_name, source_type, _) ->
      List.map
        (fun (target_name, target_type, expected_type) ->
          ( source_name ^ " word view to " ^ target_name,
            single
              [
                imm ~type_:source_type 0 bits;
                word_view ~type_:target_type 1 0;
                return_value ~type_:target_type 2 1;
                ret 3;
              ],
            expected_type,
            bits ))
        types)
    types
  @ word_view_shared_cases ()

let logical_bytes () =
  List.iter
    (fun mode ->
      List.iter
        (fun (operator, opcode) ->
          let source = "2" ^ operator ^ "4;" in
          let checked = source_graph ~mode source in
          let compiled = source_image ~mode source in
          inspect_image checked compiled;
          Alcotest.(check string)
            (source ^ " normalizes both full-width inputs")
            ("48b8020000000000000048b90400000000000000"
           ^ "4885c00f95c0480fb6c04885c90f95c1480fb6c9" ^ "48" ^ opcode ^ "c8c3"
            )
            (hex (Native.code compiled));
          Alcotest.(check int)
            (source ^ " IR count") 5
            (Native.ir_instructions compiled);
          Alcotest.(check int)
            (source ^ " machine count")
            10
            (Native.machine_instructions compiled);
          Alcotest.(check int)
            (source ^ " byte count") 44
            (String.length (Native.code compiled));
          Alcotest.(check int)
            (source ^ " working registers")
            2
            (Native.register_peak compiled);
          Alcotest.(check string)
            (source ^ " result class") "I64"
            (type_name (Native.value_type compiled)))
        [ ("&&", "21"); ("||", "09"); ("^^", "31") ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let logical_source_types_and_chains () =
  let cases =
    [
      ("0x8000000000000000&&1;", Native.I64);
      ("0||0xFFFFFFFFFFFFFFFF;", Native.I64);
      ("0x8000000000000000^^0;", Native.I64);
      ("((~0x8000000000000000)&&1)+41;", Native.I64);
      ("((0x8000000000000000||0)-2)<0;", Native.I64);
      ("!(0x8000000000000000^^0);", Native.I64);
      ("(~(0x8000000000000000&&1))+1;", Native.I64);
      ("1(U64i);", Native.U64);
      ("0xFFFFFFFFFFFFFFFF(I64i)+1;", Native.I64);
      ("0x8000000000000000(I64i)<0;", Native.I64);
      ("!0(U64i);", Native.U64);
      ("0x8000000000000000(I64i)(U64i);", Native.U64);
      ("#define HIGH 0x8000000000000000\nHIGH&&256;", Native.I64);
    ]
    @ List.map
        (fun (source, _) -> (source, Native.I64))
        (logical_source_cases ())
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) ->
          let checked = source_graph ~mode source in
          let compiled = source_image ~mode source in
          inspect_image checked compiled;
          Alcotest.(check string)
            (source ^ " declared result")
            (type_name expected)
            (type_name (Native.value_type compiled));
          Alcotest.(check string)
            (source ^ " driver and graph agree")
            (Native.code (image checked))
            (Native.code compiled))
        cases;
      List.iter
        (fun (source, expected) ->
          Alcotest.(check (list string))
            (source ^ " forwards the unsigned chain class")
            expected
            (source_image ~mode source |> Native.code |> decoded_mnemonics
            |> List.filter (String.starts_with ~prefix:"set")))
        [
          ("(~0x8000000000000000)>0>-1;", [ "seta"; "seta"; "setne"; "setne" ]);
          ("(~0x8000000000000000)<-1<0;", [ "setb"; "setb"; "setne"; "setne" ]);
          ("((~0x8000000000000000)>0)>-1;", [ "seta"; "setg" ]);
          ("((0x8000000000000000&&1)-2)<0;", [ "setne"; "setne"; "setl" ]);
        ];
      let checked = source_graph ~mode "1<(2*3)<7;" in
      let items = descriptions checked in
      let matching opcode =
        List.filter (fun (d : Sequence.description) -> d.opcode = opcode) items
      in
      let products = matching Opcode.Ic_mul in
      Alcotest.(check int)
        "chain evaluates its middle multiplication once" 1
        (List.length products);
      let middle = (Option.get (List.hd products).result).value_id in
      let comparisons = matching Opcode.Ic_less in
      Alcotest.(check int)
        "chain retains two comparisons" 2 (List.length comparisons);
      Alcotest.(check bool)
        "first comparison reads the shared middle" true
        (Sequence.Value_id.equal middle
           (List.nth (List.nth comparisons 0).operands 1));
      Alcotest.(check bool)
        "second comparison reads the same middle" true
        (Sequence.Value_id.equal middle
           (List.hd (List.nth comparisons 1).operands));
      let combinations = matching Opcode.Ic_and_and in
      Alcotest.(check int)
        "chain combines its comparison values once" 1 (List.length combinations);
      Alcotest.(check (list int))
        "chain combines the two Boolean results"
        (List.map
           (fun (d : Sequence.description) ->
             Sequence.Value_id.to_int (Option.get d.result).value_id)
           comparisons)
        (List.map Sequence.Value_id.to_int (List.hd combinations).operands))
    [ Preprocessor.Jit; Preprocessor.Aot ];
  List.iter
    (fun (opcode, operator, _, _) ->
      List.iter
        (fun (left_type, right_type) ->
          let checked =
            single
              [
                imm ~type_:left_type 0 Int64.min_int;
                imm ~type_:right_type 1 (-1L);
                binary 2 opcode 0 1;
                return_value 3 2;
                ret 4;
              ]
          in
          let compiled = image checked in
          inspect_image checked compiled;
          Alcotest.(check string)
            (operator ^ " always declares independent I64")
            "I64"
            (type_name (Native.value_type compiled)))
        [ (i64, i64); (i64, u64); (u64, i64); (u64, u64) ])
    logical_operations

let logical_shared_allocation () =
  List.iter
    (fun (_, checked, _) -> inspect_image checked (image checked))
    (logical_shared_cases ());
  List.iter
    (fun (opcode, suffix) ->
      let checked = high_register_logical_graph opcode in
      let compiled = image checked in
      let expected =
        String.concat ""
          [
            "48b80b00000000000000";
            "48b90d00000000000000";
            "48ba1100000000000000";
            "49b81300000000000000";
            "49b91700000000000000";
            "49ba0000000001000000";
            "49bb0000000000000000";
            "4d85d2410f95c24d0fb6d2";
            "4d85db410f95c34d0fb6db";
            "4d" ^ suffix ^ "da";
            "49bb1f00000000000000";
            "4d01da";
            "4c01d0";
            "4801c8";
            "4801d0";
            "4c01c0";
            "4c01c8";
            "c3";
          ]
      in
      Alcotest.(check string)
        "R10 is tested first and R11 is reusable after the logical" expected
        (hex (Native.code compiled));
      Alcotest.(check int)
        "extended logical peak" 7
        (Native.register_peak compiled);
      Alcotest.(check int)
        "extended logical IR count" 17
        (Native.ir_instructions compiled);
      Alcotest.(check int)
        "extended logical machine count" 22
        (Native.machine_instructions compiled);
      Alcotest.(check int)
        "extended logical byte count" 124
        (String.length (Native.code compiled));
      let duplicate =
        single
          [ imm 0 Int64.min_int; binary 1 opcode 0 0; return_value 2 1; ret 3 ]
      in
      let compiled = image duplicate in
      inspect_image duplicate compiled;
      Alcotest.(check string)
        "duplicate operands still use both working registers"
        ("48b800000000000000804885c00f95c0480fb6c0" ^ "4885c00f95c1480fb6c948"
       ^ suffix ^ "c8c3")
        (hex (Native.code compiled));
      Alcotest.(check int)
        "duplicate scratch contributes to peak" 2
        (Native.register_peak compiled);
      let checked = consumed_scratch_logical_graph opcode in
      let compiled = image checked in
      inspect_image checked compiled;
      let expected =
        "4885c90f95c0480fb6c04885d20f95c1480fb6c948" ^ suffix ^ "c8"
      in
      Alcotest.(check string)
        "scratch overwrites RCX only after its TEST into the expired RAX"
        expected
        (Native.code compiled |> fun code -> hex (String.sub code 33 23));
      Alcotest.(check int)
        "consumed input supplies scratch without a fourth register" 3
        (Native.register_peak compiled))
    [
      (Opcode.Ic_and_and, "21");
      (Opcode.Ic_or_or, "09");
      (Opcode.Ic_xor_xor, "31");
    ]

let word_view_bytes_and_types () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected_type, ir) ->
          let checked = source_graph ~mode source in
          let compiled = source_image ~mode source in
          inspect_image checked compiled;
          Alcotest.(check string)
            (source ^ " preserves all immediate bits")
            "48b80000000000000080c3"
            (hex (Native.code compiled));
          Alcotest.(check int)
            (source ^ " counts zero-byte views as IR")
            ir
            (Native.ir_instructions compiled);
          Alcotest.(check int)
            (source ^ " only loads and returns")
            2
            (Native.machine_instructions compiled);
          Alcotest.(check int)
            (source ^ " reuses its dying input")
            1
            (Native.register_peak compiled);
          Alcotest.(check string)
            (source ^ " declares its target class")
            (type_name expected_type)
            (type_name (Native.value_type compiled)))
        [
          ("0x8000000000000000(I64i);", Native.I64, 4);
          ("0x8000000000000000(U64i);", Native.U64, 4);
          ("0x8000000000000000(I64i)(U64i);", Native.U64, 5);
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ];
  List.iter
    (fun (label, checked, expected_type, _) ->
      let compiled = image checked in
      inspect_image checked compiled;
      Alcotest.(check string)
        label (type_name expected_type)
        (type_name (Native.value_type compiled));
      if List.length (descriptions checked) = 4 then (
        Alcotest.(check string)
          (label ^ " retains the high and low halves without emitting a view")
          "48b80100000001000080c3"
          (hex (Native.code compiled));
        Alcotest.(check (list int))
          (label ^ " IR, machine and register counts")
          [ 4; 2; 1 ]
          [
            Native.ir_instructions compiled;
            Native.machine_instructions compiled;
            Native.register_peak compiled;
          ]))
    (word_view_cases ());
  let _, checked, _, _ = List.hd (word_view_shared_cases ()) in
  let compiled = image checked in
  Alcotest.(check string)
    "a shared view copies the full register"
    "48b800000000000000804889c14801c8c3"
    (hex (Native.code compiled));
  Alcotest.(check int)
    "shared view machine count" 4
    (Native.machine_instructions compiled);
  Alcotest.(check int) "shared view peak" 2 (Native.register_peak compiled)

let logical_and_view_limits () =
  let duplicate =
    single
      [
        imm 0 Int64.min_int;
        binary 1 Opcode.Ic_and_and 0 0;
        return_value 2 1;
        ret 3;
      ]
  in
  let view =
    single
      [ imm 0 Int64.min_int; word_view 1 0; return_value ~type_:u64 2 1; ret 3 ]
  in
  let _, shared_view, _, _ = List.hd (word_view_shared_cases ()) in
  let cases =
    List.map
      (fun (_, operator, _, _) ->
        (operator, source_graph ("2" ^ operator ^ "4;"), 5, 44))
      logical_operations
    @ [
        ("duplicate logical", duplicate, 4, 34);
        ( "extended logical",
          high_register_logical_graph Opcode.Ic_and_and,
          17,
          124 );
        ("zero-byte view", view, 4, 11);
        ("shared view", shared_view, 5, 17);
      ]
  in
  List.iter
    (fun (label, checked, ir, bytes) ->
      let exact =
        compile ~max_ir_instructions:ir ~max_code_bytes:bytes checked
        |> require_ok native_errors
      in
      Alcotest.(check int)
        (label ^ " exact code quota")
        bytes
        (String.length (Native.code exact));
      Alcotest.(check int)
        (label ^ " exact IR quota")
        ir
        (Native.ir_instructions exact);
      ignore
        (compile ~max_ir_instructions:(ir - 1) ~max_code_bytes:bytes checked
        |> reject ~code:"HCBACK0001" (label ^ " one fewer IR instruction"));
      ignore
        (compile ~max_ir_instructions:ir ~max_code_bytes:(bytes - 1) checked
        |> reject ~code:"HCBACK0005" (label ^ " one fewer byte"));
      Alcotest.(check string)
        (label ^ " quota failures leave compilation reusable")
        (Native.code exact)
        (Native.code (image checked)))
    cases

let logical_and_view_pressure () =
  List.iter
    (fun (opcode, operator, _, _) ->
      List.iter
        (fun (duplicate, count, survivors) ->
          let checked =
            logical_pressure_graph ~duplicate opcode count survivors
          in
          let compiled = image checked in
          inspect_image checked compiled;
          Alcotest.(check int)
            (Printf.sprintf
               "%s count=%d duplicate=%b counts both working registers" operator
               count duplicate)
            7
            (Native.register_peak compiled))
        [
          (false, 5, [ 0; 1; 2; 3; 4 ]);
          (false, 6, [ 0; 2; 3; 4; 5 ]);
          (false, 7, [ 2; 3; 4; 5; 6 ]);
          (true, 5, [ 0; 1; 2; 3; 4 ]);
          (true, 6, [ 1; 2; 3; 4; 5 ]);
        ];
      List.iter
        (fun (duplicate, count, survivors) ->
          let label =
            Printf.sprintf "%s count=%d duplicate=%b cannot allocate scratch"
              operator count duplicate
          in
          let errors =
            compile ~max_stack_bytes:0
              (logical_pressure_graph ~duplicate opcode count survivors)
            |> reject ~code:"HCBACK0004" label
          in
          Alcotest.(check bool)
            (label ^ " identifies the logical")
            true
            (List.exists
               (fun (e : Native.error) -> e.span = Some (fixture_span count))
               errors))
        [
          (false, 6, [ 0; 1; 2; 3; 4; 5 ]);
          (false, 7, [ 0; 2; 3; 4; 5; 6 ]);
          (true, 7, [ 1; 2; 3; 4; 5; 6 ]);
          (true, 6, [ 0; 1; 2; 3; 4; 5 ]);
        ])
    logical_operations;
  let checked = word_view_pressure_graph 6 in
  let compiled = image checked in
  inspect_image checked compiled;
  Alcotest.(check int)
    "shared word view occupies the seventh register" 7
    (Native.register_peak compiled);
  let errors =
    compile ~max_stack_bytes:0 (word_view_pressure_graph 7)
    |> reject ~code:"HCBACK0004" "shared word view needs an eighth register"
  in
  Alcotest.(check bool)
    "view pressure identifies the view" true
    (List.exists
       (fun (e : Native.error) -> e.span = Some (fixture_span 7))
       errors)

let preflight_error_at label code position checked =
  let errors = compile ~max_code_bytes:1 checked |> reject ~code label in
  Alcotest.(check bool)
    (label ^ " precedes byte planning at its own source")
    true
    (List.exists
       (fun (e : Native.error) -> e.span = Some (fixture_span position))
       errors)

let nonword_native_types () =
  [
    primitive ~form:Type.Public_spelling Primitive_type.I64;
    primitive ~form:Type.Public_spelling Primitive_type.U64;
    primitive Primitive_type.I8;
    primitive Primitive_type.U8;
    primitive ~pointer_depth:1 Primitive_type.I64;
  ]

let logical_malformed () =
  List.iter
    (fun (opcode, operator, _, _) ->
      let checked =
        single
          [
            imm 0 Int64.min_int;
            imm 1 0L;
            binary 2 opcode 0 1;
            return_value 3 0;
            ret 4;
          ]
      in
      preflight_error_at
        (operator ^ " cannot declare U64")
        "HCBACK0003" 2
        (replace_instruction 2
           (fun d -> { d with target_type = Some u64 })
           checked);
      preflight_error_at
        (operator ^ " unused result cannot have a payload")
        "HCBACK0003" 2
        (replace_instruction 2
           (fun d -> { d with payload = Some (Sequence.Integer 0L) })
           checked);
      preflight_error_at
        (operator ^ " unused result cannot have nonzero flags")
        "HCBACK0002" 2
        (replace_instruction 2 (fun d -> { d with flags = 0x200L }) checked);
      List.iter
        (fun type_ ->
          preflight_error_at
            (operator ^ " requires an internal word target")
            "HCBACK0002" 2
            (replace_instruction 2
               (fun d -> { d with target_type = Some type_ })
               checked))
        (nonword_native_types ());
      preflight_error_at
        (operator ^ " result does not inherit operand U64")
        "HCBACK0003" 4
        (single
           [
             imm ~type_:u64 0 Int64.min_int;
             imm 1 0L;
             binary 2 opcode 0 1;
             imm 3 (-2L);
             binary ~type_:u64 4 Opcode.Ic_add 2 3;
             return_value ~type_:u64 5 4;
             ret 6;
           ]))
    logical_operations;
  preflight_error_at "unused power after logical preparation still rejects"
    "HCBACK0002" 3
    (single
       [
         imm 0 2L;
         imm 1 4L;
         binary 2 Opcode.Ic_and_and 0 1;
         binary 3 Opcode.Ic_power 0 1;
         return_value 4 2;
         ret 5;
       ])

let word_view_malformed () =
  let checked =
    single [ imm 0 Int64.min_int; word_view 1 0; return_value 2 0; ret 3 ]
  in
  List.iter
    (fun payload ->
      preflight_error_at "unused word view requires integer payload zero"
        "HCBACK0003" 1
        (replace_instruction 1 (fun d -> { d with payload }) checked))
    [
      None;
      Some (Sequence.Integer (-1L));
      Some (Sequence.Integer 2L);
      Some (Sequence.Float_bits 0L);
    ];
  preflight_error_at "parenthesized word view remains unsupported" "HCBACK0002"
    1
    (replace_instruction 1
       (fun d -> { d with payload = Some (Sequence.Integer 1L) })
       checked);
  preflight_error_at "word view rejects nonzero flags" "HCBACK0002" 1
    (replace_instruction 1 (fun d -> { d with flags = 0x200L }) checked);
  List.iter
    (fun type_ ->
      preflight_error_at "word view cannot declare a nonword target"
        "HCBACK0002" 1
        (replace_instruction 1
           (fun d -> { d with target_type = Some type_ })
           checked);
      preflight_error_at "word view cannot admit an unsupported input producer"
        "HCBACK0002" 0
        (single
           [
             imm ~type_ 0 1L; word_view ~type_:i64 1 0; return_value 2 1; ret 3;
           ]))
    (nonword_native_types ());
  preflight_error_at "word view must replace the original computation class"
    "HCBACK0003" 3
    (single
       [
         imm ~type_:u64 0 1L;
         word_view ~type_:i64 1 0;
         imm 2 1L;
         binary ~type_:u64 3 Opcode.Ic_add 1 2;
         return_value ~type_:u64 4 3;
         ret 5;
       ]);
  preflight_error_at "return must use the word view's declared target"
    "HCBACK0003" 2
    (single [ imm 0 1L; word_view 1 0; return_value 2 1; ret 3 ])

let logical_source_boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun (contents, code) ->
          let session, config, source = source_inputs ~mode contents in
          match Native_expression.compile session ~config ~source with
          | Ok _ ->
              Alcotest.fail
                ("unsupported native source was accepted: " ^ contents)
          | Error errors ->
              Alcotest.(check bool)
                (contents ^ " expected boundary")
                true
                (List.exists
                   (fun (e : Diagnostic.t) ->
                     e.code = code && e.primary.start >= 0
                     && e.primary.stop >= e.primary.start
                     && e.primary.stop <= String.length contents)
                   errors))
        [
          ("0&&(2`3);", "HCBACK0002");
          ("1||(2`3);", "HCBACK0002");
          ("0^^(2`3);", "HCBACK0002");
          ("1.0&&1;", "HCBACK0002");
          ("1||1.0;", "HCBACK0002");
          ("1.0^^0;", "HCBACK0002");
          ("1==2<3==1;", "HCEVAL0002");
          ("1==2<3<4==1;", "HCEVAL0002");
          ("1!=2>=3!=1;", "HCEVAL0002");
          ("0&&(1==2<3==1);", "HCEVAL0002");
          ("1.0<2<3;", "HCEVAL0002");
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let tests =
  [
    Alcotest.test_case "extended REX and ModRM orientations have exact bytes"
      `Quick encoder_extended_register_bytes;
    Alcotest.test_case "CL shift encoder bytes cover all volatile registers"
      `Quick encoder_shift_bytes;
    Alcotest.test_case "DIV/IDIV guards and private-status encoder bytes" `Quick
      encoder_divmod_status_bytes;
    Alcotest.test_case "narrow RBP memory forms preserve width and extension"
      `Quick encoder_narrow_frame_bytes;
    Alcotest.test_case "private R9 arena forms have exact independent bytes"
      `Quick encoder_arena_bytes;
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
    Alcotest.test_case "shift source classes, opcodes and canonical bytes"
      `Quick shift_source_types_and_bytes;
    Alcotest.test_case "shift RCX aliases and shared values preserve operands"
      `Quick shift_shared_allocation;
    Alcotest.test_case "shift exact budgets and RCX spill pressure" `Quick
      shift_limits_and_pressure;
    Alcotest.test_case "division/remainder source classes and guard ordering"
      `Quick divmod_source_types_and_guards;
    Alcotest.test_case "division/remainder fixed-register aliases and spills"
      `Quick divmod_shared_allocation;
    Alcotest.test_case "division/remainder status metadata and ABI contracts"
      `Quick divmod_status_metadata;
    Alcotest.test_case "guarded division exact IR, code and frame budgets"
      `Quick divmod_limits;
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
    Alcotest.test_case "shift flags, payloads, types and F64 domains reject"
      `Quick shift_malformed;
    Alcotest.test_case "division flags, payloads, types and F64 domains reject"
      `Quick divmod_malformed;
    Alcotest.test_case "predicate encoder condition and byte-register goldens"
      `Quick encoder_predicate_bytes;
    Alcotest.test_case "stack encoder bytes and bounded constructors" `Quick
      encoder_stack_bytes;
    Alcotest.test_case "six source comparisons and NOT emit exact bytes" `Quick
      predicate_bytes;
    Alcotest.test_case "predicate source classes and COM forwarding" `Quick
      predicate_source_types;
    Alcotest.test_case "predicate aliases, duplicates and extended registers"
      `Quick predicate_shared_allocation;
    Alcotest.test_case "predicate exact byte, IR and fresh-register limits"
      `Quick predicate_limits;
    Alcotest.test_case "predicate type and dead-producer preflight failures"
      `Quick predicate_malformed;
    Alcotest.test_case "general and parenthesized casts remain unsupported"
      `Quick predicate_rejections;
    Alcotest.test_case "logical source emits exact full-width truth bytes"
      `Quick logical_bytes;
    Alcotest.test_case "logical source classes and shared comparison chains"
      `Quick logical_source_types_and_chains;
    Alcotest.test_case "logical sharing, duplicate operands and extended bytes"
      `Quick logical_shared_allocation;
    Alcotest.test_case
      "word views preserve bits and replace computation classes" `Quick
      word_view_bytes_and_types;
    Alcotest.test_case "logical and zero-byte view exact resource quotas" `Quick
      logical_and_view_limits;
    Alcotest.test_case "logical scratch and shared-view register boundaries"
      `Quick logical_and_view_pressure;
    Alcotest.test_case "spill frames, source pressure and Windows unwind bytes"
      `Quick spill_frames_and_unwind;
    Alcotest.test_case "spill IR, code, frame and hard-slot limits" `Quick
      spill_limits_and_hard_bound;
    Alcotest.test_case "spill lifetimes, operation shapes and dead preflight"
      `Quick spill_lifetimes_and_preflight;
    Alcotest.test_case "spill code and unwind metadata are caller-immutable"
      `Quick immutable_spill_metadata;
    Alcotest.test_case
      "logical and word-view types, payloads and dead preflight" `Quick
      (fun () ->
        logical_malformed ();
        word_view_malformed ());
    Alcotest.test_case "eager native source and unsupported chain boundaries"
      `Quick logical_source_boundaries;
  ]
