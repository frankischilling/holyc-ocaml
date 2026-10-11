module Operand = Holyc_lib__Asm.Operand
module Instruction = Holyc_lib__Asm.Instruction
module Opcode = Holyc_lib__Asm.Opcode
module Register = Holyc_lib__Asm.Register

let register spelling =
  match Register.find spelling with
  | Some register -> Operand.Register register
  | None -> Alcotest.failf "missing pinned register %s" spelling

let parse ?(mode = Operand.Use64) ?(relative = false) tokens =
  match Operand.parse ~mode ~relative tokens with
  | Ok operand -> operand
  | Error error ->
      Alcotest.failf "operand token %d: %s" error.token_index error.message

let select ?(mode = Operand.Use64) spelling arguments =
  let opcode =
    match Opcode.resolve spelling with
    | Some opcode -> opcode
    | None -> Alcotest.failf "missing pinned opcode %s" spelling
  in
  let relatives =
    Opcode.first_form_relative_arguments (Opcode.resolved_opcode opcode)
  in
  let operands =
    List.mapi
      (fun index tokens ->
        parse ~mode
          ~relative:(Option.value ~default:false (List.nth_opt relatives index))
          tokens)
      arguments
  in
  Instruction.select ~mode ~opcode ~operands

let selected ?mode spelling arguments =
  match select ?mode spelling arguments with
  | Ok selection -> selection
  | Error error -> Alcotest.fail error.message

let check_line label line selection =
  Alcotest.(check int) label line selection.Instruction.form.source_line

let memory ?(width = 8) register_name =
  [
    Operand.Size width;
    Operand.Punctuation '[';
    register register_name;
    Operand.Punctuation ']';
  ]

let immediate_widths () =
  let positive =
    selected "MOV" [ [ register "AL" ]; [ Operand.Integer 65L ] ]
  in
  check_line "unsigned byte form comes first" 268 positive;
  Alcotest.(check int) "byte immediate length" 2 positive.encoded_bytes;
  let negative =
    selected "MOV"
      [ [ register "AL" ]; [ Operand.Operator "-"; Operand.Integer 1L ] ]
  in
  check_line "negative byte uses signed form" 269 negative;
  let wide =
    selected "MOV" [ [ register "RAX" ]; [ Operand.Integer 4294967296L ] ]
  in
  check_line "full unsigned word" 275 wide;
  Alcotest.(check int) "full qword immediate" 8 wide.immediate_bytes;
  let compact =
    selected "MOV" [ [ register "RAX" ]; [ Operand.Integer 42L ] ]
  in
  check_line "original compact word form" 274 compact;
  Alcotest.(check int) "REX.W omitted" 5 compact.encoded_bytes;
  match select "MOV" [ [ register "AL" ]; [ Operand.Integer 256L ] ] with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "out-of-range byte immediate was admitted"

let modes_and_relative_forms () =
  let word =
    selected ~mode:Operand.Use16 "MOV"
      [ [ register "AX" ]; [ Operand.Integer 4660L ] ]
  in
  let override =
    selected ~mode:Operand.Use32 "MOV"
      [ [ register "AX" ]; [ Operand.Integer 4660L ] ]
  in
  check_line "16-bit MOV form" 270 word;
  Alcotest.(check int) "USE16 encoding width" 3 word.encoded_bytes;
  Alcotest.(check int)
    "override excluded from selection cost" 3 override.selection_bytes;
  Alcotest.(check int)
    "override included in emitted width" 4 override.encoded_bytes;
  Alcotest.(check bool) "operand prefix" true override.operand_prefix;
  let call16 =
    selected ~mode:Operand.Use16 "CALL" [ [ Operand.Symbol "@@target" ] ]
  in
  let call64 = selected "CALL" [ [ Operand.Symbol "@@target" ] ] in
  check_line "USE16 relative call" 569 call16;
  check_line "USE64 relative call" 571 call64;
  Alcotest.(check bool)
    "unresolved label remains visible" true call64.unresolved;
  let branch = selected "JZ" [ [ Operand.Symbol "@@target" ] ] in
  check_line "alias preserves original short branch" 606 branch;
  Alcotest.(check string)
    "canonical alias" "JE"
    (Opcode.spelling (Opcode.resolved_opcode branch.opcode));
  Alcotest.(check bool)
    "alias identity" true
    (Opcode.resolved_is_alias branch.opcode);
  match select "PUSH" [ [ register "AX" ] ] with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "original NOT_IN_64_BIT PUSH form was admitted"

let source_masks () =
  let mask names =
    List.fold_left
      (fun value bit -> Int64.logor value (Int64.shift_left 1L bit))
      0L names
  in
  Alcotest.(check int64)
    "RAX categories"
    (mask [ 15; 19; 35 ])
    (Operand.argument_mask (parse [ register "RAX" ]));
  Alcotest.(check int64)
    "ST0 retains STI compatibility"
    (mask [ 46; 47 ])
    (Operand.argument_mask (parse [ register "ST0" ]));
  Alcotest.(check int64)
    "ST3 has STI only" (mask [ 47 ])
    (Operand.argument_mask (parse [ register "ST3" ]));
  Alcotest.(check int64)
    "FS segment categories"
    (mask [ 39; 43 ])
    (Operand.argument_mask (parse [ register "FS" ]))

let address_layout () =
  let address =
    [
      Operand.Size 8;
      register "FS";
      Operand.Punctuation ':';
      Operand.Punctuation '[';
      register "RBX";
      Operand.Operator "+";
      register "RSI";
      Operand.Operator "*";
      Operand.Integer 8L;
      Operand.Operator "+";
      Operand.Integer 16L;
      Operand.Punctuation ']';
    ]
  in
  let operand = parse address in
  Alcotest.(check string)
    "scaled register occupies original reg1" "RSI"
    (Register.spelling (Option.get (Operand.register1 operand)));
  Alcotest.(check string)
    "base occupies original reg2" "RBX"
    (Register.spelling (Option.get (Operand.register2 operand)));
  let instruction = selected "MOV" [ [ register "RAX" ]; address ] in
  check_line "qword load form" 261 instruction;
  Alcotest.(check (option int)) "ModRM" (Some 0x44) instruction.modrm;
  Alcotest.(check (option int)) "SIB" (Some 0xF3) instruction.sib;
  Alcotest.(check (option int))
    "FS prefix register" (Some 4) instruction.segment_prefix;
  Alcotest.(check int) "segment prefix included" 6 instruction.encoded_bytes;
  let rbp = selected "MOV" [ [ register "RAX" ]; memory "RBP" ] in
  Alcotest.(check int) "RBP requires zero displacement" 1 rbp.displacement_bytes;
  let rsp = selected "MOV" [ [ register "RAX" ]; memory "RSP" ] in
  Alcotest.(check (option int)) "RSP synthetic no-index SIB" (Some 0x24) rsp.sib;
  let zero =
    parse
      [
        Operand.Size 8;
        Operand.Integer 0L;
        Operand.Punctuation '[';
        register "RAX";
        Operand.Punctuation ']';
      ]
  in
  Alcotest.(check bool)
    "resolved zero displacement cleared" false
    (Operand.immediate_present zero)

let x87_and_ambiguity () =
  let unsized =
    selected "FLD"
      [ [ Operand.Punctuation '['; register "RAX"; Operand.Punctuation ']' ] ]
  in
  check_line "unsized x87 tie selects first form" 1167 unsized;
  Alcotest.(check bool)
    "unspecified width warning" true
    (List.mem Instruction.Unspecified_size unsized.warnings);
  Alcotest.(check bool)
    "multiple original candidates" true
    (unsized.candidate_count > 1);
  let double = selected "FLD" [ memory "RAX" ] in
  check_line "sized x87 double form" 1168 double;
  Alcotest.(check (option int)) "source x87 omits REX.W" None double.rex;
  let stack = selected "FADDP" [ [ register "ST1" ]; [ register "ST0" ] ] in
  check_line "x87 fixed ST0 and STI form" 1217 stack;
  let lock = selected "LOCK" [] in
  check_line "LOCK is an independent opcode" 864 lock;
  Alcotest.(check int) "LOCK byte count" 1 lock.encoded_bytes

let preserved_source_quirks () =
  let tokens =
    [
      Operand.Size 8;
      Operand.Punctuation '[';
      register "RAX";
      Operand.Operator "+";
      register "RBX";
      Operand.Operator "+";
      register "RCX";
      Operand.Punctuation ']';
    ]
  in
  let operand = parse tokens in
  Alcotest.(check string)
    "third register replaces second" "RCX"
    (Register.spelling (Option.get (Operand.register2 operand)));
  let scaled =
    [
      Operand.Size 8;
      Operand.Punctuation '[';
      register "RAX";
      Operand.Operator "*";
      Operand.Integer 3L;
      Operand.Punctuation ']';
    ]
  in
  let operand = parse scaled in
  Alcotest.(check int64)
    "source retains nonstandard scale" 3L (Operand.scale operand);
  let instruction = selected "MOV" [ [ register "RDX" ]; scaled ] in
  Alcotest.(check (option int))
    "nonstandard scale retains zero SIB scale bits" (Some 5) instruction.sib;
  (match select "REP_OUTSB" [] with
  | Error error ->
      Alcotest.(check bool)
        "empty original form is explicit" true
        (String.length error.message > 0)
  | Ok _ -> Alcotest.fail "empty source opcode form was silently repaired");
  match
    Operand.parse ~mode:Operand.Use64 ~relative:false
      [
        Operand.Integer 8L;
        Operand.Punctuation '[';
        register "RAX";
        Operand.Operator "+";
        Operand.Integer 16L;
        Operand.Punctuation ']';
      ]
  with
  | Error error ->
      Alcotest.(check int) "duplicate immediate token" 4 error.token_index
  | Ok _ -> Alcotest.fail "two original displacement expressions were accepted"

let expression_provenance () =
  let offset =
    parse
      [
        Operand.Size 8;
        Operand.Type_name "CTask";
        Operand.Punctuation '.';
        Operand.Symbol "rip";
        Operand.Punctuation '[';
        register "RAX";
        Operand.Punctuation ']';
      ]
  in
  Alcotest.(check bool)
    "class member offset is deferred" true
    (Operand.unresolved offset);
  let narrow =
    selected "MOV"
      [
        [ register "AL" ];
        [ Operand.Type_name "C"; Operand.Punctuation '.'; Operand.Symbol "x" ];
      ]
  in
  Alcotest.(check bool)
    "known class offset is not forced to absolute address width" true
    narrow.unresolved;
  let sizeof =
    parse
      [
        Operand.Symbol "sizeof";
        Operand.Punctuation '(';
        Operand.Size 8;
        Operand.Punctuation ')';
      ]
  in
  Alcotest.(check bool)
    "query requires original layout" true
    (Operand.unresolved sizeof);
  let arithmetic =
    parse
      [
        Operand.Integer 1L;
        Operand.Operator "+";
        Operand.Integer 2L;
        Operand.Operator "*";
        Operand.Integer 3L;
      ]
  in
  Alcotest.(check (option int64))
    "generated arithmetic precedence" (Some 7L)
    (Option.get (Operand.immediate arithmetic)).value;
  let comparison =
    parse
      [
        Operand.Integer 1L;
        Operand.Operator "==";
        Operand.Integer 2L;
        Operand.Operator "<";
        Operand.Integer 3L;
      ]
  in
  Alcotest.(check bool)
    "pending comparisons do not invent a value" true
    (Operand.unresolved comparison);
  match
    Operand.parse ~mode:Operand.Use64 ~relative:false
      [ Operand.Opaque "unparsed input" ]
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "unclassified input acquired expression authority"

let typed_expression_continuations () =
  let floating = parse [ Operand.Floating (Int64.bits_of_float 1.5) ] in
  Alcotest.(check bool)
    "float keeps typed unresolved value" true
    (Operand.unresolved floating);
  let string = parse [ Operand.String_literal "abc\000" ] in
  Alcotest.(check bool)
    "string requires original misc-data allocation" true
    (Operand.unresolved string);
  let indexed =
    parse
      [
        Operand.Value_symbol "values";
        Operand.Expression_open_bracket;
        Operand.Integer 1L;
        Operand.Expression_close_bracket;
      ]
  in
  Alcotest.(check bool)
    "C index is not an assembly address" false (Operand.indirect indexed);
  Alcotest.(check bool)
    "indexed expression requires later execution" true
    (Operand.unresolved indexed);
  Alcotest.(check bool)
    "global value does not become an absolute export" false
    (Option.get (Operand.immediate indexed)).absolute;
  let byte_value =
    selected "MOV"
      [
        [ register "AL" ];
        [
          Operand.Value_symbol "values";
          Operand.Expression_open_bracket;
          Operand.Integer 1L;
          Operand.Expression_close_bracket;
        ];
      ]
  in
  Alcotest.(check bool)
    "byte global value form is retained for later evaluation" true
    byte_value.unresolved;
  let cast =
    parse
      [
        Operand.Symbol "address";
        Operand.Punctuation '(';
        Operand.Type_name "CTask";
        Operand.Operator "*";
        Operand.Punctuation ')';
        Operand.Expression_open_bracket;
        Operand.Integer 1L;
        Operand.Expression_close_bracket;
      ]
  in
  Alcotest.(check bool)
    "class pointer cast remains unresolved" true (Operand.unresolved cast);
  let within_address =
    parse
      [
        Operand.Punctuation '[';
        register "RAX";
        Operand.Operator "+";
        Operand.Value_symbol "values";
        Operand.Expression_open_bracket;
        Operand.Integer 1L;
        Operand.Expression_close_bracket;
        Operand.Punctuation ']';
      ]
  in
  Alcotest.(check bool)
    "index inside memory displacement retained" true
    (Operand.indirect within_address);
  Alcotest.(check bool)
    "nested displacement stays unresolved" true
    (Operand.unresolved within_address);
  let continuation =
    parse
      [
        Operand.Punctuation '[';
        register "RAX";
        Operand.Operator "+";
        Operand.Integer 2L;
        register "RBX";
        Operand.Punctuation ']';
      ]
  in
  Alcotest.(check string)
    "register resumes after immediate expression" "RBX"
    (Register.spelling (Option.get (Operand.register2 continuation)));
  Alcotest.(check (option int64))
    "one parsed displacement" (Some 2L)
    (Option.get (Operand.immediate continuation)).value;
  (match
     Operand.parse ~mode:Operand.Use64 ~relative:false
       [
         Operand.Punctuation '[';
         register "RAX";
         Operand.Operator "+";
         Operand.Integer 2L;
         Operand.Integer 3L;
         Operand.Punctuation ']';
       ]
   with
  | Error error ->
      Alcotest.(check int)
        "second immediate remains invalid" 4 error.token_index
  | Ok _ -> Alcotest.fail "adjacent displacement expressions were accepted");
  List.iter
    (fun tokens ->
      match Operand.parse ~mode:Operand.Use64 ~relative:false tokens with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "malformed original query grammar was accepted")
    [
      [
        Operand.Symbol "sizeof";
        Operand.Punctuation '(';
        Operand.Integer 1L;
        Operand.Operator "+";
        Operand.Integer 2L;
        Operand.Punctuation ')';
      ];
      [
        Operand.Symbol "offset";
        Operand.Punctuation '(';
        Operand.Type_name "CTask";
        Operand.Punctuation ')';
      ];
      [
        Operand.Symbol "defined";
        Operand.Punctuation '(';
        Operand.Symbol "a";
        Operand.Operator "+";
        Operand.Symbol "b";
        Operand.Punctuation ')';
      ];
    ]

let deferred_runtime_faults () =
  List.iter
    (fun tokens ->
      let operand = parse tokens in
      Alcotest.(check bool)
        "syntax folding does not execute a pending expression" true
        (Operand.unresolved operand))
    [
      [
        Operand.Integer 0L;
        Operand.Operator "&&";
        Operand.Integer 1L;
        Operand.Operator "/";
        Operand.Integer 0L;
      ];
      [
        Operand.Integer 1L;
        Operand.Operator "||";
        Operand.Integer 1L;
        Operand.Operator "/";
        Operand.Integer 0L;
      ];
      [ Operand.Integer 1L; Operand.Operator "/"; Operand.Integer 0L ];
    ]

let full_cast_types () =
  List.iter
    (fun tokens ->
      let operand = parse tokens in
      Alcotest.(check bool)
        "source cast type is retained for expression compilation" true
        (Operand.unresolved operand))
    [
      [
        Operand.Symbol "address";
        Operand.Punctuation '(';
        Operand.Type_name "U8";
        Operand.Punctuation '[';
        Operand.Punctuation ']';
        Operand.Punctuation '[';
        Operand.Integer 4L;
        Operand.Punctuation ']';
        Operand.Punctuation ')';
      ];
      [
        Operand.Symbol "address";
        Operand.Punctuation '(';
        Operand.Type_name "U0";
        Operand.Punctuation '(';
        Operand.Operator "*";
        Operand.Punctuation ')';
        Operand.Punctuation '(';
        Operand.Type_name "I64";
        Operand.Punctuation ')';
        Operand.Punctuation ')';
      ];
      [
        Operand.Symbol "address";
        Operand.Punctuation '(';
        Operand.Type_name "U0";
        Operand.Punctuation '(';
        Operand.Operator "*";
        Operand.Punctuation ')';
        Operand.Punctuation '(';
        Operand.Symbol "reg";
        register "RAX";
        Operand.Type_name "I64";
        Operand.Symbol "x";
        Operand.Operator "=";
        Operand.Integer 1L;
        Operand.Punctuation ';';
        Operand.Type_name "U8";
        Operand.Operator "*";
        Operand.Symbol "s";
        Operand.Punctuation ',';
        Operand.Operator "...";
        Operand.Punctuation ')';
        Operand.Punctuation ')';
      ];
    ];
  List.iter
    (fun tokens ->
      match Operand.parse ~mode:Operand.Use64 ~relative:false tokens with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "invalid source cast declaration was admitted")
    [
      [
        Operand.Symbol "address";
        Operand.Punctuation '(';
        Operand.Type_name "U8";
        Operand.Operator "*";
        Operand.Operator "*";
        Operand.Operator "*";
        Operand.Operator "*";
        Operand.Operator "*";
        Operand.Punctuation ')';
      ];
      [
        Operand.Symbol "address";
        Operand.Punctuation '(';
        Operand.Type_name "U8";
        Operand.Punctuation '[';
        Operand.Integer 2L;
        Operand.Punctuation ']';
        Operand.Punctuation '[';
        Operand.Punctuation ']';
        Operand.Punctuation ')';
      ];
      [
        Operand.Symbol "address";
        Operand.Punctuation '(';
        Operand.Type_name "U0";
        Operand.Punctuation '(';
        Operand.Operator "*";
        Operand.Punctuation ')';
        Operand.Punctuation '(';
        Operand.Type_name "I64";
        Operand.Symbol "x";
        Operand.Punctuation '[';
        Operand.Integer 4L;
        Operand.Punctuation ']';
        Operand.Punctuation ')';
        Operand.Punctuation ')';
      ];
    ]

let unsigned_shift_count () =
  let tokens =
    [
      Operand.Operator "-";
      Operand.Integer 1L;
      Operand.Operator ">>";
      Operand.Integer (Int64.logor Int64.min_int 1L);
    ]
  in
  let operand = parse tokens in
  Alcotest.(check (option int64))
    "either unsigned operand selects logical shift" (Some Int64.max_int)
    (Option.get (Operand.immediate operand)).value;
  let instruction = selected "MOV" [ [ register "RAX" ]; tokens ] in
  check_line "logical shift result requires full qword immediate" 275
    instruction;
  Alcotest.(check int)
    "shift value is not truncated to compact immediate" 8
    instruction.immediate_bytes

let expression_states () =
  List.iter
    (fun tokens ->
      match Operand.parse ~mode:Operand.Use64 ~relative:false tokens with
      | Error _ -> ()
      | Ok _ ->
          Alcotest.fail "known-invalid source expression state was admitted")
    [
      [ Operand.Integer 1L; Operand.Operator "="; Operand.Integer 2L ];
      [ Operand.Integer 1L; Operand.Punctuation '('; Operand.Punctuation ')' ];
      [
        Operand.Integer 1L;
        Operand.Punctuation '(';
        Operand.Integer 2L;
        Operand.Punctuation ')';
      ];
      [
        Operand.Symbol "Label";
        Operand.Punctuation '(';
        Operand.Integer 1L;
        Operand.Punctuation ')';
      ];
      [ Operand.Integer 1L; Operand.Operator "++" ];
      [
        Operand.Punctuation '(';
        Operand.Integer 1L;
        Operand.Punctuation ')';
        Operand.Operator "++";
        Operand.Operator "++";
      ];
      [
        Operand.Function_symbol "F";
        Operand.Punctuation '(';
        Operand.Type_name "U8";
        Operand.Punctuation ')';
      ];
      [
        Operand.Function_symbol "F";
        Operand.Punctuation '(';
        Operand.Integer 1L;
        Operand.Punctuation ')';
        Operand.Operator "=";
        Operand.Integer 2L;
      ];
      [
        Operand.Punctuation '(';
        Operand.Integer 1L;
        Operand.Punctuation ')';
        Operand.Expression_open_bracket;
        Operand.Integer 2L;
        Operand.Expression_close_bracket;
      ];
      [
        Operand.Punctuation '(';
        Operand.Integer 1L;
        Operand.Punctuation ')';
        Operand.Punctuation '.';
        Operand.Symbol "field";
      ];
    ];
  List.iter
    (fun tokens ->
      let operand = parse tokens in
      Alcotest.(check bool)
        "valid source expression state remains deferred" true
        (Operand.unresolved operand))
    [
      [
        Operand.Punctuation '(';
        Operand.Integer 1L;
        Operand.Punctuation ')';
        Operand.Operator "++";
      ];
      [
        Operand.Function_symbol "F";
        Operand.Punctuation '(';
        Operand.Type_name "C";
        Operand.Punctuation '.';
        Operand.Symbol "x";
        Operand.Punctuation ')';
      ];
      [ Operand.String_literal "first"; Operand.String_literal "second" ];
      [
        Operand.Integer 0L;
        Operand.Punctuation '(';
        Operand.Type_name "U0";
        Operand.Punctuation '(';
        Operand.Operator "*";
        Operand.Punctuation ')';
        Operand.Punctuation '(';
        Operand.Punctuation ';';
        Operand.Type_name "I64";
        Operand.Symbol "x";
        Operand.Punctuation ';';
        Operand.Punctuation ';';
        Operand.Punctuation ')';
        Operand.Punctuation ')';
      ];
      [
        Operand.Integer 0L;
        Operand.Punctuation '(';
        Operand.Type_name "U0";
        Operand.Punctuation '(';
        Operand.Operator "*";
        Operand.Punctuation ')';
        Operand.Punctuation '(';
        Operand.Symbol "reg";
        Operand.Operator "...";
        Operand.Punctuation ')';
        Operand.Punctuation ')';
      ];
    ]

let source_function_shapes () =
  let two =
    Operand.Function_call
      { name = "Sum"; parameters = [ false; false ]; variadic = false }
  in
  let optional =
    Operand.Function_call
      { name = "Sum"; parameters = [ false; true ]; variadic = false }
  in
  let varargs =
    Operand.Function_call
      { name = "Log"; parameters = [ false ]; variadic = true }
  in
  List.iter
    (fun tokens ->
      let operand = parse tokens in
      Alcotest.(check bool)
        "source parameter shape controls deferred call" true
        (Operand.unresolved operand))
    [
      [ two; Operand.Integer 1L; Operand.Integer 2L ];
      [ optional; Operand.Integer 1L ];
      [
        optional;
        Operand.Punctuation '(';
        Operand.Integer 1L;
        Operand.Punctuation ')';
      ];
      [
        optional;
        Operand.Punctuation '(';
        Operand.Integer 1L;
        Operand.Punctuation ',';
        Operand.Punctuation ')';
      ];
      [
        varargs;
        Operand.Punctuation '(';
        Operand.Integer 1L;
        Operand.Punctuation ',';
        Operand.Integer 2L;
        Operand.Punctuation ',';
        Operand.Integer 3L;
        Operand.Punctuation ')';
      ];
      [ varargs; Operand.Integer 1L ];
    ];
  List.iter
    (fun tokens ->
      match Operand.parse ~mode:Operand.Use64 ~relative:false tokens with
      | Error _ -> ()
      | Ok _ ->
          Alcotest.fail
            "a call that violates its source parameter shape was admitted")
    [
      [ two; Operand.Integer 1L; Operand.Punctuation ','; Operand.Integer 2L ];
      [
        two;
        Operand.Punctuation '(';
        Operand.Integer 1L;
        Operand.Punctuation ')';
      ];
      [ optional; Operand.Punctuation '('; Operand.Punctuation ')' ];
      [ Operand.Function_symbol "UnknownShape"; Operand.Integer 1L ];
    ]

let inline_types_and_variable_calls () =
  let body =
    [
      Operand.Type_definition { name = "Frame"; token_count = 7 };
      Operand.Symbol "Frame";
      Operand.Punctuation '{';
      Operand.Type_name "I64";
      Operand.Symbol "x";
      Operand.Punctuation ';';
      Operand.Punctuation '}';
    ]
  in
  let four_pointers = List.init 4 (fun _ -> Operand.Operator "*") in
  let definition =
    [ Operand.Integer 0L; Operand.Punctuation '('; Operand.Type_name "U8" ]
    @ four_pointers @ body @ four_pointers
    @ [ Operand.Punctuation ')' ]
  in
  let operand = parse definition in
  Alcotest.(check bool)
    "prevalidated aggregate type stays deferred" true
    (Operand.unresolved operand);
  Alcotest.(check int)
    "type body retains one token per source token" (List.length definition)
    (List.length (Option.get (Operand.immediate operand)).expression);
  let callback_with_inline_parameter suffix =
    [
      Operand.Integer 0L;
      Operand.Punctuation '(';
      Operand.Type_name "U0";
      Operand.Punctuation '(';
      Operand.Operator "*";
      Operand.Punctuation ')';
      Operand.Punctuation '(';
      Operand.Type_name "I64";
    ]
    @ body @ suffix
    @ [ Operand.Punctuation ')'; Operand.Punctuation ')' ]
  in
  List.iter
    (fun suffix ->
      let operand = parse (callback_with_inline_parameter suffix) in
      Alcotest.(check bool)
        "inline definition resets function-parameter mode" true
        (Operand.unresolved operand))
    [
      [ Operand.Punctuation '['; Operand.Integer 2L; Operand.Punctuation ']' ];
      [ Operand.Operator "="; Operand.Integer 0L ];
    ];
  let scalar =
    Operand.Variable_symbol { name = "X"; parameters = None; variadic = false }
  in
  let callback =
    Operand.Variable_symbol
      { name = "Callback"; parameters = Some [ false; true ]; variadic = false }
  in
  let cast =
    [
      scalar;
      Operand.Punctuation '(';
      Operand.Type_name "I64";
      Operand.Punctuation '(';
      Operand.Operator "*";
      Operand.Punctuation ')';
      Operand.Punctuation '(';
      Operand.Type_name "I64";
      Operand.Punctuation ')';
      Operand.Punctuation ')';
    ]
  in
  List.iter
    (fun tokens ->
      let operand = parse tokens in
      Alcotest.(check bool)
        "declared callback or explicit cast retains source grammar" true
        (Operand.unresolved operand))
    [
      [ callback ];
      [
        callback;
        Operand.Punctuation '(';
        Operand.Integer 1L;
        Operand.Punctuation ')';
      ];
      [
        scalar;
        Operand.Punctuation '(';
        Operand.Type_name "I64";
        Operand.Punctuation ')';
      ];
      cast
      @ [ Operand.Punctuation '('; Operand.Integer 1L; Operand.Punctuation ')' ];
    ];
  List.iter
    (fun tokens ->
      match Operand.parse ~mode:Operand.Use64 ~relative:false tokens with
      | Error _ -> ()
      | Ok _ ->
          Alcotest.fail "invalid declared callback or type span was admitted")
    [
      [
        scalar;
        Operand.Punctuation '(';
        Operand.Integer 1L;
        Operand.Punctuation ')';
      ];
      [ callback; Operand.Integer 1L ];
      [ callback; Operand.Punctuation '('; Operand.Punctuation ')' ];
      [
        callback;
        Operand.Punctuation '(';
        Operand.Type_name "I64";
        Operand.Punctuation ')';
      ];
      cast @ [ Operand.Punctuation '('; Operand.Punctuation ')' ];
      [
        Operand.Integer 0L;
        Operand.Punctuation '(';
        Operand.Type_name "U8";
        Operand.Type_definition { name = "Broken"; token_count = 99 };
        Operand.Punctuation ')';
      ];
    ]

let function_address_categories () =
  let address absolute =
    [ Operand.Operator "&"; Operand.Address_symbol { name = "F"; absolute } ]
  in
  let jit = parse (address (Some false)) in
  let aot = parse (address (Some true)) in
  let pending = parse (address None) in
  Alcotest.(check int)
    "JIT function address does not force relocation width" 0 (Operand.size jit);
  Alcotest.(check int)
    "AOT function body forces original absolute width" 4 (Operand.size aot);
  Alcotest.(check int)
    "AOT function body USE16 absolute width" 2
    (Operand.size (parse ~mode:Operand.Use16 (address (Some true))));
  Alcotest.(check bool)
    "JIT address category is known" true
    (Option.get (Operand.immediate jit)).absolute_known;
  Alcotest.(check bool)
    "unresolved export address category stays explicit" false
    (Option.get (Operand.immediate pending)).absolute_known;
  let selected_pending = selected "MOV" [ [ register "AL" ]; address None ] in
  Alcotest.(check bool)
    "unresolved category cannot certify selected form" true
    selected_pending.unresolved;
  let parity =
    parse (address (Some true) @ [ Operand.Operator "-" ] @ address (Some true))
  in
  Alcotest.(check bool)
    "two proven function addresses preserve even source parity" false
    (Option.get (Operand.immediate parity)).absolute;
  let mixed =
    parse (address None @ [ Operand.Operator "+"; Operand.Symbol "Target" ])
  in
  Alcotest.(check bool)
    "unknown parity survives another address atom" false
    (Option.get (Operand.immediate mixed)).absolute_known;
  Alcotest.(check int)
    "unknown total parity cannot prove a mandatory absolute width" 0
    (Operand.size mixed);
  (match select "MOV" [ [ register "AL" ]; address (Some true) ] with
  | Error _ -> ()
  | Ok _ ->
      Alcotest.fail
        "proven AOT absolute address was accepted in a byte-only form");
  match
    Operand.parse ~mode:Operand.Use64 ~relative:false
      (address (Some false)
      @ [ Operand.Punctuation '('; Operand.Integer 1L; Operand.Punctuation ')' ]
      )
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "an address atom was mistaken for a function call"

let tests =
  [
    Alcotest.test_case "source immediate widths" `Quick immediate_widths;
    Alcotest.test_case "mode and relative form rules" `Quick
      modes_and_relative_forms;
    Alcotest.test_case "source argument masks" `Quick source_masks;
    Alcotest.test_case "source address layouts" `Quick address_layout;
    Alcotest.test_case "x87 selection and size ambiguity" `Quick
      x87_and_ambiguity;
    Alcotest.test_case "preserved original form quirks" `Quick
      preserved_source_quirks;
    Alcotest.test_case "deferred expression provenance" `Quick
      expression_provenance;
    Alcotest.test_case "typed expression continuations" `Quick
      typed_expression_continuations;
    Alcotest.test_case "deferred expression faults" `Quick
      deferred_runtime_faults;
    Alcotest.test_case "full source cast type forms" `Quick full_cast_types;
    Alcotest.test_case "unsigned shift count selects qword form" `Quick
      unsigned_shift_count;
    Alcotest.test_case "original expression modifier states" `Quick
      expression_states;
    Alcotest.test_case "source function-call parameter shapes" `Quick
      source_function_shapes;
    Alcotest.test_case "inline cast types and declared callbacks" `Quick
      inline_types_and_variable_calls;
    Alcotest.test_case "source function address categories" `Quick
      function_address_categories;
  ]
