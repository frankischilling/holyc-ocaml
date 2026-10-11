open Holyc_lib
module T = Test_parser
module Operand = Asm_operand

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let instructions items =
  List.filter_map
    (function
      | Ast.Inline_assembly_instruction operation -> Some operation
      | Ast.Inline_assembly_directive _
      | Ast.Inline_assembly_label _
      | Ast.Inline_assembly_empty _ -> None)
    items

let rec statement_items = function
  | Ast.Assembly_block_statement block -> block.assembly_items
  | Ast.Inline_assembly_statement statement -> statement.inline_assembly_items
  | Ast.Block_statement block ->
      List.concat_map statement_items block.block_statements
  | Ast.Sequence_statement sequence ->
      List.concat_map
        (fun element -> statement_items element.Ast.sequence_statement)
        sequence.sequence_elements
  | _ -> []

let block ?compilation_mode ?(source_context = false) body =
  let parse =
    if source_context then T.parse_assembly_source_string else T.parse_string
  in
  let session, source, output =
    parse ?compilation_mode ("asm {" ^ body ^ "\n}")
  in
  let assembly = T.expect_ast output |> T.expect_one_top_level_assembly in
  (session, source, output, assembly)

let direct ?compilation_mode ?(source_context = false) body =
  let parse =
    if source_context then T.parse_assembly_source_string else T.parse_string
  in
  let session, source, output =
    parse ?compilation_mode ("U0 AssemblyFixture(){" ^ body ^ "\n}")
  in
  let definition = T.expect_ast output |> T.expect_one_definition in
  let items = T.expect_function_body definition |> statement_items in
  (session, source, output, items)

let standalone ?mode ?compilation_mode ?(source_context = false) contents =
  let session, config =
    if source_context then T.assembly_source_session ?compilation_mode ()
    else (Session.create (), T.config ?compilation_mode (Sys.getcwd ()))
  in
  let source =
    Session.add_source session ~path:"assembly-operations.ASM" ~contents
  in
  let output =
    Holyc_lib.parse_assembly_detailed ?mode session ~config ~source
  in
  (session, source, output)

let program_instructions ?compilation_mode contents =
  let _, _, output = T.parse_string ?compilation_mode contents in
  let ast = T.expect_ast output in
  List.concat_map
    (function
      | Ast.Top_level_statement statement -> statement_items statement
      | Ast.Function_definition definition ->
          T.expect_function_body definition |> statement_items
      | _ -> [])
    ast.Ast.items
  |> instructions

let expect_assembly output =
  match output.Parser.assembly with
  | Some assembly -> assembly
  | None ->
      Alcotest.fail
        (String.concat "; "
           (List.map
              (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
              output.diagnostics))

let opcode (operation : Ast.inline_assembly_operation) =
  operation.inline_assembly_opcode.assembly_source_token.raw

let operand index (operation : Ast.inline_assembly_operation) =
  (List.nth operation.inline_assembly_operands index).inline_assembly_operand

let instruction_names items = instructions items |> List.map opcode

let signature items =
  instructions items
  |> List.map (fun (operation : Ast.inline_assembly_operation) ->
      let operands =
        operation.inline_assembly_operands
        |> List.map (fun (operand : Ast.inline_assembly_operand) ->
            operand.inline_assembly_operand_tokens
            |> List.map (fun (token : Ast.assembly_token) ->
                token.assembly_source_token.raw)
            |> String.concat " ")
        |> String.concat ", "
      in
      Printf.sprintf "%s %s | %d | %d" (opcode operation) operands
        (Operand.mode_bits operation.inline_assembly_mode)
        operation.inline_assembly_selection.form.source_line)

let first_error diagnostics =
  match
    List.find_opt
      (fun (diagnostic : Diagnostic.t) ->
        diagnostic.severity = Diagnostic.Error)
      diagnostics
  with
  | Some diagnostic -> diagnostic
  | None -> Alcotest.fail "expected an assembler error diagnostic"

let register_name = Option.map Asm_register.spelling

let line_independent_streams () =
  let compact =
    "MOV RAX,U64 8[RBP] MOV RBX,42 LOCK BTS U32 [RCX],0 REP_MOVSB RET1 8"
  in
  let wrapped =
    "MOV\n\
     RAX,\n\
     U64 8[\n\
     RBP\n\
     ]\n\
     MOV\n\
     RBX,\n\
     42\n\
     LOCK\n\
     BTS\n\
     U32 [RCX],\n\
     0\n\
     REP_MOVSB\n\
     RET1\n\
     8"
  in
  List.iter
    (fun compilation_mode ->
      let _, _, _, compact_block = block ~compilation_mode compact in
      let _, _, _, wrapped_block = block ~compilation_mode wrapped in
      let _, _, _, compact_direct = direct ~compilation_mode compact in
      let _, _, _, wrapped_direct = direct ~compilation_mode wrapped in
      let _, _, compact_standalone = standalone ~compilation_mode compact in
      let _, _, wrapped_standalone = standalone ~compilation_mode wrapped in
      let expected = signature compact_block.assembly_items in
      List.iter
        (fun (name, items) ->
          Alcotest.(check (list string)) name expected (signature items))
        [
          ("wrapped block", wrapped_block.assembly_items);
          ("direct stream", compact_direct);
          ("wrapped direct stream", wrapped_direct);
          ( "standalone stream",
            (expect_assembly compact_standalone).assembly_unit_items );
          ( "wrapped standalone stream",
            (expect_assembly wrapped_standalone).assembly_unit_items );
        ];
      Alcotest.(check (list string))
        "opcode boundaries do not depend on source lines"
        [ "MOV"; "MOV"; "LOCK"; "BTS"; "REP_MOVSB"; "RET1" ]
        (instruction_names compact_block.assembly_items);
      Alcotest.(check bool)
        "physical source lines remain available after semantic parsing" true
        (List.length wrapped_block.assembly_lines
        > List.length compact_block.assembly_lines))
    modes

let labels_and_original_prefixes () =
  let source =
    "ENTRY:: MOV RAX,42 @@again: LOCK BTS U64 [RBX],0 JNZ @@again REP_MOVSB \
     EXIT: RET"
  in
  let _, _, _, assembly = block source in
  let item_name = function
    | Ast.Inline_assembly_instruction operation -> opcode operation
    | Ast.Inline_assembly_label label -> label.assembly_label_name.spelling
    | Ast.Inline_assembly_directive directive ->
        directive.inline_assembly_directive_token.assembly_source_token.raw
    | Ast.Inline_assembly_empty _ -> ";"
  in
  Alcotest.(check (list string))
    "labels and prefix opcodes have distinct ordered entries"
    [
      "ENTRY";
      "MOV";
      "@@again";
      "LOCK";
      "BTS";
      "JNZ";
      "REP_MOVSB";
      "EXIT";
      "RET";
    ]
    (List.map item_name assembly.assembly_items);
  let labels =
    List.filter_map
      (function
        | Ast.Inline_assembly_label label -> Some label
        | _ -> None)
      assembly.assembly_items
  in
  Alcotest.(check (list string))
    "original exported, local and global label kinds"
    [ "exported_global"; "local"; "global" ]
    (List.map
       (fun label -> T.assembly_label_kind_name label.Ast.assembly_label_kind)
       labels);
  let operations = instructions assembly.assembly_items in
  Alcotest.(check (list int))
    "LOCK is the original single-byte opcode" [ 0xf0 ]
    (List.nth operations 1).inline_assembly_selection.form.opcode_bytes;
  Alcotest.(check int)
    "LOCK does not absorb the following BTS operands" 0
    (List.length (List.nth operations 1).inline_assembly_operands);
  Alcotest.(check (list int))
    "REP_MOVSB uses the pinned 64-bit form" [ 0xf3; 0x48; 0xa4 ]
    (List.nth operations 4).inline_assembly_selection.form.opcode_bytes

let use_modes_and_entry_reset () =
  let source = "USE16 REP_MOVSB USE32 REP_MOVSB USE64 REP_MOVSB" in
  let _, _, _, assembly = block source in
  let operations = instructions assembly.assembly_items in
  Alcotest.(check (list int))
    "USE directives affect subsequent operations" [ 16; 32; 64 ]
    (List.map
       (fun operation -> Operand.mode_bits operation.Ast.inline_assembly_mode)
       operations);
  Alcotest.(check (list int))
    "the original mode-exclusion flag chooses the correct table row"
    [ 915; 915; 916 ]
    (List.map
       (fun operation ->
         operation.Ast.inline_assembly_selection.form.source_line)
       operations);
  let _, _, _, direct_items = direct "USE16 REP_MOVSB; I64 value; REP_MOVSB;" in
  Alcotest.(check (list int))
    "a language declaration ends the original assembly mode scope" [ 16; 64 ]
    (instructions direct_items
    |> List.map (fun operation ->
        Operand.mode_bits operation.Ast.inline_assembly_mode));
  let _, _, _, direct_labels = direct "USE32 REP_MOVSB next: REP_MOVSB;" in
  Alcotest.(check (list int))
    "a direct language label also starts a new assembly entry" [ 32; 64 ]
    (instructions direct_labels
    |> List.map (fun operation ->
        Operand.mode_bits operation.Ast.inline_assembly_mode));
  List.iter
    (fun mode ->
      let _, _, output = standalone ~mode "REP_MOVSB" in
      let unit_ = expect_assembly output in
      Alcotest.(check int)
        "standalone caller mode is retained" (Operand.mode_bits mode)
        (Operand.mode_bits unit_.assembly_unit_initial_mode);
      Alcotest.(check int)
        "standalone operation uses the caller mode" (Operand.mode_bits mode)
        ( List.hd (instructions unit_.assembly_unit_items) |> fun operation ->
          Operand.mode_bits operation.inline_assembly_mode ))
    [ Operand.Use16; Operand.Use32; Operand.Use64 ]

let sizes_registers_and_addresses () =
  let _, _, _, assembly =
    block ~source_context:true
      "MOV AL,U8 1[RBP] MOV AX,I16 2[RBP] MOV EAX,U32 4[RBP] MOV RAX,I64 \
       8[RBP] MOV R8u8,AL MOV R9u16,AX MOV R10u32,EAX MOV R11u64,RAX LEA \
       RDX,U64 24[RBP+RAX*4] MOV RDI,U64 FS:CTask.hash_table[RAX] PUSH FS"
  in
  let operations = instructions assembly.assembly_items in
  Alcotest.(check (list int))
    "signed and unsigned size keywords retain byte widths" [ 1; 2; 4; 8 ]
    (List.init 4 (fun index ->
         Operand.size (operand 1 (List.nth operations index))));
  Alcotest.(check (list string))
    "TempleOS extended-register aliases remain source spellings"
    [ "R8u8"; "R9u16"; "R10u32"; "R11u64" ]
    (List.init 4 (fun index ->
         operand 0 (List.nth operations (index + 4))
         |> Operand.register1 |> Option.get |> Asm_register.spelling));
  let indexed = operand 1 (List.nth operations 8) in
  Alcotest.(check bool)
    "brackets retain indirection" true (Operand.indirect indexed);
  Alcotest.(check (option string))
    "the scaled register is the original first register" (Some "RAX")
    (register_name (Operand.register1 indexed));
  Alcotest.(check (option string))
    "the unscaled register is the original second register" (Some "RBP")
    (register_name (Operand.register2 indexed));
  Alcotest.(check int64)
    "literal scale survives parsing" 4L (Operand.scale indexed);
  Alcotest.(check (option int64))
    "displacement before brackets is retained" (Some 24L)
    (Operand.immediate indexed |> Option.get).value;
  let segmented = operand 1 (List.nth operations 9) in
  Alcotest.(check (option string))
    "segment prefix" (Some "FS")
    (register_name (Operand.segment segmented));
  Alcotest.(check bool)
    "a segmented memory operand is not a direct segment register" false
    (Operand.just_segment segmented);
  Alcotest.(check bool)
    "a plain FS operand retains its direct segment identity" true
    (Operand.just_segment (operand 0 (List.nth operations 10)));
  Alcotest.(check bool)
    "member displacements stay unresolved until expression/fixup evaluation"
    true
    (Operand.unresolved segmented)

let aliases_x87_and_nonstandard_arity () =
  let _, _, _, assembly =
    block
      "BPT JZ target LOOPZ target FLD U64 [RDX] FMUL ST0,ST0 FADDP ST1,ST0 \
       FXCH ST7 IMUL RAX IMUL2 RAX,10 RET RET1 8 target:"
  in
  let operations = instructions assembly.assembly_items in
  Alcotest.(check (list int))
    "the pinned dialect has fixed opcode arities"
    [ 0; 1; 1; 1; 2; 2; 1; 1; 2; 0; 1 ]
    (List.map
       (fun operation -> List.length operation.Ast.inline_assembly_operands)
       operations);
  let aliases =
    List.init 3 (fun index ->
        let operation = List.nth operations index in
        match operation.inline_assembly_opcode.assembly_token_kind with
        | Ast.Assembly_opcode_token { canonical_spelling; source_is_alias } ->
            Alcotest.(check bool)
              (opcode operation ^ " is marked as an alias")
              true source_is_alias;
            canonical_spelling
        | _ -> Alcotest.fail "expected a classified opcode")
  in
  Alcotest.(check (list string))
    "aliases preserve their original canonical operations"
    [ "INT3"; "JE"; "LOOPE" ] aliases;
  Alcotest.(check (option string))
    "x87 destination is a stack register" (Some "ST1")
    (operand 0 (List.nth operations 5) |> Operand.register1 |> register_name);
  Alcotest.(check (option string))
    "x87 source ST0 is preserved" (Some "ST0")
    (operand 1 (List.nth operations 5) |> Operand.register1 |> register_name);
  Alcotest.(check int)
    "FXCH uses the source's explicit single stack operand" 1255
    (List.nth operations 6).inline_assembly_selection.form.source_line

let numeric_and_symbolic_immediates () =
  let _, _, _, assembly =
    block ~source_context:true
      "MOV AL,255 MOV AX,-32768 MOV EAX,0xffffffff MOV RAX,0x8000000000000000 \
       MOV RAX,40+2 MOV RAX,'AB' MOV RCX,cnts.time_stamp_freq>>3 CALL &Target \
       JNZ @@later @@later: RET"
  in
  let operations = instructions assembly.assembly_items in
  let value index =
    operand 1 (List.nth operations index) |> Operand.immediate |> Option.get
    |> fun immediate -> immediate.value
  in
  Alcotest.(check (list (option int64)))
    "numeric immediate bits follow HolyC literal and expression semantics"
    [
      Some 255L;
      Some (-32768L);
      Some 4294967295L;
      Some Int64.min_int;
      Some 42L;
      Some 0x4241L;
    ]
    (List.init 6 value);
  let symbolic = operand 1 (List.nth operations 6) in
  Alcotest.(check bool)
    "a source member expression is kept for later evaluation" true
    (Operand.unresolved symbolic);
  Alcotest.(check (option int64))
    "unresolved expression has no fabricated numeric result" None
    (Operand.immediate symbolic |> Option.get).value;
  let target = operand 0 (List.nth operations 7) in
  Alcotest.(check bool)
    "address-of expression preserves absolute intent" true
    (Operand.immediate target |> Option.get).absolute;
  Alcotest.(check bool)
    "a relative label remains unresolved without an instruction position" true
    (List.nth operations 8).inline_assembly_selection.unresolved

let typed_expression_boundaries () =
  let contents =
    "class CAssemblyPacket { U64 RAX; };\n\
     U8 S[4];\n\
     asm {\n\
     MOV AL,S[1]\n\
     MOV RAX,(S)[1]\n\
     MOV RAX,[RAX+S[1]]\n\
     MOV RAX,S(CAssemblyPacket*)[1]\n\
     MOV RAX,U64 Label[RAX]\n\
     MOV RAX,sizeof(S)[RAX]\n\
     }\n\
     U0 StaticAssemblyIndex() {\n\
     static U8 S[4];\n\
     MOV AL,S[1]\n\
     MOV RAX,(S)[1]\n\
     MOV RAX,[RAX+S[1]]\n\
     }"
  in
  List.iter
    (fun compilation_mode ->
      let operations = program_instructions ~compilation_mode contents in
      let values = List.map (operand 1) operations in
      Alcotest.(check (list bool))
        "typed value indices stay inside expressions; assembly brackets stay \
         indirect"
        [ false; false; true; false; true; true; false; false; true ]
        (List.map Operand.indirect values);
      Alcotest.(check (list int))
        "typed, parenthesized and cast indices retain their expression boundary"
        [ 1; 1; 1; 1; 0; 0; 1; 1; 1 ]
        (List.map
           (fun value ->
             (Operand.immediate value |> Option.get).expression
             |> List.filter (function
               | Operand.Expression_open_bracket -> true
               | _ -> false)
             |> List.length)
           values);
      List.iter
        (fun value ->
          Alcotest.(check bool)
            "typed values and source queries wait for expression evaluation"
            true (Operand.unresolved value);
          Alcotest.(check (option int64))
            "typed values have no fabricated numeric result" None
            (Operand.immediate value |> Option.get).value)
        values;
      let cast =
        (Operand.immediate (List.nth values 3) |> Option.get).expression
      in
      Alcotest.(check bool)
        "the declared class remains a postfix cast" true
        (List.exists
           (function
             | Operand.Type_name "CAssemblyPacket" -> true
             | _ -> false)
           cast);
      let _, _, output =
        T.parse_string ~compilation_mode "asm {MOV RAX,U64 Label[1]}"
      in
      Alcotest.(check string)
        "a bare export cannot turn its second immediate into a typed index"
        "HCPARSE0151" (first_error output.diagnostics).code)
    modes

let deferred_expression_shapes () =
  let contents =
    "class CAssemblyPacket { U64 RAX; };\n\
     CAssemblyPacket packet;\n\
     asm {\n\
     MOV RAX,sizeof(((U8*)))\n\
     MOV RAX,offset((CAssemblyPacket.RAX))\n\
     MOV RAX,packet.RAX\n\
     MOV RAX,1.5\n\
     MOV RAX,\"pending\"\n\
     MOV RAX,32>>1>>2\n\
     MOV RAX,1&&2\n\
     }"
  in
  List.iter
    (fun compilation_mode ->
      let operations = program_instructions ~compilation_mode contents in
      Alcotest.(check int)
        "queries, members, literals and deferred operators form complete \
         instructions"
        7 (List.length operations);
      let values = List.map (operand 1) operations in
      List.iter
        (fun value ->
          Alcotest.(check bool)
            "valid deferred syntax retains unresolved state" true
            (Operand.unresolved value);
          Alcotest.(check bool)
            "queries and literals are immediate expressions" false
            (Operand.indirect value))
        values;
      let expression index =
        (Operand.immediate (List.nth values index) |> Option.get).expression
      in
      Alcotest.(check bool)
        "a member named RAX is not a register operand" true
        (List.exists
           (function
             | Operand.Symbol "RAX" -> true
             | _ -> false)
           (expression 2)
        && not
             (List.exists
                (function
                  | Operand.Register _ -> true
                  | _ -> false)
                (expression 2)));
      Alcotest.(check bool)
        "floating literal keeps its exact source value" true
        (List.exists
           (function
             | Operand.Floating bits -> bits = Int64.bits_of_float 1.5
             | _ -> false)
           (expression 3));
      Alcotest.(check bool)
        "string literal keeps its decoded bytes" true
        (List.exists
           (function
             | Operand.String_literal "pending" -> true
             | _ -> false)
           (expression 4));
      let local =
        program_instructions ~compilation_mode
          "U0 LocalAssemblyOffset(){I64 local; MOV RAX,&local;}"
        |> List.hd |> operand 1
      in
      Alcotest.(check bool)
        "address-of automatic local retains its stack-offset expression" true
        (Operand.unresolved local);
      let _, _, output =
        T.parse_string ~compilation_mode
          "U0 LocalAssemblyValue(){I64 local; MOV RAX,local;}"
      in
      Alcotest.(check string)
        "automatic local values require the source address-of spelling"
        "HCPARSE0151" (first_error output.diagnostics).code)
    modes

let declared_class_offsets () =
  List.iter
    (fun compilation_mode ->
      let operation =
        program_instructions ~compilation_mode
          "class C {U8 leading; U8 x;}; asm {MOV AL,C.x}"
        |> List.hd
      in
      let offset = operand 1 operation in
      Alcotest.(check int)
        "a declared class offset does not force an export address width" 0
        (Operand.size offset);
      Alcotest.(check bool)
        "the class offset remains a value pending layout evaluation" true
        (Operand.unresolved offset);
      Alcotest.(check bool)
        "a class offset is not an absolute export address" false
        (Operand.immediate offset |> Option.get).absolute;
      Alcotest.(check int)
        "the declared offset matches the original byte immediate form" 268
        operation.inline_assembly_selection.form.source_line)
    modes

let source_modifier_boundaries () =
  List.iter
    (fun compilation_mode ->
      List.iter
        (fun body ->
          let _, _, output =
            T.parse_string ~compilation_mode ("asm {" ^ body ^ "\n}")
          in
          ignore (first_error output.diagnostics);
          Alcotest.(check bool)
            (body ^ " rejects in a brace stream")
            true
            (Option.is_none output.ast);
          let _, _, output = standalone ~compilation_mode body in
          ignore (first_error output.diagnostics);
          Alcotest.(check bool)
            (body ^ " rejects in a standalone stream")
            true
            (Option.is_none output.assembly))
        [
          "MOV RAX,1()";
          "MOV RAX,1(2)";
          "MOV RAX,Label()";
          "MOV RAX,Label(1)";
          "MOV RAX,1++";
          "MOV RAX,1=2";
        ];
      let _, _, _, parenthesized =
        block ~compilation_mode "MOV RAX,(1)++ NOP"
      in
      Alcotest.(check (list string))
        "parenthesized terms retain the source modifier state" [ "MOV"; "NOP" ]
        (instruction_names parenthesized.assembly_items);
      let _, _, output =
        standalone ~compilation_mode "#define BAD 1()\nMOV RAX,BAD"
      in
      let diagnostic = first_error output.diagnostics in
      Alcotest.(check bool)
        "a rejected generated call keeps its expansion provenance" true
        (List.exists
           (fun (related : Diagnostic.related) ->
             T.contains related.message "BAD"
             && T.contains related.message "expanded")
           diagnostic.secondary);
      Alcotest.(check bool)
        "a rejected generated call keeps its definition provenance" true
        (List.exists
           (fun (related : Diagnostic.related) ->
             T.contains related.message "BAD"
             && T.contains related.message "declared")
           diagnostic.secondary))
    modes

let declared_function_call_shapes () =
  let declarations =
    "I64 F(I64 x,I64 y=2);\n\
     I64 Pair(I64 x,I64 y);\n\
     I64 Variadic(I64 x,...);\n\
     I64 Defaults(I64 x=1,I64 y=2);\n"
  in
  let body =
    "MOV RAX,F 1 MOV RBX,F(1,2) MOV RCX,Pair 3 4 MOV RDX,Variadic 5 MOV \
     R8,Variadic(6,7,8) MOV R9,Defaults"
  in
  List.iter
    (fun compilation_mode ->
      let operations =
        program_instructions ~compilation_mode
          (declarations ^ "asm {" ^ body ^ "\n}")
      in
      Alcotest.(check int)
        "declared call arity leaves the next assembly operation intact" 6
        (List.length operations);
      Alcotest.(check (list (list string)))
        "parenthesis-free calls consume required arguments without commas"
        [
          [ "F"; "1" ];
          [ "F"; "("; "1"; ","; "2"; ")" ];
          [ "Pair"; "3"; "4" ];
          [ "Variadic"; "5" ];
          [ "Variadic"; "("; "6"; ","; "7"; ","; "8"; ")" ];
          [ "Defaults" ];
        ]
        (List.map
           (fun operation ->
             List.nth operation.Ast.inline_assembly_operands 1
             |> T.inline_assembly_operand_spellings)
           operations);
      List.iter
        (fun operation ->
          Alcotest.(check bool)
            "declared calls retain deferred values" true
            (operand 1 operation |> Operand.unresolved))
        operations;
      List.iter
        (fun invalid ->
          let _, _, output =
            T.parse_string ~compilation_mode
              (declarations ^ "asm {" ^ invalid ^ "\n}")
          in
          ignore (first_error output.diagnostics);
          Alcotest.(check bool)
            (invalid ^ " violates the declared call shape")
            true
            (Option.is_none output.ast))
        [
          "MOV RAX,Pair 1 NOP";
          "MOV RAX,F(1,2,3)";
          "MOV RAX,F(,2)";
          "MOV RAX,Pair(1)";
          "MOV RAX,F 1,2";
          "MOV RAX,F 1 2";
          "MOV RAX,Variadic 5 6";
        ])
    modes

let source_cast_declarators () =
  List.iter
    (fun compilation_mode ->
      let operations =
        program_instructions ~compilation_mode
          "U8 S[4]; asm {\n\
           MOV RAX,S(U8[4])[1]\n\
           MOV RAX,0(U0(*)(I64))\n\
           MOV RAX,0(U0(*)(;I64 value;;))\n\
           MOV RAX,0(U0(*)(U0 (*callback)(I64 value)))\n\
           }"
      in
      Alcotest.(check int)
        "array casts and nested function signatures consume complete operands" 4
        (List.length operations);
      List.iter
        (fun operation ->
          Alcotest.(check bool)
            "a source cast remains a deferred expression" true
            (operand 1 operation |> Operand.unresolved))
        operations;
      List.iter
        (fun cast ->
          let _, _, output =
            T.parse_string ~compilation_mode ("asm {MOV RAX," ^ cast ^ "\n}")
          in
          ignore (first_error output.diagnostics);
          Alcotest.(check bool)
            (cast ^ " violates the pinned cast declarator grammar")
            true
            (Option.is_none output.ast))
        [ "0(U8[4][])"; "0(U8*****)"; "0(U0(*)(U8 value[4]))" ])
    modes

let variable_and_cast_call_shapes () =
  let declarations =
    "I64 Scalar;\n\
     U8 *Pointer;\n\
     I64 (*Callback)(I64 value,I64 optional=2);\n\
     I64 (*VariadicCallback)(I64 value,...);\n"
  in
  let source =
    declarations
    ^ "asm {\n\
       MOV RAX,Scalar\n\
       MOV RBX,Scalar(I64(*)(I64))(1)\n\
       MOV RCX,Callback(3)\n\
       MOV RDX,Callback(4,5)\n\
       MOV R8,VariadicCallback(6,7,8)\n\
       MOV R9,Callback\n\
       MOV R10,0(I64(*)(I64))(1)\n\
       MOV R11,0(I64(*)(I64 optional=2))()\n\
       }\n\
       U0 StaticCallShapes(){\n\
       static I64 Local;\n\
       static I64 (*LocalCallback)(I64 value);\n\
       MOV RAX,Local(I64(*)(I64))(1)\n\
       MOV RBX,LocalCallback(I64(*)(I64))(1)\n\
       MOV RCX,LocalCallback\n\
       }"
  in
  List.iter
    (fun compilation_mode ->
      let operations = program_instructions ~compilation_mode source in
      Alcotest.(check int)
        "scalar casts, callback calls and bare callback values remain distinct"
        11 (List.length operations);
      List.iter
        (fun operation ->
          let value = operand 1 operation in
          Alcotest.(check bool)
            "calls and variable values remain deferred" true
            (Operand.unresolved value);
          Alcotest.(check bool)
            "call syntax does not become an assembly memory operand" false
            (Operand.indirect value))
        operations;
      let variable_parameters name operation =
        Operand.tokens (operand 1 operation)
        |> List.find_map (function
          | Operand.Variable_symbol variable when variable.name = name ->
              Some variable.parameters
          | _ -> None)
        |> Option.get
      in
      Alcotest.(check (option (list bool)))
        "a declared scalar has no callable shape" None
        (variable_parameters "Scalar" (List.nth operations 0));
      Alcotest.(check (option (list bool)))
        "a global callback keeps its required and default parameter shape"
        (Some [ false; true ])
        (variable_parameters "Callback" (List.nth operations 2));
      Alcotest.(check (option (list bool)))
        "a static local callback is a value in original assembly expressions"
        None
        (variable_parameters "LocalCallback" (List.nth operations 10));
      List.iter
        (fun body ->
          let _, _, output =
            T.parse_string ~compilation_mode
              (declarations ^ "asm {" ^ body ^ "\n}")
          in
          ignore (first_error output.diagnostics);
          Alcotest.(check bool)
            (body ^ " violates its source callable shape")
            true
            (Option.is_none output.ast))
        [
          "MOV RAX,Scalar(1)";
          "MOV RAX,Pointer(1)";
          "MOV RAX,Callback()";
          "MOV RAX,Callback(1,2,3)";
          "MOV RAX,Callback 1";
          "MOV RAX,VariadicCallback()";
          "MOV RAX,0(I64)(1)";
          "MOV RAX,0(I64(*)(I64))()";
          "MOV RAX,0(I64(*)(I64))(1,2)";
          "MOV RAX,0(I64(*)(I64))(1)(2)";
        ];
      List.iter
        (fun local ->
          let _, _, output =
            T.parse_string ~compilation_mode
              ("U0 BadStaticCall(){" ^ local ^ " MOV RAX,Local(1);}")
          in
          ignore (first_error output.diagnostics);
          Alcotest.(check bool)
            "static local values require an explicit callback cast in assembly"
            true
            (Option.is_none output.ast))
        [ "static I64 Local;"; "static I64 (*Local)(I64 value);" ])
    modes

let function_address_lifecycles () =
  let internal_declaration =
    T.pinned_lines "Compiler/CompilerA.HH" ~first:169 ~last:169
    ^ "\n"
    ^ T.pinned_lines "Kernel/KernelB.HH" ~first:9 ~last:10
    ^ "\n"
  in
  let check_address ~absolute ~size operation =
    let value = operand 1 operation in
    let immediate = Operand.immediate value |> Option.get in
    Alcotest.(check bool)
      "the source function address category is known" true
      immediate.absolute_known;
    Alcotest.(check bool)
      "function addresses retain the mode's original absolute parity" absolute
      immediate.absolute;
    Alcotest.(check int)
      "known function address parity controls the original minimum width" size
      (Operand.size value);
    Alcotest.(check bool)
      "knowing an address category does not fabricate an executable address"
      true (Operand.unresolved value)
  in
  List.iter
    (fun compilation_mode ->
      let aot = compilation_mode = Preprocessor.Aot in
      let operations =
        program_instructions ~compilation_mode
          "I64 Complete(){return 0;} asm {USE16 MOV AX,&Complete USE32 MOV \
           EAX,&Complete USE64 MOV RAX,&Complete MOV RDX,&Complete-&Complete}"
      in
      Alcotest.(check int)
        "completed function address and parity expressions remain separate" 4
        (List.length operations);
      List.iter2
        (fun width operation ->
          check_address ~absolute:aot ~size:(if aot then width else 0) operation)
        [ 2; 4; 4 ]
        [ List.nth operations 0; List.nth operations 1; List.nth operations 2 ];
      check_address ~absolute:false ~size:0 (List.nth operations 3);
      let bound =
        program_instructions ~compilation_mode
          "_extern _BOUND I64 Bound(); asm {MOV RAX,&Bound}"
        |> List.hd
      in
      check_address ~absolute:aot ~size:(if aot then 4 else 0) bound;
      List.iter
        (fun contents ->
          let _, _, output = T.parse_string ~compilation_mode contents in
          if aot then
            let ast = T.expect_ast output in
            List.concat_map
              (function
                | Ast.Top_level_statement statement -> statement_items statement
                | Ast.Function_definition definition ->
                    T.expect_function_body definition |> statement_items
                | _ -> [])
              ast.items
            |> instructions
            |> List.iter (check_address ~absolute:true ~size:4)
          else (
            Alcotest.(check string)
              "fresh external or self address is forbidden by the JIT extern \
               counter"
              "HCPARSE0151" (first_error output.diagnostics).code;
            Alcotest.(check bool)
              "a rejected JIT external address produces no AST" true
              (Option.is_none output.ast)))
        [
          "extern I64 Pending(); asm {MOV RAX,&Pending}";
          "I64 Self(){asm {MOV RAX,&Self} return 0;} asm {MOV RBX,&Self}";
        ];
      let internal =
        program_instructions ~compilation_mode
          (internal_declaration ^ "asm {MOV RAX,&Bsf 1 NOP}")
      in
      Alcotest.(check (list string))
        "an internal function still consumes its original required arguments"
        [ "MOV"; "NOP" ] (List.map opcode internal);
      Alcotest.(check bool)
        "address-of an internal function retains call semantics" true
        (List.exists
           (function
             | Operand.Function_call { name = "Bsf"; parameters = [ false ]; _ }
               -> true
             | _ -> false)
           (Operand.tokens (operand 1 (List.hd internal))));
      let _, _, missing_internal_argument =
        T.parse_string ~compilation_mode
          (internal_declaration ^ "asm {MOV RAX,&Bsf NOP}")
      in
      ignore (first_error missing_internal_argument.diagnostics);
      Alcotest.(check bool)
        "an internal function cannot use the ordinary function-address shortcut"
        true
        (Option.is_none missing_internal_argument.ast);
      let _, _, narrow =
        T.parse_string ~compilation_mode
          "I64 Narrow(){return 0;} asm {MOV AL,&Narrow}"
      in
      if aot then
        Alcotest.(check string)
          "a known AOT address cannot select a byte-only immediate form"
          "HCPARSE0175" (first_error narrow.diagnostics).code
      else
        let ast = T.expect_ast narrow in
        let operation =
          ast.items
          |> List.concat_map (function
            | Ast.Top_level_statement statement -> statement_items statement
            | _ -> [])
          |> instructions |> List.hd
        in
        Alcotest.(check bool)
          "the narrow JIT candidate remains provisional until address \
           evaluation"
          true operation.inline_assembly_selection.unresolved)
    modes

let function_address_evidence_snapshots () =
  List.iter
    (fun compilation_mode ->
      let session = Session.create () in
      ignore
        (Symbol_visibility.Environment.add ~origin:Session_registration
           (Session.symbols session) ~name:"Registered" ~kind:Function ());
      let config = T.config ~compilation_mode (Sys.getcwd ()) in
      let parse_assembly path =
        let source =
          Session.add_source session ~path ~contents:"MOV RAX,&Registered"
        in
        Holyc_lib.parse_assembly_detailed session ~config ~source
        |> expect_assembly
      in
      let before = parse_assembly "before-definition.ASM" in
      let before_operation =
        instructions before.assembly_unit_items |> List.hd
      in
      let before_value = operand 1 before_operation in
      let before_immediate = Operand.immediate before_value |> Option.get in
      Alcotest.(check bool)
        "a registered function without lifecycle evidence leaves parity unknown"
        false before_immediate.absolute_known;
      Alcotest.(check bool)
        "the selected form retains unknown function-address evidence" true
        before_operation.inline_assembly_selection.unresolved;
      let sources = Session.sources session in
      let retained_json = Ast_dump.assembly_json sources before in
      let source =
        Session.add_source session ~path:"definition.HC"
          ~contents:"I64 Registered(){return 0;}"
      in
      ignore (Holyc_lib.parse_detailed session ~config ~source |> T.expect_ast);
      let after = parse_assembly "after-definition.ASM" in
      let after_operation = instructions after.assembly_unit_items |> List.hd in
      let after_immediate =
        operand 1 after_operation |> Operand.immediate |> Option.get
      in
      Alcotest.(check bool)
        "completing an unknown entry cannot invent missing internal/import \
         flags"
        false after_immediate.absolute_known;
      Alcotest.(check bool)
        "completing a body does not rewrite an earlier operand snapshot" false
        before_immediate.absolute_known;
      Alcotest.(check string)
        "later declarations leave previously dumped operand evidence unchanged"
        retained_json
        (Ast_dump.assembly_json sources before);
      let open Yojson.Safe.Util in
      let json_immediate assembly =
        Ast_dump.assembly_to_yojson sources assembly
        |> member "items" |> to_list |> List.hd |> member "operands" |> to_list
        |> fun operands ->
        List.nth operands 1 |> member "value" |> member "immediate"
      in
      Alcotest.(check bool)
        "JSON explicitly represents unknown address parity" false
        (json_immediate before |> member "absolute_known" |> to_bool);
      Alcotest.(check bool)
        "JSON preserves unknown flags after an unknown declaration is completed"
        false
        (json_immediate after |> member "absolute_known" |> to_bool))
    modes;
  let session = Session.create () in
  ignore
    (Symbol_visibility.Environment.add ~origin:Session_registration
       (Session.symbols session) ~name:"Exported" ~kind:Export_system_symbol ());
  let config = T.config ~compilation_mode:Preprocessor.Aot (Sys.getcwd ()) in
  let declare path contents =
    let source = Session.add_source session ~path ~contents in
    Holyc_lib.parse_detailed session ~config ~source |> T.expect_ast |> ignore
  in
  let assembly path =
    let source =
      Session.add_source session ~path ~contents:"MOV RAX,&Exported"
    in
    Holyc_lib.parse_assembly_detailed session ~config ~source |> expect_assembly
  in
  let immediate assembly =
    instructions assembly.Ast.assembly_unit_items
    |> List.hd |> operand 1 |> Operand.immediate |> Option.get
  in
  declare "export-prototype.HC" "extern I64 Exported();";
  let before = assembly "export-before-body.ASM" in
  let sources = Session.sources session in
  let retained_json = Ast_dump.assembly_json sources before in
  Alcotest.(check bool)
    "an existing export without IMM/IMPORT flags keeps AOT address parity \
     unknown"
    false (immediate before).absolute_known;
  declare "export-body.HC" "I64 Exported(){return 0;}";
  let after = assembly "export-after-body.ASM" in
  Alcotest.(check bool)
    "a source-declared function body establishes ordinary AOT address evidence"
    true (immediate after).absolute_known;
  Alcotest.(check bool)
    "the completed ordinary AOT body has absolute address parity" true
    (immediate after).absolute;
  Alcotest.(check bool)
    "the earlier operand keeps its export-dependent unknown parity" false
    (immediate before).absolute_known;
  Alcotest.(check string)
    "completing a source prototype does not rewrite the previous operand dump"
    retained_json
    (Ast_dump.assembly_json sources before);
  let open Yojson.Safe.Util in
  let json_immediate assembly =
    Ast_dump.assembly_to_yojson sources assembly
    |> member "items" |> to_list |> List.hd |> member "operands" |> to_list
    |> fun operands ->
    List.nth operands 1 |> member "value" |> member "immediate"
  in
  Alcotest.(check bool)
    "JSON retains unknown parity before the source body completes" false
    (json_immediate before |> member "absolute_known" |> to_bool);
  Alcotest.(check bool)
    "JSON records proven parity after the source body completes" true
    (json_immediate after |> member "absolute_known" |> to_bool);
  Alcotest.(check bool)
    "JSON records the completed body's AOT absolute parity" true
    (json_immediate after |> member "absolute" |> to_bool)

let inline_aggregate_casts () =
  let body =
    "MOV RAX,0(U8 class CInline {U8 x;}) MOV RBX,0(CInline*) MOV RCX,0(U16 \
     union UInline {U8 low;U16 wide;}[2])[1] MOV RDX,0(I64 class \
     CCallbackResult {I64 result;}(*)(I64))(1) MOV R8,0(U8 class CFirst {U8 \
     first;} union USecond {U8 second;}*) NOP"
  in
  let check items =
    let operations = instructions items in
    Alcotest.(check (list string))
      "inline definitions leave following instructions intact"
      [ "MOV"; "MOV"; "MOV"; "MOV"; "MOV"; "NOP" ]
      (List.map opcode operations);
    let values =
      List.filter_map
        (fun operation -> List.nth_opt operation.Ast.inline_assembly_operands 1)
        operations
    in
    Alcotest.(check (list int))
      "definitions remain attached to the operand that declares them"
      [ 1; 0; 1; 1; 2 ]
      (List.map
         (fun value -> List.length value.Ast.inline_assembly_type_definitions)
         values);
    let definitions =
      List.concat_map
        (fun value -> value.Ast.inline_assembly_type_definitions)
        values
    in
    Alcotest.(check (list string))
      "chained inline definitions retain source order"
      [ "CInline"; "UInline"; "CCallbackResult"; "CFirst"; "USecond" ]
      (List.map
         (fun (definition : Ast.aggregate_definition) ->
           definition.name.spelling)
         definitions);
    Alcotest.(check (list bool))
      "inline classes and unions retain their declared kinds"
      [ false; true; false; false; true ]
      (List.map
         (fun (definition : Ast.aggregate_definition) ->
           definition.aggregate_kind = Ast.Union_aggregate)
         definitions);
    Alcotest.(check (list (list string)))
      "the shared class parser retains complete member declarations"
      [ [ "x" ]; [ "low"; "wide" ]; [ "result" ]; [ "first" ]; [ "second" ] ]
      (List.map
         (fun (definition : Ast.aggregate_definition) ->
           List.concat_map
             (fun member ->
               (T.expect_aggregate_member_declaration member).member_declarators
               |> List.map (fun (member : Ast.aggregate_member_declarator) ->
                   member.member_name.spelling))
             definition.members)
         definitions);
    let chained = List.nth definitions 4 in
    ignore
      (T.expect_named_specifier "CFirst"
         (Option.get chained.backing).backing_type_specifier);
    List.iter
      (fun value ->
        Alcotest.(check bool)
          "inline type definitions and calls stay deferred" true
          (Operand.unresolved value.Ast.inline_assembly_operand))
      values;
    let reused = List.nth values 1 in
    Alcotest.(check bool)
      "an inline definition is visible to the following operand" true
      (List.exists
         (function
           | Operand.Type_name "CInline" -> true
           | _ -> false)
         (Operand.tokens reused.inline_assembly_operand))
  in
  List.iter
    (fun compilation_mode ->
      let _, _, _, brace = block ~compilation_mode body in
      check brace.assembly_items;
      let _, _, _, direct = direct ~compilation_mode body in
      check direct;
      let _, _, standalone_output = standalone ~compilation_mode body in
      check (expect_assembly standalone_output).assembly_unit_items;
      let session, _, data_output =
        standalone ~compilation_mode
          "DU64 0(U8 class CData{U8 x;}),2 DUP(0(U16 union UData{U8 x;})); MOV \
           RAX,0(CData*) MOV RBX,0(UData*)"
      in
      let data_assembly = expect_assembly data_output in
      let directive =
        data_assembly.assembly_unit_items
        |> List.find_map (function
          | Ast.Inline_assembly_directive directive -> Some directive
          | _ -> None)
        |> Option.get
      in
      Alcotest.(check (list string))
        "data and DUP expression definitions remain attached in source order"
        [ "CData"; "UData" ]
        (List.map
           (fun (definition : Ast.aggregate_definition) ->
             definition.name.spelling)
           directive.inline_assembly_directive_type_definitions);
      List.iter2
        (fun name operation ->
          Alcotest.(check bool)
            "a type declared in a data directive is visible to later operands"
            true
            (List.exists
               (function
                 | Operand.Type_name spelling -> spelling = name
                 | _ -> false)
               (Operand.tokens (operand 1 operation))))
        [ "CData"; "UData" ]
        (instructions data_assembly.assembly_unit_items);
      let open Yojson.Safe.Util in
      let data_json =
        Ast_dump.assembly_to_yojson (Session.sources session) data_assembly
        |> member "items" |> to_list |> List.hd
      in
      Alcotest.(check (list string))
        "directive JSON uses the shared retained-definition schema"
        [ "CData"; "UData" ]
        (data_json |> member "type_definitions" |> to_list
        |> List.map (fun definition ->
            definition |> member "name" |> member "spelling" |> to_string));
      List.iter
        (fun cast ->
          let _, _, output =
            standalone ~compilation_mode ("MOV RAX," ^ cast ^ " NOP")
          in
          ignore (first_error output.diagnostics);
          Alcotest.(check bool)
            (cast ^ " retains the shared declaration validation")
            true
            (Option.is_none output.assembly))
        [
          "0(U8 class {U8 value;})";
          "0(U8 class CBad {MissingType value;})";
          "0(U8 class CBad {U8 ;})";
          "0(U8 class CBad {U8 value})";
          "0(U8 class CBad {U8 value;}*****)";
        ])
    modes

let inline_aggregate_cast_provenance () =
  let body =
    "#define AGG class\n\
     #define FIELD U8\n\
     MOV RAX,0(U8 AGG CGenerated {FIELD RAX;})"
  in
  let session, _, output = standalone body in
  let assembly = expect_assembly output in
  let operation = assembly.assembly_unit_items |> instructions |> List.hd in
  let value = List.nth operation.inline_assembly_operands 1 in
  let definition = List.hd value.inline_assembly_type_definitions in
  Alcotest.(check bool)
    "the inline aggregate keyword keeps its macro definition" true
    (Option.is_some definition.aggregate_keyword_location.defined_at);
  Alcotest.(check bool)
    "the inline aggregate keyword keeps its expansion" true
    (Option.is_some definition.aggregate_keyword_location.generated_from);
  let field_declaration =
    List.hd definition.members |> T.expect_aggregate_member_declaration
  in
  let field =
    T.expect_primitive_specifier field_declaration.member_type_specifier
  in
  Alcotest.(check bool)
    "an inline member type keeps its macro definition" true
    (Option.is_some field.location.defined_at);
  Alcotest.(check string)
    "register spelling inside the definition remains a member name" "RAX"
    (List.hd field_declaration.member_declarators).member_name.spelling;
  Alcotest.(check (list string))
    "operand tokens retain the complete expanded inline definition"
    [ "0"; "("; "U8"; "class"; "CGenerated"; "{"; "U8"; "RAX"; ";"; "}"; ")" ]
    (List.map
       (fun (token : Ast.assembly_token) -> token.assembly_source_token.raw)
       value.inline_assembly_operand_tokens);
  let sources = Session.sources session in
  let human = Ast_dump.assembly_human sources assembly in
  let json = Ast_dump.assembly_json sources assembly in
  Alcotest.(check string)
    "inline definition human output is deterministic" human
    (Ast_dump.assembly_human sources assembly);
  Alcotest.(check string)
    "inline definition JSON output is deterministic" json
    (Ast_dump.assembly_json sources assembly);
  Alcotest.(check bool)
    "the human operand dump includes the retained declaration" true
    (T.contains human "aggregate_definition aggregate_kind=class"
    && T.contains human "CGenerated"
    && T.contains human "member_declaration");
  let open Yojson.Safe.Util in
  let operands =
    Yojson.Safe.from_string json
    |> member "items" |> to_list |> List.hd |> member "operands" |> to_list
  in
  Alcotest.(check bool)
    "an operand without definitions omits the optional JSON field" false
    (List.mem_assoc "type_definitions" (List.hd operands |> to_assoc));
  let inline =
    List.nth operands 1 |> member "type_definitions" |> to_list |> List.hd
  in
  Alcotest.(check string)
    "inline declarations reuse the ordinary aggregate JSON schema"
    "aggregate_definition"
    (inline |> member "kind" |> to_string);
  Alcotest.(check string)
    "inline declaration JSON retains its name" "CGenerated"
    (inline |> member "name" |> member "spelling" |> to_string);
  Alcotest.(check bool)
    "inline declaration JSON retains generated keyword provenance" true
    (inline |> member "aggregate_keyword" |> member "location"
   |> member "generated_from" <> `Null);
  let _, _, failed =
    standalone
      "#define FIELD MissingType\nMOV RAX,0(U8 class CBad {FIELD value;})"
  in
  let diagnostic = first_error failed.diagnostics in
  Alcotest.(check bool)
    "invalid generated members retain their definition provenance" true
    (List.exists
       (fun (related : Diagnostic.related) ->
         T.contains related.message "FIELD"
         && T.contains related.message "declared")
       diagnostic.secondary);
  T.with_temp_directory (fun directory ->
      let root_path = Filename.concat directory "root.ASM" in
      let include_path = Filename.concat directory "aggregate.HH" in
      T.write_file root_path "#include \"aggregate.HH\"";
      T.write_file include_path "MOV RAX,0(U8 class CIncluded {U8 value;})";
      let session = Session.create () in
      let source =
        Session.load_source session ~path:root_path |> Result.get_ok
      in
      let output =
        Holyc_lib.parse_assembly_detailed session ~config:(T.config directory)
          ~source
      in
      let operation =
        (expect_assembly output).assembly_unit_items |> instructions |> List.hd
      in
      let definition =
        (List.nth operation.inline_assembly_operands 1)
          .inline_assembly_type_definitions |> List.hd
      in
      let source =
        Source_manager.find (Session.sources session)
          definition.name.location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "an included inline definition keeps its canonical source"
        (Unix.realpath include_path)
        (Source_file.path source))

let unsized_warning_and_scoped_directives () =
  let _, _, output, assembly = block "INC [RAX] RET" in
  let warnings =
    List.filter
      (fun (diagnostic : Diagnostic.t) ->
        diagnostic.severity = Diagnostic.Warning)
      output.diagnostics
  in
  Alcotest.(check bool)
    "the original unsized warning preserves the accepted assembly" true
    (List.exists
       (fun (diagnostic : Diagnostic.t) -> diagnostic.code = "HCPARSE0176")
       warnings);
  Alcotest.(check bool)
    "the selected operation retains its original ambiguity warning" true
    ((List.hd (instructions assembly.assembly_items)).inline_assembly_selection
       .warnings <> []);
  let _, _, standalone_output = standalone "ORG 0 ALIGN 8,0 RET" in
  let assembly = expect_assembly standalone_output in
  Alcotest.(check (list string))
    "standalone ORG and ALIGN precede the operation" [ "RET" ]
    (instruction_names assembly.assembly_unit_items);
  let _, _, high_bit_alignment =
    standalone ~compilation_mode:Preprocessor.Jit
      "ALIGN 0x8000000000000000,0 RET"
  in
  Alcotest.(check (list string))
    "the original signed JIT ALIGN comparison accepts the high-bit power of two"
    [ "RET" ]
    (instruction_names (expect_assembly high_bit_alignment).assembly_unit_items);
  let _, _, oversized_alignment =
    standalone ~compilation_mode:Preprocessor.Jit "ALIGN 16,0 RET"
  in
  Alcotest.(check string)
    "ordinary positive JIT alignment above eight still rejects" "HCPARSE0179"
    (first_error oversized_alignment.diagnostics).code;
  List.iter
    (fun source ->
      let _, _, output = T.parse_string source in
      Alcotest.(check string)
        "function assembly preserves the original ORG/ALIGN restriction"
        "HCPARSE0153" (first_error output.diagnostics).code)
    [
      "U0 F(){ORG 0;}";
      "U0 F(){ALIGN 8,0;}";
      "U0 F(){asm {ORG 0}}";
      "U0 F(){asm {ALIGN 8,0}}";
    ]

let source_consumers () =
  let cases =
    [
      ("Compiler/CMisc.HC", 12, 46, "RET1");
      ("Compiler/Templates.HC", 9, 21, "FSUBR");
      ("Kernel/KMathA.HC", 3, 19, "FADDP");
      ("Kernel/KHashA.HC", 40, 50, "LEA");
      ("Demo/Asm/AsmAndC1.HC", 17, 30, "RET1");
      ("Demo/Asm/AsmAndC2.HC", 20, 50, "CALL");
    ]
  in
  List.iter
    (fun (path, first, last, witness) ->
      let contents = T.pinned_lines path ~first ~last in
      List.iter
        (fun compilation_mode ->
          let _, _, _, assembly =
            block ~compilation_mode ~source_context:true contents
          in
          let _, _, standalone =
            standalone ~compilation_mode ~source_context:true contents
          in
          Alcotest.(check bool)
            (path ^ " retains the audited source operation")
            true
            (List.mem witness (instruction_names assembly.assembly_items));
          Alcotest.(check (list string))
            (path ^ " shares brace and standalone form selection")
            (signature assembly.assembly_items)
            (signature (expect_assembly standalone).assembly_unit_items))
        modes)
    cases;
  let direct_contents =
    T.pinned_lines "Demo/Asm/AsmAndC3.HC" ~first:13 ~last:17
  in
  let _, _, _, items = direct ~source_context:true direct_contents in
  Alcotest.(check (list string))
    "the original direct-assembly demonstration remains accepted"
    [ "MOV"; "STC"; "ADC"; "CALL"; "MOV" ]
    (instruction_names items)

let original_graphics_address_of_members () =
  let declarations =
    String.concat "\n"
      [
        T.pinned_lines "Adam/Gr/GrGlbls.HC" ~first:3 ~last:4;
        T.pinned_lines "Adam/Gr/GrGlbls.HC" ~first:12 ~last:13;
        T.pinned_lines "Adam/Gr/GrGlbls.HC" ~first:43 ~last:43;
      ]
  in
  let body =
    String.concat "\n"
      [
        T.pinned_lines "Adam/Gr/GrAsm.HC" ~first:263 ~last:263;
        T.pinned_lines "Adam/Gr/GrTextBase.HC" ~first:76 ~last:76;
      ]
  in
  let check operations =
    Alcotest.(check (list string))
      "the original graphics operation boundaries remain intact"
      [ "MOV"; "ADD" ]
      (List.map opcode operations);
    List.iter2
      (fun member_name operation ->
        let value = operand 1 operation in
        Alcotest.(check bool)
          "the address-of global member remains indirect assembly memory" true
          (Operand.indirect value);
        Alcotest.(check int)
          "the original explicit memory width remains eight bytes" 8
          (Operand.size value);
        let immediate = Operand.immediate value |> Option.get in
        Alcotest.(check (option int64))
          "member addresses await the original layout and evaluation" None
          immediate.value;
        Alcotest.(check bool)
          "address-of and the source member name remain in the expression" true
          (List.exists
             (function
               | Operand.Operator "&" | Operand.Punctuation '&' -> true
               | _ -> false)
             immediate.expression
          && List.exists
               (function
                 | Operand.Symbol name -> name = member_name
                 | _ -> false)
               immediate.expression))
      [ "win_z_buf"; "text_base" ]
      operations
  in
  List.iter
    (fun compilation_mode ->
      check
        (program_instructions ~compilation_mode
           (declarations ^ "\nasm {\n" ^ body ^ "\n}"));
      check
        (program_instructions ~compilation_mode
           (declarations ^ "\nU0 GraphicsAssembly(){\n" ^ body ^ "\n}"));
      let session, _, declarations_output =
        T.parse_string ~compilation_mode declarations
      in
      ignore (T.expect_ast declarations_output);
      let source =
        Session.add_source session ~path:"original-graphics.ASM" ~contents:body
      in
      let output =
        Holyc_lib.parse_assembly_detailed session
          ~config:(T.config ~compilation_mode (Sys.getcwd ()))
          ~source
      in
      check ((expect_assembly output).assembly_unit_items |> instructions);
      List.iter
        (fun contents ->
          let _, _, missing_context = standalone ~compilation_mode contents in
          ignore (first_error missing_context.diagnostics);
          Alcotest.(check bool)
            "an absent export cannot acquire global-variable member syntax" true
            (Option.is_none missing_context.assembly))
        [
          body;
          T.pinned_lines "Demo/Asm/AsmAndC3.HC" ~first:17 ~last:17;
          "MOV RAX,Label.member";
        ])
    modes

let invalid_operands =
  [
    ("missing first operand", "MOV");
    ("missing comma", "MOV RAX RBX");
    ("missing second operand", "MOV RAX,");
    ("unclosed memory operand", "MOV RAX,U64 [RBP");
    ("missing displacement expression", "MOV RAX,U64 [RBP+*]");
    ("nonliteral scale", "MOV RAX,U64 [RAX*factor]");
    ("dangling expression operator", "MOV RAX,1+");
    ("register widths disagree", "MOV RAX,EAX");
    ("memory cannot fill both MOV slots", "MOV U64 [RAX],U64 [RBX]");
    ("x87 operand in integer MOV", "MOV RAX,ST1");
    ("x87 arithmetic requires ST0", "FADD ST1,ST2");
    ("byte immediate overflow", "MOV AL,256");
    ("word immediate overflow", "MOV AX,65536");
    ("generic IMUL arity is not TempleOS IMUL2", "IMUL RAX,10");
    ("generic RET arity is not TempleOS RET1", "RET 8");
    ("generic REP spelling is not a catalog opcode", "REP MOVSB");
    ("local labels cannot be exported", "@@local:: RET");
  ]

let malformed_streams () =
  List.iter
    (fun compilation_mode ->
      List.iter
        (fun (name, source) ->
          let _, _, output =
            T.parse_string ~compilation_mode ("asm {" ^ source ^ "}")
          in
          ignore (first_error output.diagnostics);
          Alcotest.(check bool)
            (name ^ " has no accepted assembly-block AST")
            true
            (Option.is_none output.ast);
          if source <> "RET 8" then (
            let _, _, output =
              T.parse_string ~compilation_mode ("U0 Bad(){" ^ source ^ "}")
            in
            ignore (first_error output.diagnostics);
            Alcotest.(check bool)
              (name ^ " has no accepted direct-assembly AST")
              true
              (Option.is_none output.ast));
          let _, _, output = standalone ~compilation_mode source in
          ignore (first_error output.diagnostics);
          Alcotest.(check bool)
            (name ^ " has no accepted standalone assembly")
            true
            (Option.is_none output.assembly))
        invalid_operands)
    modes

let direct_zero_operand_boundary () =
  let _, _, output, items = direct "RET 8;" in
  Alcotest.(check (list string))
    "RET consumes no operand before an ordinary expression statement" [ "RET" ]
    (instruction_names items);
  let rec expression_values = function
    | Ast.Expression_statement statement ->
        [
          T.expect_integer_expression statement.expression_statement_expression;
        ]
    | Ast.Block_statement block ->
        List.concat_map expression_values block.block_statements
    | Ast.Sequence_statement sequence ->
        List.concat_map
          (fun element -> expression_values element.Ast.sequence_statement)
          sequence.sequence_elements
    | _ -> []
  in
  Alcotest.(check (list int64))
    "the following integer remains a HolyC statement" [ 8L ]
    (T.expect_ast output |> T.expect_one_definition |> T.expect_function_body
   |> expression_values)

let generated_operand_provenance () =
  let session, _, output =
    T.parse_string
      "#define OP MOV\n\
       #define DST RAX\n\
       #define MEM U64 8[RBP]\n\
       asm {OP DST,MEM}"
  in
  let assembly = T.expect_ast output |> T.expect_one_top_level_assembly in
  let operation = List.hd (instructions assembly.assembly_items) in
  let tokens =
    operation.inline_assembly_opcode
    :: List.concat_map
         (fun operand -> operand.Ast.inline_assembly_operand_tokens)
         operation.inline_assembly_operands
  in
  List.iter
    (fun spelling ->
      let token =
        List.find
          (fun token -> token.Ast.assembly_source_token.raw = spelling)
          tokens
      in
      Alcotest.(check bool)
        (spelling ^ " retains the macro expansion site")
        true
        (Option.is_some token.assembly_token_location.generated_from);
      Alcotest.(check bool)
        (spelling ^ " retains the macro definition site")
        true
        (Option.is_some token.assembly_token_location.defined_at))
    [ "MOV"; "RAX"; "U64"; "8"; "RBP" ];
  Alcotest.(check int)
    "macro-expanded operands still carry validated width" 8
    (Operand.size (operand 1 operation));
  let json = Ast_dump.json (Session.sources session) (T.expect_ast output) in
  Alcotest.(check bool)
    "structured operations remain present in the public dump" true
    (T.contains json "\"generated_from\"");
  let _, _, invalid = T.parse_string "#define BAD [RAX*]\nasm {MOV RAX,BAD}" in
  let diagnostic = first_error invalid.diagnostics in
  Alcotest.(check bool)
    "operand diagnostics keep the definition expansion context" true
    (List.exists
       (fun (related : Diagnostic.related) ->
         T.contains related.message "BAD"
         && T.contains related.message "expanded")
       diagnostic.secondary)

let included_operation_provenance () =
  T.with_temp_directory (fun directory ->
      let root_path = Filename.concat directory "root.HC" in
      let include_path = Filename.concat directory "instructions.HC" in
      T.write_file root_path "#include \"instructions\"";
      T.write_file include_path "asm {MOV RAX,U64 8[RBP]}";
      let session = Session.create () in
      let source =
        Session.load_source session ~path:root_path |> Result.get_ok
      in
      let output =
        Holyc_lib.parse_detailed session ~config:(T.config directory) ~source
      in
      let assembly = T.expect_ast output |> T.expect_one_top_level_assembly in
      let operation = List.hd (instructions assembly.assembly_items) in
      let location = operation.inline_assembly_operation_location in
      let source =
        Source_manager.find (Session.sources session) location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "an operation points at the included file"
        (Unix.realpath include_path)
        (Source_file.path source);
      let memory = List.nth operation.inline_assembly_operands 1 in
      Alcotest.(check bool)
        "operand and operation retain the same included source" true
        (memory.inline_assembly_operand_location.span.source
       = location.span.source);
      T.write_file include_path "asm {MOV RAX,U64 [RBP}";
      let session = Session.create () in
      let source =
        Session.load_source session ~path:root_path |> Result.get_ok
      in
      let output =
        Holyc_lib.parse_detailed session ~config:(T.config directory) ~source
      in
      let diagnostic = first_error output.diagnostics in
      Alcotest.(check int)
        "an included operand failure retains one include origin" 1
        (List.length diagnostic.include_stack))

let operation_spans_and_dumps () =
  let contents = "asm {ENTRY:: MOV\nRAX,\nU64 8[RBP]\nJZ ENTRY RET}" in
  let session, source, output = T.parse_string contents in
  let ast = T.expect_ast output in
  let assembly = T.expect_one_top_level_assembly ast in
  let first = List.hd (instructions assembly.assembly_items) in
  let span = first.inline_assembly_operation_location.span in
  Alcotest.(check string)
    "operation span covers all operand lines and excludes the next opcode"
    "MOV\nRAX,\nU64 8[RBP]"
    (String.sub (Source_file.contents source) span.start (Span.length span));
  let sources = Session.sources session in
  Alcotest.(check string)
    "human operation dump is deterministic"
    (Ast_dump.human sources ast)
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON operation dump is deterministic"
    (Ast_dump.json sources ast)
    (Ast_dump.json sources ast);
  let _, _, again = standalone "MOV RAX,U64 8[RBP] JZ ENTRY RET" in
  let _, _, repeated = standalone "MOV RAX,U64 8[RBP] JZ ENTRY RET" in
  Alcotest.(check (list string))
    "fresh standalone parses retain identical form choices"
    (signature (expect_assembly again).assembly_unit_items)
    (signature (expect_assembly repeated).assembly_unit_items)

let labels_after_operations_bind_gotos () =
  List.iter
    (fun (source, expected) ->
      let session, _, output = T.parse_string source in
      let ast = T.expect_ast output in
      let _, _, resolution = Test_label_resolution.collect session ast in
      let function_ = Test_label_resolution.resolved_function "F" resolution in
      let labels = Semantic_label_resolution.function_labels function_ in
      Alcotest.(check (list string))
        "labels after opcodes enter the shared goto namespace" expected
        (Test_label_resolution.label_names function_);
      Alcotest.(check (list int))
        "each goto binds its following assembly definition"
        (List.map (fun _ -> 1) expected)
        (List.map Semantic_label_resolution.label_goto_count labels);
      Alcotest.(check (list string))
        "bound definitions retain their assembly kind"
        (List.map (fun _ -> "assembly-global") expected)
        (List.map
           (fun label ->
             Semantic_label_resolution.label_definition_kind label
             |> Semantic_label_resolution.definition_kind_name)
           labels))
    [
      ("U0 F(){goto later;asm {NOP later: RET}}", [ "later" ]);
      ( "U0 F(){goto later;goto last;asm {NOP later: NOP last: RET}}",
        [ "later"; "last" ] );
    ]

let tests =
  List.map
    (fun (name, test) -> Alcotest.test_case name `Quick test)
    [
      ( "line-independent brace, direct and standalone streams",
        line_independent_streams );
      ( "ordered labels and original prefix opcodes",
        labels_and_original_prefixes );
      ( "USE mode transitions and original entry resets",
        use_modes_and_entry_reset );
      ( "operand sizes, extended registers and addresses",
        sizes_registers_and_addresses );
      ( "aliases, x87 and TempleOS opcode arity",
        aliases_x87_and_nonstandard_arity );
      ( "numeric, symbolic and relative immediates",
        numeric_and_symbolic_immediates );
      ( "typed indices preserve assembler expression boundaries",
        typed_expression_boundaries );
      ( "queries, member names and literals retain deferred expressions",
        deferred_expression_shapes );
      ( "declared class offsets retain narrow immediate forms",
        declared_class_offsets );
      ( "literal and label modifiers follow source states",
        source_modifier_boundaries );
      ( "declared function shapes bound assembly expressions",
        declared_function_call_shapes );
      ( "source array and function-pointer cast declarators",
        source_cast_declarators );
      ( "variable and cast calls retain source callable shapes",
        variable_and_cast_call_shapes );
      ( "function addresses follow original declaration lifecycles",
        function_address_lifecycles );
      ( "function address evidence remains an immutable operand snapshot",
        function_address_evidence_snapshots );
      ( "inline aggregate casts retain definitions and publication",
        inline_aggregate_casts );
      ( "inline aggregate casts retain source provenance",
        inline_aggregate_cast_provenance );
      ( "unsized warnings and function directive restrictions",
        unsized_warning_and_scoped_directives );
      ("original Compiler, Kernel and Demo consumers", source_consumers);
      ( "original graphics address-of member operands",
        original_graphics_address_of_members );
      ("malformed and incompatible operands reject", malformed_streams);
      ( "direct zero-operand instruction leaves following language expression",
        direct_zero_operand_boundary );
      ( "macro operands and diagnostics retain provenance",
        generated_operand_provenance );
      ( "included operations and errors retain provenance",
        included_operation_provenance );
      ( "complete operation spans and deterministic dumps",
        operation_spans_and_dumps );
      ( "assembly labels after opcodes bind language gotos",
        labels_after_operations_bind_gotos );
    ]
