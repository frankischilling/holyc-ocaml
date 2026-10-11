open Holyc_lib
module Encoding = Assembly_encoding

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let config ?(compilation_mode = Preprocessor.Jit) () =
  match
    Preprocessor.Config.create ~working_directory:(Sys.getcwd ())
      ~compilation_mode ()
  with
  | Ok config -> config
  | Error message -> Alcotest.fail message

let parse_output ?(compilation_mode = Preprocessor.Jit) contents =
  let session = Session.create () in
  let config = config ~compilation_mode () in
  let source =
    Session.add_source session ~path:"assembly-directives.ASM" ~contents
  in
  let output = Holyc_lib.parse_assembly_detailed session ~config ~source in
  (session, config, output)

let parse ?compilation_mode contents =
  let session, config, output = parse_output ?compilation_mode contents in
  match output.Parser.assembly with
  | Some assembly -> (session, config, assembly)
  | None ->
      Alcotest.failf "directive fixture did not parse: %s"
        (String.concat "; "
           (List.map
              (fun (diagnostic : Diagnostic.t) ->
                diagnostic.code ^ ": " ^ diagnostic.message)
              output.diagnostics))

let hex bytes =
  let digits = "0123456789abcdef" in
  String.init
    (2 * String.length bytes)
    (fun index ->
      let byte = Char.code bytes.[index / 2] in
      digits.[if index mod 2 = 0 then byte lsr 4 else byte land 15])

let emit ?position ?max_work ?max_binary_input_bytes ?(max_code_bytes = 4096)
    config assembly =
  match
    Encoding.encode ?position ~config ?max_work ?max_binary_input_bytes
      ~max_code_bytes assembly
  with
  | Ok encoded -> encoded
  | Error diagnostic ->
      Alcotest.failf "directive encoding failed: %s: %s" diagnostic.code
        diagnostic.message

let reject ?position ?max_work ?max_binary_input_bytes ?(max_code_bytes = 4096)
    ?code label config assembly =
  match
    Encoding.encode ?position ~config ?max_work ?max_binary_input_bytes
      ~max_code_bytes assembly
  with
  | Ok encoded ->
      Alcotest.failf "%s unexpectedly emitted %d bytes" label
        (String.length encoded.bytes)
  | Error diagnostic ->
      Option.iter
        (fun code ->
          Alcotest.(check string)
            (label ^ " diagnostic code")
            code diagnostic.code)
        code;
      Alcotest.(check bool)
        (label ^ " is an error") true
        (diagnostic.severity = Diagnostic.Error);
      diagnostic

let check_bytes ?position ?max_code_bytes compilation_mode source expected =
  let _, config, assembly = parse ~compilation_mode source in
  let encoded = emit ?position ?max_code_bytes config assembly in
  Alcotest.(check string) source expected (hex encoded.bytes);
  encoded

(* Expected data bytes follow Asm.HC:268-390,815-885. These literals are
   independent of the encoder and contain no captured TempleOS execution. *)
let widths_and_truncation () =
  List.iter
    (fun compilation_mode ->
      List.iter
        (fun (source, expected) ->
          ignore (check_bytes compilation_mode source expected))
        [
          ("DU8 0x123,-1;", "23ff");
          ("DU16 0x11223344,-1;", "4433ffff");
          ("DU32 0x1122334455667788,-1;", "88776655ffffffff");
          ("DU64 0x1122334455667788,-1;", "8877665544332211ffffffffffffffff");
          ("DU16 1 2,3,;", "010002000300");
          ("DU8; DU16; DU32; DU64;", "");
          ("DU8 'AB'; DU16 'AB';", "414142");
        ])
    modes

let strings_are_byte_sequences () =
  List.iter
    (fun compilation_mode ->
      List.iter
        (fun (width, tail) ->
          ignore
            (check_bytes compilation_mode
               (Printf.sprintf "DU%d \"A\\0B\",\"\",\"C\",0x1234;" width)
               ("41004243" ^ tail)))
        [ (8, "34"); (16, "3412"); (32, "34120000"); (64, "3412000000000000") ];
      ignore
        (check_bytes compilation_mode
           "DU64 \"A\" \"B\",0x1122334455667788; DU16 \"C\";"
           "4142887766554433221143"))
    modes

let scalar_expressions () =
  List.iter
    (fun compilation_mode ->
      List.iter
        (fun (source, expected) ->
          ignore (check_bytes compilation_mode source expected))
        [
          ("DU8 1+2*3,(1+2)*3,~0,1<<7;", "0709ff80");
          ("DU16 257(U8),255(I8),65537(U16);", "0101ff000100");
          ("DU64 4294967295(I32);", "ffffffff00000000");
          ("DU64 1.0,-2.5;", "000000000000f03f00000000000004c0");
          ("DU64 1.0+2.5;", "0000000000000c40");
          ("DU64 3.75(I64),3(F64);", "0000000000000e400300000000000000");
          ("DU8 1<2<3,3<2<1,0||5,2&&3;", "01000101");
          ("DU8 0(U64)<1<-1,-2.0<0xFFFFFFFFFFFFFFFF<0;", "0101");
          ("DU64 1+2<<3;", "1100000000000000");
          ( "DU64 0xFFFFFFFFFFFFFFFF>>63,(-1)>>63;",
            "0100000000000000ffffffffffffffff" );
          ("DU64 (~0(U64))>>63;", "0100000000000000");
          ("DU8 (~0)>>63;", "ff");
          ("DU64 -2`2;", "00000000000010c0");
          ( "DU64 0`-1,(-4.0)`0.5,(-4.0)`1.5,(-4.0)`-1.5;",
            "0000000000000000000000000000004000000000000020c0000000000000c0bf"
          );
        ])
    modes

let instruction_expression_layout () =
  List.iter
    (fun compilation_mode ->
      let zero = check_bytes compilation_mode "MOV RAX,[RBX+$$];" "488b03" in
      Alcotest.(check int)
        "a known zero displacement uses the original displacement-free form" 3
        (List.hd zero.instructions).size;
      ignore
        (check_bytes compilation_mode "DU8 1; MOV RAX,[RBX+$$];" "01488b4301");
      let floating =
        check_bytes compilation_mode "MOV RAX,1.0;" "48b8000000000000f03f"
      in
      Alcotest.(check int)
        "an evaluated F64 payload selects the complete immediate field" 10
        (List.hd floating.instructions).size;
      let _, config, forward =
        parse ~compilation_mode "MOV RAX,[RBX+Later]; Later: RET;"
      in
      ignore
        (reject ~code:"HCASM0004"
           "a forward displacement requiring a resized instruction needs layout"
           config forward))
    modes

let repetition_and_current_position () =
  List.iter
    (fun compilation_mode ->
      ignore
        (check_bytes compilation_mode
           "DU16 3 DUP(0x1234),0 DUP(7),-2 DUP(9),0xABCD;" "341234123412cdab");
      ignore
        (check_bytes compilation_mode
           "DU8 0 DUP(1),-9223372036854775808 DUP(2),7;" "07");
      ignore (check_bytes compilation_mode "DU8 $$,$$;" "0001");
      ignore (check_bytes compilation_mode "DU8 2 DUP($$),$$;" "000002");
      let encoded =
        check_bytes compilation_mode
          "ORG 0x1003; DU8 0xAA; DU16 $$; DU16 $$-$$; DU8 3 DUP($$);"
          "aa01000000050505"
      in
      Alcotest.(check (option int64))
        "ORG is retained without changing raw current-position data"
        (Some 0x1003L) encoded.origin;
      let relocations =
        encoded.relocations
        |> List.map (fun (relocation : Encoding.relocation) ->
            (relocation.offset, relocation.width, relocation.kind))
        |> List.sort compare
      in
      Alcotest.(check (list (triple int int string)))
        "absolute current-position parity produces one record per field"
        [
          (1, 2, "AAT_ADD_U16");
          (5, 1, "AAT_ADD_U8");
          (6, 1, "AAT_ADD_U8");
          (7, 1, "AAT_ADD_U8");
        ]
        relocations;
      let multiplied =
        check_bytes compilation_mode "DU8 1; DU16 0*$$;" "010000"
      in
      Alcotest.(check int)
        "address parity survives multiplication by zero" 1
        (List.length multiplied.relocations);
      let _, config, unresolved =
        parse ~compilation_mode "DU8 0 DUP(MissingValue);"
      in
      ignore
        (reject "zero repetition still validates its value" config unresolved))
    modes

let malformed_repetition () =
  List.iter
    (fun compilation_mode ->
      List.iter
        (fun source ->
          let _, _, output = parse_output ~compilation_mode source in
          Alcotest.(check bool)
            ("malformed repetition has no assembly: " ^ source)
            true
            (Option.is_none output.Parser.assembly);
          Alcotest.(check bool)
            ("malformed repetition has an error: " ^ source)
            true
            (List.exists
               (fun (diagnostic : Diagnostic.t) ->
                 diagnostic.severity = Diagnostic.Error)
               output.diagnostics))
        [
          "DU8 2 DUP(3 DUP(4));";
          "DU16 2 DUP(1,2);";
          "DU32 2 DUP();";
          "DU64 2 DUP(1;";
          "DU8 2 DUP 1;";
          "DU8 DUP(1);";
        ])
    modes

let origins_and_alignment () =
  List.iter
    (fun compilation_mode ->
      let encoded =
        check_bytes compilation_mode
          "IMPORT; NOLIST; DU8; USE16; ORG 0x1003; DU8 1; ALIGN 8,0x190; DU8 2;"
          "019090909090909002"
      in
      Alcotest.(check (option int64))
        "origin value" (Some 0x1003L) encoded.origin;
      Alcotest.(check int64)
        "origin leaves raw output position unchanged" 0L encoded.position;
      Alcotest.(check int64)
        "end position advances by raw emitted bytes" 9L encoded.end_position;
      Alcotest.(check int) "alignment exponent" 3 encoded.max_align_bits;
      let already_aligned =
        check_bytes compilation_mode
          "DU8 1; ALIGN 1,0xCC; ALIGN 8,0xCC; ALIGN 8,0xDD;" "01cccccccccccccc"
      in
      Alcotest.(check int)
        "repeated alignment does not emit another block" 8
        (String.length already_aligned.bytes);
      let high_bit =
        check_bytes compilation_mode
          "DU8 1; ALIGN 0x8000000000000000,0xCC; DU8 2;" "0102"
      in
      Alcotest.(check int)
        "original high-bit alignment retains its exponent" 63
        high_bit.max_align_bits;
      let sentinel =
        check_bytes compilation_mode "ORG 9223372036854775807; DU8 42;" "2a"
      in
      Alcotest.(check (option int64))
        "the original invalid-origin sentinel stays unset" None sentinel.origin;
      let replaced_sentinel =
        check_bytes compilation_mode
          "ORG 9223372036854775807; ORG 0x1234; DU8 42;" "2a"
      in
      Alcotest.(check (option int64))
        "an original sentinel origin permits a later origin" (Some 0x1234L)
        replaced_sentinel.origin;
      let _, config, origin = parse ~compilation_mode "ORG 0;" in
      ignore
        (reject ~position:1L "ORG requires an initial raw position of zero"
           config origin);
      List.iter
        (fun source ->
          let _, config, assembly = parse ~compilation_mode source in
          ignore (reject source config assembly))
        [ "ORG 1; ORG 1;"; "DU8 0; ORG 1;"; "NOP; ORG 0;" ])
    modes;
  ignore
    (check_bytes Preprocessor.Aot "DU8 1; ALIGN 16,0xAB;"
       "01ababababababababababababababab");
  List.iter
    (fun source ->
      let _, _, output =
        parse_output ~compilation_mode:Preprocessor.Jit source
      in
      Alcotest.(check bool)
        ("JIT alignment syntax rejects: " ^ source)
        true
        (Option.is_none output.Parser.assembly))
    [ "ALIGN 16,0;"; "ALIGN 0,0;"; "ALIGN 3,0;" ];
  let _, aot_config, assembly =
    parse ~compilation_mode:Preprocessor.Aot "DU8 1; ALIGN 16,0;"
  in
  ignore (emit aot_config assembly);
  ignore
    (reject "public encoder applies the configured JIT ALIGN bound"
       (config ~compilation_mode:Preprocessor.Jit ())
       assembly)

let mixed_modes_and_receipts () =
  let source =
    "ORG 0x1003; NOLIST; DU8 1; ALIGN 8,0x90; LIST; USE16; MOV AX,0x1234; \
     USE32; MOV EAX,0x12345678; NOLIST; DU16 0xABCD; USE64; RET;"
  in
  List.iter
    (fun compilation_mode ->
      let session, config, assembly = parse ~compilation_mode source in
      let encoded = emit ~max_code_bytes:19 config assembly in
      Alcotest.(check string)
        "mixed instructions and data bytes"
        "0190909090909090b83412b878563412cdabc3" (hex encoded.bytes);
      Alcotest.(check bool)
        "directive emission retains the exact parsed unit" true
        (encoded.assembly == assembly);
      Alcotest.(check (list int))
        "instructions use offsets after directive data" [ 8; 11; 18 ]
        (List.map
           (fun (entry : Encoding.instruction) -> entry.offset)
           encoded.instructions);
      Alcotest.(check (list int))
        "USE changes reach the instruction receipts" [ 16; 32; 64 ]
        (List.map
           (fun (entry : Encoding.instruction) ->
             Asm_operand.mode_bits entry.operation.inline_assembly_mode)
           encoded.instructions);
      List.iter
        (fun (entry : Encoding.instruction) ->
          match List.nth assembly.assembly_unit_items entry.item_index with
          | Ast.Inline_assembly_instruction operation ->
              Alcotest.(check bool)
                "instruction receipts retain original operation ownership" true
                (entry.operation == operation)
          | _ -> Alcotest.fail "instruction receipt names a directive item")
        encoded.instructions;
      let data_entries =
        encoded.entries
        |> List.filter (fun (entry : Encoding.entry) -> entry.size > 0)
      in
      Alcotest.(check (list bool))
        "LIST and NOLIST control subsequent emitted ranges"
        [ false; false; true; true; false; false ]
        (List.map (fun (entry : Encoding.entry) -> entry.listing) data_entries);
      List.iter
        (fun (entry : Encoding.entry) ->
          let original =
            List.nth assembly.assembly_unit_items entry.item_index
          in
          let location =
            match original with
            | Ast.Inline_assembly_instruction operation ->
                operation.inline_assembly_operation_location
            | Ast.Inline_assembly_directive directive ->
                directive.inline_assembly_directive_location
            | Ast.Inline_assembly_label label -> label.assembly_label_location
            | Ast.Inline_assembly_empty _ ->
                Alcotest.fail "an emitted range names an empty separator"
          in
          Alcotest.(check bool)
            "emitted ranges retain their original source span" true
            (entry.location.span = location.span))
        data_entries;
      let json = Encoding.to_yojson (Session.sources session) encoded in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "JSON retains complete directive AST"
        (Ast_dump.assembly_to_yojson (Session.sources session) assembly
        |> Yojson.Safe.to_string)
        (json |> member "assembly" |> Yojson.Safe.to_string);
      let listed =
        json |> member "entries" |> to_list
        |> List.filter (fun item -> item |> member "size" |> to_int > 0)
        |> List.map (fun item -> item |> member "listing" |> to_bool)
      in
      Alcotest.(check (list bool))
        "JSON retains per-range listing state"
        [ false; false; true; true; false; false ]
        listed)
    modes

let imports_and_forward_data () =
  List.iter
    (fun compilation_mode ->
      let encoded =
        check_bytes compilation_mode
          "IMPORT; IMPORT ImportedOne,ImportedOne ImportedTwo,; DU8 42;" "2a"
      in
      Alcotest.(check (list string))
        "unused imports retain declaration order and duplicates"
        [ "ImportedOne"; "ImportedOne"; "ImportedTwo" ]
        encoded.imports;
      Alcotest.(check int)
        "unused imports produce no relocation" 0
        (List.length encoded.relocations);
      let keyword_import = check_bytes compilation_mode "IMPORT if;" "" in
      Alcotest.(check (list string))
        "IMPORT accepts an original identifier with a HolyC keyword spelling"
        [ "if" ] keyword_import.imports;
      ignore (check_bytes compilation_mode "IMPORT @@x; DU8 42;" "2a");
      let _, config, imported_local =
        parse ~compilation_mode "IMPORT @@x; @@x: DU8 1;"
      in
      ignore
        (reject ~code:"HCASM0004"
           "a bare imported local spelling cannot define a local label" config
           imported_local);
      ignore (check_bytes compilation_mode "@@x: IMPORT @@x; @@x: DU8 1;" "01");
      let forward_local =
        check_bytes compilation_mode "DU64 @@x; IMPORT @@x; @@x: DU8 1;"
          "080000000000000001"
      in
      Alcotest.(check (list (triple int int string)))
        "a forward local binding precedes a later global import"
        [ (0, 8, "AAT_ADD_U64") ]
        (List.map
           (fun (relocation : Encoding.relocation) ->
             (relocation.offset, relocation.width, relocation.kind))
           forward_local.relocations);
      List.iter
        (fun source ->
          let _, _, output = parse_output ~compilation_mode source in
          Alcotest.(check bool)
            ("imported spelling cannot remain a directive: " ^ source)
            true
            (Option.is_none output.Parser.assembly);
          Alcotest.(check bool)
            "import shadowing emits a parser error" true
            (List.exists
               (fun (diagnostic : Diagnostic.t) ->
                 diagnostic.severity = Diagnostic.Error)
               output.diagnostics))
        [ "IMPORT DU8; DU8 1;" ];
      let forward =
        check_bytes compilation_mode "DU16 Later; DU8 0xAA; Later: DU8 $$;"
          "0300aa03"
      in
      Alcotest.(check (list (triple int int string)))
        "forward data and current position retain relocation widths"
        [ (0, 2, "AAT_ADD_U16"); (3, 1, "AAT_ADD_U8") ]
        (forward.relocations
        |> List.map (fun (relocation : Encoding.relocation) ->
            (relocation.offset, relocation.width, relocation.kind))
        |> List.sort compare);
      let _, _, imported_immediate =
        parse ~compilation_mode "IMPORT F; MOV RAX,F;"
      in
      let operation =
        imported_immediate.assembly_unit_items
        |> List.filter_map (function
          | Ast.Inline_assembly_instruction operation -> Some operation
          | _ -> None)
        |> List.hd
      in
      let operand = List.nth operation.inline_assembly_operands 1 in
      (match Asm_operand.immediate operand.inline_assembly_operand with
      | Some immediate ->
          Alcotest.(check bool)
            "an imported symbol contributes no absolute-address count" false
            immediate.absolute
      | None -> Alcotest.fail "the imported MOV source is not an immediate");
      List.iter
        (fun source ->
          let _, config, assembly = parse ~compilation_mode source in
          ignore
            (reject ~code:"HCASM0004" "import use requires external linkage"
               config assembly))
        [
          "IMPORT ExternalValue; DU64 ExternalValue;";
          "IMPORT F; CALL F;";
          "IMPORT if; DU64 if;";
          "IMPORT sizeof; DU64 sizeof;";
          "IMPORT offset; DU64 offset;";
          "IMPORT defined; DU64 defined;";
        ])
    modes

let function_assembly_import_scope () =
  let error_codes (output : Parser.output) =
    output.diagnostics
    |> List.filter (fun (diagnostic : Diagnostic.t) ->
        diagnostic.severity = Diagnostic.Error)
    |> List.map (fun (diagnostic : Diagnostic.t) -> diagnostic.code)
  in
  let expect_clean label (output : Parser.output) =
    match output.ast with
    | Some ast when error_codes output = [] -> ast
    | _ ->
        Alcotest.failf "%s: %s" label
          (String.concat "; "
             (List.map
                (fun (diagnostic : Diagnostic.t) ->
                  diagnostic.code ^ ": " ^ diagnostic.message)
                output.diagnostics))
  in
  let expect_absent label session name =
    match
      Symbol_visibility.Environment.find_preprocessor (Session.symbols session)
        name
    with
    | Symbol_visibility.Absent -> ()
    | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
        Alcotest.failf "%s: imported name %S remains visible" label name
  in
  let rec has_if_statement = function
    | Ast.If_statement _ -> true
    | Ast.Block_statement block ->
        List.exists has_if_statement block.block_statements
    | Ast.Sequence_statement sequence ->
        List.exists
          (fun (element : Ast.statement_sequence_element) ->
            has_if_statement element.sequence_statement)
          sequence.sequence_elements
    | _ -> false
  in
  let expect_if label (ast : Ast.module_) =
    Alcotest.(check bool)
      label true
      (List.exists
         (function
           | Ast.Function_definition definition ->
               Option.fold ~none:false ~some:has_if_statement definition.body
           | Ast.Top_level_statement statement -> has_if_statement statement
           | _ -> false)
         ast.items)
  in
  List.iter
    (fun compilation_mode ->
      let config = config ~compilation_mode () in
      let parse_source session path contents =
        let source = Session.add_source session ~path ~contents in
        Holyc_lib.parse_detailed session ~config ~source
      in
      List.iter
        (fun (label, wrap) ->
          let session = Session.create () in
          let restored =
            parse_source session "import-keyword-scope.HC"
              ("U0 Good(){" ^ wrap "IMPORT if; DU64 if; NOLIST;" ^ "if(1);}")
            |> expect_clean (label ^ " keyword restoration")
          in
          expect_if (label ^ " restores the surrounding if keyword") restored;
          let class_session = Session.create () in
          ignore
            (parse_source class_session "import-class-scope.HC"
               ("class Kept{I64 field;}; U0 Good(){"
               ^ wrap "IMPORT Kept; DU64 Kept; NOLIST;"
               ^ "} Kept after;")
            |> expect_clean (label ^ " retains the surrounding class"));
          let absent_session = Session.create () in
          let absent =
            parse_source absent_session "import-ended-scope.HC"
              ("U0 GoneAfter(){"
              ^ wrap "IMPORT Gone; DU64 Gone; NOLIST;"
              ^ "} Gone;")
          in
          Alcotest.(check (list string))
            (label ^ " import is unavailable in the following HolyC statement")
            [ "HCPARSE0001" ] (error_codes absent);
          expect_absent (label ^ " completed scope") absent_session "Gone";
          let visible =
            parse_source (Session.create ()) "import-active-scope.HC"
              ("U0 StillActive(){" ^ wrap "IMPORT DU8; DU8 1;" ^ "}")
          in
          Alcotest.(check bool)
            (label ^ " import shadows a directive while the assembly is active")
            true
            (Option.is_none visible.ast && error_codes visible <> []);
          let recovery_session = Session.create () in
          let recovered =
            parse_source recovery_session "import-recovery-scope.HC"
              ("class Kept{I64 field;}; U0 Broken(){"
              ^ wrap "IMPORT Kept,if,Gone; DU8 ,;"
              ^ "}")
          in
          Alcotest.(check bool)
            (label ^ " reaches the malformed data directive")
            true
            (Option.is_none recovered.ast
            && List.mem "HCPARSE0155" (error_codes recovered));
          expect_absent (label ^ " failed scope") recovery_session "Gone";
          let after_recovery =
            parse_source recovery_session "import-after-recovery.HC"
              "Kept after; U0 AfterRecovery(){if(1);}"
            |> expect_clean (label ^ " same-session recovery")
          in
          expect_if
            (label ^ " error recovery retains the keyword binding")
            after_recovery;
          let still_absent =
            parse_source recovery_session "import-after-recovery-missing.HC"
              "Gone;"
          in
          Alcotest.(check (list string))
            (label ^ " failed assembly does not publish imports to later input")
            [ "HCPARSE0001" ] (error_codes still_absent))
        [
          ("function asm block", fun body -> "asm {" ^ body ^ "}");
          ("direct function assembly run", Fun.id);
        ])
    modes;
  let jit_session = Session.create () in
  let jit_config = config ~compilation_mode:Preprocessor.Jit () in
  let parse_jit_source path contents =
    let source = Session.add_source jit_session ~path ~contents in
    Holyc_lib.parse_detailed jit_session ~config:jit_config ~source
  in
  let jit_after_block =
    parse_jit_source "import-top-level-jit.HC"
      "asm {IMPORT Temporary; DU64 Temporary;} Temporary;"
  in
  Alcotest.(check (list string))
    "top-level JIT imports are unavailable after the block" [ "HCPARSE0001" ]
    (error_codes jit_after_block);
  expect_absent "completed top-level JIT scope" jit_session "Temporary";
  let jit_later_input =
    parse_jit_source "import-top-level-jit-next.HC" "Temporary;"
  in
  Alcotest.(check (list string))
    "top-level JIT imports are unavailable to later input" [ "HCPARSE0001" ]
    (error_codes jit_later_input);
  let session = Session.create () in
  let config = config ~compilation_mode:Preprocessor.Aot () in
  let parse_source path contents =
    let source = Session.add_source session ~path ~contents in
    Holyc_lib.parse_detailed session ~config ~source
  in
  let expect_persistent_import label =
    match
      Symbol_visibility.Environment.find_preprocessor (Session.symbols session)
        "Persistent"
    with
    | Symbol_visibility.Present entry ->
        Alcotest.(check bool)
          label true
          (Symbol_visibility.is_assembly_import entry)
    | Symbol_visibility.Absent | Symbol_visibility.Shadowed_by_local ->
        Alcotest.failf "%s: the AOT assembly import is unavailable" label
  in
  ignore
    (parse_source "import-top-level-aot.HC"
       "asm {IMPORT Persistent; DU64 Persistent;} asm {DU64 Persistent;}"
    |> expect_clean "top-level AOT imports remain visible after the block");
  expect_persistent_import "later AOT blocks retain the original import";
  ignore
    (parse_source "import-top-level-aot-next.HC" "asm {DU64 Persistent;}"
    |> expect_clean "top-level AOT imports remain visible to later input");
  expect_persistent_import "later AOT input retains the original import"

let primitive_cast_source_identity () =
  List.iter
    (fun compilation_mode ->
      List.iter
        (fun name ->
          let session = Session.create () in
          let config = config ~compilation_mode () in
          let declaration =
            Session.add_source session ~path:"directive-cast-class.HC"
              ~contents:(Printf.sprintf "class %s{I64 field;};" name)
          in
          let declared =
            Holyc_lib.parse_detailed session ~config ~source:declaration
          in
          Alcotest.(check bool)
            ("same-spelled class declaration parses: " ^ name)
            true
            (Option.is_some declared.Parser.ast);
          let source =
            Session.add_source session ~path:"directive-cast-class.ASM"
              ~contents:(Printf.sprintf "DU64 257(%s);" name)
          in
          let output =
            Holyc_lib.parse_assembly_detailed session ~config ~source
          in
          let assembly =
            match output.Parser.assembly with
            | Some assembly -> assembly
            | None ->
                Alcotest.failf "class cast should retain its parsed type: %s"
                  (String.concat "; "
                     (List.map
                        (fun (diagnostic : Diagnostic.t) -> diagnostic.message)
                        output.diagnostics))
          in
          ignore
            (reject ~code:"HCASM0004"
               ("same-spelled class does not authorize primitive bits: " ^ name)
               config assembly))
        [ "U8"; "U64"; "F64" ];
      List.iter
        (fun name ->
          let _, config, output =
            parse_output ~compilation_mode
              (Printf.sprintf "IMPORT %s; DU64 257(%s);" name name)
          in
          match output.Parser.assembly with
          | Some assembly ->
              ignore
                (reject
                   ("imported spelling does not authorize a primitive cast: "
                  ^ name)
                   config assembly)
          | None ->
              Alcotest.(check bool)
                "imported cast spelling has an explicit parser error" true
                (List.exists
                   (fun (diagnostic : Diagnostic.t) ->
                     diagnostic.severity = Diagnostic.Error)
                   output.diagnostics))
        [ "U8"; "U64"; "F64" ])
    modes

let data_label_scopes () =
  List.iter
    (fun compilation_mode ->
      let encoded =
        check_bytes compilation_mode
          "First:: DU16 @@tail; OrdinaryOne: DU8 0xAA; @@tail: DU8 $$; \
           Second:: DU16 @@tail; OrdinaryTwo: DU8 0xBB; @@tail: DU8 $$;"
          "0300aa030700bb07"
      in
      Alcotest.(check (list (pair string int64)))
        "exported labels retain their raw positions"
        [ ("First", 0L); ("Second", 4L) ]
        (encoded.symbols
        |> List.filter (fun (symbol : Encoding.symbol) -> symbol.exported)
        |> List.map (fun (symbol : Encoding.symbol) ->
            (symbol.name, symbol.position)));
      let local_scopes =
        encoded.symbols
        |> List.filter_map (fun (symbol : Encoding.symbol) ->
            Option.map (fun scope -> (scope, symbol.position)) symbol.scope)
        |> List.sort compare
      in
      Alcotest.(check (list (pair string int64)))
        "ordinary labels do not reset exported local-label scopes"
        [ ("First", 3L); ("Second", 7L) ]
        local_scopes;
      ignore (check_bytes compilation_mode "Repeated: Repeated: DU8 42;" "2a");
      let _, config, repeated =
        parse ~compilation_mode "Repeated: DU8 42; Repeated:"
      in
      ignore
        (reject "a repeated label cannot acquire another output position" config
           repeated))
    modes

let inserted_binary_tokens () =
  let saved_source text =
    let buffer = Buffer.create 96 in
    let word value =
      for byte = 0 to 3 do
        Buffer.add_char buffer (Char.chr ((value lsr (8 * byte)) land 0xff))
      done
    in
    Buffer.add_string buffer text;
    Buffer.add_char buffer '\000';
    word 7;
    word 0;
    word 3;
    word 1;
    Buffer.add_string buffer "\xAA\000\xCC";
    Buffer.contents buffer
  in
  let parse_saved compilation_mode text =
    let session = Session.create () in
    let config =
      match
        Preprocessor.Config.create ~working_directory:(Sys.getcwd ())
          ~compilation_mode ~physical_nul_terminates:true ()
      with
      | Ok config -> config
      | Error message -> Alcotest.fail message
    in
    let source =
      Session.add_source session ~path:"assembly-directives-saved.ASM"
        ~contents:(saved_source text)
    in
    let output = Holyc_lib.parse_assembly_detailed session ~config ~source in
    match output.Parser.assembly with
    | Some assembly -> (session, config, assembly)
    | None ->
        Alcotest.failf "saved directive did not parse: %s"
          (String.concat "; "
             (List.map
                (fun (diagnostic : Diagnostic.t) ->
                  diagnostic.code ^ ": " ^ diagnostic.message)
                output.diagnostics))
  in
  List.iter
    (fun compilation_mode ->
      let _, config, assembly =
        parse_saved compilation_mode "DU16 $IS,\"size\",BI=7$;"
      in
      let encoded = emit config assembly in
      Alcotest.(check string)
        "inserted binary size emits its integer width" "0300"
        (hex encoded.bytes);
      let argument =
        assembly.assembly_unit_items
        |> List.find_map (function
          | Ast.Inline_assembly_directive directive ->
              List.find_opt
                (fun (token : Ast.assembly_token) ->
                  token.assembly_source_token.kind
                  = Token_kind.Inserted_binary_size)
                directive.inline_assembly_directive_arguments
          | _ -> None)
        |> Option.get
      in
      let record = Option.get argument.assembly_source_token.binary_record in
      Alcotest.(check int64) "inserted size record number" 7L record.number;
      Alcotest.(check int64)
        "inserted size record extent" 3L record.declared_size;
      Alcotest.(check bool)
        "inserted size retains payload completeness" true
        record.payload_complete;
      let _, config, pointer =
        parse_saved compilation_mode "DU8 $IB,\"payload\",BI=7$;"
      in
      ignore
        (reject ~code:"HCASM0004"
           "inserted binary pointer needs compiler-owned data storage" config
           pointer))
    modes

let output_and_work_limits () =
  List.iter
    (fun compilation_mode ->
      let _, config, assembly =
        parse ~compilation_mode "DU8 1; ALIGN 8,0x90; DU16 2 DUP(0x1234); RET;"
      in
      let encoded = emit ~max_code_bytes:13 config assembly in
      Alcotest.(check string)
        "exact mixed-output limit" "019090909090909034123412c3"
        (hex encoded.bytes);
      ignore
        (reject ~max_code_bytes:12 ~code:"HCASM0002"
           "mixed-output limit one byte short" config assembly);
      Alcotest.(check bool)
        "directive execution reports charged work" true
        (encoded.work >= String.length encoded.bytes);
      let exact =
        emit ~max_code_bytes:13 ~max_work:encoded.work config assembly
      in
      Alcotest.(check string)
        "exact work limit preserves the output" encoded.bytes exact.bytes;
      ignore
        (reject ~max_work:(encoded.work - 1)
           "work limit one step below the completed work" config assembly);
      ignore (reject ~max_work:0 "zero work allowance" config assembly);
      ignore (reject ~max_work:(-1) "negative work allowance" config assembly);
      let _, config, repeated =
        parse ~compilation_mode "DU64 9223372036854775807 DUP(0x7E);"
      in
      ignore
        (reject ~max_code_bytes:64 ~code:"HCASM0002"
           "repetition multiplication exceeds the output bound" config repeated);
      let _, config, words = parse ~compilation_mode "DU64 1; DU8 2;" in
      let last =
        emit
          ~position:(Int64.sub Int64.max_int 9L)
          ~max_code_bytes:9 config words
      in
      Alcotest.(check int64)
        "data can end at the final signed output address" Int64.max_int
        last.end_position;
      ignore
        (reject
           ~position:(Int64.sub Int64.max_int 8L)
           ~max_code_bytes:9 ~code:"HCASM0003"
           "later data exceeds the signed output position" config words))
    modes

let macro_error_provenance () =
  List.iter
    (fun compilation_mode ->
      let _, config, assembly =
        parse ~compilation_mode
          "#define TOO_MUCH DU64 8 DUP(0x7E);\nDU8 1;\nTOO_MUCH\n"
      in
      let diagnostic =
        reject ~max_code_bytes:8 ~code:"HCASM0002"
          "expanded directive exceeds output limit" config assembly
      in
      Alcotest.(check bool)
        "directive diagnostic preserves expansion and definition sites" true
        (List.length diagnostic.secondary >= 2))
    modes

let tests =
  [
    Alcotest.test_case "directive scalar widths and low-byte truncation" `Quick
      widths_and_truncation;
    Alcotest.test_case "strings retain raw bytes at every DU width" `Quick
      strings_are_byte_sequences;
    Alcotest.test_case "closed typed expressions and F64 payloads" `Quick
      scalar_expressions;
    Alcotest.test_case "instruction expression values precede fixed layout"
      `Quick instruction_expression_layout;
    Alcotest.test_case "scalar DUP and current-position relocations" `Quick
      repetition_and_current_position;
    Alcotest.test_case "malformed nested and list DUP syntax" `Quick
      malformed_repetition;
    Alcotest.test_case "ORG and original relative ALIGN rules" `Quick
      origins_and_alignment;
    Alcotest.test_case "mixed modes, listing state and source receipts" `Quick
      mixed_modes_and_receipts;
    Alcotest.test_case "unused imports and forward directive data" `Quick
      imports_and_forward_data;
    Alcotest.test_case "function assembly imports end with their block or run"
      `Quick function_assembly_import_scope;
    Alcotest.test_case "primitive casts retain their resolved source identity"
      `Quick primitive_cast_source_identity;
    Alcotest.test_case "data labels preserve exported local scopes" `Quick
      data_label_scopes;
    Alcotest.test_case "inserted binary size and pointer provenance" `Quick
      inserted_binary_tokens;
    Alcotest.test_case "directive output, work and position boundaries" `Quick
      output_and_work_limits;
    Alcotest.test_case "expanded directive diagnostics retain provenance" `Quick
      macro_error_provenance;
  ]
