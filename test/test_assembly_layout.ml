open Holyc_lib
module Encoding = Assembly_encoding

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let parse ?(compilation_mode = Preprocessor.Jit) contents =
  let session = Session.create () in
  let config =
    match
      Preprocessor.Config.create ~working_directory:(Sys.getcwd ())
        ~compilation_mode ()
    with
    | Ok config -> config
    | Error message -> Alcotest.fail message
  in
  let source =
    Session.add_source session ~path:"assembly-layout.ASM" ~contents
  in
  let output = Holyc_lib.parse_assembly_detailed session ~config ~source in
  match output.Parser.assembly with
  | Some assembly -> (session, config, assembly)
  | None ->
      Alcotest.failf "layout fixture did not parse: %s"
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

let contains text fragment =
  let rec search offset =
    offset + String.length fragment <= String.length text
    && (String.sub text offset (String.length fragment) = fragment
       || search (offset + 1))
  in
  search 0

let emit ?position ?symbols ?max_work ?(max_code_bytes = 65536) config assembly
    =
  match
    Encoding.encode ?position ?symbols ~config ?max_work ~max_code_bytes
      assembly
  with
  | Ok encoded -> encoded
  | Error diagnostic ->
      Alcotest.failf "layout encoding failed: %s: %s" diagnostic.code
        diagnostic.message

let expect_error label = function
  | Error (diagnostic : Diagnostic.t) ->
      Alcotest.(check bool)
        (label ^ " has error severity")
        true
        (diagnostic.severity = Diagnostic.Error);
      diagnostic
  | Ok _ -> Alcotest.failf "%s unexpectedly succeeded" label

let reject ?position ?symbols ?max_work ?(max_code_bytes = 65536) label config
    assembly =
  Encoding.encode ?position ?symbols ~config ?max_work ~max_code_bytes assembly
  |> expect_error label

let check_bytes ?position ?symbols compilation_mode source expected =
  let _, config, assembly = parse ~compilation_mode source in
  let encoded = emit ?position ?symbols config assembly in
  Alcotest.(check string) source expected (hex encoded.bytes);
  encoded

let link ?symbols ~base encoded =
  match Encoding.link ?symbols ~base encoded with
  | Ok linked -> linked
  | Error diagnostic ->
      Alcotest.failf "layout linking failed: %s: %s" diagnostic.code
        diagnostic.message

(* Literal instruction bytes come from OpCodes.DD and the field stores in
   Asm.HC:268-390,395-813. These are source-derived fixtures, not runtime captures. *)
let original_compiler_and_kernel_blocks () =
  List.iter
    (fun compilation_mode ->
      ignore
        (check_bytes compilation_mode
           (* Compiler/Templates.HC:87-94, the complete StrLen template. *)
           "@@45: MOV RDX,RAX; @@50: MOV BL,U8 [RAX]; INC RAX; TEST BL,BL; JNZ \
            @@50; SUB RAX,RDX; DEC RAX;"
           "488bd08a1848ffc084db75f7482bc248ffc8");
      ignore
        (check_bytes compilation_mode
           (* Kernel/KMathA.HC:370-373, the FPATAN retry loop. *)
           "@@05: FPATAN; FSTSW; TEST AX,0x400; JNZ @@05;"
           "d9f3dfe066a9000475f6"))
    modes

let ordinary_and_local_branches () =
  List.iter
    (fun compilation_mode ->
      List.iter
        (fun (position, expected) ->
          ignore
            (check_bytes ~position compilation_mode
               "Start: JMP End; DU8 0xAA; End: JMP Start;" expected))
        [ (0L, "eb01aaebfb"); (0x1234L, "eb01aaebfb") ];
      let encoded =
        check_bytes compilation_mode
          "First:: @@head: JMP @@tail; DU8 0xAA; @@tail: JMP @@head; Second:: \
           @@head: JMP @@tail; DU8 0xBB; @@tail: JMP @@head;"
          "eb01aaebfbeb01bbebfb"
      in
      let locals =
        encoded.symbols
        |> List.filter_map (fun (symbol : Encoding.symbol) ->
            Option.map (fun scope -> (scope, symbol.position)) symbol.scope)
        |> List.sort compare
      in
      Alcotest.(check (list (pair string int64)))
        "each export has independent forward and backward local targets"
        [ ("First", 0L); ("First", 3L); ("Second", 5L); ("Second", 8L) ]
        locals)
    modes

(* Asm.HC:203-205,689-701 fixes the full field while its expression is
   unresolved. A later zero value does not remove or shorten that field. *)
let preserved_forward_displacements () =
  List.iter
    (fun compilation_mode ->
      List.iter
        (fun (source, expected) ->
          ignore (check_bytes compilation_mode source expected))
        [
          ("MOV RAX,[RBX+Later]; Later: RET;", "488b8307000000c3");
          ("MOV RAX,[RBX+Later-Later]; Later: RET;", "488b8300000000c3");
          ("MOV RAX,[RBX+RCX*4+Later]; Later: RET;", "488b848b08000000c3");
          ("USE32; MOV EAX,[EBX+Later]; Later: RET;", "8b8306000000c3");
          ("USE16; MOV AX,[BX+Later]; Later: RET;", "678b830500c3");
          ("Here: MOV RAX,[RBX+Here];", "488b03");
          ("DU8 1; Here: MOV RAX,[RBX+Here];", "01488b4301");
          ("MOV RDX,[Later]; Later: RET;", "488b1500000000c3");
        ])
    modes

let repeated_label_identity () =
  List.iter
    (fun compilation_mode ->
      let repeated =
        check_bytes compilation_mode
          "Root:: @@target: Root:: Ordinary: DU8 1; JMP @@target;" "01ebfd"
      in
      let roots =
        List.filter
          (fun (symbol : Encoding.symbol) -> symbol.name = "Root")
          repeated.symbols
      in
      Alcotest.(check int)
        "repeated exports retain one binding identity" 1 (List.length roots);
      Alcotest.(check int)
        "the symbol records both same-position definitions" 2
        (List.length (List.hd roots).definition_item_indices);
      let alias =
        check_bytes compilation_mode
          "Alias: @@target: Alias:: DU8 1; JMP @@target;" "01ebfd"
      in
      Alcotest.(check bool)
        "repeating an ordinary label with :: does not promote its binding" false
        (List.exists
           (fun (symbol : Encoding.symbol) ->
             symbol.name = "Alias" && symbol.exported)
           alias.symbols);
      let external_repeat =
        check_bytes
          ~symbols:[ ("Same", 8L) ]
          compilation_mode "DU64 @@later; Same:: @@later:" "0800000000000000"
      in
      Alcotest.(check bool)
        "an equal-address external repeat does not create an export" false
        (List.exists
           (fun (symbol : Encoding.symbol) ->
             symbol.name = "Same" && symbol.exported)
           external_repeat.symbols);
      ignore (check_bytes compilation_mode "Repeated: Repeated: RET;" "c3");
      List.iter
        (fun source ->
          let _, config, assembly = parse ~compilation_mode source in
          ignore (reject source config assembly))
        [
          "Repeated: NOP; Repeated:";
          "Root:: @@target: NOP; @@target:";
          "First:: JMP @@missing; Second:: @@missing: RET;";
        ])
    modes

(* AsmResolve.HC:19-39 patches the selected branch field. It never widens it. *)
let short_branch_boundaries () =
  List.iter
    (fun compilation_mode ->
      List.iter
        (fun width ->
          ignore
            (check_bytes compilation_mode
               ("JMP " ^ width ^ "Later; DU8 127 DUP(0x90); Later: RET;")
               ("eb7f" ^ String.concat "" (List.init 127 (fun _ -> "90")) ^ "c3"));
          ignore
            (check_bytes compilation_mode
               ("Earlier: DU8 126 DUP(0x90); JMP " ^ width ^ "Earlier;")
               (String.concat "" (List.init 126 (fun _ -> "90")) ^ "eb80"));
          List.iter
            (fun source ->
              let _, config, assembly = parse ~compilation_mode source in
              ignore (reject source config assembly))
            [
              "JMP " ^ width ^ "Later; DU8 128 DUP(0x90); Later: RET;";
              "Earlier: DU8 127 DUP(0x90); JMP " ^ width ^ "Earlier;";
            ])
        [ ""; "I8 " ])
    modes

let near_branch_boundaries () =
  List.iter
    (fun compilation_mode ->
      List.iter
        (fun (source, expected) ->
          ignore (check_bytes compilation_mode source expected))
        [
          ("USE16; JMP I16 Later+32767; Later:", "e9ff7f");
          ("USE16; Earlier: JMP I16 Earlier-32765;", "e90080");
          ("JMP I32 Later+2147483647; Later:", "e9ffffff7f");
          ("Earlier: JMP I32 Earlier-2147483643;", "e900000080");
          ("JNZ I32 Later; Later: RET;", "0f8500000000c3");
          ("JMP I32 Later; Later: RET;", "e900000000c3");
        ];
      List.iter
        (fun source ->
          let _, config, assembly = parse ~compilation_mode source in
          ignore (reject source config assembly))
        [
          "USE16; JMP I16 Later+32768; Later:";
          "USE16; Earlier: JMP I16 Earlier-32766;";
          "JMP I32 Later+2147483648; Later:";
          "Earlier: JMP I32 Earlier-2147483644;";
        ])
    modes

let alignment_listing_and_receipts () =
  List.iter
    (fun compilation_mode ->
      let normalized =
        check_bytes compilation_mode "DU32 42,F; F:" "2a00000008000000"
      in
      Alcotest.(check string)
        "deferred data has a normalized zero placeholder, not a retained scalar"
        "2a00000000000000"
        (hex normalized.initial_bytes);
      let session, config, assembly =
        parse ~compilation_mode
          "ORG 0x1003; LIST; Entry:: JMP Tail; NOLIST; DU8 0xAA; ALIGN 8,0x90; \
           LIST; Tail: MOV RAX,[RBX+After]; After: RET;"
      in
      let encoded = emit ~max_code_bytes:16 config assembly in
      Alcotest.(check string)
        "fixed fields and alignment share the same final label positions"
        "eb06aa9090909090488b830f000000c3" (hex encoded.bytes);
      Alcotest.(check string)
        "normalized placeholders remain distinct from final fixup bytes"
        "ebfeaa9090909090488b8300000000c3"
        (hex encoded.initial_bytes);
      Alcotest.(check (list (triple int int (option int64))))
        "deferred fields retain their selected width and relative base"
        [ (1, 1, Some 2L); (11, 4, None) ]
        (List.map
           (fun (fixup : Encoding.fixup) ->
             (fixup.offset, fixup.width, fixup.relative_base))
           encoded.fixups);
      List.iter
        (fun (fixup : Encoding.fixup) ->
          Alcotest.(check int64)
            "deferred field position uses raw coordinates"
            (Int64.of_int fixup.offset)
            fixup.position;
          let identities =
            List.map
              (fun name ->
                List.find
                  (fun (symbol : Encoding.symbol) -> symbol.name = name)
                  encoded.symbols
                |> fun symbol -> symbol.id)
              fixup.symbols
          in
          Alcotest.(check (list int))
            "fixups retain the exact symbol identities they referenced"
            identities fixup.symbol_ids;
          match List.nth assembly.assembly_unit_items fixup.item_index with
          | Ast.Inline_assembly_instruction operation ->
              let source_span =
                operation.inline_assembly_operation_location.span
              in
              Alcotest.(check bool)
                "fixup source stays within its original instruction" true
                (fixup.location.span.source = source_span.source
                && fixup.location.span.start >= source_span.start
                && fixup.location.span.stop <= source_span.stop)
          | _ -> Alcotest.fail "instruction fixup points at another item")
        encoded.fixups;
      Alcotest.(check (option int64))
        "ORG is independent of the raw layout" (Some 0x1003L) encoded.origin;
      Alcotest.(check (list int))
        "final instruction offsets" [ 0; 8; 15 ]
        (List.map
           (fun (entry : Encoding.instruction) -> entry.offset)
           encoded.instructions);
      let ranges =
        List.filter
          (fun (entry : Encoding.entry) -> entry.size > 0)
          encoded.entries
      in
      Alcotest.(check (list (triple int int bool)))
        "listing state follows source order through fixups and padding"
        [
          (0, 2, true); (2, 1, false); (3, 5, false); (8, 7, true); (15, 1, true);
        ]
        (List.map
           (fun (entry : Encoding.entry) ->
             (entry.offset, entry.size, entry.listing))
           ranges);
      List.iter
        (fun (entry : Encoding.instruction) ->
          match List.nth assembly.assembly_unit_items entry.item_index with
          | Ast.Inline_assembly_instruction original ->
              Alcotest.(check bool)
                "a patched instruction retains its original operation" true
                (entry.operation == original)
          | _ -> Alcotest.fail "instruction receipt points at another item")
        encoded.instructions;
      let json = Encoding.to_yojson (Session.sources session) encoded in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "report retains the exact parsed source representation"
        (Ast_dump.assembly_to_yojson (Session.sources session) assembly
        |> Yojson.Safe.to_string)
        (json |> member "assembly" |> Yojson.Safe.to_string);
      let map = Encoding.map encoded in
      Alcotest.(check bool)
        "map labels use final raw offsets after alignment" true
        (contains map "0x0000000000000008 global Tail"
        && contains map "0x000000000000000f global After");
      let listing = Encoding.listing (Session.sources session) encoded in
      Alcotest.(check bool)
        "listing displays patched branch and displacement bytes" true
        (contains listing "eb06" && contains listing "488b830f000000");
      Alcotest.(check bool)
        "listing omits the provisional branch field" false
        (contains listing "ebfe");
      Alcotest.(check bool)
        "listing retains the original source path" true
        (contains listing "assembly-layout.ASM:");
      Alcotest.(check bool)
        "NOLIST suppresses the data and padding ranges" false
        (contains listing "  aa" || contains listing "9090909090");
      let linked = link ~base:0x1003L encoded in
      let linked_json =
        Encoding.linked_to_yojson (Session.sources session) linked
      in
      Alcotest.(check string)
        "linked report has its own schema" "holyc-linked-assembly-v1"
        (linked_json |> member "schema" |> to_string);
      Alcotest.(check string)
        "linked report applies its explicit base to the absolute displacement"
        "eb06aa9090909090488b8312100000c3"
        (linked_json |> member "bytes" |> to_string);
      Alcotest.(check string)
        "linked report retains the final unrelocated image"
        "eb06aa9090909090488b830f000000c3"
        (linked_json |> member "image" |> member "bytes" |> to_string);
      let exact =
        emit ~max_work:encoded.work ~max_code_bytes:16 config assembly
      in
      Alcotest.(check string)
        "the reported work covers layout, field patches and the snapshot"
        encoded.bytes exact.bytes;
      ignore
        (reject ~max_work:(encoded.work - 1) ~max_code_bytes:16
           "one step below the reported layout work" config assembly);
      ignore
        (reject ~max_code_bytes:15 "one byte below the complete layout" config
           assembly))
    modes

(* LoadOneImport, Kernel/KLoad.HC:32-49, overwrites each requested-width field.
   An import expression's provisional addend is not a loader addend. *)
let imported_fields_and_linking () =
  List.iter
    (fun compilation_mode ->
      let encoded =
        check_bytes compilation_mode
          "IMPORT Target; DU8 Target; DU16 Target; DU32 Target; DU64 Target; \
           CALL Target;"
          "000000000000000000000000000000e800000000"
      in
      Alcotest.(check string)
        "normalized imported relative field precedes global import resolution"
        "000000000000000000000000000000e8ecffffff"
        (hex encoded.initial_bytes);
      let imports =
        encoded.relocations
        |> List.filter_map (fun (relocation : Encoding.relocation) ->
            Option.map
              (fun name -> (relocation.offset, relocation.width, name))
              relocation.symbol)
        |> List.sort compare
      in
      Alcotest.(check (list (triple int int string)))
        "every used import retains its exact patch field"
        [
          (0, 1, "Target");
          (1, 2, "Target");
          (3, 4, "Target");
          (7, 8, "Target");
          (16, 4, "Target");
        ]
        imports;
      Alcotest.(check (list (triple int int string)))
        "patch kinds retain KernelA.HH:408-415 numeric identities"
        [
          (0, 5, "IET_IMM_U8");
          (1, 7, "IET_IMM_U16");
          (3, 9, "IET_IMM_U32");
          (7, 11, "IET_IMM_I64");
          (16, 8, "IET_REL_I32");
        ]
        (encoded.relocations
        |> List.map (fun (relocation : Encoding.relocation) ->
            (relocation.offset, relocation.type_id, relocation.kind))
        |> List.sort compare);
      let linked = link ~base:0x100L ~symbols:[ ("Target", 0x1234L) ] encoded in
      Alcotest.(check string)
        "link applies each import width and the relative field end"
        "343412341200003412000000000000e820110000" (hex linked.bytes);
      Alcotest.(check string)
        "link keeps the source image unchanged"
        "000000000000000000000000000000e800000000" (hex encoded.bytes);
      Alcotest.(check bool)
        "linked output retains its exact input image" true
        (linked.image == encoded);
      Alcotest.(check int64) "loaded start" 0x100L linked.position;
      Alcotest.(check int64) "loaded end" 0x114L linked.end_position;
      let addend =
        check_bytes compilation_mode "IMPORT F; DU32 F+7;" "07000000"
      in
      let linked = link ~base:0L ~symbols:[ ("F", 0x1234L) ] addend in
      Alcotest.(check string)
        "the loader overwrites a provisional expression addend" "34120000"
        (hex linked.bytes);
      let call_addend =
        check_bytes compilation_mode "IMPORT F; CALL F+7;" "e807000000"
      in
      Alcotest.(check string)
        "an imported call also overwrites its provisional addend" "e82f110000"
        (hex (link ~base:0x100L ~symbols:[ ("F", 0x1234L) ] call_addend).bytes);
      let displacement =
        check_bytes compilation_mode "IMPORT F; MOV U32 [F],1;"
          "c7050000000001000000"
      in
      Alcotest.(check string)
        "an imported relative displacement uses its field end before the \
         immediate"
        "c705fa00000001000000"
        (hex (link ~base:0x100L ~symbols:[ ("F", 0x200L) ] displacement).bytes);
      let unused = check_bytes compilation_mode "IMPORT Unused; RET;" "c3" in
      Alcotest.(check string)
        "an unused import needs no external address" "c3"
        (hex (link ~base:0L unused).bytes))
    modes

let imported_occurrences_and_shadowing () =
  List.iter
    (fun compilation_mode ->
      List.iter
        (fun source ->
          let _, config, assembly = parse ~compilation_mode source in
          ignore (reject source config assembly))
        [ "IMPORT F; DU32 F+F;"; "IMPORT F,G; DU32 F+G;" ];
      let encoded =
        check_bytes
          ~symbols:[ ("F", 0x1234L) ]
          compilation_mode "IMPORT F; DU32 F;" "00000000"
      in
      ignore
        (Encoding.link ~base:0L encoded
        |> expect_error "an explicit IMPORT shadows an encode-time address");
      ignore
        (Encoding.link ~base:0L ~symbols:[ ("F", 1L); ("F", 1L) ] encoded
        |> expect_error "duplicate supplied import addresses"))
    modes

let absolute_and_external_relocations () =
  List.iter
    (fun compilation_mode ->
      let encoded =
        check_bytes compilation_mode
          "DU8 Head; DU16 Head; DU32 Head; DU64 Head; Head:: RET;"
          "0f0f000f0000000f00000000000000c3"
      in
      let linked = link ~base:0x1234L encoded in
      Alcotest.(check string)
        "absolute module fields receive the load base at each width"
        "434312431200004312000000000000c3" (hex linked.bytes);
      Alcotest.(check (list (pair string int64)))
        "export positions use loaded coordinates"
        [ ("Head", 0x1243L) ]
        (List.map
           (fun (symbol : Encoding.symbol) -> (symbol.name, symbol.position))
           linked.symbols);
      let wrapped =
        check_bytes compilation_mode "DU8 Head; Head: RET;" "01c3"
      in
      Alcotest.(check string)
        "AAT_ADD_U8 retains the original modular field store" "00c3"
        (hex (link ~base:0xffL wrapped).bytes);
      let external_call =
        check_bytes
          ~symbols:[ ("KernelEntry", 0x2000L) ]
          compilation_mode "CALL KernelEntry;" "e8fb1f0000"
      in
      Alcotest.(check string)
        "relative calls to fixed external addresses subtract the load base"
        "e8fb1e0000"
        (hex (link ~base:0x100L external_call).bytes))
    modes

let linked_relative_boundaries () =
  List.iter
    (fun compilation_mode ->
      let encoded = check_bytes compilation_mode "IMPORT F; JMP I8 F;" "eb00" in
      List.iter
        (fun (target, expected) ->
          let linked = link ~base:0x100L ~symbols:[ ("F", target) ] encoded in
          Alcotest.(check string)
            "linked relative byte boundary" expected (hex linked.bytes))
        [ (0x181L, "eb7f"); (0x82L, "eb80") ];
      List.iter
        (fun target ->
          ignore
            (Encoding.link ~base:0x100L ~symbols:[ ("F", target) ] encoded
            |> expect_error "linked relative byte outside selected width"))
        [ 0x182L; 0x81L ];
      ignore
        (Encoding.link ~base:Int64.max_int ~symbols:[ ("F", 0L) ] encoded
        |> expect_error "loaded image exceeds the signed address space");
      ignore
        (Encoding.link ~base:(-1L) ~symbols:[ ("F", 0L) ] encoded
        |> expect_error "negative load base"))
    modes

let required_symbols_and_macro_provenance () =
  List.iter
    (fun compilation_mode ->
      List.iter
        (fun source ->
          let _, config, assembly = parse ~compilation_mode source in
          ignore (reject source config assembly))
        [
          "JMP Missing;";
          "MOV RAX,[RBX+Missing];";
          "DU64 Missing;";
          "DU8 0 DUP(Missing);";
        ];
      let _, config, assembly =
        parse ~compilation_mode
          "#define TOO_FAR JMP I8 Tail;\nTOO_FAR\nDU8 128 DUP(0);\nTail: RET;\n"
      in
      let diagnostic = reject "expanded branch range failure" config assembly in
      Alcotest.(check bool)
        "fixup diagnostics retain expansion and definition sites" true
        (List.length diagnostic.secondary >= 2);
      let _, config, assembly =
        parse ~compilation_mode
          "#define LOAD MOV RAX,[RBX+Target];\nLOAD\nTarget: RET;\n"
      in
      let encoded = emit config assembly in
      let fixup = List.hd encoded.fixups in
      Alcotest.(check bool)
        "successful fixup retains its expansion site" true
        (Option.is_some fixup.location.generated_from);
      Alcotest.(check bool)
        "successful fixup retains its definition site" true
        (Option.is_some fixup.location.defined_at))
    modes

let tests =
  [
    Alcotest.test_case "original Compiler and Kernel local-branch blocks" `Quick
      original_compiler_and_kernel_blocks;
    Alcotest.test_case "ordinary and scoped local forward and backward branches"
      `Quick ordinary_and_local_branches;
    Alcotest.test_case "unresolved displacements keep their original full width"
      `Quick preserved_forward_displacements;
    Alcotest.test_case "repeated labels preserve the original scope identity"
      `Quick repeated_label_identity;
    Alcotest.test_case "short branches retain their exact signed boundaries"
      `Quick short_branch_boundaries;
    Alcotest.test_case
      "near branches retain their selected word and dword forms" `Quick
      near_branch_boundaries;
    Alcotest.test_case
      "fixed layout retains alignment and source listing receipts" `Quick
      alignment_listing_and_receipts;
    Alcotest.test_case
      "import fields link by original loader width and position" `Quick
      imported_fields_and_linking;
    Alcotest.test_case
      "import occurrences and explicit declarations keep identity" `Quick
      imported_occurrences_and_shadowing;
    Alcotest.test_case "absolute module and fixed external address relocations"
      `Quick absolute_and_external_relocations;
    Alcotest.test_case
      "link validates signed relative fields and loaded addresses" `Quick
      linked_relative_boundaries;
    Alcotest.test_case "required symbols and macro fixups preserve diagnostics"
      `Quick required_symbols_and_macro_provenance;
  ]
