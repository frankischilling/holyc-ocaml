open Holyc_lib

let parse contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"assembly-layout-ownership.ASM" ~contents
  in
  let config =
    match Preprocessor.Config.create ~working_directory:(Sys.getcwd ()) () with
    | Ok config -> config
    | Error message -> Alcotest.fail message
  in
  let output = Holyc_lib.parse_assembly_detailed session ~config ~source in
  match output.Parser.assembly with
  | Some assembly -> assembly
  | None ->
      Alcotest.failf "ownership fixture did not parse: %s"
        (String.concat "; "
           (List.map
              (fun (diagnostic : Diagnostic.t) -> diagnostic.message)
              output.diagnostics))

let operation (assembly : Ast.assembly_unit) =
  match
    List.filter_map
      (function
        | Ast.Inline_assembly_instruction operation -> Some operation
        | _ -> None)
      assembly.assembly_unit_items
  with
  | [ operation ] -> operation
  | _ -> Alcotest.fail "ownership fixture requires exactly one instruction"

let second_operand operation =
  match operation.Ast.inline_assembly_operands with
  | [ _; operand ] -> operand
  | _ -> Alcotest.fail "ownership fixture requires two operands"

let remake_operand ~tokens (operand : Ast.inline_assembly_operand) =
  Ast.make_inline_assembly_operand ~kind:operand.inline_assembly_operand_kind
    ~operand:operand.inline_assembly_operand
    ~type_definitions:operand.inline_assembly_type_definitions
    ~size_prefix:operand.inline_assembly_size_prefix
    ~segment_prefix:operand.inline_assembly_segment_prefix ~tokens
    ~location:operand.inline_assembly_operand_location

let replace_second assembly replacement =
  let original = operation assembly in
  let operands =
    match original.inline_assembly_operands with
    | [ first; _ ] -> [ first; replacement ]
    | _ -> Alcotest.fail "ownership fixture requires two operands"
  in
  let operation =
    Ast.make_inline_assembly_operation ~opcode:original.inline_assembly_opcode
      ~mode:original.inline_assembly_mode
      ~selection:original.inline_assembly_selection ~operands
      ~separator:original.inline_assembly_separator
      ~semicolon:original.inline_assembly_semicolon
      ~location:original.inline_assembly_operation_location
  in
  Ast.make_assembly_unit
    ~items:[ Ast.Inline_assembly_instruction operation ]
    ~lines:assembly.assembly_unit_lines
    ~initial_mode:assembly.assembly_unit_initial_mode
    ~final_mode:assembly.assembly_unit_final_mode
    ~location:assembly.assembly_unit_location

let encode ?(symbols = []) assembly =
  Assembly_encoding.encode ~symbols ~max_code_bytes:64 assembly

let reject ?symbols ?(code = "HCASM0005") label assembly =
  match encode ?symbols assembly with
  | Ok encoded ->
      Alcotest.failf "%s unexpectedly emitted %d bytes" label
        (String.length encoded.bytes)
  | Error diagnostic ->
      Alcotest.(check string)
        (label ^ ": " ^ diagnostic.message)
        code diagnostic.code

let copy_token (token : Ast.assembly_token) =
  Ast.make_assembly_token_with_primitive
    ~primitive:token.assembly_token_primitive ~kind:token.assembly_token_kind
    ~source_token:token.assembly_source_token
    ~location:token.assembly_token_location

let unchanged_reconstruction () =
  List.iter
    (fun (source, expected) ->
      let assembly = parse source in
      let original = second_operand (operation assembly) in
      let tokens =
        List.map copy_token original.inline_assembly_operand_tokens
      in
      let replacement = remake_operand ~tokens original in
      Alcotest.(check bool)
        "reconstruction retains the selected model operand" true
        (replacement.inline_assembly_operand == original.inline_assembly_operand);
      match
        encode
          ~symbols:[ ("Missing", 4L) ]
          (replace_second assembly replacement)
      with
      | Error diagnostic ->
          Alcotest.failf "unchanged reconstruction: %s" diagnostic.message
      | Ok encoded ->
          Alcotest.(check string)
            "unchanged reconstruction preserves bytes" expected encoded.bytes)
    [
      ("MOV RAX,Missing+5;", "\xb8\x09\x00\x00\x00");
      ("MOV RAX,I64 FS:[RBX+Missing];", "\x64\x48\x8b\x43\x04");
    ]

let unresolved_name_substitution () =
  let assembly = parse "MOV RAX,Missing;" in
  let original = second_operand (operation assembly) in
  List.iter
    (fun source ->
      let substituted = second_operand (operation (parse source)) in
      let replacement =
        remake_operand ~tokens:substituted.inline_assembly_operand_tokens
          original
      in
      reject
        ~symbols:[ ("Other", 7L) ]
        ("unresolved expression replaced by " ^ source)
        (replace_second assembly replacement))
    [ "MOV RAX,7;"; "MOV RAX,Other;" ]

let replace_token ~original_source ~original_spelling ~replacement_source
    ~replacement_spelling =
  let assembly = parse original_source in
  let original = second_operand (operation assembly) in
  let donor = second_operand (operation (parse replacement_source)) in
  let replacement =
    match
      List.filter
        (fun (token : Ast.assembly_token) ->
          token.assembly_source_token.raw = replacement_spelling)
        donor.inline_assembly_operand_tokens
    with
    | [ token ] -> token
    | _ -> Alcotest.failf "expected one donor token %S" replacement_spelling
  in
  let count = ref 0 in
  let tokens =
    List.map
      (fun (token : Ast.assembly_token) ->
        if token.assembly_source_token.raw = original_spelling then (
          incr count;
          replacement)
        else token)
      original.inline_assembly_operand_tokens
  in
  Alcotest.(check int) "exactly one source token is replaced" 1 !count;
  replace_second assembly (remake_operand ~tokens original)

let immediate_payload_and_kind () =
  List.iter
    (fun (label, replacement_source, replacement_spelling) ->
      reject
        ~symbols:[ ("Missing", 4L) ]
        label
        (replace_token ~original_source:"MOV RAX,Missing+5;"
           ~original_spelling:"5" ~replacement_source ~replacement_spelling))
    [
      ("integer payload changes", "MOV RAX,Missing+7;", "7");
      ("integer becomes floating", "MOV RAX,Missing+5.0;", "5.0");
      ("integer becomes a name", "MOV RAX,Missing+Other;", "Other");
    ]

let operator_and_address_tokens () =
  List.iter
    (fun ( label,
           original_source,
           original_spelling,
           replacement_source,
           replacement_spelling ) ->
      reject
        ~symbols:[ ("Missing", 4L) ]
        label
        (replace_token ~original_source ~original_spelling ~replacement_source
           ~replacement_spelling))
    [
      ("operator changes", "MOV RAX,Missing+5;", "+", "MOV RAX,Missing-5;", "-");
      ( "address register changes",
        "MOV RAX,I64 [RBX+Missing];",
        "RBX",
        "MOV RAX,I64 [RDX+Missing];",
        "RDX" );
      ( "address width prefix changes",
        "MOV RAX,I64 [RBX+Missing];",
        "I64",
        "MOV EAX,I32 [RBX+Missing];",
        "I32" );
    ]

let check_bytes ?symbols label expected assembly =
  match encode ?symbols assembly with
  | Error diagnostic ->
      Alcotest.failf "%s: %s: %s" label diagnostic.code diagnostic.message
  | Ok encoded ->
      Alcotest.(check string) label expected encoded.bytes;
      encoded

let external_names_shadow_syntax () =
  List.iter
    (fun (label, source, name, expected) ->
      let assembly = parse source in
      ignore
        (check_bytes (label ^ " without external binding") expected assembly);
      reject
        ~symbols:[ (name, 7L) ]
        ~code:"HCASM0004"
        (label ^ " shadowed by external binding")
        assembly)
    [
      ("opcode", "MOV RAX,RBX;", "MOV", "\x48\x8b\xc3");
      ("register operand", "MOV RAX,RBX;", "RAX", "\x48\x8b\xc3");
      ("address register", "MOV RAX,[RBX];", "RBX", "\x48\x8b\x03");
      ("segment prefix", "MOV RAX,FS:[RBX];", "FS", "\x64\x48\x8b\x03");
      ("size prefix", "MOV RAX,I64 [RBX];", "I64", "\x48\x8b\x03");
      ("postfix type", "MOV RAX,7(I64);", "I64", "\xb8\x07\x00\x00\x00");
      ( "directive primitive",
        "DU64 7(I64);",
        "I64",
        "\x07\x00\x00\x00\x00\x00\x00\x00" );
      ("DUP repetition", "DU8 2 DUP(7);", "DUP", "\x07\x07");
    ]

let unused_external_collisions () =
  ignore
    (check_bytes
       ~symbols:
         [
           ("MOV", 7L);
           ("RAX", 7L);
           ("RBX", 7L);
           ("FS", 7L);
           ("I64", 7L);
           ("DUP", 7L);
         ]
       "unused colliding names do not reject assembly" "\x90" (parse "NOP;"))

let imported_opcode_name () =
  let placeholder = "\x00\x00\x00\x00\x00\x00\x00\x00" in
  let encoded =
    check_bytes
      ~symbols:[ ("MOV", 0x1234L) ]
      "an explicit import retains its placeholder" placeholder
      (parse "IMPORT MOV; DU64 MOV;")
  in
  Alcotest.(check string)
    "the import starts with the same placeholder" placeholder
    encoded.initial_bytes;
  Alcotest.(check (list string))
    "opcode spelling remains an import" [ "MOV" ] encoded.imports;
  match
    Assembly_encoding.link ~symbols:[ ("MOV", 0x5678L) ] ~base:0x1000L encoded
  with
  | Error diagnostic ->
      Alcotest.failf "imported opcode name: %s: %s" diagnostic.code
        diagnostic.message
  | Ok linked ->
      Alcotest.(check string)
        "linking uses the separate import address"
        "\x78\x56\x00\x00\x00\x00\x00\x00" linked.bytes

let tests =
  [
    Alcotest.test_case
      "unchanged reconstructed operands retain source ownership" `Quick
      unchanged_reconstruction;
    Alcotest.test_case "unresolved names cannot borrow other expression tokens"
      `Quick unresolved_name_substitution;
    Alcotest.test_case "immediate source tokens retain their values and kinds"
      `Quick immediate_payload_and_kind;
    Alcotest.test_case
      "operators and address tokens retain their parsed meaning" `Quick
      operator_and_address_tokens;
    Alcotest.test_case "external names invalidate retained syntax roles" `Quick
      external_names_shadow_syntax;
    Alcotest.test_case "unused external names may collide with assembly syntax"
      `Quick unused_external_collisions;
    Alcotest.test_case "an imported opcode spelling keeps its import binding"
      `Quick imported_opcode_name;
  ]
