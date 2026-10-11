module Operand = Holyc_lib.Asm_operand
module Instruction = Holyc_lib.Asm_instruction
module Encoder = Holyc_lib.Asm_encoder
module Opcode = Holyc_lib.Asm_opcode
module Register = Holyc_lib.Asm_register

let register spelling =
  match Register.find spelling with
  | Some value -> Operand.Register value
  | None -> Alcotest.failf "missing original register %s" spelling

let memory ?(width = 8) ?segment inner =
  [ Operand.Size width ]
  @ (match segment with
    | None -> []
    | Some spelling -> [ register spelling; Operand.Punctuation ':' ])
  @ [ Operand.Punctuation '[' ]
  @ inner
  @ [ Operand.Punctuation ']' ]

let displaced ?width spelling =
  memory ?width
    [ register spelling; Operand.Operator "+"; Operand.Value_symbol "offset" ]

let selected ?(mode = Operand.Use64) spelling arguments =
  let opcode =
    match Opcode.resolve spelling with
    | Some opcode -> opcode
    | None -> Alcotest.failf "missing original opcode %s" spelling
  in
  let relative =
    Opcode.first_form_relative_arguments (Opcode.resolved_opcode opcode)
  in
  let operands =
    List.mapi
      (fun index tokens ->
        let relative =
          Option.value ~default:false (List.nth_opt relative index)
        in
        match Operand.parse ~mode ~relative tokens with
        | Ok operand -> operand
        | Error error ->
            Alcotest.failf "%s operand %d token %d: %s" spelling index
              error.token_index error.message)
      arguments
  in
  match Instruction.select ~mode ~opcode ~operands with
  | Ok selection -> selection
  | Error error -> Alcotest.failf "%s selection: %s" spelling error.message

let resolved selection values =
  match Instruction.resolve_values selection values with
  | Ok selection -> selection
  | Error error -> Alcotest.failf "numeric resolution: %s" error.message

let contains text fragment =
  let rec at offset =
    offset + String.length fragment <= String.length text
    && (String.sub text offset (String.length fragment) = fragment
       || at (offset + 1))
  in
  at 0

let check_bytes ?(position = 0L) label selection expected =
  match
    Encoder.encode ~max_bytes:(String.length expected) ~position selection
  with
  | Error error -> Alcotest.failf "%s: %s" label error.message
  | Ok emission ->
      Alcotest.(check string) label expected emission.bytes;
      Alcotest.(check int)
        (label ^ " size") (String.length expected) emission.size

let check_layout label (original : Instruction.selection)
    (resolved : Instruction.selection) =
  Alcotest.(check bool)
    (label ^ " original emission layout")
    true
    (original.opcode == resolved.opcode
    && original.form == resolved.form
    && original.mode = resolved.mode
    && original.candidate_count = resolved.candidate_count
    && original.warnings = resolved.warnings
    && original.selection_bytes = resolved.selection_bytes
    && original.encoded_bytes = resolved.encoded_bytes
    && original.operand_prefix = resolved.operand_prefix
    && original.address_prefix = resolved.address_prefix
    && original.segment_prefix = resolved.segment_prefix
    && original.rex = resolved.rex
    && original.modrm = resolved.modrm
    && original.sib = resolved.sib
    && original.immediate_bytes = resolved.immediate_bytes
    && original.displacement_bytes = resolved.displacement_bytes
    && Instruction.numeric_fields original = Instruction.numeric_fields resolved
    )

let check_fields label selection displacement immediate =
  let describe =
    Option.map (fun (field : Instruction.numeric_field) ->
        (field.bytes, field.operand_index, field.relative, field.address))
  in
  let actual_displacement, actual_immediate =
    Instruction.numeric_fields selection
  in
  Alcotest.(check bool)
    (label ^ " displacement owner")
    true
    (describe actual_displacement = displacement);
  Alcotest.(check bool)
    (label ^ " immediate owner")
    true
    (describe actual_immediate = immediate)

let encode_error ?(position = 0L) ~index label fragment selection =
  match Encoder.encode ~position selection with
  | Ok _ -> Alcotest.failf "%s unexpectedly emitted bytes" label
  | Error error ->
      Alcotest.(check (option int))
        (label ^ " operand") index error.operand_index;
      Alcotest.(check bool)
        (label ^ ": " ^ error.message)
        true
        (contains error.message fragment)

let resolve_error ~index label fragment selection values =
  match Instruction.resolve_values selection values with
  | Ok _ -> Alcotest.failf "%s unexpectedly resolved" label
  | Error error ->
      Alcotest.(check (option int))
        (label ^ " operand") index error.operand_index;
      Alcotest.(check bool)
        (label ^ ": " ^ error.message)
        true
        (contains error.message fragment)

let forward_base_displacements () =
  let original = selected "MOV" [ [ register "RAX" ]; displaced "RBX" ] in
  check_fields "unresolved base" original (Some (4, Some 1, false, false)) None;
  List.iter
    (fun (word, expected) ->
      let selection = resolved original [ None; Some (word, false) ] in
      check_layout "resolved base" original selection;
      check_bytes "resolved base retains dword displacement" selection expected)
    [
      (0L, "\x48\x8b\x83\x00\x00\x00\x00");
      (1L, "\x48\x8b\x83\x01\x00\x00\x00");
      (127L, "\x48\x8b\x83\x7f\x00\x00\x00");
      (-128L, "\x48\x8b\x83\x80\xff\xff\xff");
      (2147483647L, "\x48\x8b\x83\xff\xff\xff\x7f");
      (-2147483648L, "\x48\x8b\x83\x00\x00\x00\x80");
    ];
  List.iter
    (fun word ->
      encode_error ~index:(Some 1) "resolved dword displacement overflow"
        "displacement out of range"
        (resolved original [ None; Some (word, false) ]))
    [ 2147483648L; -2147483649L ];
  Alcotest.(check bool) "source remains unresolved" true original.unresolved;
  encode_error ~index:(Some 1) "source still needs its expression" "resolution"
    original

let mode_prefixes_and_sib () =
  let word =
    selected ~mode:Operand.Use16 "MOV"
      [ [ register "AX" ]; displaced ~width:2 "EBX" ]
  in
  let fixed = resolved word [ None; Some (0L, false) ] in
  check_layout "USE16" word fixed;
  check_fields "USE16" fixed (Some (2, Some 1, false, false)) None;
  check_bytes "USE16 retains address prefix and word displacement" fixed
    "\x67\x8b\x83\x00\x00";
  List.iter
    (fun value ->
      encode_error ~index:(Some 1) "resolved word displacement overflow"
        "displacement out of range"
        (resolved word [ None; Some (value, false) ]))
    [ 32768L; -32769L ];
  let original =
    selected "MOV"
      [
        memory ~segment:"FS"
          [
            register "R13";
            Operand.Operator "+";
            register "R12";
            Operand.Operator "*";
            Operand.Integer 4L;
            Operand.Operator "+";
            Operand.Value_symbol "offset";
          ];
        [ register "R9" ];
      ]
  in
  let fixed = resolved original [ Some (0L, false); None ] in
  check_layout "SIB" original fixed;
  check_fields "SIB" fixed (Some (4, Some 0, false, false)) None;
  check_bytes "SIB retains segment, extended registers and displacement" fixed
    "\x64\x4f\x89\x8c\xa5\x00\x00\x00\x00"

let single_operand_fields () =
  let original = selected "INC" [ displaced "RBP" ] in
  let fixed = resolved original [ Some (0L, false) ] in
  check_layout "single base" original fixed;
  check_fields "single base" fixed (Some (4, Some 0, false, false)) None;
  check_bytes "single base retains full displacement" fixed
    "\x48\xff\x85\x00\x00\x00\x00";
  let original = selected "INC" [ memory [ Operand.Symbol "target" ] ] in
  let fixed = resolved original [ Some (8L, true) ] in
  check_layout "single target" original fixed;
  check_fields "single target" fixed (Some (4, Some 0, true, true)) None;
  check_bytes "single target remains relative to instruction end" fixed
    "\x48\xff\x05\x01\x00\x00\x00"

let partial_resolution_and_ownership () =
  let original =
    selected "ADD" [ displaced "RBX"; [ Operand.Value_symbol "value" ] ]
  in
  check_fields "two fields" original
    (Some (4, Some 0, false, false))
    (Some (1, Some 1, false, false));
  let partial = resolved original [ Some (0L, false); None ] in
  check_layout "partial resolution" original partial;
  Alcotest.(check bool) "partial remains unresolved" true partial.unresolved;
  Alcotest.(check bool)
    "None retains the exact original operand" true
    (List.nth original.operands 1 == List.nth partial.operands 1);
  encode_error ~index:(Some 1) "partial expression still missing" "resolution"
    partial;
  let fixed = resolved partial [ None; Some (-1L, false) ] in
  check_layout "complete resolution" original fixed;
  Alcotest.(check bool) "both expressions resolved" false fixed.unresolved;
  Alcotest.(check bool)
    "second resolution retains the first resolved operand" true
    (List.hd partial.operands == List.hd fixed.operands);
  Alcotest.(check bool)
    "resolved operands retain source tokens" true
    (List.for_all2
       (fun before after -> Operand.tokens before == Operand.tokens after)
       original.operands fixed.operands);
  check_bytes "zero displacement precedes the immediate" fixed
    "\x48\x83\x83\x00\x00\x00\x00\xff";
  check_bytes "independent values occupy their original fields"
    (resolved original [ Some (0x12345678L, false); Some (127L, false) ])
    "\x48\x83\x83\x78\x56\x34\x12\x7f"

let branches_keep_selected_width () =
  let original = selected "JMP" [ [ Operand.Symbol "target" ] ] in
  check_fields "short branch" original None (Some (1, Some 0, true, false));
  List.iter
    (fun (target, expected) ->
      let fixed = resolved original [ Some (target, true) ] in
      check_layout "short branch" original fixed;
      check_bytes "resolved short branch boundary" fixed expected)
    [ (129L, "\xeb\x7f"); (-126L, "\xeb\x80") ];
  List.iter
    (fun target ->
      let fixed = resolved original [ Some (target, true) ] in
      check_layout "out-of-range short branch" original fixed;
      encode_error ~index:(Some 0) "short branch never widens"
        "relative target out of range" fixed)
    [ 130L; -127L ];
  let wide = selected "JMP" [ [ Operand.Size 4; Operand.Symbol "target" ] ] in
  let fixed = resolved wide [ Some (0x107L, true) ] in
  check_layout "wide branch" wide fixed;
  check_fields "wide branch" fixed None (Some (4, Some 0, true, false));
  check_bytes ~position:0x100L "near wide branch never narrows" fixed
    "\xe9\x02\x00\x00\x00"

let immediate_ranges_and_address_category () =
  let signed =
    selected "ADD" [ [ register "RDX" ]; [ Operand.Value_symbol "value" ] ]
  in
  List.iter
    (fun (value, expected) ->
      let fixed = resolved signed [ None; Some (value, false) ] in
      check_layout "signed immediate" signed fixed;
      check_bytes "signed byte boundary" fixed expected)
    [ (-128L, "\x48\x83\xc2\x80"); (127L, "\x48\x83\xc2\x7f") ];
  List.iter
    (fun value ->
      encode_error ~index:(Some 1) "signed immediate never widens"
        "immediate out of range"
        (resolved signed [ None; Some (value, false) ]))
    [ -129L; 128L ];
  let unsigned =
    selected "MOV" [ [ register "DL" ]; [ Operand.Value_symbol "value" ] ]
  in
  check_bytes "unsigned byte maximum"
    (resolved unsigned [ None; Some (255L, false) ])
    "\xb2\xff";
  List.iter
    (fun value ->
      encode_error ~index:(Some 1) "unsigned immediate retains its range"
        "immediate out of range"
        (resolved unsigned [ None; Some (value, false) ]))
    [ -1L; 256L ];
  let unknown_category =
    selected "ADD"
      [
        [ register "RDX" ];
        [ Operand.Address_symbol { name = "export"; absolute = None } ];
      ]
  in
  let fixed = resolved unknown_category [ None; Some (7L, true) ] in
  check_layout "resolved address category" unknown_category fixed;
  let operand = List.nth fixed.operands 1 in
  Alcotest.(check int)
    "direct address acquires the source width" 4 (Operand.size operand);
  (match Operand.immediate operand with
  | Some immediate ->
      Alcotest.(check bool) "resolved address parity" true immediate.absolute;
      Alcotest.(check bool)
        "resolved address category is known" true immediate.absolute_known
  | None -> Alcotest.fail "resolved expression lost its immediate");
  check_bytes "address category retains the reserved byte field" fixed
    "\x48\x83\xc2\x07"

let relative_and_absolute_addresses () =
  let relative =
    selected "MOV" [ [ register "RAX" ]; memory [ Operand.Symbol "target" ] ]
  in
  let fixed = resolved relative [ None; Some (0x10cL, true) ] in
  check_layout "RIP-relative address" relative fixed;
  check_fields "RIP-relative address" fixed (Some (4, Some 1, true, true)) None;
  check_bytes ~position:0x100L "relative address retains its base" fixed
    "\x48\x8b\x05\x05\x00\x00\x00";
  let absolute =
    selected ~mode:Operand.Use32 "MOV"
      [ [ register "EDX" ]; memory ~width:4 [ Operand.Symbol "target" ] ]
  in
  let fixed = resolved absolute [ None; Some (4294967295L, true) ] in
  check_layout "absolute address" absolute fixed;
  check_fields "absolute address" fixed (Some (4, Some 1, false, true)) None;
  check_bytes "absolute address accepts an unsigned dword pattern" fixed
    "\x8b\x15\xff\xff\xff\xff";
  encode_error ~index:(Some 1) "absolute address exceeds its reserved field"
    "displacement out of range"
    (resolved absolute [ None; Some (4294967296L, true) ])

let slot_shape_and_implicit_zero () =
  let original = selected "MOV" [ [ register "RAX" ]; displaced "RBX" ] in
  List.iter
    (resolve_error ~index:None "value slot count"
       "requires 2 operand value slots" original)
    [ []; [ None ]; [ None; None; None ] ];
  resolve_error ~index:(Some 0) "register cannot receive a numeric value"
    "no immediate expression" original
    [ Some (0L, false); None ];
  let nop = selected "NOP" [] in
  check_bytes "empty value list for an empty operand list" (resolved nop [])
    "\x90";
  resolve_error ~index:None "NOP rejects an extra numeric slot"
    "requires 0 operand value slots" nop
    [ Some (0L, false) ];
  let implicit =
    selected "MOV" [ [ register "RAX" ]; memory [ register "RBP" ] ]
  in
  check_fields "implicit zero" implicit (Some (1, None, false, false)) None;
  resolve_error ~index:(Some 1) "implicit zero has no source immediate"
    "no immediate expression" implicit
    [ None; Some (1L, false) ];
  List.iter
    (fun (base, expected) ->
      let original =
        selected "MOV"
          [
            [ register "RAX" ];
            memory [ register base; Operand.Operator "+"; Operand.Integer 0L ];
          ]
      in
      let fixed = resolved original [ None; Some (0L, false) ] in
      check_layout "unchanged zero" original fixed;
      check_bytes "unchanged explicit zero retains its encoding" fixed expected;
      List.iter
        (fun replacement ->
          resolve_error ~index:(Some 1) "zero cannot acquire field ownership"
            "no numeric field" original [ None; Some replacement ])
        [ (1L, false); (0L, true) ])
    [ ("RBX", "\x48\x8b\x03"); ("RBP", "\x48\x8b\x45\x00") ]

let tests =
  [
    Alcotest.test_case "forward base displacement fields stay reserved" `Quick
      forward_base_displacements;
    Alcotest.test_case "mode prefixes and SIB survive numeric resolution" `Quick
      mode_prefixes_and_sib;
    Alcotest.test_case "single-operand fields keep their source roles" `Quick
      single_operand_fields;
    Alcotest.test_case
      "partial resolution preserves operand and field ownership" `Quick
      partial_resolution_and_ownership;
    Alcotest.test_case "resolved branches keep their selected width" `Quick
      branches_keep_selected_width;
    Alcotest.test_case
      "resolved immediates retain ranges and address categories" `Quick
      immediate_ranges_and_address_category;
    Alcotest.test_case "relative and absolute address fields stay distinct"
      `Quick relative_and_absolute_addresses;
    Alcotest.test_case "value slot shape and implicit-zero ownership" `Quick
      slot_shape_and_implicit_zero;
  ]
