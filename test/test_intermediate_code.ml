module Opcode = Holyc_lib.Ir_opcode
module Source = Intermediate_code_source

let exact_numeric_domain () =
  Alcotest.(check int) "declared count" 0xB9 Opcode.count;
  Alcotest.(check int) "constructor count" 0xB9 (List.length Opcode.all);
  Alcotest.(check int) "metadata count" 0xB9 (List.length Opcode.information);
  Alcotest.(check (list int))
    "numeric codes" (List.init 0xB9 Fun.id)
    (List.map Opcode.to_code Opcode.all)

let every_lookup_round_trips () =
  List.iter
    (fun opcode ->
      let info = Opcode.info opcode in
      Alcotest.(check bool)
        (Printf.sprintf "%s numeric lookup" info.source_name)
        true
        (Opcode.of_code info.code = Some opcode);
      Alcotest.(check bool)
        (Printf.sprintf "%s source lookup" info.source_name)
        true
        (Opcode.of_source_name info.source_name = Some opcode);
      Alcotest.(check bool)
        (Printf.sprintf "%s display lookup" info.source_name)
        true
        (Opcode.of_display_name info.display_name = Some opcode);
      Alcotest.(check string)
        "source rendering" info.source_name
        (Opcode.to_source_name opcode);
      Alcotest.(check string)
        "display rendering" info.display_name
        (Opcode.to_display_name opcode))
    Opcode.all

let source_argument = function
  | Source.Zero -> Opcode.Zero
  | Source.One -> Opcode.One
  | Source.Two -> Opcode.Two
  | Source.Variable -> Opcode.Variable

let source_structural = function
  | Source.Null -> Opcode.Null
  | Source.Dereference -> Opcode.Dereference
  | Source.Assignment -> Opcode.Assignment
  | Source.Comparison -> Opcode.Comparison

let generated_metadata_matches_pinned_source () =
  let pinned = (Test_intermediate_code_source.pinned_tables ()).entries in
  List.iter2
    (fun (source : Source.entry) (generated : Opcode.info) ->
      Alcotest.(check string)
        "source name" source.source_name generated.source_name;
      Alcotest.(check string)
        "display name" source.display_name generated.display_name;
      Alcotest.(check int) "numeric code" source.code generated.code;
      Alcotest.(check bool)
        "argument count" true
        (source_argument source.argument_count = generated.argument_count);
      Alcotest.(check int)
        "result count" source.result_count generated.result_count;
      Alcotest.(check bool)
        "structural type" true
        (source_structural source.structural_type = generated.structural_type);
      Alcotest.(check bool) "fpop" source.pops_float generated.pops_float;
      Alcotest.(check bool)
        "not_const" source.prevents_constant_folding
        generated.prevents_constant_folding;
      Alcotest.(check int)
        "definition line" source.definition_line generated.definition_line;
      Alcotest.(check int)
        "metadata line" source.metadata_line generated.metadata_line)
    pinned Opcode.information

let representative_metadata () =
  let mul = Opcode.info Opcode.Ic_mul in
  Alcotest.(check int) "MUL code" 0x30 mul.code;
  Alcotest.(check bool)
    "MUL takes two arguments" true
    (mul.argument_count = Opcode.Two);
  Alcotest.(check int) "MUL produces one result" 1 mul.result_count;
  Alcotest.(check bool) "MUL pops floating input" true mul.pops_float;
  Alcotest.(check bool)
    "MUL may be constant" false mul.prevents_constant_folding;
  Alcotest.(check bool)
    "DEREF structural type" true
    ((Opcode.info Opcode.Ic_deref).structural_type = Opcode.Dereference);
  Alcotest.(check bool)
    "ASSIGN structural type" true
    ((Opcode.info Opcode.Ic_assign).structural_type = Opcode.Assignment);
  Alcotest.(check bool)
    "comparison structural type" true
    ((Opcode.info Opcode.Ic_equ_equ).structural_type = Opcode.Comparison);
  Alcotest.(check bool)
    "ADD_RSP variable arguments" true
    ((Opcode.info Opcode.Ic_add_rsp).argument_count = Opcode.Variable)

let source_and_display_names_remain_distinct () =
  let swap = Opcode.info Opcode.Ic_swap_i64 in
  Alcotest.(check string) "source constant" "IC_SWAP_I64" swap.source_name;
  Alcotest.(check string) "table display" "SWAP_U64" swap.display_name;
  let branch = Opcode.info Opcode.Ic_br_equ_equ2 in
  Alcotest.(check string)
    "branch source constant" "IC_BR_EQU_EQU2" branch.source_name;
  Alcotest.(check string)
    "branch table display" "BR_2EQU_EQU" branch.display_name

let rejects_unknown_lookups () =
  Alcotest.(check bool) "negative numeric code" true (Opcode.of_code (-1) = None);
  Alcotest.(check bool)
    "sentinel is not an opcode" true
    (Opcode.of_code Opcode.count = None);
  Alcotest.(check bool)
    "unknown source name" true
    (Opcode.of_source_name "IC_UNKNOWN" = None);
  Alcotest.(check bool)
    "unknown display name" true
    (Opcode.of_display_name "UNKNOWN" = None)

let provenance () =
  Alcotest.(check string)
    "reference commit" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    Opcode.reference_commit;
  Alcotest.(check (list (pair string string)))
    "source checksums"
    [
      ( "Compiler/CompilerA.HH",
        "9eca54eff7d1c0803172e45e5483a57262e24f7b759a6a727c29beaf660967b2" );
      ( "Compiler/CInit.HC",
        "f187d11043dcceb8791409a3e6809ea26e9c3b4f182fe2cbe5c5e644e6938b19" );
    ]
    (List.map
       (fun (source : Opcode.source) -> (source.path, source.sha256))
       Opcode.sources)

let deterministic_order () =
  let sorted = List.sort Opcode.compare (List.rev Opcode.all) in
  Alcotest.(check bool) "numeric order" true (sorted = Opcode.all);
  Alcotest.(check bool)
    "equal identity" true
    (Opcode.equal Opcode.Ic_end Opcode.Ic_end);
  Alcotest.(check bool)
    "different identity" false
    (Opcode.equal Opcode.Ic_end Opcode.Ic_nop1)

let tests =
  [
    Alcotest.test_case "numeric domain" `Quick exact_numeric_domain;
    Alcotest.test_case "lookup round trips" `Quick every_lookup_round_trips;
    Alcotest.test_case "pinned metadata" `Quick
      generated_metadata_matches_pinned_source;
    Alcotest.test_case "representative metadata" `Quick representative_metadata;
    Alcotest.test_case "source and display names" `Quick
      source_and_display_names_remain_distinct;
    Alcotest.test_case "unknown lookups" `Quick rejects_unknown_lookups;
    Alcotest.test_case "source provenance" `Quick provenance;
    Alcotest.test_case "deterministic order" `Quick deterministic_order;
  ]
