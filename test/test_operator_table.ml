module Operator = Holyc_lib.Operator

let expected_spellings =
  [
    "<<=";
    ">>=";
    "...";
    "++";
    "--";
    "->";
    "::";
    "<<";
    ">>";
    "==";
    "!=";
    "<=";
    ">=";
    "&&";
    "||";
    "^^";
    "*=";
    "/=";
    "%=";
    "&=";
    "|=";
    "^=";
    "+=";
    "-=";
    "..";
    "$$";
  ]

let generated_operator_data () =
  Alcotest.(check (list string))
    "operator spellings" expected_spellings
    (List.map fst Operator.all);
  let expected_ids =
    [
      0x112;
      0x113;
      0x124;
      0x105;
      0x106;
      0x107;
      0x108;
      0x109;
      0x10A;
      0x10B;
      0x10C;
      0x10D;
      0x10E;
      0x10F;
      0x110;
      0x111;
      0x114;
      0x115;
      0x122;
      0x116;
      0x117;
      0x118;
      0x119;
      0x11A;
      0x123;
    ]
    |> List.map Option.some
    |> fun ids -> ids @ [ None ]
  in
  Alcotest.(check (list (option int)))
    "TempleOS token IDs" expected_ids
    (List.map
       (fun (_, operator) -> Operator.templeos_token_id operator)
       Operator.all)

let prefix_boundaries () =
  let check text expected width =
    match Operator.find_prefix text ~offset:0 with
    | None -> Alcotest.fail (Printf.sprintf "expected an operator in %S" text)
    | Some (actual, actual_width) ->
        Alcotest.(check string)
          "operator"
          (Operator.spelling expected)
          (Operator.spelling actual);
        Alcotest.(check int) "width" width actual_width
  in
  check "<<=x" Operator.Shift_left_assign 3;
  check "<<x" Operator.Shift_left 2;
  check "...x" Operator.Ellipsis 3;
  check "..x" Operator.Dot_dot 2;
  check "$$x" Operator.Current_position 2;
  Alcotest.(check bool)
    "single less-than is punctuation" true
    (Option.is_none (Operator.find_prefix "<x" ~offset:0));
  Alcotest.(check bool)
    "single dot is punctuation" true
    (Option.is_none (Operator.find_prefix ".x" ~offset:0))

let provenance () =
  Alcotest.(check string)
    "reference commit" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    Operator.reference_commit;
  Alcotest.(check string)
    "dual table path" "Compiler/CInit.HC"
    (Operator.source_path Operator.Increment);
  Alcotest.(check int)
    "increment source line" 262
    (Operator.source_line Operator.Increment);
  Alcotest.(check bool)
    "increment dual group" true
    (Operator.provenance Operator.Increment = Operator.Dual_table 1);
  Alcotest.(check string)
    "ellipsis path" "Compiler/Lex.HC"
    (Operator.source_path Operator.Ellipsis);
  Alcotest.(check int)
    "ellipsis source line" 1079
    (Operator.source_line Operator.Ellipsis);
  Alcotest.(check bool)
    "dollar origin" true
    (Operator.provenance Operator.Current_position = Operator.Dollar_sequence)

let precedence_table () =
  Alcotest.(check int) "precedence count" 17 (List.length Operator.precedences);
  Alcotest.(check (list int))
    "precedence values"
    (List.init 17 (fun index -> index * 4))
    (List.map
       (fun (entry : Operator.precedence) -> entry.value)
       Operator.precedences);
  Alcotest.(check string)
    "precedence source" "Compiler/CompilerA.HH" Operator.precedence_source_path

let binary_table () =
  Alcotest.(check int) "binary count" 31 (List.length Operator.binary_operators);
  let lookup spelling =
    List.find
      (fun (entry : Operator.binary_operator) ->
        String.equal entry.spelling spelling)
      Operator.binary_operators
  in
  let power = lookup "`" in
  let shift = lookup "<<" in
  let multiply = lookup "*" in
  let modulo_assign = lookup "%=" in
  Alcotest.(check string)
    "binary source" "Compiler/CInit.HC" Operator.binary_source_path;
  Alcotest.(check bool)
    "power right association" true
    (power.association = Operator.Right);
  Alcotest.(check int) "power precedence" 0x10 power.precedence_value;
  Alcotest.(check string) "power IC" "IC_POWER" power.ic_name;
  Alcotest.(check bool)
    "shift left flag" true
    (shift.association = Operator.Left);
  Alcotest.(check bool)
    "multiply has no flag" true
    (multiply.association = Operator.Unspecified);
  Alcotest.(check string)
    "modulo assignment IC" "IC_MOD_EQU" modulo_assign.ic_name;
  Alcotest.(check int) "modulo assignment IC ID" 0x4B modulo_assign.ic_id;
  Alcotest.(check int) "modulo assignment line" 324 modulo_assign.source_line;
  Alcotest.(check bool)
    "no ternary token" true
    (not
       (List.exists
          (fun (entry : Operator.binary_operator) ->
            String.equal entry.spelling "?")
          Operator.binary_operators))

let tests =
  [
    Alcotest.test_case "generated operator data" `Quick generated_operator_data;
    Alcotest.test_case "prefix boundaries" `Quick prefix_boundaries;
    Alcotest.test_case "source provenance" `Quick provenance;
    Alcotest.test_case "precedence table" `Quick precedence_table;
    Alcotest.test_case "binary table" `Quick binary_table;
  ]
