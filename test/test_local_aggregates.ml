open Holyc_lib
module Cases = Local_aggregate_cases
module VM = Ir_integer_interpreter

let describe errors =
  errors
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (describe errors)

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let inputs mode text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"local-aggregates.hc" ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  (session, source, config)

let parse mode text =
  let session, source, config = inputs mode text in
  (session, Holyc_lib.parse_with_config session ~source ~config |> diagnostics)

let run ?max_steps ?max_initializer_steps mode text =
  let session, source, config = inputs mode (Cases.headers ^ text) in
  run_integer_program_report ?max_initializer_steps session ~source ~config
    ~max_steps:(Option.value ~default:100_000 max_steps)

let value ?(expected = 42L) report =
  let result = integer_program_report_outcome report |> diagnostics in
  Alcotest.(check (option int64))
    "original result" (Some expected)
    (VM.final_value result.value |> Option.map (fun word -> word.VM.bits))

let failure code report =
  match integer_program_report_outcome report with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors)

let parser_shapes () =
  List.iter
    (fun mode ->
      let session, ast =
        parse mode
          {|U0 Make(){public class A{U8 a;};extern union B,extern class C;I64 class D{I64 a;} value;}I64 After;|}
      in
      let indexed = Ast.declaration_items ast in
      Alcotest.(check (list int))
        "shared source indices" [ 0; 1; 2; 3; 4; 5 ] (List.map fst indexed);
      let classes =
        List.filter_map
          (fun (_, item) ->
            match item with
            | Ast.Aggregate_definition d -> Some (d.name.spelling, d.semicolon)
            | Ast.Aggregate_forward_declaration d ->
                Some (d.name.spelling, d.semicolon)
            | _ -> None)
          indexed
      in
      Alcotest.(check (list string))
        "original declaration order" [ "A"; "B"; "C"; "D" ]
        (List.map fst classes);
      Alcotest.(check (list bool))
        "original semicolons"
        [ true; false; true; false ]
        (List.map (fun (_, s) -> Option.is_some s) classes);
      let definition =
        match List.hd ast.items with
        | Ast.Function_definition f -> f
        | _ -> Alcotest.fail "function missing"
      in
      let body =
        match definition.body with
        | Some (Ast.Block_statement b) -> b
        | _ -> Alcotest.fail "block missing"
      in
      let first =
        match List.hd body.block_statements with
        | Ast.Aggregate_declaration_statement a -> a
        | _ -> Alcotest.fail "class statement missing"
      in
      Alcotest.(check bool)
        "original item identity" true
        (first.aggregate_statement_item == snd (List.nth indexed 1));
      Alcotest.(check bool)
        "factory rejects a function item" true
        (Result.is_error (Ast.make_aggregate_statement (List.hd ast.items)));
      let rec count = function
        | `Assoc fields ->
            (if
               List.assoc_opt "kind" fields
               = Some (`String "aggregate_declaration_statement")
             then 1
             else 0)
            + List.fold_left
                (fun total (_, node) -> total + count node)
                0 fields
        | `List nodes ->
            List.fold_left (fun total node -> total + count node) 0 nodes
        | _ -> 0
      in
      Alcotest.(check int)
        "complete nested JSON" 4
        (Ast_dump.to_yojson (Session.sources session) ast |> count))
    modes

let layout mode text =
  let session, ast = parse mode text in
  let declarations = collect_declarations session ast |> checked in
  let aggregates = resolve_aggregates session ~declarations ast |> checked in
  let headers =
    resolve_aggregate_headers session ~declarations ~aggregates ast |> checked
  in
  let members = collect_members session ~declarations ast |> checked in
  let types =
    resolve_member_types session ~declarations ~aggregates ~headers ~members ast
    |> checked
  in
  layout_aggregates session ~declarations ~aggregates ~headers ~members:types
    ast
  |> checked

let semantic_layouts () =
  List.iter
    (fun mode ->
      let layouts =
        layout mode
          {|U0 Make(){class Base{U8 a;};class Child:Base{I64 b;};union U{U8 a;I64 b;};}class Tail{U8 a[42];};|}
        |> Semantic_aggregate_layout.layouts
      in
      Alcotest.(check (list string))
        "nested and outer types"
        [ "Base"; "Child"; "U"; "Tail" ]
        (List.map
           (fun (l : Semantic_aggregate_layout.aggregate_layout) ->
             Semantic_symbol.name l.symbol)
           layouts);
      Alcotest.(check (list int64))
        "closed sizes including inherited base" [ 1L; 9L; 8L; 42L ]
        (List.map
           (fun (l : Semantic_aggregate_layout.aggregate_layout) -> l.size)
           layouts);
      Alcotest.(check (list int))
        "indices match declarations" [ 1; 2; 3; 4 ]
        (List.map
           (fun (l : Semantic_aggregate_layout.aggregate_layout) ->
             l.item_index)
           layouts))
    modes

let parser_failures () =
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          let session, source, config = inputs mode text in
          Alcotest.(check bool)
            text true
            (Result.is_error (parse_with_config session ~source ~config)))
        [
          "U0 Make(){class C{U8 a;} variable;}";
          "U0 Make(){I64 class C{I64 a;};}";
          "extern class C,extern union D;";
        ];
      List.iter
        (fun text -> ignore (parse mode text))
        [
          "U0 Make(){I64 class C{I64 a;} *p;static U8 union U{U8 a;} value;}";
          "U0 Make(){if(0)class C{U8 a;};I64 n;}";
          "{I64 class C{I64 a;} *global;}";
          "U0 Make(){class C{I64 a fmt \"%d\";};}";
        ])
    modes

let source_authority () =
  let module Ledger = Task_declarations in
  List.iter
    (fun mode ->
      let session, source, config =
        inputs mode "U0 Make(){class C{U8 a;};extern union D;}I64 After;"
      in
      let ledger = Ledger.create_source session ~source |> checked in
      let foreign = Ledger.create_source session ~source |> checked in
      let completions = ref [] in
      let declaration event =
        let result = Ledger.observe ledger event in
        Result.bind result (fun () ->
            match event with
            | Parser.Function_header_completed header ->
                Ledger.complete_source_defaults ledger header
            | Parser.Aggregate_completed receipt ->
                completions := (receipt, event) :: !completions;
                Alcotest.(check bool)
                  "live completion cannot be replayed" true
                  (Result.is_error (Ledger.observe ledger event));
                Alcotest.(check bool)
                  "foreign ledger has no preceding phases" true
                  (Result.is_error (Ledger.observe foreign event));
                Ok ()
            | _ -> Ok ())
      in
      let commands : Parser.command_sink =
        {
          lexical_lookup = None;
          checkpoint = Some (Ledger.observe_command ledger);
          reference = Some (Ledger.observe_reference ledger);
          query = Some (Ledger.observe_query ledger);
          implicit_output = Some (Ledger.observe_implicit_output ledger);
          declaration = Some declaration;
          dimension_count = Some (Ledger.grammar_dimension_count ledger);
          call = None;
          command = (fun _ -> Ok ());
          resume = (fun () -> Ok ());
        }
      in
      let output =
        Parser.parse ~commands ~sources:(Session.sources session)
          ~symbols:(Session.symbols session)
          ~definitions:(Session.definitions session)
          ~config source
      in
      let ast =
        match output.ast with
        | Some ast -> ast
        | None -> Alcotest.fail (describe output.diagnostics)
      in
      let indexed = Ast.declaration_items ast in
      Alcotest.(check int)
        "two exact aggregate completions" 2 (List.length !completions);
      List.iter
        (fun ((receipt : Parser.completed_aggregate), event) ->
          Alcotest.(check bool)
            "statement retains completion item" true
            (List.exists
               (fun (_, item) -> item == receipt.aggregate_item)
               indexed);
          Alcotest.(check bool)
            "receipt expires after parser returns" false
            (Parser.aggregate_completion_is_current receipt);
          Alcotest.(check bool)
            "expired callback rejected" true
            (Result.is_error (Ledger.observe ledger event)))
        !completions;
      ignore (Ledger.seal_source ledger ast |> diagnostics);
      Alcotest.(check bool)
        "foreign source ledger cannot seal" true
        (Result.is_error (Ledger.seal_source foreign ast));
      let rebuilt =
        Ast.make_module ~source:ast.source ~span:ast.span ~items:ast.items
      in
      Alcotest.(check bool)
        "reconstructed module cannot seal" true
        (Result.is_error (Ledger.seal_source ledger rebuilt)))
    modes

let effects () =
  let report = run Preprocessor.Jit Cases.effects in
  value report;
  Alcotest.(check string)
    "once-only original bound and offset" "offdim"
    (integer_program_report_output_bytes report);
  let failed = run Preprocessor.Jit Cases.reached_failure in
  failure "HCPARSE0115" failed;
  Alcotest.(check string)
    "effect before malformed local tail" "kept"
    (integer_program_report_output_bytes failed);
  List.iter
    (fun mode ->
      let report = run mode Cases.local_position in
      value report;
      Alcotest.(check string)
        "local position before nested input" "P"
        (integer_program_report_output_bytes report))
    modes;
  failure "HCRUN0004" (run Preprocessor.Aot Cases.effects);
  failure "HCRUN0006"
    (run Preprocessor.Aot
       "I64 Count(){return 42;}U0 Make(){class C{U8 a[Count()];};}sizeof(C);")

let quotas () =
  let baseline = run Preprocessor.Jit Cases.effects in
  value baseline;
  let prep = integer_program_report_preparation_work baseline |> Option.get in
  let steps =
    (integer_program_report_outcome baseline |> diagnostics).value
    |> VM.executed_steps
  in
  value
    (run ~max_steps:steps ~max_initializer_steps:prep Preprocessor.Jit
       Cases.effects);
  failure "HCIRVM0007"
    (run ~max_steps:(steps - 1) Preprocessor.Jit Cases.effects);
  failure "HCIRVM0007"
    (run ~max_initializer_steps:(prep - 1) Preprocessor.Jit Cases.effects)

let existing_limits () =
  List.iter
    (fun mode ->
      value
        (run mode
           "U0 Make(){class Base{U8 a;};class Child:Base{U8 \
            b;};}sizeof(Child)+40;");
      failure "HCSEMA0074"
        (run mode "I64 F(){I64 class C{I64 a;} value;return 42;}F();");
      List.iter
        (fun (code, text) -> failure code (run mode text))
        Cases.object_failures)
    modes;
  failure "HCSEMA0046" (run Preprocessor.Jit Cases.suffix_completion_boundary);
  failure "HCPARSE0048" (run Preprocessor.Aot Cases.child_backed_type);
  failure "HCSEMA0046" (run Preprocessor.Aot Cases.completed_during_lookahead);
  value ~expected:44L (run Preprocessor.Aot Cases.replaced_during_dimension)

let object_effects () =
  let baseline = run Preprocessor.Jit Cases.object_effects in
  value baseline;
  Alcotest.(check string)
    "local object layout executes once before calls" "offdim"
    (integer_program_report_output_bytes baseline);
  let prep = integer_program_report_preparation_work baseline |> Option.get in
  let steps =
    (integer_program_report_outcome baseline |> diagnostics).value
    |> VM.executed_steps
  in
  value
    (run ~max_steps:steps ~max_initializer_steps:prep Preprocessor.Jit
       Cases.object_effects);
  failure "HCIRVM0007"
    (run ~max_steps:(steps - 1) Preprocessor.Jit Cases.object_effects);
  failure "HCIRVM0007"
    (run ~max_initializer_steps:(prep - 1) Preprocessor.Jit Cases.object_effects)

let () =
  Alcotest.run "Classes and unions in statements"
    [
      ( "parser",
        [
          Alcotest.test_case "original items, comma delimiters and indices"
            `Quick parser_shapes;
          Alcotest.test_case "local forms and invalid attached variables" `Quick
            parser_failures;
          Alcotest.test_case "original live source authority" `Quick
            source_authority;
        ] );
      ( "semantic",
        [
          Alcotest.test_case "nested layouts and inherited metadata" `Quick
            semantic_layouts;
        ] );
      ( "IR",
        List.concat_map
          (fun mode ->
            List.map
              (fun (name, text) ->
                Alcotest.test_case
                  ((if mode = Preprocessor.Jit then "JIT " else "AOT ") ^ name)
                  `Quick
                  (fun () -> value (run mode text)))
              Cases.values)
          modes
        @ List.map
            (fun (name, text) ->
              Alcotest.test_case ("JIT " ^ name) `Quick (fun () ->
                  value (run Preprocessor.Jit text)))
            Cases.jit_values
        @ [
            Alcotest.test_case "source effects and position ordering" `Quick
              effects;
            Alcotest.test_case "exact preparation and runtime budgets" `Quick
              quotas;
            Alcotest.test_case "local object layout effects and budgets" `Quick
              object_effects;
            Alcotest.test_case "object visibility and memory boundaries" `Quick
              existing_limits;
          ] );
    ]
