open Holyc_lib
module P = Test_parser

(* This executor supplies text to the real streaming parser. Runtime #exe
   coverage belongs to Test_stateful_exe; these tests isolate grammar, source
   ownership, lookup restoration and callback ordering. *)
let parse ?(mode = Preprocessor.Jit) ?max_generated_bytes ?max_definition_depth
    ?working_directory ?(same_task = false) ?fail_command ?enter_failure
    ?commands ?(on_enter = fun () -> ())
    ?(configure = fun _ execution -> execution) contents =
  let session = Session.create () in
  let task = if same_task then session else Session.create () in
  let source = Session.add_source session ~path:"stream.HC" ~contents in
  let config =
    Preprocessor.Config.create ?working_directory ~compilation_mode:mode
      ?max_generated_bytes ?max_definition_depth ()
    |> function
    | Ok config -> config
    | Error message -> Alcotest.fail message
  in
  let visited = ref [] in
  let aborted = ref 0 in
  let finished = ref 0 in
  let enter opener =
    on_enter ();
    let body = Buffer.create 32 in
    let command item =
      visited := item :: !visited;
      match fail_command with
      | Some message ->
          Error
            [
              Diagnostic.make ~code:"TESTEXE" ~severity:Diagnostic.Error
                ~message ~primary:opener ();
            ]
      | None ->
          (match item with
          | Ast.Top_level_statement (Ast.Implicit_output_statement statement)
            -> (
              match statement.fixed_argument with
              | Ast.Marker_fixed_argument (Ast.String_literal literal) -> (
                  match literal.literal_value with
                  | Ast.Bytes_value text -> Buffer.add_string body text
                  | _ -> ())
              | _ -> ())
          | Ast.Top_level_statement (Ast.Expression_statement expression) -> (
              match expression.expression_statement_expression with
              | Ast.String_literal { literal_value = Ast.Bytes_value text; _ }
                -> Buffer.add_string body text
              | _ -> ())
          | _ -> ());
          Ok ()
    in
    match enter_failure with
    | Some message ->
        Error
          [
            Diagnostic.make ~code:"TESTENTER" ~severity:Diagnostic.Error
              ~message ~primary:opener ();
          ]
    | None ->
        Ok
          (configure opener
             {
               Parser.definitions = Session.definitions task;
               symbols = Session.symbols task;
               commands =
                 { reference = None; command; resume = (fun () -> Ok ()) };
               finish =
                 (fun () ->
                   incr finished;
                   Ok (Buffer.contents body));
               abort = (fun () -> incr aborted);
             })
  in
  let output =
    Parser.parse ?commands ~execute_stream:enter
      ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  (session, source, output, List.rev !visited, !finished, !aborted)

let integers ast =
  List.map
    (function
      | Ast.Top_level_statement (Ast.Expression_statement expression) -> (
          match expression.expression_statement_expression with
          | Ast.Integer_literal { literal_value = Ast.Integer_value n; _ } -> n
          | _ -> Alcotest.fail "expected an integer expression")
      | _ -> Alcotest.fail "expected an expression statement")
    ast.Ast.items

let expect_integers expected (_, _, output, _, _, _) =
  Alcotest.(check (list int64))
    "parsed integers" expected
    (integers (P.expect_ast output))

let error code (_, _, output, _, _, _) =
  Alcotest.(check bool) "parse failed" true (Parser.has_errors output);
  Alcotest.(check bool)
    "diagnostic code" true
    (List.exists (fun d -> d.Diagnostic.code = code) output.diagnostics)

let joined_token () =
  let _, source, output, _, _, _ = parse {|#exe {"4";}2;|} in
  let ast = P.expect_ast output in
  Alcotest.(check (list int64)) "joined integer" [ 42L ] (integers ast);
  match ast.items with
  | [ Ast.Top_level_statement (Ast.Expression_statement expression) ] ->
      let location =
        Ast.expression_location expression.expression_statement_expression
      in
      Alcotest.(check int)
        "two source segments" 2
        (List.length location.source_segments);
      let origin = Option.get location.generated_from in
      Alcotest.(check bool)
        "directive origin" true
        (Source_id.equal origin.source (Source_file.id source));
      Alcotest.(check int) "origin starts at hash" 0 origin.start;
      Alcotest.(check int) "origin covers directive" 4 origin.stop
  | _ -> Alcotest.fail "expected one joined expression"

let command_grammar () =
  let _, _, output, visited, _, _ =
    parse {|#exe {I64 F(I64 n){while(n) --n;return 42;}"42;";}|}
  in
  Alcotest.(check (list int64))
    "outer contains only generated source" [ 42L ]
    (integers (P.expect_ast output));
  match visited with
  | [ Ast.Function_definition f; Ast.Top_level_statement _ ] ->
      Alcotest.(check string) "task function" "F" f.name.spelling
  | _ -> Alcotest.fail "stream commands must use ordinary function grammar"

let mode_restoration () =
  parse ~mode:Preprocessor.Aot
    "#exe {\n\
     #ifjit\n\
     \"42;\";\n\
     #else\n\
     \"0;\";\n\
     #endif\n\
     }\n\
     #ifaot\n\
     7;\n\
     #else\n\
     0;\n\
     #endif"
  |> expect_integers [ 42L; 7L ]

let local_restoration () =
  let _, _, output, _, _, _ =
    parse ~same_task:true
      "I64 N; I64 F(){I64 N; #exe {\n\
       #ifdef N\n\
       \"#ifdef N\\nreturn 0;\\n#else\\nreturn N;\\n#endif\\n\";\n\
       #else\n\
       \"return 1;\";\n\
       #endif\n\
       }}"
  in
  match (P.expect_ast output).items with
  | [ Ast.Global_variable _; Ast.Function_definition f ] -> (
      match f.body with
      | Some (Ast.Block_statement block) ->
          let rec returns_local = function
            | Ast.Return_statement
                { return_value = Some (Ast.Identifier_expression n); _ } ->
                n.spelling = "N"
            | Ast.Sequence_statement sequence ->
                List.exists
                  (fun element -> returns_local element.Ast.sequence_statement)
                  sequence.sequence_elements
            | _ -> false
          in
          Alcotest.(check bool)
            "restored local return" true
            (List.exists returns_local block.block_statements)
      | _ -> Alcotest.fail "expected function body")
  | _ -> Alcotest.fail "expected global and function"

let faults () =
  let result =
    parse ~fail_command:"command failed" {|#exe {"42;";"0;";}I64 Later;|}
  in
  error "TESTEXE" result;
  let session, _, _, visited, finished, aborted = result in
  Alcotest.(check int) "only reached command delivered" 1 (List.length visited);
  Alcotest.(check int) "failed block did not finish" 0 finished;
  Alcotest.(check int) "buffer released" 1 aborted;
  Alcotest.(check bool)
    "later declaration was not parsed" true
    (Symbol_visibility.Environment.find_preprocessor (Session.symbols session)
       "Later"
    = Symbol_visibility.Absent)

let generated_diagnostic () =
  let _, source, output, _, _, _ = parse {|#exe {"I64 ;";}|} in
  Alcotest.(check bool)
    "invalid generated statement" true (Parser.has_errors output);
  Alcotest.(check bool)
    "directive trace retained" true
    (List.exists
       (fun diagnostic ->
         List.exists
           (fun (related : Diagnostic.related) ->
             Source_id.equal related.span.source (Source_file.id source)
             && related.span.start = 0 && related.span.stop = 4)
           diagnostic.Diagnostic.secondary)
       output.diagnostics)

let block_warnings () =
  let _, _, output, _, _, _ = parse "#exe {#assert 0\n\"42;\";}" in
  Alcotest.(check (list int64))
    "generation succeeds with warning" [ 42L ]
    (integers (P.expect_ast output));
  Alcotest.(check int)
    "inner warning retained" 1
    (List.length
       (List.filter
          (fun d -> d.Diagnostic.severity = Diagnostic.Warning)
          output.diagnostics))

let recovery_cannot_execute () =
  List.iter
    (fun source ->
      let entered = ref 0 in
      let _, _, output, visited, finished, aborted =
        parse ~on_enter:(fun () -> incr entered) source
      in
      Alcotest.(check bool) "first block failed" true (Parser.has_errors output);
      Alcotest.(check int) "only failed block entered" 1 !entered;
      Alcotest.(check int) "later command not executed" 0 (List.length visited);
      Alcotest.(check int) "no buffer finished" 0 finished;
      Alcotest.(check int) "failed buffer released" 1 aborted)
    [
      {|#exe {I64 } #exe {"42;";} I64 Later;|}; {|#exe {{I64 } #exe {"42;";}}}|};
    ]

let selected_occurrence () =
  let selected = ref [] in
  let commands : Parser.command_sink =
    {
      reference =
        Some
          (fun receipt ->
            selected := receipt :: !selected;
            Ok ());
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let session, _, output, _, _, _ =
    parse ~same_task:true ~commands
      {|I64 F(){return 42;}; F #exe {I64 F(I64 n){return n;}} ;|}
  in
  let ast = P.expect_ast output in
  let occurrence =
    List.find_map
      (function
        | Ast.Top_level_statement
            (Ast.Expression_statement
               {
                 expression_statement_expression =
                   Ast.Call_expression
                     { call_callee = Ast.Identifier_expression identifier; _ };
                 _;
               }) -> Some identifier
        | _ -> None)
      ast.items
    |> Option.get
  in
  let receipt =
    List.find
      (fun receipt -> Parser.selected_identifier receipt == occurrence)
      !selected
  in
  Alcotest.(check bool)
    "receipt retains owning environment" true
    (Parser.selected_environment receipt == Session.symbols session);
  let selected_entry =
    match Parser.selected_lookup receipt with
    | Symbol_visibility.Present entry -> entry
    | _ -> Alcotest.fail "expected selected function"
  in
  Alcotest.(check int)
    "selected header has zero parameters" 0
    ( Symbol_visibility.function_call_shape selected_entry |> Option.get
    |> fun shape -> List.length shape.parameters );
  match
    Symbol_visibility.Environment.find_preprocessor (Session.symbols session)
      "F"
  with
  | Symbol_visibility.Present latest ->
      Alcotest.(check bool)
        "later publication has different identity" true
        (latest != selected_entry);
      Alcotest.(check int)
        "later header has one parameter" 1
        ( Symbol_visibility.function_call_shape latest |> Option.get
        |> fun shape -> List.length shape.parameters )
  | _ -> Alcotest.fail "expected later function"

let warning_only_failures () =
  List.iter
    (fun phase ->
      let configure span (execution : Parser.stream_execution) =
        let diagnostics =
          [
            Diagnostic.make ~code:"TESTWARN" ~severity:Diagnostic.Warning
              ~message:"failed with a warning" ~primary:span ();
          ]
        in
        match phase with
        | `Command ->
            {
              execution with
              commands =
                {
                  execution.commands with
                  command = (fun _ -> Error diagnostics);
                };
            }
        | `Resume ->
            {
              execution with
              commands =
                {
                  execution.commands with
                  resume = (fun () -> Error diagnostics);
                };
            }
        | `Finish -> { execution with finish = (fun () -> Error diagnostics) }
      in
      let result = parse ~configure {|#exe {"42;";}I64 Later;|} in
      let session, _, output, _, _, aborted = result in
      Alcotest.(check bool)
        "Error always makes parsing fail" true (Parser.has_errors output);
      Alcotest.(check int) "failed buffer released" 1 aborted;
      Alcotest.(check bool)
        "later declaration not reached" true
        (Symbol_visibility.Environment.find_preprocessor
           (Session.symbols session) "Later"
        = Symbol_visibility.Absent))
    [ `Command; `Resume; `Finish ]

let warning_before_quota () =
  let _, _, output, _, _, _ =
    parse ~max_generated_bytes:2 "#exe {#assert 0\n\"42;\";}"
  in
  Alcotest.(check (list bool))
    "warning precedes generation failure" [ false; true ]
    (List.map
       (fun d -> d.Diagnostic.severity = Diagnostic.Error)
       output.diagnostics)

let compile_generated_ast () =
  List.iter
    (fun mode ->
      let entered = ref 0 in
      let session, _, output, _, _, _ =
        parse ~mode
          ~on_enter:(fun () -> incr entered)
          {|#exe {"I64 G=40;I64 F(){return G+2;}";}F();|}
      in
      let ast = P.expect_ast output in
      let config =
        Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
      in
      let compiled =
        compile_integer_ast session ~config ast
        |> Test_integer_functions.checked
        |> fun checked -> checked.value
      in
      let result =
        Ir_integer_interpreter.execute_program
          ~globals:(integer_program_globals compiled)
          ~initialization:(integer_program_initialization compiled)
          ~runtime_calls:(integer_program_runtime_calls compiled)
          ~functions:(integer_program_functions compiled)
          ~max_steps:1000 ~max_frame_bytes:1024 ~max_call_depth:16
          (integer_program_entry compiled)
        |> Result.map_error (fun errors ->
            String.concat "; "
              (List.map (fun e -> e.Ir_integer_interpreter.message) errors))
      in
      let result =
        match result with
        | Ok result -> result
        | Error e -> Alcotest.fail e
      in
      Alcotest.(check int64)
        "generated AST runs through shared IR" 42L
        ( Ir_integer_interpreter.final_value result |> Option.get |> fun word ->
          word.bits );
      Alcotest.(check int) "lowering did not invoke the parser again" 1 !entered)
    Test_integer_globals.modes

let opener_warning () =
  let source = "#exe #assert 0\n{\"42;\";}" in
  let _, _, output, _, _, _ = parse ~enter_failure:"entry failed" source in
  Alcotest.(check (list bool))
    "opener warning survives entry failure" [ false; true ]
    (List.map
       (fun d -> d.Diagnostic.severity = Diagnostic.Error)
       output.diagnostics);
  let success = parse source in
  expect_integers [ 42L ] success;
  let _, _, output, _, _, _ = success in
  Alcotest.(check int)
    "opener warning appears once on success" 1
    (List.length output.diagnostics)

let pending_command_order () =
  let committed = ref 0L in
  let pending = ref None in
  let commands : Parser.command_sink =
    {
      reference = None;
      command =
        (function
        | Ast.Top_level_statement
            (Ast.Expression_statement
               {
                 expression_statement_expression =
                   Ast.Integer_literal
                     { literal_value = Ast.Integer_value n; _ };
                 _;
               }) ->
            pending := Some n;
            Ok ()
        | _ -> Ok ());
      resume =
        (fun () ->
          Option.iter (fun n -> committed := n) !pending;
          pending := None;
          Ok ());
    }
  in
  let configure _ (execution : Parser.stream_execution) =
    {
      execution with
      finish = (fun () -> Ok (Int64.to_string !committed ^ ";"));
    }
  in
  let _, _, output, _, _, _ = parse ~commands ~configure "1;#exe {}" in
  Alcotest.(check (list int64))
    "directive sees state before pending command" [ 1L; 0L ]
    (integers (P.expect_ast output));
  Alcotest.(check int64) "final command was resumed at EOF" 0L !committed

let included_commands () =
  P.with_temp_directory (fun working_directory ->
      P.write_file (Filename.concat working_directory "emit.HC") "\"42;\";";
      parse ~working_directory "#exe {#include \"emit.HC\"\n}"
      |> expect_integers [ 42L ])

let shared_generation_quota () =
  let source = "#exe {#define TEXT \"42;\"\nTEXT;}" in
  parse ~max_generated_bytes:8 source |> expect_integers [ 42L ];
  parse ~max_generated_bytes:7 source |> error "HCPP0013"

let tests =
  [
    Alcotest.test_case "directive precedes pending command resume" `Quick
      pending_command_order;
    Alcotest.test_case "included task commands resume the block" `Quick
      included_commands;
    Alcotest.test_case "definitions and generation share a byte bound" `Quick
      shared_generation_quota;
    Alcotest.test_case "opener diagnostics survive entry failure" `Quick
      opener_warning;
    Alcotest.test_case "joined token retains both sources" `Quick joined_token;
    Alcotest.test_case "stream command grammar" `Quick command_grammar;
    Alcotest.test_case "nested generated frames" `Quick (fun () ->
        parse {|#exe {#exe {"\"42;\";";}}|} |> expect_integers [ 42L ]);
    Alcotest.test_case "temporary JIT mode" `Quick mode_restoration;
    Alcotest.test_case "task lookup hides and restores outer locals" `Quick
      local_restoration;
    Alcotest.test_case "fault stops delivery and injection" `Quick faults;
    Alcotest.test_case "missing closing brace" `Quick (fun () ->
        error "HCPARSE0162" (parse {|#exe {"42;";|}));
    Alcotest.test_case "generated byte boundary" `Quick (fun () ->
        parse ~max_generated_bytes:3 {|#exe {"42;";}|}
        |> expect_integers [ 42L ];
        parse ~max_generated_bytes:2 {|#exe {"42;";}|} |> error "HCPP0013");
    Alcotest.test_case "nested execution depth" `Quick (fun () ->
        parse ~max_definition_depth:1 {|#exe {#exe {}}|} |> error "HCPP0012");
    Alcotest.test_case "generated diagnostics retain directive trace" `Quick
      generated_diagnostic;
    Alcotest.test_case "block warnings survive successful generation" `Quick
      block_warnings;
    Alcotest.test_case "syntax recovery cannot execute later blocks" `Quick
      recovery_cannot_execute;
    Alcotest.test_case "opening brace expands in outer definitions" `Quick
      (fun () ->
        parse "#define OPEN {\n#exe OPEN \"42;\";}" |> expect_integers [ 42L ]);
    Alcotest.test_case "definition identities belong to their environments"
      `Quick (fun () ->
        parse
          "#define OUTER #exe {INNER;}\n#exe {#define INNER \"42;\"\n}\nOUTER"
        |> expect_integers [ 42L ];
        parse "#exe {#define INNER INNER\nINNER;}" |> error "HCPP0011");
    Alcotest.test_case "lookahead cannot rebind a selected callee" `Quick
      (fun () ->
        let _, _, output, _, _, _ =
          parse ~same_task:true
            {|I64 F(){return 42;}; F #exe {I64 F(I64 n){return n;}} ;|}
        in
        ignore (P.expect_ast output));
    Alcotest.test_case "selection receipt owns exact AST occurrence and entry"
      `Quick selected_occurrence;
    Alcotest.test_case "warning-only callback failures remain fatal" `Quick
      warning_only_failures;
    Alcotest.test_case "warnings precede generation quota failure" `Quick
      warning_before_quota;
    Alcotest.test_case "generated AST lowers through ordinary IR" `Quick
      compile_generated_ast;
    Alcotest.test_case "global is published before its initializer" `Quick
      (fun () ->
        let _, _, output, _, _, _ =
          parse ~same_task:true
            "I64 G=#exe {\n#ifdef G\n\"42\";\n#else\n\"0\";\n#endif\n};G;"
        in
        match (P.expect_ast output).items with
        | Ast.Global_declaration { declarators = [ global ]; _ } :: _ -> (
            match global.global_initial_value with
            | Some
                {
                  global_initializer_value =
                    Ast.Scalar_initializer
                      (Ast.Integer_literal
                         { literal_value = Ast.Integer_value value; _ });
                  _;
                } ->
                Alcotest.(check int64)
                  "visible during own initializer" 42L value
            | _ -> Alcotest.fail "expected scalar initializer")
        | _ -> Alcotest.fail "expected global");
    Alcotest.test_case "inactive directive is not executed" `Quick (fun () ->
        parse ~fail_command:"must not execute"
          "#if 0\n#exe {\"0;\";}\n#endif\n42;"
        |> expect_integers [ 42L ]);
  ]
