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
                 {
                   query = None;
                   reference = None;
                   declaration = None;
                   checkpoint = None;
                   command;
                   resume = (fun () -> Ok ());
                 };
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
      query = None;
      reference =
        Some
          (fun receipt ->
            selected := receipt :: !selected;
            Ok ());
      declaration = None;
      checkpoint = None;
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
      query = None;
      reference = None;
      declaration = None;
      checkpoint = None;
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

let declaration_sink consume =
  Parser.
    {
      query = None;
      reference = None;
      declaration = Some consume;
      checkpoint = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }

let global_publication_timing () =
  let events = ref [] in
  let at_directive = ref [] in
  let commands =
    declaration_sink (fun event ->
        events := event :: !events;
        Ok ())
  in
  let _, _, output, _, _, _ =
    parse ~same_task:true ~commands
      ~on_enter:(fun () -> at_directive := List.rev !events)
      {|I64 A=40,B=#exe {"2";};|}
  in
  let ast = P.expect_ast output in
  (match !at_directive with
  | [
   Parser.Global_declared first;
   Parser.Global_completed (same, node);
   Parser.Global_declared second;
  ] ->
      Alcotest.(check bool)
        "first completion owns declaration" true (first == same);
      Alcotest.(check string)
        "first initialized before second directive" "A" node.name.spelling;
      Alcotest.(check string)
        "second already provisionally visible" "B" second.global_name.spelling
  | _ -> Alcotest.fail "wrong declaration events before second initializer");
  match (ast.items, List.rev !events) with
  | ( [ Ast.Global_declaration declaration ],
      [
        Parser.Global_declared first;
        Parser.Global_completed (_, a);
        Parser.Global_declared second;
        Parser.Global_completed (_, b);
      ] ) ->
      Alcotest.(check bool)
        "completed nodes are final AST declarators" true
        (match declaration.declarators with
        | [ left; right ] -> left == a && right == b
        | _ -> false);
      Alcotest.(check bool)
        "base type is the original source object" true
        (first.global_header.type_specifier == declaration.type_specifier
        && second.global_header.type_specifier == declaration.type_specifier);
      Alcotest.(check bool)
        "name and dimensions remain exact" true
        (first.global_name == a.name
        && second.global_dimensions == b.array_dimensions)
  | _ -> Alcotest.fail "expected two complete declaration witnesses"

let function_publication_timing () =
  let phase = ref "absent" in
  let seen = ref [] in
  let events = ref [] in
  let commands =
    declaration_sink (fun event ->
        events := event :: !events;
        (match event with
        | Parser.Function_declared _ -> phase := "provisional"
        | Parser.Function_header_completed _ -> phase := "header"
        | Parser.Function_body_completed _ -> phase := "body"
        | _ -> ());
        Ok ())
  in
  let _, _, output, _, _, _ =
    parse ~same_task:true ~commands
      ~on_enter:(fun () -> seen := !phase :: !seen)
      {|I64 F(I64 n=#exe {"1";})#exe {}{return n;}#exe {};#exe {}|}
  in
  let ast = P.expect_ast output in
  Alcotest.(check (list string))
    "native publication phases at directives"
    [ "provisional"; "provisional"; "header"; "body" ]
    (List.rev !seen);
  match (ast.items, List.rev !events) with
  | ( Ast.Function_definition definition :: _,
      [
        Parser.Function_declared provisional;
        Parser.Function_header_completed header;
        Parser.Function_body_completed (same_header, same_definition);
      ] ) ->
      Alcotest.(check bool)
        "header retains exact provisional publication" true
        (header.function_publication == provisional);
      Alcotest.(check bool)
        "body completion retains exact header and definition" true
        (same_header == header && same_definition == definition);
      Alcotest.(check bool)
        "parenthesis and parameter source nodes are shared" true
        (provisional.function_opening_parenthesis
         == definition.opening_parenthesis
        && header.parameters == definition.parameters
        && header.closing_parenthesis == definition.closing_parenthesis);
      Alcotest.(check int)
        "header completion preserves entry identity"
        (Symbol_visibility.id provisional.function_entry)
        (Symbol_visibility.id header.completed_entry);
      Alcotest.(check bool)
        "provisional entry remains an immutable snapshot" true
        (Option.is_none
           (Symbol_visibility.function_call_shape provisional.function_entry))
  | _ -> Alcotest.fail "expected linked function declaration phases"

let function_completion_preserves_shadow () =
  let events = ref [] in
  let commands =
    declaration_sink (fun event ->
        events := event :: !events;
        Ok ())
  in
  let session, _, output, _, _, _ =
    parse ~same_task:true ~commands
      {|I64 F(I64 n=#exe {I64 F(I64 a,I64 b){return a+b;}"1";}){return n;}|}
  in
  ignore (P.expect_ast output);
  match List.rev !events with
  | Parser.Function_declared provisional
    :: Parser.Function_header_completed header
    :: _ ->
      let entries =
        Symbol_visibility.Environment.all (Session.symbols session)
        |> List.filter (fun entry -> Symbol_visibility.name entry = "F")
      in
      Alcotest.(check int)
        "one registration per function" 2 (List.length entries);
      let selected =
        Symbol_visibility.Environment.find_function (Session.symbols session)
          "F"
        |> Option.get
      in
      Alcotest.(check bool)
        "nested shadow remains newest" false
        (selected == header.completed_entry);
      Alcotest.(check int)
        "nested header still has two parameters" 2
        (List.length
           (Option.get (Symbol_visibility.function_call_shape selected))
             .parameters);
      Alcotest.(check bool)
        "old provisional receipt is unchanged" true
        (Option.is_none
           (Symbol_visibility.function_call_shape provisional.function_entry))
  | _ -> Alcotest.fail "expected outer function events"

let global_alias_selection_precedes_dimensions () =
  let events = ref [] in
  let commands =
    declaration_sink (fun event ->
        events := event :: !events;
        Ok ())
  in
  let _, _, output, _, _, _ =
    parse ~same_task:true ~commands {|I64 A=40;I64 A[#exe {I64 A=100;"2";}];|}
  in
  ignore (P.expect_ast output);
  let declarations =
    List.rev !events
    |> List.filter_map (function
      | Parser.Global_declared publication -> Some publication
      | _ -> None)
  in
  match declarations with
  | [ first; second ] -> (
      match second.global_previous with
      | Symbol_visibility.Present selected ->
          Alcotest.(check bool)
            "alias candidate is frozen at name token" true
            (selected == first.global_entry)
      | _ -> Alcotest.fail "lost selected earlier global")
  | _ -> Alcotest.fail "expected two outer globals"

let publication_failure_stops_lexing () =
  let events = ref 0 in
  let entered = ref 0 in
  let commands =
    declaration_sink (fun event ->
        incr events;
        match event with
        | Parser.Function_declared publication ->
            Error
              [
                Diagnostic.make ~code:"TESTPUB" ~severity:Diagnostic.Error
                  ~message:"rejected provisional function"
                  ~primary:publication.function_name.location.span ();
              ]
        | _ -> Alcotest.fail "no completion should follow rejected declaration")
  in
  let result =
    parse ~same_task:true ~commands
      ~on_enter:(fun () -> incr entered)
      {|I64 F(I64 n=#exe {"42";}){return n;}#exe {}|}
  in
  error "TESTPUB" result;
  Alcotest.(check int) "only provisional event delivered" 1 !events;
  Alcotest.(check int)
    "failure prevents default and following directives" 0 !entered

let eof_body_completion () =
  let completed = ref 0 in
  let commands =
    declaration_sink (fun event ->
        (match event with
        | Parser.Function_body_completed _ -> incr completed
        | _ -> ());
        Ok ())
  in
  let _, _, output, _, _, _ = parse ~same_task:true ~commands "I64 F()" in
  ignore (P.expect_ast output);
  Alcotest.(check int) "native EOF completes an empty body" 1 !completed

let buffered_function_header_selection () =
  List.iter
    (fun source ->
      let completed = ref None in
      let references = ref [] in
      let consume = function
        | Parser.Function_header_completed header ->
            completed := Some header;
            Ok ()
        | _ -> Ok ()
      in
      let commands =
        {
          (declaration_sink consume) with
          reference =
            Some
              (fun selection ->
                references := selection :: !references;
                Ok ());
        }
      in
      let _, _, output, _, _, _ = parse ~same_task:true ~commands source in
      ignore (P.expect_ast output);
      match (!completed, !references) with
      | Some header, [ selection ] -> (
          Alcotest.(check string)
            "buffered recursive callee" "F"
            (Parser.selected_identifier selection).spelling;
          match Parser.selected_lookup selection with
          | Symbol_visibility.Present entry ->
              Alcotest.(check bool)
                "buffered selection uses completed exact header" true
                (entry == header.completed_entry)
          | _ -> Alcotest.fail "lost buffered function selection")
      | _ ->
          Alcotest.fail "expected a completed header and its buffered reference")
    [ "I64 F(I64 n) F 42;"; "extern I64 F(I64 n) F 42;" ]

let consumed_provisional_selection () =
  let provisional = ref None in
  let completed = ref None in
  let references = ref [] in
  let reference selection =
    references := selection :: !references;
    Ok ()
  in
  let commands =
    declaration_sink (function
      | Parser.Function_declared publication ->
          provisional := Some publication;
          Ok ()
      | Parser.Function_header_completed header ->
          completed := Some header;
          Ok ()
      | _ -> Ok ())
  in
  let configure _ (execution : Parser.stream_execution) =
    {
      execution with
      commands = { execution.commands with reference = Some reference };
    }
  in
  let _, _, output, _, _, _ =
    parse ~same_task:true ~commands ~configure
      {|I64 F(I64 n=#exe {F();"1";}) F;|}
  in
  ignore (P.expect_ast output);
  match (!provisional, !completed, !references) with
  | Some publication, Some header, [ selection ] -> (
      match Parser.selected_lookup selection with
      | Symbol_visibility.Present entry ->
          Alcotest.(check bool)
            "consumed nested reference retains provisional snapshot" true
            (entry == publication.function_entry
            && entry != header.completed_entry);
          Alcotest.(check bool)
            "consumed snapshot still has no call shape" true
            (Option.is_none (Symbol_visibility.function_call_shape entry))
      | _ -> Alcotest.fail "lost nested provisional selection")
  | _ -> Alcotest.fail "expected one consumed nested reference"

let buffered_selection_preserves_newer_function () =
  let completed = ref None in
  let references = ref [] in
  let commands =
    {
      (declaration_sink (function
        | Parser.Function_header_completed header ->
            completed := Some header;
            Ok ()
        | _ -> Ok ()))
      with
      reference =
        Some
          (fun selection ->
            references := selection :: !references;
            Ok ());
    }
  in
  let _, _, output, _, _, _ =
    parse ~same_task:true ~commands
      {|I64 F(I64 n=#exe {I64 F(I64 a,I64 b){return a+b;}"1";}) F 20 22;|}
  in
  ignore (P.expect_ast output);
  match (!completed, !references) with
  | Some header, [ selection ] -> (
      match Parser.selected_lookup selection with
      | Symbol_visibility.Present entry ->
          Alcotest.(check bool)
            "older header completion does not replace selected shadow" true
            (entry != header.completed_entry);
          Alcotest.(check int)
            "selected shadow still takes two arguments" 2
            (List.length
               (Option.get (Symbol_visibility.function_call_shape entry))
                 .parameters)
      | _ -> Alcotest.fail "lost newer function selection")
  | _ -> Alcotest.fail "expected buffered shadow selection"

let prototype_source_identity () =
  let completed = ref None in
  let commands =
    declaration_sink (function
      | Parser.Function_header_completed header ->
          completed := Some header;
          Ok ()
      | _ -> Ok ())
  in
  let _, _, output, _, _, _ =
    parse ~same_task:true ~commands "extern I64 F(I64 n=42);"
  in
  match ((P.expect_ast output).items, !completed) with
  | [ Ast.Function_prototype prototype ], Some header ->
      Alcotest.(check bool)
        "prototype shares original parentheses and parameter nodes" true
        (prototype.opening_parenthesis
         == header.function_publication.function_opening_parenthesis
        && prototype.closing_parenthesis == header.closing_parenthesis
        && prototype.parameters == header.parameters)
  | _ -> Alcotest.fail "expected completed prototype header"

let body_sequence_publication () =
  let phase = ref "absent" in
  let seen = ref [] in
  let commands =
    declaration_sink (fun event ->
        (match event with
        | Parser.Function_header_completed _ -> phase := "header"
        | Parser.Function_body_completed _ -> phase := "body"
        | _ -> ());
        Ok ())
  in
  let _, _, output, _, _, _ =
    parse ~same_task:true ~commands
      ~on_enter:(fun () -> seen := !phase :: !seen)
      "I64 F() 1,2;#exe {};#exe {}"
  in
  ignore (P.expect_ast output);
  Alcotest.(check (list string))
    "body completion follows entire statement sequence" [ "header"; "body" ]
    (List.rev !seen)

let declaration_environment_ownership () =
  let declarations = ref [] in
  let consume event =
    (match event with
    | Parser.Global_declared publication ->
        declarations := publication :: !declarations
    | _ -> ());
    Ok ()
  in
  let commands = declaration_sink consume in
  let configure _ (execution : Parser.stream_execution) =
    {
      execution with
      commands = { execution.commands with declaration = Some consume };
    }
  in
  let session, _, output, _, _, _ =
    parse ~mode:Preprocessor.Aot ~commands ~configure
      "I64 Outer;#exe {I64 Inner;}"
  in
  let ast = P.expect_ast output in
  match (List.rev !declarations, ast.items) with
  | [ outer; inner ], [ Ast.Global_variable variable ] ->
      Alcotest.(check bool)
        "outer publication retains outer environment" true
        (outer.global_environment == Session.symbols session);
      Alcotest.(check bool)
        "task publication owns separate task environment" false
        (inner.global_environment == outer.global_environment);
      Alcotest.(check bool)
        "singleton AST retains declaration children" true
        (outer.global_name == variable.name
        && outer.global_header.type_specifier == variable.type_specifier
        && outer.global_dimensions == variable.array_dimensions)
  | _ -> Alcotest.fail "expected outer and task publications"

let command_receipt_ownership () =
  let contexts = ref [] in
  let sequences = ref [] in
  let references = ref [] in
  let declarations = ref [] in
  let checkpoint event =
    (match event with
    | Parser.Sequence_started context -> contexts := context :: !contexts
    | Parser.Sequence_completed sequence -> sequences := sequence :: !sequences
    | _ -> ());
    Ok ()
  in
  let sink =
    {
      (declaration_sink (fun event ->
           declarations := event :: !declarations;
           Ok ()))
      with
      Parser.checkpoint = Some checkpoint;
      reference =
        Some
          (fun selection ->
            references := selection :: !references;
            Ok ());
    }
  in
  let configure _ (execution : Parser.stream_execution) =
    {
      execution with
      commands = { execution.commands with checkpoint = Some checkpoint };
    }
  in
  let session, source, output, _, _, _ =
    parse ~mode:Preprocessor.Aot ~same_task:true ~commands:sink ~configure
      {|#exe {} I64 A=#exe {"42";};1;#exe {} I64 B;#exe {} A;|}
  in
  let ast = P.expect_ast output in
  let contexts = List.rev !contexts in
  let outer = List.hd contexts in
  Alcotest.(check bool)
    "root has no parent" true
    (Option.is_none (Parser.context_parent outer));
  Alcotest.(check bool)
    "outer mode remains AOT" true
    (Parser.context_mode outer = Preprocessor.Aot);
  List.iter
    (fun context ->
      Alcotest.(check bool)
        "context owns exact registered input" true
        (Parser.context_source context == source
        && Parser.context_sources context == Session.sources session);
      Alcotest.(check bool)
        "context owns selected task environment" true
        (Parser.context_environment context == Session.symbols session))
    contexts;
  let kinds =
    List.tl contexts
    |> List.map (fun context ->
        Alcotest.(check bool)
          "nested mode is temporarily JIT" true
          (Parser.context_mode context = Preprocessor.Jit);
        match Parser.context_parent context with
        | Some (Parser.Before_first_command parent) ->
            Alcotest.(check bool)
              "initial directive owns outer context" true (parent == outer);
            0
        | Some (Parser.Reading_command start) ->
            Alcotest.(check bool)
              "initializer suspends outer declaration" true
              (start.command_context == outer);
            1
        | Some (Parser.Awaiting_resume command) ->
            Alcotest.(check bool)
              "pending directive owns outer command" true
              (command.command_start.command_context == outer);
            2
        | None -> Alcotest.fail "nested context lost its parent")
  in
  Alcotest.(check (list int))
    "nested entry retains all suspended phases" [ 0; 1; 1; 2 ] kinds;
  List.iter
    (fun (sequence : Parser.completed_sequence) ->
      let previous = ref None in
      let items =
        List.mapi
          (fun ordinal (command : Parser.completed_command) ->
            Alcotest.(check int)
              "original command ordinal" ordinal
              command.command_start.command_ordinal;
            Alcotest.(check bool)
              "exact completed predecessor" true
              (match (command.command_start.command_predecessor, !previous) with
              | None, None -> true
              | Some left, Some right -> left == right
              | _ -> false);
            previous := Some command;
            match command.command_ast.items with
            | [ item ] -> item
            | _ -> Alcotest.fail "partial command view")
          sequence.sequence_commands
      in
      Alcotest.(check bool)
        "sequence reuses complete source commands" true
        (List.for_all2 ( == ) items sequence.sequence_ast.items);
      if sequence.sequence_context == outer then
        Alcotest.(check bool)
          "parse returns original sequence view" true
          (sequence.sequence_ast == ast))
    !sequences;
  let declaration_start =
    List.find_map
      (function
        | Parser.Global_declared publication ->
            Some publication.global_header.declaration_command
        | _ -> None)
      !declarations
    |> Option.get
  in
  Alcotest.(check bool)
    "declaration owns original outer command" true
    (declaration_start.command_context == outer);
  let selected =
    List.find
      (fun receipt -> (Parser.selected_identifier receipt).spelling = "A")
      !references
  in
  Alcotest.(check bool)
    "selected occurrence owns later outer command" true
    ((Parser.selected_command selected).command_context == outer
    && (Parser.selected_command selected).command_ordinal
       > declaration_start.command_ordinal)

let checkpoint_failure_cleanup () =
  List.iter
    (fun (prefix, reached_phase) ->
      List.iter
        (fun phase ->
          let aborts = ref 0 in
          let entered = ref 0 in
          let checkpoint event =
            let kind, context =
              match event with
              | Parser.Sequence_started context -> (0, context)
              | Parser.Command_started start -> (1, start.command_context)
              | Parser.Command_completed command ->
                  (2, command.command_start.command_context)
              | Parser.Command_resumed command ->
                  (3, command.command_start.command_context)
              | Parser.Sequence_completed sequence ->
                  (4, sequence.sequence_context)
              | Parser.Sequence_aborted context ->
                  incr aborts;
                  (5, context)
            in
            if kind = phase then
              Error
                [
                  Diagnostic.make ~code:"TESTCHECKPOINT"
                    ~severity:Diagnostic.Warning
                    ~message:"consumer rejected checkpoint"
                    ~primary:
                      (Span.unsafe_make
                         ~source:
                           (Source_file.id (Parser.context_source context))
                         ~start:0 ~stop:0)
                    ();
                ]
            else Ok ()
          in
          let commands =
            {
              (declaration_sink (fun _ -> Ok ())) with
              Parser.checkpoint = Some checkpoint;
            }
          in
          let _, _, output, _, _, _ =
            parse ~commands
              ~on_enter:(fun () -> incr entered)
              (prefix ^ "#exe {}")
          in
          Alcotest.(check bool)
            "warning-only rejection is fatal" true (Parser.has_errors output);
          Alcotest.(check bool)
            "missing fatal diagnostic supplied" true
            (List.exists
               (fun d -> d.Diagnostic.code = "HCPARSE0161")
               output.diagnostics);
          Alcotest.(check int) "one context abort" 1 !aborts;
          Alcotest.(check int)
            "lookahead retains source grammar timing"
            (if phase < reached_phase then 0 else 1)
            !entered)
        [ 0; 1; 2; 3; 4 ])
    [ ("1;", 2); ("I64 N;", 3) ]

let query_consumption_order () =
  let events = ref [] in
  let trace = ref [] in
  let query event =
    events := event :: !events;
    trace :=
      (match event with
      | Parser.Query_root _ -> "root"
      | Parser.Query_member_started _ -> "dot"
      | Parser.Query_member _ -> "member"
      | Parser.Query_completed _ -> "complete")
      :: !trace;
    Ok ()
  in
  let commands : Parser.command_sink =
    {
      checkpoint = None;
      reference = None;
      query = Some query;
      declaration = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let session, _, output, _, _, _ =
    parse ~same_task:true ~commands
      ~on_enter:(fun () -> trace := "enter" :: !trace)
      "defined(Missing #exe {I64 Missing;});sizeof I64 #exe {}.one #exe {}.two \
       #exe {};offset I64 #exe {}.one #exe {}.two #exe {};defined 42;defined \
       return;"
  in
  let ast = P.expect_ast output in
  Alcotest.(check (list string))
    "each query read precedes its following directive"
    [
      "root";
      "enter";
      "complete";
      "root";
      "enter";
      "dot";
      "member";
      "enter";
      "dot";
      "member";
      "enter";
      "complete";
      "root";
      "enter";
      "dot";
      "member";
      "enter";
      "dot";
      "member";
      "enter";
      "complete";
      "root";
      "complete";
      "root";
      "complete";
    ]
    (List.rev !trace);
  let completed =
    List.filter_map
      (function
        | Parser.Query_completed query -> Some query
        | _ -> None)
      (List.rev !events)
  in
  List.iter2
    (fun query item ->
      let expression =
        match item with
        | Ast.Top_level_statement (Ast.Expression_statement statement) ->
            statement.expression_statement_expression
        | _ -> Alcotest.fail "expected query statement"
      in
      Alcotest.(check bool)
        "original completed expression" true
        (query.Parser.query_expression == expression);
      let root = query.query_root in
      Alcotest.(check bool)
        "exact frontend owner" true
        (root.query_environment == Session.symbols session);
      Alcotest.(check bool)
        "root command owns original statement" true
        (List.exists
           (function
             | Parser.Query_root original -> original == root
             | _ -> false)
           !events);
      match (root.query_node, expression) with
      | Parser.Defined_target operand, Ast.Defined_expression expression ->
          Alcotest.(check bool)
            "original operand" true
            (operand == expression.defined_operand);
          Alcotest.(check bool)
            "native identifier-like presence"
            (operand.defined_operand_spelling = "return")
            root.query_present
      | Parser.Sizeof_target target, Ast.Sizeof_expression expression ->
          Alcotest.(check bool)
            "original sizeof root" true
            (target == expression.sizeof_target);
          List.iter2
            (fun receipt member ->
              match receipt.Parser.query_member_node with
              | Parser.Sizeof_member original ->
                  Alcotest.(check bool)
                    "original sizeof member" true (original == member);
                  Alcotest.(check bool)
                    "sizeof dot keeps its exact start child" true
                    (receipt.query_member_start.member_start_dot
                   == original.sizeof_member_dot)
              | _ -> Alcotest.fail "wrong query member kind")
            query.query_members expression.sizeof_members
      | Parser.Offset_target target, Ast.Offset_expression expression ->
          Alcotest.(check bool)
            "original offset root" true
            (target == expression.offset_target);
          List.iter2
            (fun receipt member ->
              match receipt.Parser.query_member_node with
              | Parser.Offset_member original ->
                  Alcotest.(check bool)
                    "original offset member" true (original == member);
                  Alcotest.(check bool)
                    "offset dot keeps its exact start child" true
                    (receipt.query_member_start.member_start_dot
                   == original.offset_member_dot)
              | _ -> Alcotest.fail "wrong query member kind")
            query.query_members expression.offset_members
      | _ -> Alcotest.fail "query receipt substituted its AST child")
    completed ast.items

let query_native_presence () =
  let present = ref [] in
  let query = function
    | Parser.Query_root root ->
        present := root.query_present :: !present;
        Ok ()
    | _ -> Ok ()
  in
  let commands : Parser.command_sink =
    {
      checkpoint = None;
      reference = None;
      query = Some query;
      declaration = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let _, _, output, _, _, _ =
    parse ~commands
      ~configure:(fun _ execution ->
        {
          execution with
          Parser.symbols = Symbol_visibility.Environment.create ();
          commands;
        })
      "defined return;defined Missing;defined 42;\n\
       #define NUMBER 42\n\
       defined NUMBER;I64 F(I64 n){return defined n;}#exe {defined return;}"
  in
  ignore (P.expect_ast output);
  Alcotest.(check (list bool))
    "presence uses expanded token and saved hash/local"
    [ true; false; false; false; true; false ]
    (List.rev !present)

let query_rejection_order () =
  List.iter
    (fun member ->
      let reached = ref false in
      let commands : Parser.command_sink =
        {
          checkpoint = None;
          reference = None;
          declaration = None;
          query =
            Some
              (fun event ->
                let reject, location =
                  match event with
                  | Parser.Query_root root -> (not member, root.query_location)
                  | Parser.Query_member_started start ->
                      (false, start.member_start_dot)
                  | Parser.Query_member receipt ->
                      (member, receipt.query_member_root.query_location)
                  | Parser.Query_completed query ->
                      (false, query.query_root.query_location)
                in
                if reject then
                  Error
                    [
                      Diagnostic.make ~code:"TESTQUERY"
                        ~severity:Diagnostic.Error ~message:"query read failed"
                        ~primary:location.span ();
                    ]
                else Ok ());
          command = (fun _ -> Ok ());
          resume = (fun () -> Ok ());
        }
      in
      let result =
        parse ~commands
          ~on_enter:(fun () -> reached := true)
          (if member then "sizeof I64.missing #exe {};"
           else "sizeof Missing #exe {};")
      in
      error "TESTQUERY" result;
      Alcotest.(check bool)
        "failed read stops following directive" false !reached)
    [ false; true ]

let tests =
  [
    Alcotest.test_case "query receipts retain native consumption order" `Quick
      query_consumption_order;
    Alcotest.test_case "query rejection stops later directives" `Quick
      query_rejection_order;
    Alcotest.test_case "query presence follows native identifier tokens" `Quick
      query_native_presence;
    Alcotest.test_case
      "command receipts preserve source, parent and predecessor" `Quick
      command_receipt_ownership;
    Alcotest.test_case "checkpoint rejection stops delivery and aborts context"
      `Quick checkpoint_failure_cleanup;
    Alcotest.test_case "EOF completes native empty function syntax" `Quick
      eof_body_completion;
    Alcotest.test_case "buffered function reference sees completed header"
      `Quick buffered_function_header_selection;
    Alcotest.test_case "consumed nested reference retains provisional header"
      `Quick consumed_provisional_selection;
    Alcotest.test_case "buffered function reference retains newer shadow" `Quick
      buffered_selection_preserves_newer_function;
    Alcotest.test_case "prototype completion shares exact source nodes" `Quick
      prototype_source_identity;
    Alcotest.test_case "nonblock sequence finishes before body publication"
      `Quick body_sequence_publication;
    Alcotest.test_case "publication witnesses own outer or task environment"
      `Quick declaration_environment_ownership;
    Alcotest.test_case "global events precede later initializer lookahead"
      `Quick global_publication_timing;
    Alcotest.test_case "function events preserve native header and body timing"
      `Quick function_publication_timing;
    Alcotest.test_case "header completion preserves intervening shadow" `Quick
      function_completion_preserves_shadow;
    Alcotest.test_case "global alias selection precedes dimension directives"
      `Quick global_alias_selection_precedes_dimensions;
    Alcotest.test_case "publication rejection stops all later token pulls"
      `Quick publication_failure_stops_lexing;
    Alcotest.test_case "function identity precedes parameter defaults" `Quick
      (fun () ->
        let visible = ref false in
        let configure _ (execution : Parser.stream_execution) =
          (visible :=
             match
               Symbol_visibility.Environment.find_preprocessor execution.symbols
                 "F"
             with
             | Symbol_visibility.Present entry ->
                 Symbol_visibility.kind entry = Symbol_visibility.Function
             | _ -> false);
          execution
        in
        let _, _, output, _, _, _ =
          parse ~same_task:true ~configure
            {|I64 F(I64 n=#exe {"42";}){return n;}|}
        in
        ignore (P.expect_ast output);
        Alcotest.(check bool)
          "provisional function is visible in its default" true !visible);
    Alcotest.test_case "prototype header precedes following directive" `Quick
      (fun () ->
        let _, _, output, _, _, _ =
          parse ~same_task:true
            "extern I64 F(I64 n)#exe {\n\
             #ifdef F\n\
             \";42;\";\n\
             #else\n\
             \";0;\";\n\
             #endif\n\
             }"
        in
        match (P.expect_ast output).items with
        | Ast.Function_prototype _
          :: [
               Ast.Top_level_statement
                 (Ast.Expression_statement
                    {
                      expression_statement_expression =
                        Ast.Integer_literal
                          { literal_value = Ast.Integer_value value; _ };
                      _;
                    });
             ] -> Alcotest.(check int64) "completed header is visible" 42L value
        | _ -> Alcotest.fail "expected prototype and generated integer");
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
