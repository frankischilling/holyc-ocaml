open Holyc_lib
module Cases = Compiler_exception_cases

let describe diagnostics =
  diagnostics
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"compiler-exception.HC" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  (session, source, config)

let command_sink ?checkpoint ?(resume = fun () -> Ok ())
    ?(command = fun _ -> Ok ()) () : Parser.command_sink =
  {
    checkpoint;
    resume;
    command;
    lexical_lookup = None;
    reference = None;
    call = None;
    implicit_output = None;
    query = None;
    declaration = None;
    dimension_count = None;
  }

let parse ?commands ?compiler_exception session source config =
  Parser.parse ?commands ?compiler_exception ~sources:(Session.sources session)
    ~definitions:(Session.definitions session)
    ~symbols:(Session.symbols session) ~config source

let receipt ?(reported_origin = true) label diagnostics exceptions =
  match exceptions with
  | [ exception_ ] ->
      let diagnostic = Parser.compiler_exception_diagnostic exception_ in
      Alcotest.(check string)
        (label ^ " original producer")
        "HCPARSE0168" diagnostic.code;
      Alcotest.(check bool)
        (label ^ " original diagnostic origin")
        true
        (List.exists
           (fun (reached : Diagnostic.t) ->
             reached.code = diagnostic.code
             && ((not reported_origin)
                || reached.message = diagnostic.message
                   && reached.primary = diagnostic.primary
                   && reached.secondary = diagnostic.secondary
                   && reached.include_stack = diagnostic.include_stack))
           diagnostics);
      Alcotest.(check int64)
        (label ^ " one LexExcept increment")
        1L
        (Parser.compiler_exception_error_count exception_);
      let context = Parser.compiler_exception_context exception_ in
      let source =
        Source_manager.find
          (Parser.context_sources context)
          diagnostic.primary.source
        |> Option.get
      in
      Alcotest.(check string)
        (label ^ " exact original keyword source")
        "return"
        (String.sub
           (Source_file.contents source)
           diagnostic.primary.start
           (diagnostic.primary.stop - diagnostic.primary.start));
      Alcotest.(check bool)
        (label ^ " original closed context")
        true
        (Parser.compiler_exception_is_from_context exception_ context);
      Alcotest.(check bool)
        (label ^ " closed control denies field access")
        true
        (Result.is_error (Parser.context_error_count context));
      Gc.full_major ();
      Gc.compact ();
      Alcotest.(check bool)
        (label ^ " receipt survives compaction")
        true
        (Parser.compiler_exception_is_from_context exception_ context)
  | _ ->
      Alcotest.failf "%s: expected one original Compiler receipt, got %d" label
        (List.length exceptions)

let original_phase () =
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          let session, source, config = inputs mode text in
          let seen = ref [] in
          let compiler_exception exception_ =
            let context = Parser.compiler_exception_context exception_ in
            Alcotest.(check bool)
              "receipt is original while throwing" true
              (Parser.compiler_exception_is_from_context exception_ context);
            Alcotest.(check int64)
              "native error field already incremented" 1L
              (Parser.context_error_count context |> Result.get_ok);
            seen := exception_ :: !seen
          in
          let parsed =
            parse ~commands:(command_sink ()) ~compiler_exception session source
              config
          in
          Alcotest.(check bool)
            "failed source has no accepted AST" true
            (Option.is_none parsed.ast);
          receipt "return" parsed.diagnostics !seen;
          let diagnostic = List.hd parsed.diagnostics in
          Alcotest.(check int)
            "stops at keyword before later malformed expression" 0
            diagnostic.primary.start)
        [ "return;"; "return 42;"; "return #error late\n42;"; "return @" ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let representation_and_functions () =
  List.iter
    (fun mode ->
      let session, source, config = inputs mode "return 42;" in
      let seen = ref [] in
      let parsed =
        parse
          ~compiler_exception:(fun x -> seen := x :: !seen)
          session source config
      in
      Alcotest.(check bool)
        "representation parser keeps syntactic return" true
        (Option.is_some parsed.ast);
      Alcotest.(check int)
        "representation grants no Compiler authority" 0 (List.length !seen);
      let session, source, config = inputs mode "I64 F(){return 42;}F();" in
      let parsed =
        parse ~commands:(command_sink ())
          ~compiler_exception:(fun x -> seen := x :: !seen)
          session source config
      in
      Alcotest.(check bool)
        (describe parsed.diagnostics)
        true
        (Option.is_some parsed.ast);
      Alcotest.(check int)
        "function return does not count an error" 0 (List.length !seen))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let child_ownership () =
  let session, source, config = inputs Preprocessor.Jit "1;42;" in
  let parent = ref None in
  let child_exception = ref None in
  let child_context = ref None in
  let resume () =
    if Option.is_none !child_exception then (
      let parent = Option.get !parent in
      let suspension = Parser.suspend_context parent |> Result.get_ok in
      let child =
        Session.add_source session ~path:"original-child.HC"
          ~contents:"return #error late\n42;"
      in
      let parsed =
        Parser.parse_suspended suspension ~commands:(command_sink ())
          ~compiler_exception:(fun exception_ ->
            child_exception := Some exception_;
            child_context := Some (Parser.compiler_exception_context exception_))
          ~sources:(Session.sources session)
          ~definitions:(Session.definitions session)
          ~symbols:(Session.symbols session) ~config child
        |> Result.get_ok
      in
      receipt "child" parsed.diagnostics [ Option.get !child_exception ];
      Alcotest.(check int64)
        "ordinary child leaves original parent error count zero" 0L
        (Parser.context_error_count parent |> Result.get_ok);
      Alcotest.(check bool)
        "child cannot replay consumed suspension" true
        (Result.is_error
           (Parser.parse_suspended suspension ~commands:(command_sink ())
              ~sources:(Session.sources session)
              ~definitions:(Session.definitions session)
              ~symbols:(Session.symbols session) ~config child));
      Alcotest.(check bool)
        "same sources cannot substitute parent for child" false
        (Parser.compiler_exception_is_from_context
           (Option.get !child_exception)
           parent));
    Ok ()
  in
  let checkpoint = function
    | Parser.Sequence_started context ->
        parent := Some context;
        Ok ()
    | _ -> Ok ()
  in
  let parsed =
    parse ~commands:(command_sink ~checkpoint ~resume ()) session source config
  in
  Alcotest.(check bool)
    "parser caller can retain its ordinary input" true
    (Option.is_some parsed.ast);
  let exception_ = Option.get !child_exception
  and context = Option.get !child_context in
  let foreign =
    Domain.spawn (fun () ->
        Parser.compiler_exception_is_from_context exception_ context)
    |> Domain.join
  in
  Alcotest.(check bool)
    "foreign domain has no exception authority" false foreign

let arbitrary_diagnostics () =
  let session, source, config = inputs Preprocessor.Jit "42;" in
  let seen = ref [] in
  let command _ =
    Error
      [
        Diagnostic.make ~code:"HCPARSE0168" ~severity:Diagnostic.Error
          ~message:"return requires an active function"
          ~primary:
            (Span.unsafe_make ~source:(Source_file.id source) ~start:0 ~stop:2)
          ();
      ]
  in
  let parsed =
    parse ~commands:(command_sink ~command ())
      ~compiler_exception:(fun x -> seen := x :: !seen)
      session source config
  in
  Alcotest.(check bool)
    "callback error still fails source" true
    (Option.is_none parsed.ast);
  Alcotest.(check int)
    "matching diagnostic cannot forge Compiler receipt" 0 (List.length !seen)

let source_failures () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text, output) ->
          let session, source, config =
            inputs mode
              ((if mode = Preprocessor.Jit then Cases.headers else "") ^ text)
          in
          let report =
            run_integer_program_report session ~source ~config
              ~max_steps:100_000
          in
          let diagnostics =
            match integer_program_report_outcome report with
            | Error errors -> errors
            | Ok _ -> Alcotest.fail (label ^ " unexpectedly succeeded")
          in
          receipt
            ~reported_origin:
              (label <> "child" && label <> "saved function child")
            label diagnostics
            (integer_program_report_compiler_exceptions report);
          Alcotest.(check string)
            (label ^ " reached output")
            output
            (integer_program_report_output_bytes report))
        Cases.failures)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let other_source_failures () =
  List.iter
    (fun (_, text, _) ->
      let session, source, config = inputs Preprocessor.Jit text in
      let report =
        run_integer_program_report session ~source ~config ~max_steps:100_000
      in
      Alcotest.(check bool)
        "ordinary error fails" true
        (Result.is_error (integer_program_report_outcome report));
      Alcotest.(check int)
        "ordinary error supplies no Compiler signal" 0
        (List.length (integer_program_report_compiler_exceptions report)))
    Cases.ordinary_failures;
  let session, source, config =
    inputs Preprocessor.Jit
      (Cases.headers ^ {|#exe {Print("kept");return 42;}42;|})
  in
  let report =
    run_integer_program_report session ~source ~config ~max_steps:1
  in
  Alcotest.(check bool)
    "earlier IR quota fails" true
    (Result.is_error (integer_program_report_outcome report));
  Alcotest.(check int)
    "quota prevents later Compiler producer" 0
    (List.length (integer_program_report_compiler_exceptions report))

let compilation_receipts () =
  List.iter
    (fun mode ->
      let session, source, config = inputs mode "return 42;" in
      let report = compile_integer_program_report session ~source ~config in
      let diagnostics =
        match integer_program_compilation_result report with
        | Error diagnostics -> diagnostics
        | Ok _ ->
            Alcotest.fail "failed return cannot produce a compilation unit"
      in
      receipt "compilation" diagnostics
        (integer_program_compilation_compiler_exceptions report))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let tests =
  [
    Alcotest.test_case "original return failure before expression Lex" `Quick
      original_phase;
    Alcotest.test_case "representation parser and active functions" `Quick
      representation_and_functions;
    Alcotest.test_case "original child control, suspension and domain" `Quick
      child_ownership;
    Alcotest.test_case "diagnostic text cannot forge Compiler" `Quick
      arbitrary_diagnostics;
    Alcotest.test_case "IR root, directive and saved child failures" `Quick
      source_failures;
    Alcotest.test_case "other source faults remain separate" `Quick
      other_source_failures;
    Alcotest.test_case "original source compilation failure receipts" `Quick
      compilation_receipts;
  ]
