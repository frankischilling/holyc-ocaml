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

let command_sink ?checkpoint ?call ?(resume = fun () -> Ok ())
    ?(command = fun _ -> Ok ()) () : Parser.command_sink =
  {
    checkpoint;
    resume;
    command;
    lexical_lookup = None;
    reference = None;
    call;
    implicit_output = None;
    query = None;
    declaration = None;
    dimension_count = None;
  }

let parse ?commands ?compiler_exception ?execute_stream session source config =
  Parser.parse ?commands ?compiler_exception ?execute_stream
    ~sources:(Session.sources session)
    ~definitions:(Session.definitions session)
    ~symbols:(Session.symbols session) ~config source

let receipt ?(error_count = 1L) ?(reported_origin = true)
    ?(code = "HCPARSE0168") ?(marker = "return") label diagnostics exceptions =
  match exceptions with
  | [ exception_ ] ->
      let diagnostic = Parser.compiler_exception_diagnostic exception_ in
      Alcotest.(check string)
        (label ^ " original producer")
        code diagnostic.code;
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
        error_count
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
        marker
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
      Alcotest.failf "%s: expected one original Compiler receipt, got %d (%s)"
        label (List.length exceptions) (describe diagnostics)

let expression_receipt ?(reported_origin = true) ~code ~marker label diagnostics
    exceptions =
  match exceptions with
  | [ original; cleanup ] ->
      let original_diagnostic = Parser.compiler_exception_diagnostic original in
      let cleanup_diagnostic = Parser.compiler_exception_diagnostic cleanup in
      Alcotest.(check string)
        (label ^ " original expression producer")
        code original_diagnostic.code;
      Alcotest.(check int64)
        (label ^ " original increment precedes cleanup")
        1L
        (Parser.compiler_exception_error_count original);
      Alcotest.(check bool)
        (label ^ " original producer has no synthetic cause")
        true
        (Option.is_none (Parser.compiler_exception_cause original));
      Alcotest.(check bool)
        (label ^ " cleanup retains exact caught producer")
        true
        (match Parser.compiler_exception_cause cleanup with
        | Some cause -> cause == original
        | None -> false);
      Alcotest.(check bool)
        (label ^ " both producers retain exact context")
        true
        (Parser.compiler_exception_context original
        == Parser.compiler_exception_context cleanup);
      Alcotest.(check bool)
        (label ^ " cleanup retains current token without another Lex")
        true
        (original_diagnostic.primary = cleanup_diagnostic.primary
        && original_diagnostic.secondary = cleanup_diagnostic.secondary
        && original_diagnostic.include_stack = cleanup_diagnostic.include_stack
        );
      Alcotest.(check bool)
        (label ^ " original diagnostic retained")
        true
        (List.exists
           (fun (d : Diagnostic.t) ->
             d.code = code
             && ((not reported_origin)
                || d.primary = original_diagnostic.primary
                   && d.message = original_diagnostic.message))
           diagnostics);
      receipt ~error_count:2L ~reported_origin ~code:"HCPARSE0173" ~marker label
        diagnostics [ cleanup ];
      let context = Parser.compiler_exception_context cleanup in
      let copied : Parser.compiler_exception =
        Obj.obj (Obj.dup (Obj.repr cleanup))
      in
      Alcotest.(check bool)
        (label ^ " copied cleanup cannot replace original")
        false
        (Parser.compiler_exception_is_from_context copied context);
      Alcotest.(check bool)
        (label ^ " caught cause cannot replace terminal cleanup")
        false
        (Parser.compiler_exception_is_from_context original context);
      let foreign =
        Domain.spawn (fun () ->
            Parser.compiler_exception_is_from_context cleanup context)
        |> Domain.join
      in
      Alcotest.(check bool)
        (label ^ " foreign domain cannot claim cleanup")
        false foreign;
      Gc.full_major ();
      Gc.compact ();
      Alcotest.(check bool)
        (label ^ " original cause survives collection")
        true
        (match Parser.compiler_exception_cause cleanup with
        | Some cause -> cause == original
        | None -> false)
  | _ ->
      Alcotest.failf "%s: expected producer and one cleanup, got %d (%s)" label
        (List.length exceptions) (describe diagnostics)

let nested_expression_receipt label diagnostics exceptions =
  match exceptions with
  | [ original; inner; outer ] ->
      let reached_diagnostics =
        List.filter
          (fun diagnostic ->
            List.exists
              (fun e -> diagnostic == Parser.compiler_exception_diagnostic e)
              exceptions)
          diagnostics
      in
      Alcotest.(check bool)
        (label ^ " diagnostics preserve original producer order")
        true
        (List.length reached_diagnostics = List.length exceptions
        && List.for_all2 ( == ) reached_diagnostics
             (List.map Parser.compiler_exception_diagnostic exceptions));
      Alcotest.(check (list int64))
        (label ^ " exact reached counts")
        [ 1L; 2L; 3L ]
        (List.map Parser.compiler_exception_error_count exceptions);
      Alcotest.(check (list string))
        (label ^ " original producer order")
        [ "HCPARSE0018"; "HCPARSE0173"; "HCPARSE0173" ]
        (List.map
           (fun e -> (Parser.compiler_exception_diagnostic e).code)
           exceptions);
      Alcotest.(check bool)
        (label ^ " inner cleanup retains first cause")
        true
        (Option.fold ~none:false ~some:(( == ) original)
           (Parser.compiler_exception_cause inner));
      Alcotest.(check bool)
        (label ^ " outer cleanup retains inner cause")
        true
        (Option.fold ~none:false ~some:(( == ) inner)
           (Parser.compiler_exception_cause outer));
      Alcotest.(check bool)
        (label ^ " child owns inner stack")
        true
        (Parser.compiler_exception_context original
        == Parser.compiler_exception_context inner);
      Alcotest.(check bool)
        (label ^ " parent owns separate outer stack")
        false
        (Parser.compiler_exception_context inner
        == Parser.compiler_exception_context outer);
      Alcotest.(check bool)
        (label ^ " cleanup never reads a later token")
        true
        (let primary =
           (Parser.compiler_exception_diagnostic original).primary
         in
         List.for_all
           (fun e -> (Parser.compiler_exception_diagnostic e).primary = primary)
           exceptions);
      receipt ~error_count:3L ~reported_origin:false ~code:"HCPARSE0173"
        ~marker:";" label diagnostics [ outer ]
  | _ ->
      Alcotest.failf "%s: expected three original producers, got %d (%s)" label
        (List.length exceptions) (describe diagnostics)

let call_receipt ?(reported_origin = true) ~code ~marker label diagnostics
    exceptions =
  if Cases.call_error_count code = 2 then
    expression_receipt ~reported_origin ~code ~marker label diagnostics
      exceptions
  else receipt ~reported_origin ~code ~marker label diagnostics exceptions

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
  List.iter
    (fun (code, message) ->
      let session, source, config = inputs Preprocessor.Jit "42;" in
      let seen = ref [] in
      let command _ =
        Error
          [
            Diagnostic.make ~code ~severity:Diagnostic.Error ~message
              ~primary:
                (Span.unsafe_make ~source:(Source_file.id source) ~start:0
                   ~stop:2)
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
        "matching diagnostic cannot forge Compiler receipt" 0
        (List.length !seen))
    [
      ("HCPARSE0168", "return requires an active function");
      ("HCPARSE0170", "break requires an active break target");
      ("HCPARSE0171", "try requires function headers for SysTry and SysUntry");
      ("HCPARSE0052", "expected '(' after 'if', but found integer");
      ("HCPARSE0018", "expected expression, but found ';'");
      ("HCPARSE0173", "compiler expression stack is nonempty after Compiler");
    ]

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

let suspended_parent consume =
  let session, source, config = inputs Preprocessor.Jit "42;" in
  let parent = ref None in
  let entered = ref false in
  let checkpoint = function
    | Parser.Sequence_started context ->
        parent := Some context;
        Ok ()
    | _ -> Ok ()
  in
  let resume () =
    if not !entered then (
      entered := true;
      let context = Option.get !parent in
      let suspension = Parser.suspend_context context |> Result.get_ok in
      consume session config context suspension);
    Ok ()
  in
  let parsed =
    parse ~commands:(command_sink ~checkpoint ~resume ()) session source config
  in
  Alcotest.(check bool)
    "parent parser continues" true
    (Option.is_some parsed.ast)

let suspended_parse ?compiler_exception ?execute_stream ?commands session config
    suspension source =
  Parser.parse_suspended suspension ?compiler_exception ?execute_stream
    ~commands:(Option.value commands ~default:(command_sink ()))
    ~sources:(Session.sources session)
    ~definitions:(Session.definitions session)
    ~symbols:(Session.symbols session) ~config source

let failed_child session config suspension text =
  let source =
    Session.add_source session ~path:"failed-child.HC" ~contents:text
  in
  let parsed =
    suspended_parse session config suspension source |> Result.get_ok
  in
  Alcotest.(check bool)
    "child has no accepted syntax" true
    (Option.is_none parsed.ast);
  match Parser.suspension_failed_input suspension with
  | Some failure -> failure
  | None -> Alcotest.fail "original failed child has no suspension receipt"

let failed_input_identity () =
  let saved = ref None in
  suspended_parent (fun session config parent suspension ->
      let copied_parent : Parser.command_context =
        Obj.obj (Obj.dup (Obj.repr parent))
      in
      Alcotest.(check bool)
        "copied parent cannot issue a suspension" true
        (Result.is_error (Parser.suspend_context copied_parent));
      let failure = failed_child session config suspension "return @" in
      let exception_ = Parser.failed_input_compiler_exception failure in
      let context = Parser.failed_input_context failure in
      Alcotest.(check bool)
        "exact child context" true
        (context == Parser.compiler_exception_context exception_);
      Alcotest.(check bool)
        "exact consumed suspension" true
        (Parser.failed_input_is_from_suspension failure suspension);
      Alcotest.(check bool)
        "focused unchanged parent" true
        (Parser.failed_input_is_current failure ~suspension);
      Alcotest.(check int64)
        "parent error field remains zero" 0L
        (Parser.context_error_count parent |> Result.get_ok);
      let different = Parser.suspend_context parent |> Result.get_ok in
      Alcotest.(check bool)
        "same parent cannot substitute token" false
        (Parser.failed_input_is_from_suspension failure different);
      Alcotest.(check bool)
        "substitute cannot claim failure" false
        (Parser.claim_failed_input failure ~suspension:different);
      saved := Some (failure, suspension));
  let failure, suspension = Option.get !saved in
  Alcotest.(check bool)
    "retained failure remains inspectable" true
    (Parser.failed_input_is_from_suspension failure suspension);
  Alcotest.(check bool)
    "closed parent grants no catch" false
    (Parser.failed_input_is_current failure ~suspension)

let failed_input_abort_chain () =
  suspended_parent (fun session config _ suspension ->
      let source =
        Session.add_source session ~path:"nested-failure.HC"
          ~contents:"11;#exe {22;#exe {return @}}42;"
      in
      let aborted = ref 0 and finished = ref 0 in
      let execute_stream _ =
        Ok
          Parser.
            {
              definitions = Session.definitions session;
              symbols = Session.symbols session;
              commands = command_sink ();
              finish =
                (fun () ->
                  incr finished;
                  Ok "");
              abort = (fun () -> incr aborted);
            }
      in
      let parsed =
        suspended_parse ~execute_stream session config suspension source
        |> Result.get_ok
      in
      let failure = Parser.suspension_failed_input suspension |> Option.get in
      List.iter
        (fun context ->
          Alcotest.(check bool)
            "original descendant belongs to entered input" true
            (Parser.context_is_in_suspended_input context ~suspension);
          let copied_context : Parser.command_context =
            Obj.obj (Obj.dup (Obj.repr context))
          in
          Alcotest.(check bool)
            "copied descendant cannot join entered input" false
            (Parser.context_is_in_suspended_input copied_context ~suspension))
        (Parser.failed_input_aborted_contexts failure);
      Alcotest.(check bool)
        "nested failure rejects whole child" true
        (Option.is_none parsed.ast);
      Alcotest.(check int) "both directive scopes abort" 2 !aborted;
      Alcotest.(check int) "neither directive completes" 0 !finished;
      let failure = Parser.suspension_failed_input suspension |> Option.get in
      let root = Parser.failed_input_context failure in
      let exception_ = Parser.failed_input_compiler_exception failure in
      Alcotest.(check bool)
        "original child source" true
        (Parser.context_source root == source);
      Alcotest.(check bool)
        "leaf remains distinct from child root" false
        (root == Parser.compiler_exception_context exception_);
      Alcotest.(check bool)
        "complete nested abort chain" true
        (Parser.failed_input_is_current failure ~suspension);
      Alcotest.(check string)
        "original keyword error is retained" "HCPARSE0168"
        (List.hd parsed.diagnostics).code)

let failed_input_preflight_and_generation () =
  suspended_parent (fun session config parent suspension ->
      let original =
        Session.add_source session ~path:"registered.HC" ~contents:"return;"
      in
      let copied =
        Source_file.create ~id:(Source_file.id original)
          ~path:(Source_file.path original)
          ~display_path:(Source_file.display_path original)
          ~contents:(Source_file.contents original)
      in
      Alcotest.(check bool)
        "copied source rejects preflight" true
        (Result.is_error (suspended_parse session config suspension copied));
      Alcotest.(check bool)
        "preflight leaves original token available" true
        (Parser.suspension_is_from_context suspension parent);
      let stale = Parser.suspend_context parent |> Result.get_ok in
      let parsed =
        suspended_parse session config suspension original |> Result.get_ok
      in
      Alcotest.(check bool)
        "original source reaches failure" true
        (Option.is_none parsed.ast);
      let failure = Parser.suspension_failed_input suspension |> Option.get in
      Alcotest.(check bool)
        "prior token expires when child enters" false
        (Parser.suspension_is_from_context stale parent);
      let next = Parser.suspend_context parent |> Result.get_ok in
      let accepted =
        Session.add_source session ~path:"later-child.HC" ~contents:"1;"
      in
      let parsed =
        suspended_parse session config next accepted |> Result.get_ok
      in
      Alcotest.(check bool)
        "later original child succeeds" true
        (Option.is_some parsed.ast);
      Alcotest.(check bool)
        "later child expires old catch authority" false
        (Parser.failed_input_is_current failure ~suspension);
      Alcotest.(check bool)
        "older failure still has its source origin" true
        (Parser.failed_input_is_from_suspension failure suspension))

let failed_input_foreign_domain_and_claim () =
  suspended_parent (fun session config _ suspension ->
      let failure = failed_child session config suspension "return 42;" in
      Gc.full_major ();
      Gc.compact ();
      Alcotest.(check bool)
        "compaction retains original abort proof" true
        (Parser.failed_input_is_current failure ~suspension);
      let foreign =
        Domain.spawn (fun () ->
            Option.is_none (Parser.suspension_failed_input suspension)
            && (not (Parser.failed_input_is_from_suspension failure suspension))
            && (not (Parser.failed_input_is_current failure ~suspension))
            && not (Parser.claim_failed_input failure ~suspension))
        |> Domain.join
      in
      Alcotest.(check bool)
        "foreign domain cannot read or claim authority" true foreign;
      Alcotest.(check bool)
        "original domain claims once" true
        (Parser.claim_failed_input failure ~suspension);
      Alcotest.(check bool)
        "repeated claim rejects" false
        (Parser.claim_failed_input failure ~suspension);
      Alcotest.(check bool)
        "claim retains provenance" true
        (Parser.failed_input_is_from_suspension failure suspension))

let failed_input_other_failures () =
  List.iter
    (fun text ->
      suspended_parent (fun session config _ suspension ->
          let source =
            Session.add_source session ~path:"ordinary-failure.HC"
              ~contents:text
          in
          let parsed =
            suspended_parse session config suspension source |> Result.get_ok
          in
          Alcotest.(check bool)
            "ordinary child fails" true
            (Option.is_none parsed.ast);
          Alcotest.(check bool)
            "ordinary failure grants no typed catch" true
            (Option.is_none (Parser.suspension_failed_input suspension))))
    [ "I64 X=;"; "#error ordinary\n42;"; "@" ];
  suspended_parent (fun session config _ suspension ->
      let source =
        Session.add_source session ~path:"forged-failure.HC" ~contents:"42;"
      in
      let forged =
        Diagnostic.make ~code:"HCPARSE0168" ~severity:Diagnostic.Error
          ~primary:
            (Span.unsafe_make ~source:(Source_file.id source) ~start:0 ~stop:2)
          ~message:"return requires an active function" ()
      in
      let commands = command_sink ~command:(fun _ -> Error [ forged ]) () in
      ignore
        (suspended_parse ~commands session config suspension source
        |> Result.get_ok);
      Alcotest.(check bool)
        "matching callback diagnostic grants no typed catch" true
        (Option.is_none (Parser.suspension_failed_input suspension)))

let failed_input_unexpected_cleanup () =
  List.iter
    (fun stage ->
      suspended_parent (fun session config _ suspension ->
          let source =
            Session.add_source session ~path:"callback-failure.HC"
              ~contents:"return;"
          in
          let seen = ref None in
          let compiler_exception exception_ =
            seen := Some exception_;
            if stage = "observer" then failwith "unexpected observer"
          in
          let checkpoint = function
            | Parser.Sequence_aborted _ when stage = "cleanup exception" ->
                failwith "unexpected cleanup"
            | Parser.Sequence_aborted _ when stage = "cleanup diagnostic" ->
                Error
                  [
                    Diagnostic.make ~code:"HCIRVM0001"
                      ~severity:Diagnostic.Error
                      ~primary:
                        (Span.unsafe_make ~source:(Source_file.id source)
                           ~start:0 ~stop:6)
                      ~message:"cleanup rejected" ();
                  ]
            | _ -> Ok ()
          in
          let raised =
            try
              ignore
                (suspended_parse ~compiler_exception
                   ~commands:(command_sink ~checkpoint ())
                   session config suspension source);
              false
            with Failure _ | Fun.Finally_raised (Failure _) -> true
          in
          Alcotest.(check bool)
            "unexpected exception remains an exception"
            (stage <> "cleanup diagnostic")
            raised;
          Alcotest.(check bool)
            "producer was actually reached" true (Option.is_some !seen);
          Alcotest.(check bool)
            "unexpected unwind grants no failed-input receipt" true
            (Option.is_none (Parser.suspension_failed_input suspension))))
    [ "observer"; "cleanup exception"; "cleanup diagnostic" ];
  suspended_parent (fun session config _ suspension ->
      let source =
        Session.add_source session ~path:"stream-cleanup-failure.HC"
          ~contents:"#exe {return;}42;"
      in
      let execute_stream _ =
        Ok
          Parser.
            {
              definitions = Session.definitions session;
              symbols = Session.symbols session;
              commands = command_sink ();
              finish = (fun () -> Ok "");
              abort = (fun () -> failwith "unexpected stream cleanup");
            }
      in
      let raised =
        try
          ignore
            (suspended_parse ~execute_stream session config suspension source);
          false
        with Failure _ | Fun.Finally_raised (Failure _) -> true
      in
      Alcotest.(check bool)
        "directive cleanup overrides Compiler unwind" true raised;
      Alcotest.(check bool)
        "unexpected directive cleanup grants no catch" true
        (Option.is_none (Parser.suspension_failed_input suspension)))

let failed_input_saved_enclosing_tables () =
  List.iter
    (fun mode ->
      let session, source, config = inputs mode "#exe {}42;" in
      let directive_session = Session.create () in
      let current = ref None and seen = ref false in
      let checkpoint = function
        | Parser.Sequence_started context ->
            current := Some context;
            Ok ()
        | _ -> Ok ()
      in
      let resume () =
        if not !seen then (
          seen := true;
          let parent = Option.get !current in
          let suspension = Parser.suspend_context parent |> Result.get_ok in
          let enclosing =
            Parser.suspension_enclosing_context suspension |> Result.get_ok
          in
          let child =
            Session.add_source session ~path:"saved-failed-child.HC"
              ~contents:"return;"
          in
          let child_config =
            Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
            |> Result.get_ok
          in
          Alcotest.(check bool)
            "ordinary entry rejects different saved tables" true
            (Result.is_error
               (suspended_parse session child_config suspension child));
          Alcotest.(check bool)
            "saved-table preflight preserves token" true
            (Parser.suspension_is_from_context suspension parent);
          let parsed =
            Parser.parse_suspended_enclosing suspension ~enclosing
              ~commands:(command_sink ()) ~sources:(Session.sources session)
              ~definitions:(Session.definitions session)
              ~symbols:(Session.symbols session) ~config:child_config child
            |> Result.get_ok
          in
          Alcotest.(check bool)
            "saved child fails at producer" true
            (Option.is_none parsed.ast);
          let failure =
            Parser.suspension_failed_input suspension |> Option.get
          in
          let child_context = Parser.failed_input_context failure in
          Alcotest.(check bool)
            "child retains actual saved namespace" true
            (Parser.context_environment child_context
            == Parser.context_environment enclosing);
          Alcotest.(check bool)
            "directive namespace stays distinct" false
            (Parser.context_environment child_context
            == Parser.context_environment parent);
          Alcotest.(check bool)
            "saved input has original current failure" true
            (Parser.failed_input_is_current failure ~suspension));
        Ok ()
      in
      let execute_stream _ =
        Ok
          Parser.
            {
              definitions = Session.definitions directive_session;
              symbols = Session.symbols directive_session;
              commands = command_sink ~checkpoint ~resume ();
              finish = (fun () -> Ok "");
              abort =
                (fun () ->
                  Alcotest.fail
                    "retained parser failure must not abort parent directive");
            }
      in
      let parsed =
        Parser.parse ~execute_stream ~sources:(Session.sources session)
          ~definitions:(Session.definitions session)
          ~symbols:(Session.symbols session) ~config source
      in
      Alcotest.(check bool)
        "original outer syntax completes" true
        (Option.is_some parsed.ast);
      Alcotest.(check bool) "saved child was reached" true !seen)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let saved_function_return_failure_kind () =
  List.iter
    (fun mode ->
      let session, source, config =
        inputs mode "I64 F(){#exe {}return 42;}F();"
      in
      let current = ref None and reached = ref false in
      let checkpoint = function
        | Parser.Sequence_started context ->
            current := Some context;
            Ok ()
        | _ -> Ok ()
      in
      let nested_stream _ =
        Ok
          Parser.
            {
              definitions = Session.definitions session;
              symbols = Session.symbols session;
              commands = command_sink ();
              finish = (fun () -> Ok "");
              abort = (fun () -> ());
            }
      in
      let resume () =
        if not !reached then (
          reached := true;
          let parent = Option.get !current in
          let suspension = Parser.suspend_context parent |> Result.get_ok in
          let ordinary =
            failed_child session
              (Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
              |> Result.get_ok)
              suspension "return 42;"
          in
          Alcotest.(check string)
            "ordinary child starts without function" "HCPARSE0168"
            (Parser.compiler_exception_diagnostic
               (Parser.failed_input_compiler_exception ordinary))
              .code;
          List.iter
            (fun (text, expected_code, expected_receipts) ->
              let suspension = Parser.suspend_context parent |> Result.get_ok in
              let enclosing =
                Parser.suspension_enclosing_context suspension |> Result.get_ok
              in
              let child =
                Session.add_source session ~path:"inherited-function.HC"
                  ~contents:text
              in
              let child_config =
                Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
                |> Result.get_ok
              in
              let exceptions = ref [] in
              let parsed =
                Parser.parse_suspended_enclosing suspension ~enclosing
                  ~compiler_exception:(fun exception_ ->
                    exceptions := exception_ :: !exceptions)
                  ~commands:(command_sink ()) ~execute_stream:nested_stream
                  ~sources:(Session.sources session)
                  ~definitions:(Session.definitions session)
                  ~symbols:(Session.symbols session) ~config:child_config child
                |> Result.get_ok
              in
              Alcotest.(check bool)
                "unsupported or failed child has no accepted AST" true
                (Option.is_none parsed.ast);
              Alcotest.(check string)
                "actual failure kind" expected_code
                (List.hd parsed.diagnostics).code;
              Alcotest.(check int)
                "only actual missing-function producer counts" expected_receipts
                (List.length !exceptions);
              Alcotest.(check bool)
                "unsupported saved return grants no Compiler catch"
                (expected_receipts = 1)
                (Option.is_some (Parser.suspension_failed_input suspension));
              Alcotest.(check int64)
                "child leaves original function control count zero" 0L
                (Parser.context_error_count parent |> Result.get_ok))
            [
              ("return #error late\n42;", "HCPARSE0169", 0);
              ("#exe {return 42;}42;", "HCPARSE0168", 1);
              ("I64 G(){return 1;}return 42;", "HCPARSE0168", 1);
            ]);
        Ok ()
      in
      let execute_stream _ =
        Ok
          Parser.
            {
              definitions = Session.definitions session;
              symbols = Session.symbols session;
              commands = command_sink ~checkpoint ~resume ();
              finish = (fun () -> Ok "");
              abort = (fun () -> Alcotest.fail "parent directive aborted");
            }
      in
      let parsed =
        Parser.parse ~commands:(command_sink ()) ~execute_stream
          ~sources:(Session.sources session)
          ~definitions:(Session.definitions session)
          ~symbols:(Session.symbols session) ~config source
      in
      Alcotest.(check bool)
        "original function syntax remains accepted" true
        (Option.is_some parsed.ast);
      Alcotest.(check bool)
        "original function's directive was reached" true !reached)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let inherited_function_source_failure () =
  List.iter
    (fun mode ->
      let session, source, config =
        inputs mode
          ((if mode = Preprocessor.Jit then Cases.headers else "")
          ^ Cases.inherited_function_failure)
      in
      let report =
        run_integer_program_report session ~source ~config ~max_steps:100_000
      in
      let diagnostics =
        match integer_program_report_outcome report with
        | Error errors -> errors
        | Ok _ ->
            Alcotest.fail "unsupported inherited return unexpectedly executed"
      in
      Alcotest.(check bool)
        (describe diagnostics) true
        (List.exists
           (fun (diagnostic : Diagnostic.t) -> diagnostic.code = "HCPARSE0169")
           diagnostics);
      Alcotest.(check int)
        "inherited function supplies no missing-function Compiler" 0
        (List.length (integer_program_report_compiler_exceptions report));
      Alcotest.(check string)
        "inherited return preserves earlier IR effects" "kept"
        (integer_program_report_output_bytes report))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let caught_child_source_execution () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text, output, count) ->
          let session, source, config =
            inputs mode
              ((if mode = Preprocessor.Jit then Cases.headers else "") ^ text)
          in
          let report =
            run_integer_program_report session ~source ~config
              ~max_steps:100_000
          in
          let result =
            match integer_program_report_outcome report with
            | Ok result -> result
            | Error errors -> Alcotest.fail (label ^ ": " ^ describe errors)
          in
          Alcotest.(check (option int64))
            (label ^ " original outer value")
            (Some 42L)
            (Option.map
               (fun word -> word.Ir_integer_interpreter.bits)
               (Ir_integer_interpreter.final_value result.value));
          Alcotest.(check string)
            (label ^ " preserves reached output and resumes parent")
            output
            (integer_program_report_output_bytes report);
          let exceptions = integer_program_report_compiler_exceptions report in
          Alcotest.(check int)
            (label ^ " retains counted child failures")
            count (List.length exceptions);
          List.iter
            (fun exception_ ->
              receipt label
                [ Parser.compiler_exception_diagnostic exception_ ]
                [ exception_ ])
            exceptions)
        Cases.caught_children)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let caught_child_does_not_hide_faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text) ->
          let session, source, config =
            inputs mode
              ((if mode = Preprocessor.Jit then Cases.headers else "") ^ text)
          in
          let report =
            run_integer_program_report session ~source ~config
              ~max_steps:100_000
          in
          Alcotest.(check bool)
            (label ^ " remains a failure")
            true
            (Result.is_error (integer_program_report_outcome report));
          Alcotest.(check int)
            (label ^ " retains only the caught Compiler")
            1
            (List.length (integer_program_report_compiler_exceptions report));
          Alcotest.(check string)
            (label ^ " no later effects")
            "kept"
            (integer_program_report_output_bytes report))
        Cases.faults_after_caught_children;
      List.iter
        (fun (limit, count) ->
          let session, source, config =
            inputs mode
              ((if mode = Preprocessor.Jit then Cases.headers else "")
              ^ {|#exe {StreamExePrint("Print(\"kept\");return 42;");Print("after");}42;|}
              )
          in
          let report =
            run_integer_program_report session ~source ~config
              ~max_steps:100_000 ~max_output_bytes:limit
          in
          Alcotest.(check bool)
            "output quota remains a failure" true
            (Result.is_error (integer_program_report_outcome report));
          Alcotest.(check int)
            "quota preserves reached exception count" count
            (List.length (integer_program_report_compiler_exceptions report)))
        [ (3, 0); (4, 1) ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let failed_child_ledger_ownership () =
  let module VM = Ir_integer_interpreter in
  let module D = Task_declarations in
  let session, source, config = inputs Preprocessor.Jit "42;" in
  let runtime =
    VM.create_task_state ~table:(Session.semantic_symbols session) ()
    |> Result.get_ok
  in
  let ledger = D.create ~runtime session |> Result.get_ok in
  let parent = ref None and entered = ref false in
  let retained = ref None in
  let call : Parser.direct_call_sink =
    { implicit = None; start = (fun _ -> Ok None); emit = (fun _ -> Ok ()) }
  in
  let checkpoint event =
    Result.map
      (fun () ->
        match event with
        | Parser.Sequence_started context when Option.is_none !parent ->
            parent := Some context
        | _ -> ())
      (D.observe_command ledger event)
  in
  let resume () =
    if not !entered then (
      entered := true;
      let parent = Option.get !parent in
      let suspension = Parser.suspend_context parent |> Result.get_ok in
      let child =
        Session.add_source session ~path:"owned-ledger-failure.HC"
          ~contents:"return @"
      in
      let events_rev = ref [] in
      let child_checkpoint event =
        Result.map
          (fun () -> events_rev := event :: !events_rev)
          (D.observe_command ledger event)
      in
      let parsed =
        suspended_parse
          ~commands:(command_sink ~call ~checkpoint:child_checkpoint ())
          session config suspension child
        |> Result.get_ok
      in
      Alcotest.(check bool)
        "original child syntax remains failed" true
        (Option.is_none parsed.ast);
      let failure = Parser.suspension_failed_input suspension |> Option.get in
      let check session runtime =
        D.check_failed_compiler_input ledger ~session ~runtime ~suspension
          failure
      in
      Alcotest.(check bool)
        "exact original ledger preflight" true
        (Result.is_ok (check session runtime));
      Alcotest.(check bool)
        "foreign session cannot claim failure" true
        (Result.is_error (check (Session.create ()) runtime));
      let foreign_runtime =
        VM.create_task_state ~table:(Session.semantic_symbols session) ()
        |> Result.get_ok
      in
      Alcotest.(check bool)
        "matching semantic table does not replace runtime" true
        (Result.is_error (check session foreign_runtime));
      Alcotest.(check bool)
        "foreign domain cannot claim ledger" true
        (Domain.join
           (Domain.spawn (fun () -> Result.is_error (check session runtime))));
      let context = Parser.failed_input_context failure in
      Alcotest.(check (option bool))
        "complete original lifecycle" (Some true)
        (Parser.context_command_events_match context ~events_rev:!events_rev);
      Alcotest.(check (option bool))
        "missing original checkpoint" (Some false)
        (Parser.context_command_events_match context
           ~events_rev:(List.tl !events_rev));
      let copied_event : Parser.command_event =
        Obj.obj (Obj.dup (Obj.repr (List.hd !events_rev)))
      in
      Alcotest.(check (option bool))
        "copied event cannot replace original" (Some false)
        (Parser.context_command_events_match context
           ~events_rev:(copied_event :: List.tl !events_rev));
      Alcotest.(check bool)
        "exact original entered context" true
        (Parser.context_is_in_suspended_input context ~suspension);
      Alcotest.(check bool)
        "parent cannot replace child" false
        (Parser.context_is_in_suspended_input parent ~suspension);
      Gc.full_major ();
      Gc.compact ();
      Alcotest.(check bool)
        "ledger preflight survives collection" true
        (Result.is_ok (check session runtime));
      retained := Some (suspension, failure));
    Ok ()
  in
  let parsed =
    parse
      ~commands:(command_sink ~call ~checkpoint ~resume ())
      session source config
  in
  Alcotest.(check bool)
    "checking a failed ledger permits parent syntax only" true
    (Option.is_some parsed.ast);
  let suspension, failure = Option.get !retained in
  Alcotest.(check bool)
    "closed parent denies ledger claim" true
    (Result.is_error
       (D.check_failed_compiler_input ledger ~session ~runtime ~suspension
          failure))

let unhandled_child_is_confined_to_original_input () =
  let session = Session.create () in
  let task = Integer_task.create session |> Result.get_ok in
  let run text =
    Integer_task.run task
      ~source:
        (Session.add_source session ~path:"successive-owned-inputs.HC"
           ~contents:text)
  in
  Alcotest.(check bool)
    "unhandled child invalidates its input" true
    (Result.is_error
       (run (Cases.headers ^ {|#exe {StreamExePrint("Unknown;");}42;|})));
  let check result =
    match result with
    | Error errors -> Alcotest.fail (describe errors)
    | Ok result ->
        Alcotest.(check (option int64))
          "later independent input still completes" (Some 42L)
          (Option.map
             (fun word -> word.Ir_integer_interpreter.bits)
             (Ir_integer_interpreter.final_value result))
  in
  check (run "42;");
  check (run {|#exe {StreamExePrint("return 42;");}42;|})

let statement_parser_stream session _ =
  Ok
    Parser.
      {
        definitions = Session.definitions session;
        symbols = Session.symbols session;
        commands = command_sink ();
        finish = (fun () -> Ok "");
        abort = (fun () -> ());
      }

let statement_phase_receipts () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text, code, marker, _) ->
          let session, source, config = inputs mode (Cases.headers ^ text) in
          let seen = ref [] in
          let parsed =
            parse ~commands:(command_sink ())
              ~execute_stream:(statement_parser_stream session)
              ~compiler_exception:(fun exception_ ->
                let context = Parser.compiler_exception_context exception_ in
                Alcotest.(check bool)
                  (label ^ " current original context")
                  true
                  (Parser.compiler_exception_is_from_context exception_ context);
                Alcotest.(check int64)
                  (label ^ " reached native error field")
                  1L
                  (Parser.context_error_count context |> Result.get_ok);
                seen := exception_ :: !seen)
              session source config
          in
          Alcotest.(check bool)
            (label ^ " aborted syntax")
            true
            (Option.is_none parsed.ast);
          receipt ~code ~marker label parsed.diagnostics !seen)
        Cases.statement_failures)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let statement_break_targets () =
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          let session, source, config = inputs mode text in
          let seen = ref [] in
          let parsed =
            parse ~commands:(command_sink ())
              ~compiler_exception:(fun x -> seen := x :: !seen)
              session source config
          in
          Alcotest.(check bool)
            (text ^ ": " ^ describe parsed.diagnostics)
            true
            (Option.is_some parsed.ast);
          Alcotest.(check int)
            "valid original break targets do not throw" 0 (List.length !seen))
        [
          "while(1)break;";
          "do break;while(0);";
          "for(;1;)break;";
          "while(1){if(1)break;else break;}";
          "switch(1){case 1:break;}";
          "switch(1){start:case 1:break;end:}";
          "extern U0 SysTry(I64 a,I64 b);extern U0 \
           SysUntry();while(1){try;catch;break;}";
          "while(1){lock;break;}";
          "I64 F(){while(1)break;return 42;}";
          "I64 F(){goto return;return 42;}";
        ];
      List.iter
        (fun text ->
          let session, source, config = inputs mode text in
          let seen = ref [] in
          let parsed =
            parse
              ~compiler_exception:(fun x -> seen := x :: !seen)
              session source config
          in
          Alcotest.(check bool)
            "representation break syntax remains available" true
            (Option.is_some parsed.ast);
          Alcotest.(check int)
            "representation parser supplies no throw" 0 (List.length !seen))
        [
          "break;";
          "try;catch;";
          "while(1)for(break;1;);";
          "while(1)lock break;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let statement_lexer_order () =
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          let session, source, config = inputs mode text in
          let seen = ref [] in
          let parsed =
            parse ~commands:(command_sink ())
              ~execute_stream:(statement_parser_stream session)
              ~compiler_exception:(fun x -> seen := x :: !seen)
              session source config
          in
          Alcotest.(check bool)
            "lexer fault aborts input" true
            (Option.is_none parsed.ast);
          Alcotest.(check int)
            "failed Lex cannot issue a later statement throw" 0
            (List.length !seen))
        [ "break #error reached\n;"; "try #error reached\n;" ];
      let text =
        "try #exe {extern U0 SysTry(I64 a,I64 b);extern U0 SysUntry();} ; \
         catch;"
      in
      let session, source, config = inputs mode text in
      let seen = ref [] in
      let parsed =
        parse ~commands:(command_sink ())
          ~execute_stream:(statement_parser_stream session)
          ~compiler_exception:(fun x -> seen := x :: !seen)
          session source config
      in
      Alcotest.(check bool)
        (describe parsed.diagnostics)
        true
        (Option.is_some parsed.ast);
      Alcotest.(check int)
        "headers reached during Lex are selected before body" 0
        (List.length !seen))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let statement_source_execution () =
  List.iter
    (fun mode ->
      let run text =
        let session, source, config =
          inputs mode
            ((if mode = Preprocessor.Jit then Cases.headers else "") ^ text)
        in
        run_integer_program_report session ~source ~config ~max_steps:100_000
      in
      List.iter
        (fun (label, text, code, marker, output) ->
          let report = run text in
          let diagnostics =
            match integer_program_report_outcome report with
            | Error errors -> errors
            | Ok _ -> Alcotest.fail (label ^ " unexpectedly executed")
          in
          receipt ~reported_origin:false ~code ~marker label diagnostics
            (integer_program_report_compiler_exceptions report);
          Alcotest.(check string)
            (label ^ " reached output")
            output
            (integer_program_report_output_bytes report))
        Cases.statement_failures;
      List.iter
        (fun (label, text, code, marker, output) ->
          let report = run text in
          let result =
            match integer_program_report_outcome report with
            | Ok result -> result
            | Error errors -> Alcotest.fail (label ^ ": " ^ describe errors)
          in
          Alcotest.(check (option int64))
            (label ^ " parent resumes")
            (Some 42L)
            (Option.map
               (fun word -> word.Ir_integer_interpreter.bits)
               (Ir_integer_interpreter.final_value result.value));
          Alcotest.(check string)
            (label ^ " retained effects")
            output
            (integer_program_report_output_bytes report);
          receipt ~code ~marker label
            (List.map Parser.compiler_exception_diagnostic
               (integer_program_report_compiler_exceptions report))
            (integer_program_report_compiler_exceptions report))
        Cases.statement_caught_children)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let call_phase_receipts () =
  let shape count variadic : Symbol_visibility.function_call_shape =
    {
      parameters =
        List.init count (fun _ ->
            { Symbol_visibility.parameter_name = None; has_default = false });
      variadic;
    }
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (text, supplied, code, marker, implicit) ->
          let session, source, config = inputs mode text in
          let direct = ref None and output = ref None and seen = ref [] in
          ignore
            (Symbol_visibility.Environment.add (Session.symbols session)
               ~name:"F" ~kind:Symbol_visibility.Function ());
          let call : Parser.direct_call_sink =
            {
              start =
                (fun start ->
                  direct := Some start;
                  let copy : Parser.call_start =
                    Obj.obj (Obj.dup (Obj.repr start))
                  in
                  Alcotest.(check bool)
                    "copied start has no live authority" false
                    (Parser.call_start_is_current copy);
                  Alcotest.(check bool)
                    "copied start cannot claim" false
                    (Parser.claim_call_start copy);
                  Ok (Some supplied));
              emit = (fun _ -> Alcotest.fail "failed call cannot emit");
              implicit =
                Some
                  {
                    arguments =
                      (fun selection ->
                        output := Some selection;
                        let copy : Parser.implicit_output_selection =
                          Obj.obj (Obj.dup (Obj.repr selection))
                        in
                        Alcotest.(check bool)
                          "copied implicit arguments have no live authority"
                          false
                          (Parser.implicit_arguments_are_current copy);
                        Alcotest.(check bool)
                          "copied implicit arguments cannot claim" false
                          (Parser.claim_implicit_arguments copy);
                        Ok (Some supplied));
                    emission =
                      (fun _ ->
                        Alcotest.fail "failed implicit call cannot emit");
                  };
            }
          in
          let parsed =
            parse ~commands:(command_sink ~call ())
              ~compiler_exception:(fun exception_ ->
                seen := exception_ :: !seen)
              session source config
          in
          call_receipt ~code ~marker text parsed.diagnostics (List.rev !seen);
          let terminal = List.hd !seen in
          let exception_ =
            Option.value
              (Parser.compiler_exception_call_origin terminal)
              ~default:terminal
          in
          Alcotest.(check bool)
            "delimiter requires original call metadata" true
            (Parser.compiler_exception_requires_call_shape exception_);
          let check_shape = function
            | Some (Some reached) ->
                Alcotest.(check bool)
                  "exact callback result retained after abort" true
                  (reached == supplied);
                let copy =
                  { reached with Symbol_visibility.variadic = reached.variadic }
                in
                Alcotest.(check bool)
                  "equal copied shape is not original" false (reached == copy)
            | _ -> Alcotest.fail "successful callback result was lost"
          in
          if implicit then (
            let selection = Option.get !output in
            let copy : Parser.implicit_output_selection =
              Obj.obj (Obj.dup (Obj.repr selection))
            in
            check_shape (Parser.implicit_supplied_shape selection);
            Alcotest.(check bool)
              "original implicit producer" true
              (Parser.compiler_exception_is_from_implicit_arguments exception_
                 selection);
            Alcotest.(check bool)
              "copied implicit producer rejected" false
              (Parser.compiler_exception_is_from_implicit_arguments exception_
                 copy);
            Alcotest.(check bool)
              "copy cannot read returned shape" true
              (Option.is_none (Parser.implicit_supplied_shape copy)))
          else
            let start = Option.get !direct in
            let copy : Parser.call_start = Obj.obj (Obj.dup (Obj.repr start)) in
            check_shape (Parser.call_start_supplied_shape start);
            Alcotest.(check bool)
              "original direct producer" true
              (Parser.compiler_exception_is_from_call_start exception_ start);
            Alcotest.(check bool)
              "copied direct producer rejected" false
              (Parser.compiler_exception_is_from_call_start exception_ copy);
            Alcotest.(check bool)
              "copy cannot read returned shape" true
              (Option.is_none (Parser.call_start_supplied_shape copy)))
        [
          ("F(1 2);", shape 2 false, "HCPARSE0024", "2", false);
          ("F(1;", shape 1 false, "HCPARSE0025", ";", false);
          ("F(1);", shape 0 false, "HCPARSE0025", "1", false);
          ("F(1,);", shape 2 false, "HCPARSE0018", ")", false);
          ( "extern U0 Print(U8 *fmt,...);\"text\"}",
            shape 1 true,
            "HCPARSE0167",
            "}",
            true );
          ( "extern U0 PutChars(I64 n);''(42;",
            shape 1 false,
            "HCPARSE0167",
            ";",
            true );
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ];
  List.iter
    (fun mode ->
      List.iter
        (fun (text, supplied) ->
          let session, source, config = inputs mode text in
          ignore
            (Symbol_visibility.Environment.add (Session.symbols session)
               ~name:"F" ~kind:Symbol_visibility.Function ());
          let seen = ref [] in
          let call : Parser.direct_call_sink =
            {
              implicit = None;
              start = (fun _ -> Ok supplied);
              emit = (fun _ -> Ok ());
            }
          in
          let parsed =
            parse ~commands:(command_sink ~call ())
              ~compiler_exception:(fun exception_ ->
                seen := exception_ :: !seen)
              session source config
          in
          Alcotest.(check bool)
            "ordinary argument failure stays failed" true
            (Option.is_none parsed.ast);
          Alcotest.(check int)
            "unshaped delimiters and missing operands cannot forge Compiler" 0
            (List.length !seen))
        [ ("F(1 2);", None); ("F(1+;", None) ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let zero_argument_marker_phases () =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, target, variadic, marker, fails) ->
          let session, source, config = inputs mode text in
          let supplied : Symbol_visibility.function_call_shape =
            { parameters = []; variadic }
          in
          let phases = ref [] and seen = ref [] and selected = ref None in
          let call : Parser.direct_call_sink =
            {
              start = (fun _ -> Alcotest.fail "no explicit call expected");
              emit = (fun _ -> Alcotest.fail "no explicit emission expected");
              implicit =
                Some
                  {
                    arguments =
                      (fun selection ->
                        Alcotest.(check bool)
                          "original target" true
                          (Parser.implicit_target selection = target);
                        selected := Some selection;
                        phases := "arguments" :: !phases;
                        Ok (Some supplied));
                    emission =
                      (fun selection ->
                        Alcotest.(check bool)
                          "emission retains exact selection" true
                          (selection == Option.get !selected);
                        Alcotest.(check bool)
                          "original callback shape precedes emission" true
                          (match Parser.implicit_supplied_shape selection with
                          | Some (Some shape) -> shape == supplied
                          | _ -> false);
                        Alcotest.(check int64)
                          "emission precedes statement error" 0L
                          (Parser.context_error_count
                             (Parser.implicit_command selection).command_context
                          |> Result.get_ok);
                        phases := "emission" :: !phases;
                        Ok ());
                  };
            }
          in
          let compiler_exception exception_ =
            phases := "Compiler" :: !phases;
            seen := exception_ :: !seen
          in
          let parsed =
            parse ~commands:(command_sink ~call ()) ~compiler_exception session
              source config
          in
          Alcotest.(check bool)
            (describe parsed.diagnostics)
            (not fails)
            (Option.is_some parsed.ast);
          Alcotest.(check (list string))
            "original call completes before statement producer"
            (if fails then [ "arguments"; "emission"; "Compiler" ]
             else [ "arguments"; "emission" ])
            (List.rev !phases);
          if fails then (
            receipt ~code:"HCPARSE0046" ~marker text parsed.diagnostics !seen;
            Alcotest.(check bool)
              "statement producer is separate from argument failure" false
              (Parser.compiler_exception_requires_call_shape (List.hd !seen))))
        [
          ( "extern U0 Print();\"first\" \"second\";",
            Ast.Print_target,
            false,
            "\"first\"",
            true );
          ( "extern U0 Print();\"text\" #error late\n",
            Ast.Print_target,
            false,
            "\"text\"",
            true );
          ( "extern U0 PutChars();'A' #error late\n",
            Ast.Put_chars_target,
            false,
            "'A'",
            true );
          ( "extern U0 PutChars(...);'A';",
            Ast.Put_chars_target,
            true,
            "'A'",
            true );
          ("extern U0 Print();\"\";", Ast.Print_target, false, "", false);
          ("extern U0 PutChars();''();", Ast.Put_chars_target, false, "", false);
          ("extern U0 Print(...);\"text\";", Ast.Print_target, true, "", false);
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let default_traversal_phases () =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, defaults, code, marker, emitted) ->
          let session, source, config = inputs mode text in
          let supplied : Symbol_visibility.function_call_shape =
            {
              parameters =
                List.map
                  (fun has_default ->
                    { Symbol_visibility.parameter_name = None; has_default })
                  defaults;
              variadic = false;
            }
          in
          let phases = ref [] and seen = ref [] and selected = ref None in
          let call : Parser.direct_call_sink =
            {
              start = (fun _ -> Alcotest.fail "no explicit call");
              emit = (fun _ -> Alcotest.fail "no explicit emission");
              implicit =
                Some
                  {
                    arguments =
                      (fun selection ->
                        selected := Some selection;
                        phases := "arguments" :: !phases;
                        Ok (Some supplied));
                    emission =
                      (fun selection ->
                        Alcotest.(check bool)
                          "exact original selection emits" true
                          (selection == Option.get !selected);
                        phases := "emission" :: !phases;
                        Ok ());
                  };
            }
          in
          let parsed =
            parse ~commands:(command_sink ~call ())
              ~compiler_exception:(fun exception_ ->
                phases := "Compiler" :: !phases;
                seen := exception_ :: !seen)
              session source config
          in
          receipt ~code ~marker text parsed.diagnostics !seen;
          Alcotest.(check (list string))
            "original default producer phase"
            (if emitted then [ "arguments"; "emission"; "Compiler" ]
             else [ "arguments"; "Compiler" ])
            (List.rev !phases);
          let selection = Option.get !selected and exception_ = List.hd !seen in
          Alcotest.(check bool)
            "exact returned default shape survives abort" true
            (match Parser.implicit_supplied_shape selection with
            | Some (Some shape) -> shape == supplied
            | _ -> false);
          Alcotest.(check bool)
            "only argument delimiter has argument authority" (not emitted)
            (Parser.compiler_exception_requires_call_shape exception_);
          Alcotest.(check bool)
            "original argument producer affinity" (not emitted)
            (Parser.compiler_exception_is_from_implicit_arguments exception_
               selection);
          let copy : Parser.implicit_output_selection =
            Obj.obj (Obj.dup (Obj.repr selection))
          in
          Alcotest.(check bool)
            "copied default selection has no producer authority" false
            (Parser.compiler_exception_is_from_implicit_arguments exception_
               copy))
        [
          ( "extern U0 Print(I64 n=0);\"first\" \"second\" #error late\n",
            [ true ],
            "HCPARSE0046",
            "\"first\"",
            true );
          ( "extern U0 Print(I64 n=0,I64 m=1);\"text\" #error late\n",
            [ true; true ],
            "HCPARSE0167",
            "\"text\"",
            false );
          ( "extern U0 Print(U8 *s,I64 n=7);\"text\",42 #error late\n",
            [ false; true ],
            "HCPARSE0046",
            "42",
            true );
          ( "extern U0 Print(U8 *s,I64 n=7,I64 m);\"text\",42 #error late\n",
            [ false; true; false ],
            "HCPARSE0167",
            "42",
            false );
          ( "extern U0 PutChars(I64 a=40,I64 b=2);'A' #error late\n",
            [ true; true ],
            "HCPARSE0046",
            "'A'",
            true );
          ( "extern U0 Print(I64 n=0);\"\"42 #error late\n",
            [ true ],
            "HCPARSE0046",
            "42",
            true );
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let failed_call_shape_ownership () =
  let module D = Task_declarations in
  let module VM = Ir_integer_interpreter in
  List.iter
    (fun (shape_policy, child_text) ->
      let copied_shape = shape_policy <> 0 in
      let session, source, config = inputs Preprocessor.Jit "42;" in
      let runtime =
        VM.create_task_state ~table:(Session.semantic_symbols session) ()
        |> Result.get_ok
      in
      let ledger = D.create ~runtime session |> Result.get_ok in
      let parent = ref None and entered = ref false in
      let checkpoint event =
        Result.map
          (fun () ->
            match event with
            | Parser.Sequence_started context when Option.is_none !parent ->
                parent := Some context
            | _ -> ())
          (D.observe_command ledger event)
      in
      let call : Parser.direct_call_sink =
        {
          implicit = None;
          start =
            (fun start ->
              let copy : Parser.call_start =
                Obj.obj (Obj.dup (Obj.repr start))
              in
              Alcotest.(check bool)
                "ledger rejects copied live start" true
                (Result.is_error (D.observe_call_start ledger copy));
              Result.map
                (fun shape ->
                  if shape_policy = 2 then None
                  else
                    Option.map
                      (fun (shape : Symbol_visibility.function_call_shape) ->
                        if copied_shape then
                          { shape with variadic = shape.variadic }
                        else shape)
                      shape)
                (D.observe_call_start ledger start));
          emit = D.observe_call_emission ledger;
        }
      in
      let resume () =
        if not !entered then (
          entered := true;
          let suspension =
            Parser.suspend_context (Option.get !parent) |> Result.get_ok
          in
          let child =
            Session.add_source session ~path:"owned-call-shape.HC"
              ~contents:child_text
          in
          let commands =
            {
              (command_sink ~call ~checkpoint:(D.observe_command ledger) ()) with
              Parser.declaration =
                Some
                  (fun event ->
                    Result.bind (D.observe ledger event) (fun () ->
                        Result.bind
                          (D.admit_function_phase ledger ~runtime event)
                          (fun () ->
                            match event with
                            | Parser.Function_header_completed header ->
                                D.admit_function_header ledger ~runtime header
                            | _ -> Ok ())));
              reference = Some (D.observe_reference ledger);
            }
          in
          let parsed =
            suspended_parse ~commands session config suspension child
            |> Result.get_ok
          in
          Alcotest.(check bool)
            "delimiter fails child syntax" true
            (Option.is_none parsed.ast);
          let failure =
            match Parser.suspension_failed_input suspension with
            | Some failure -> failure
            | None ->
                Alcotest.fail
                  ("missing call failure: " ^ describe parsed.diagnostics)
          in
          let check () =
            D.check_failed_compiler_input ledger ~session ~runtime ~suspension
              failure
          in
          let foreign_session = Session.create () in
          let foreign_runtime =
            VM.create_task_state
              ~table:(Session.semantic_symbols foreign_session)
              ()
            |> Result.get_ok
          in
          let foreign =
            D.create ~runtime:foreign_runtime foreign_session |> Result.get_ok
          in
          Alcotest.(check bool)
            "unobserved directive ledger cannot replace producer" false
            (Result.is_ok
               (D.check_failed_compiler_input foreign ~directive_ledger:foreign
                  ~session:foreign_session ~runtime:foreign_runtime ~suspension
                  failure));
          Alcotest.(check bool)
            "catch requires exact native callback result" (not copied_shape)
            (Result.is_ok (check ()));
          Alcotest.(check bool)
            "foreign directive cannot supply missing shape authority"
            (not copied_shape)
            (Result.is_ok
               (D.check_failed_compiler_input ledger ~directive_ledger:foreign
                  ~session ~runtime ~suspension failure));
          Gc.full_major ();
          Gc.compact ();
          Alcotest.(check bool)
            "native result identity survives collection" (not copied_shape)
            (Result.is_ok (check ())));
        Ok ()
      in
      let parsed =
        parse
          ~commands:(command_sink ~checkpoint ~resume ())
          session source config
      in
      Alcotest.(check bool)
        (describe parsed.diagnostics)
        true
        (Option.is_some parsed.ast))
    [
      (0, "extern I64 F(I64 a,I64 b);F(1 2);");
      (1, "extern I64 F(I64 a,I64 b);F(1 2);");
      (0, "extern I64 F(I64 a,I64 b);F(1,);");
      (1, "extern I64 F(I64 a,I64 b);F(1,);");
      (2, "extern I64 F(I64 a,I64 b);F(1,);");
    ]

let call_source_execution () =
  let run mode with_headers text =
    let session, source, config =
      inputs mode
        ((if with_headers && mode = Preprocessor.Jit then Cases.headers else "")
        ^ text)
    in
    run_integer_program_report session ~source ~config ~max_steps:100_000
  in
  List.iter
    (fun (label, text, code, marker) ->
      let report = run Preprocessor.Jit false text in
      let diagnostics =
        match integer_program_report_outcome report with
        | Error errors -> errors
        | Ok _ -> Alcotest.fail (label ^ " unexpectedly executed")
      in
      call_receipt ~reported_origin:false ~code ~marker label diagnostics
        (integer_program_report_compiler_exceptions report))
    Cases.call_failures;
  List.iter
    (fun mode ->
      List.iter
        (fun (label, with_headers, text, code, marker, output) ->
          let report = run mode with_headers text in
          let result =
            match integer_program_report_outcome report with
            | Ok result -> result
            | Error errors -> Alcotest.fail (label ^ ": " ^ describe errors)
          in
          Alcotest.(check (option int64))
            (label ^ " parent resumes")
            (Some 42L)
            (Option.map
               (fun word -> word.Ir_integer_interpreter.bits)
               (Ir_integer_interpreter.final_value result.value));
          Alcotest.(check string)
            (label ^ " retained effects")
            output
            (integer_program_report_output_bytes report);
          let exceptions = integer_program_report_compiler_exceptions report in
          call_receipt ~code ~marker label
            (List.map Parser.compiler_exception_diagnostic exceptions)
            exceptions)
        Cases.call_caught_children)
    [ Preprocessor.Jit; Preprocessor.Aot ];
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text, output) ->
          let report = run mode false text in
          let result =
            match integer_program_report_outcome report with
            | Ok result -> result
            | Error errors -> Alcotest.fail (label ^ ": " ^ describe errors)
          in
          Alcotest.(check (option int64))
            (label ^ " generated result")
            (Some 42L)
            (Option.map
               (fun word -> word.Ir_integer_interpreter.bits)
               (Ir_integer_interpreter.final_value result.value));
          Alcotest.(check string)
            (label ^ " output") output
            (integer_program_report_output_bytes report);
          Alcotest.(check int)
            (label ^ " no Compiler throw")
            0
            (List.length (integer_program_report_compiler_exceptions report)))
        Cases.call_successes)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let expression_phase_receipts () =
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          let session, source, config = inputs mode text in
          let seen = ref [] and counts = ref [] in
          let parsed =
            parse ~commands:(command_sink ())
              ~compiler_exception:(fun exception_ ->
                let context = Parser.compiler_exception_context exception_ in
                Alcotest.(check bool)
                  "each producer owns context at callback" true
                  (Parser.compiler_exception_is_from_context exception_ context);
                Alcotest.(check bool)
                  "expression producer retains exact registered source" true
                  (match
                     Source_manager.find
                       (Parser.context_sources context)
                       (Parser.compiler_exception_diagnostic exception_).primary
                         .source
                   with
                  | Some original -> original == source
                  | None -> false);
                counts :=
                  (Parser.context_error_count context |> Result.get_ok)
                  :: !counts;
                seen := exception_ :: !seen)
              session source config
          in
          Alcotest.(check bool)
            "expression fails before accepting AST" true
            (Option.is_none parsed.ast);
          Alcotest.(check (list int64))
            "two original error field increments" [ 1L; 2L ] (List.rev !counts);
          expression_receipt ~code:"HCPARSE0018" ~marker:";" text
            parsed.diagnostics (List.rev !seen);
          let seen = ref [] in
          let represented =
            parse
              ~compiler_exception:(fun x -> seen := x :: !seen)
              session source config
          in
          Alcotest.(check bool)
            "representation reports malformed expression" true
            (Option.is_none represented.ast);
          Alcotest.(check int)
            "representation cannot create cleanup authority" 0
            (List.length !seen);
          Alcotest.(check bool)
            "representation cannot create cleanup diagnostic" false
            (List.exists
               (fun (d : Diagnostic.t) -> d.code = "HCPARSE0173")
               represented.diagnostics))
        [ "1+;"; "(1+;"; "-;" ];
      List.iter
        (fun (_, text) ->
          let session, source, config = inputs mode text in
          let seen = ref [] in
          ignore
            (parse ~commands:(command_sink ())
               ~compiler_exception:(fun x -> seen := x :: !seen)
               session source config);
          Alcotest.(check int)
            "generic expression failure cannot invoke cleanup" 0
            (List.length !seen))
        (Cases.expression_noncompiler_failures
        @
        if mode = Preprocessor.Aot then
          Cases.expression_aot_noncompiler_failures
        else []);
      List.iter
        (fun code ->
          let session, source, config = inputs mode "F(1);" in
          ignore
            (Symbol_visibility.Environment.add (Session.symbols session)
               ~name:"F" ~kind:Symbol_visibility.Function ());
          let seen = ref [] in
          let call : Parser.direct_call_sink =
            {
              start =
                (fun _ ->
                  Error
                    [
                      Diagnostic.make ~code ~severity:Diagnostic.Error
                        ~message:"callback expression failure"
                        ~primary:
                          (Span.unsafe_make ~source:(Source_file.id source)
                             ~start:0 ~stop:1)
                        ();
                    ]);
              emit = (fun _ -> Alcotest.fail "rejected start cannot emit");
              implicit = None;
            }
          in
          let parsed =
            parse ~commands:(command_sink ~call ())
              ~compiler_exception:(fun x -> seen := x :: !seen)
              session source config
          in
          Alcotest.(check bool)
            "callback fault aborts active expression" true
            (Option.is_none parsed.ast);
          Alcotest.(check int)
            "callback text grants no cleanup authority" 0 (List.length !seen))
        [ "HCPARSE0018"; "HCPARSE0173" ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let expression_source_execution () =
  List.iter
    (fun mode ->
      let run text =
        let session, source, config =
          inputs mode
            ((if mode = Preprocessor.Jit then Cases.headers else "") ^ text)
        in
        run_integer_program_report session ~source ~config ~max_steps:100_000
      in
      List.iter
        (fun (label, text, marker) ->
          let report = run text in
          let diagnostics =
            match integer_program_report_outcome report with
            | Error errors -> errors
            | Ok _ -> Alcotest.fail (label ^ " unexpectedly executed")
          in
          expression_receipt ~reported_origin:false ~code:"HCPARSE0018" ~marker
            label diagnostics
            (integer_program_report_compiler_exceptions report);
          let session, source, config = inputs mode text in
          let compiled =
            compile_integer_program_report session ~source ~config
          in
          let diagnostics =
            match integer_program_compilation_result compiled with
            | Error errors -> errors
            | Ok _ -> Alcotest.fail (label ^ " unexpectedly compiled")
          in
          expression_receipt ~reported_origin:false ~code:"HCPARSE0018" ~marker
            (label ^ " compilation") diagnostics
            (integer_program_compilation_compiler_exceptions compiled))
        Cases.expression_failures;
      List.iter
        (fun (label, text, marker, output) ->
          let report = run text in
          let result =
            match integer_program_report_outcome report with
            | Ok result -> result
            | Error errors -> Alcotest.fail (label ^ ": " ^ describe errors)
          in
          Alcotest.(check (option int64))
            (label ^ " caught cleanup resumes outer input")
            (Some 42L)
            (Option.map
               (fun word -> word.Ir_integer_interpreter.bits)
               (Ir_integer_interpreter.final_value result.value));
          Alcotest.(check string)
            (label ^ " retains reached output and skips failing tail")
            output
            (integer_program_report_output_bytes report);
          let exceptions = integer_program_report_compiler_exceptions report in
          expression_receipt ~code:"HCPARSE0018" ~marker label
            (List.map Parser.compiler_exception_diagnostic exceptions)
            exceptions)
        Cases.expression_caught_children;
      List.iter
        (fun (label, text, expected) ->
          let report = run text in
          let result =
            match integer_program_report_outcome report with
            | Ok result -> result
            | Error errors -> Alcotest.fail (label ^ ": " ^ describe errors)
          in
          Alcotest.(check (option int64))
            label (Some expected)
            (Option.map
               (fun word -> word.Ir_integer_interpreter.bits)
               (Ir_integer_interpreter.final_value result.value));
          Alcotest.(check int)
            (label ^ " no Compiler producer")
            0
            (List.length (integer_program_report_compiler_exceptions report)))
        Cases.expression_successes;
      List.iter
        (fun (label, text) ->
          let report = run text in
          Alcotest.(check bool)
            (label ^ " remains failed")
            true
            (Result.is_error (integer_program_report_outcome report));
          Alcotest.(check int)
            (label ^ " no Compiler cleanup")
            0
            (List.length (integer_program_report_compiler_exceptions report)))
        (Cases.expression_noncompiler_failures
        @
        if mode = Preprocessor.Aot then
          Cases.expression_aot_noncompiler_failures
        else []);
      let report = run "#exe {Print(\"kept\");1+;}42;" in
      List.iter
        (fun (label, text) ->
          let child = run text in
          Alcotest.(check bool)
            (label ^ " unaudited child is not caught")
            true
            (Result.is_error (integer_program_report_outcome child));
          Alcotest.(check int)
            (label ^ " no child Compiler authority")
            0
            (List.length (integer_program_report_compiler_exceptions child));
          Alcotest.(check string)
            (label ^ " child reached output retained")
            "kept"
            (integer_program_report_output_bytes child))
        Cases.expression_uncaught_children;
      Alcotest.(check string)
        "expression cleanup preserves earlier directive output" "kept"
        (integer_program_report_output_bytes report);
      let report = run Cases.expression_successive_catches in
      Alcotest.(check bool)
        "successive expression catches both resume" true
        (Result.is_ok (integer_program_report_outcome report));
      Alcotest.(check string)
        "successive catches retain reached output" "abafter"
        (integer_program_report_output_bytes report);
      let nested = run Cases.expression_nested_directive in
      Alcotest.(check bool)
        "nested directive fails original expression" true
        (Result.is_error (integer_program_report_outcome nested));
      nested_expression_receipt "nested directive"
        (integer_program_report_outcome nested |> Result.get_error)
        (integer_program_report_compiler_exceptions nested);
      let caught = run Cases.expression_caught_nested_directive in
      Alcotest.(check bool)
        "nested expression cleanup is caught once" true
        (Result.is_ok (integer_program_report_outcome caught));
      Alcotest.(check string)
        "nested cleanup preserves reached child output" "keptafter"
        (integer_program_report_output_bytes caught);
      let exceptions = integer_program_report_compiler_exceptions caught in
      nested_expression_receipt "caught nested directive"
        (List.map Parser.compiler_exception_diagnostic exceptions)
        exceptions;
      (match integer_program_report_compiler_exceptions report with
      | [ first; cleanup; second; cleanup2 ] ->
          List.iter
            (fun exceptions ->
              expression_receipt ~code:"HCPARSE0018" ~marker:";"
                "successive catch"
                (List.map Parser.compiler_exception_diagnostic exceptions)
                exceptions)
            [ [ first; cleanup ]; [ second; cleanup2 ] ]
      | _ ->
          Alcotest.fail
            "successive catches must retain both two-producer chains");
      let report = run Cases.expression_fault_after_catch in
      Alcotest.(check bool)
        "later runtime error remains failed" true
        (Result.is_error (integer_program_report_outcome report));
      Alcotest.(check int)
        "later runtime error adds no cleanup" 2
        (List.length (integer_program_report_compiler_exceptions report));
      Alcotest.(check string)
        "later runtime error retains child output" "kept"
        (integer_program_report_output_bytes report);
      List.iter
        (fun (limit, expected_count, output) ->
          let session, source, config =
            inputs mode
              ((if mode = Preprocessor.Jit then Cases.headers else "")
              ^ Cases.expression_quota_after_catch)
          in
          let report =
            run_integer_program_report session ~source ~config
              ~max_steps:100_000 ~max_output_bytes:limit
          in
          Alcotest.(check bool)
            "output quota remains failed" true
            (Result.is_error (integer_program_report_outcome report));
          Alcotest.(check int)
            "quota preserves only reached producers" expected_count
            (List.length (integer_program_report_compiler_exceptions report));
          Alcotest.(check string)
            "quota preserves reached child bytes" output
            (integer_program_report_output_bytes report))
        [ (3, 0, ""); (4, 2, "kept") ])
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
    Alcotest.test_case "failed input owns its consumed suspension" `Quick
      failed_input_identity;
    Alcotest.test_case "failed input retains complete directive abort chain"
      `Quick failed_input_abort_chain;
    Alcotest.test_case "failed input source preflight and child generation"
      `Quick failed_input_preflight_and_generation;
    Alcotest.test_case "failed input domain, collection and single claim" `Quick
      failed_input_foreign_domain_and_claim;
    Alcotest.test_case "ordinary child diagnostics cannot authorize a catch"
      `Quick failed_input_other_failures;
    Alcotest.test_case
      "unexpected failure and rejected cleanup cannot authorize a catch" `Quick
      failed_input_unexpected_cleanup;
    Alcotest.test_case "failed input preserves original saved compiler tables"
      `Quick failed_input_saved_enclosing_tables;
    Alcotest.test_case
      "saved function returns cannot forge missing-function Compiler" `Quick
      saved_function_return_failure_kind;
    Alcotest.test_case
      "inherited function source failure has no Compiler authority" `Quick
      inherited_function_source_failure;
    Alcotest.test_case "caught Compiler children preserve owned work and resume"
      `Quick caught_child_source_execution;
    Alcotest.test_case
      "caught children keep incomplete bindings and other faults invalid" `Quick
      caught_child_does_not_hide_faults;
    Alcotest.test_case
      "failed child ledger requires original session, runtime and journal"
      `Quick failed_child_ledger_ownership;
    Alcotest.test_case "unhandled child belongs to its original input" `Quick
      unhandled_child_is_confined_to_original_input;
    Alcotest.test_case "original statement LexExcept phases" `Quick
      statement_phase_receipts;
    Alcotest.test_case "source break targets follow original PrsStmt owners"
      `Quick statement_break_targets;
    Alcotest.test_case
      "statement throws preserve Lex and header lookup ordering" `Quick
      statement_lexer_order;
    Alcotest.test_case "statement Compiler children preserve original IR work"
      `Quick statement_source_execution;
    Alcotest.test_case "call Compiler producers and nested caught IR inputs"
      `Quick call_source_execution;
    Alcotest.test_case "original call shape and producer receipts reject copies"
      `Quick call_phase_receipts;
    Alcotest.test_case "failed child catch requires original native call shape"
      `Quick failed_call_shape_ownership;
    Alcotest.test_case "zero argument marker preserves emission and Lex order"
      `Quick zero_argument_marker_phases;
    Alcotest.test_case
      "defaults preserve original delimiter and emission phases" `Quick
      default_traversal_phases;
    Alcotest.test_case
      "expression cleanup preserves original producers and controls" `Quick
      expression_phase_receipts;
    Alcotest.test_case
      "expression Compiler chains preserve original IR execution" `Quick
      expression_source_execution;
  ]
