module Option = Holyc_lib.Compiler_option

let exact_registry () =
  Alcotest.(check (list string))
    "source names"
    [
      "OPTf_ECHO";
      "OPTf_TRACE";
      "OPTf_WARN_UNUSED_VAR";
      "OPTf_WARN_PAREN";
      "OPTf_WARN_DUP_TYPES";
      "OPTf_WARN_HEADER_MISMATCH";
      "OPTf_EXTERNS_TO_IMPORTS";
      "OPTf_KEEP_PRIVATE";
      "OPTf_NO_REG_VAR";
      "OPTf_GLBLS_ON_DATA_HEAP";
      "OPTf_NO_BUILTIN_CONST";
      "OPTf_USE_IMM64";
    ]
    (List.map Option.to_string Option.all);
  Alcotest.(check (list int))
    "bit indices"
    [ 0; 1; 16; 17; 18; 19; 32; 33; 34; 35; 36; 37 ]
    (List.map (fun option -> (Option.info option).bit_index) Option.all)

let typed_lookups () =
  Alcotest.(check bool)
    "name lookup" true
    (Option.of_string "OPTf_KEEP_PRIVATE" = Some Option.Keep_private);
  Alcotest.(check bool)
    "bit lookup" true
    (Option.of_bit_index 0x24 = Some Option.No_builtin_const);
  Alcotest.(check bool) "gap lookup" true (Option.of_bit_index 7 = None);
  Alcotest.(check bool) "negative lookup" true (Option.of_bit_index (-1) = None);
  Alcotest.(check bool)
    "unknown name" true
    (Option.of_string "OPTf_UNKNOWN" = None)

let defaults_use_target_masks () =
  Alcotest.(check int64) "known mask" 0x3f000f0003L Option.known_mask;
  Alcotest.(check int64) "initial mask" 0x90000L Option.initial_mask;
  Alcotest.(check bool)
    "unused warning enabled" true
    (Option.is_enabled ~mask:Option.initial_mask Option.Warn_unused_var);
  Alcotest.(check bool)
    "header warning enabled" true
    (Option.is_enabled ~mask:Option.initial_mask Option.Warn_header_mismatch);
  Alcotest.(check bool)
    "trace disabled" false
    (Option.is_enabled ~mask:Option.initial_mask Option.Trace)

let pure_set_returns_previous_state () =
  let enabled, previous = Option.set ~mask:0L Option.Trace true in
  Alcotest.(check bool) "previously disabled" false previous;
  Alcotest.(check int64) "trace mask" 2L enabled;
  let disabled, previous = Option.set ~mask:enabled Option.Trace false in
  Alcotest.(check bool) "previously enabled" true previous;
  Alcotest.(check int64) "trace cleared" 0L disabled

let scopes_and_source_status () =
  let externs = Option.info Option.Externs_to_imports in
  Alcotest.(check bool)
    "extern parsing option" true
    (List.mem Option.Parsing externs.phases);
  Alcotest.(check bool)
    "extern linkage option" true
    (List.mem Option.Linkage externs.phases);
  let no_reg = Option.info Option.No_reg_var in
  Alcotest.(check bool)
    "optimizer option" true
    (List.mem Option.Optimization no_reg.phases);
  let globals = Option.info Option.Globals_on_data_heap in
  Alcotest.(check bool)
    "allocation option" true
    (List.mem Option.Allocation globals.phases);
  Alcotest.(check bool)
    "linkage option" true
    (List.mem Option.Linkage globals.phases);
  let use_imm64 = Option.info Option.Use_imm64 in
  Alcotest.(check bool)
    "immediate optimization option" true
    (List.mem Option.Optimization use_imm64.phases);
  Alcotest.(check bool)
    "immediate emission option" true
    (List.mem Option.Code_emission use_imm64.phases);
  Alcotest.(check bool)
    "source marks USE_IMM64 incomplete" true
    (use_imm64.source_status = Option.Source_marked_incomplete)

let provenance () =
  Alcotest.(check string)
    "reference commit" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    Option.reference_commit;
  Alcotest.(check int) "source count" 16 (List.length Option.sources);
  Alcotest.(check (list (pair int int)))
    "intentional gaps"
    [ (2, 15); (20, 31) ]
    Option.intentional_gaps;
  Alcotest.(check string)
    "controller state" "Fs->last_cc->opts" Option.api.state_expression;
  Alcotest.(check bool)
    "Option returns previous state" true Option.api.set_returns_previous;
  Alcotest.(check (pair string int))
    "BEqu source" ("Kernel/KUtils.HC", 88)
    (Option.api.bit_set_source.path, Option.api.bit_set_source.line)

let consumer_lines () =
  let use_imm64 = Option.info Option.Use_imm64 in
  Alcotest.(check (list (pair string int)))
    "relocation consumers"
    [ ("Compiler/OptPass789A.HC", 359); ("Compiler/BackFA.HC", 285) ]
    (List.map
       (fun (reference : Option.source_reference) ->
         (reference.path, reference.line))
       use_imm64.consumers);
  let echo = Option.info Option.Echo in
  Alcotest.(check bool)
    "lexical echo consumer" true
    (List.exists
       (fun (reference : Option.source_reference) ->
         String.equal reference.path "Compiler/Lex.HC" && reference.line = 257)
       echo.consumers)

module Parser = Holyc_lib.Parser
module Session = Holyc_lib.Session
module Config = Holyc_lib.Preprocessor.Config

let checked_control = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let option_sink checkpoint : Parser.command_sink =
  {
    lexical_lookup = None;
    checkpoint = Some checkpoint;
    reference = None;
    call = None;
    implicit_output = None;
    declaration = None;
    query = None;
    dimension_count = None;
    command = (fun _ -> Ok ());
    resume = (fun () -> Ok ());
  }

let parse_options ?execute_stream session config contents commands =
  let source =
    Session.add_source session ~path:"compiler-options.hc" ~contents
  in
  let output =
    Parser.parse ?execute_stream ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  if Parser.has_errors output then Alcotest.fail "option control input failed";
  output

let original_live_control () =
  let session = Session.create () in
  let config = Config.create () |> checked_control in
  let original = ref None in
  let commands =
    option_sink (function
      | Parser.Sequence_started context ->
          original := Some context;
          Alcotest.(check int64)
            "source initial mask" Option.initial_mask
            (Parser.context_compiler_options context |> checked_control);
          List.iter
            (fun option ->
              let info = Option.info option in
              let index = Int64.of_int info.bit_index in
              let get () =
                Parser.context_get_option context ~bit_index:index
                |> checked_control
              in
              Alcotest.(check bool)
                "original default" info.initially_enabled (get ());
              Alcotest.(check bool)
                "set returns previous" info.initially_enabled
                (Parser.context_set_option context ~bit_index:index true
                |> checked_control);
              Alcotest.(check bool) "set uses actual control" true (get ());
              Alcotest.(check bool)
                "repeated set returns previous" true
                (Parser.context_set_option context ~bit_index:index true
                |> checked_control);
              Alcotest.(check bool)
                "clear returns previous" true
                (Parser.context_set_option context ~bit_index:index false
                |> checked_control);
              Alcotest.(check bool) "cleared actual control" false (get ()))
            Option.all;
          let mask =
            Parser.context_compiler_options context |> checked_control
          in
          List.iter
            (fun index ->
              Alcotest.(check bool)
                "unknown read rejects" true
                (Result.is_error
                   (Parser.context_get_option context ~bit_index:index));
              Alcotest.(check bool)
                "unknown write rejects" true
                (Result.is_error
                   (Parser.context_set_option context ~bit_index:index true)))
            [ -1L; 2L; 15L; 20L; 31L; 38L; Int64.max_int ];
          Alcotest.(check int64)
            "invalid writes retain mask" mask
            (Parser.context_compiler_options context |> checked_control);
          Alcotest.(check bool)
            "other domain cannot mutate control" true
            (Domain.spawn (fun () ->
                 Result.is_error
                   (Parser.context_set_option context ~bit_index:1L true))
            |> Domain.join);
          Ok ()
      | _ -> Ok ())
  in
  ignore (parse_options session config "42;" commands);
  let original = Stdlib.Option.get !original in
  Alcotest.(check bool)
    "closed control cannot be read" true
    (Result.is_error (Parser.context_compiler_options original));
  Alcotest.(check bool)
    "closed control cannot be mutated" true
    (Result.is_error (Parser.context_set_option original ~bit_index:1L true))

let original_source_snapshots () =
  let session = Session.create () in
  let config = Config.create () |> checked_control in
  let headers = ref [] and starts = ref [] and root = ref None in
  let commands =
    {
      (option_sink (function
        | Parser.Sequence_started context ->
            root := Some context;
            Ok ()
        | Parser.Command_started start ->
            starts := start :: !starts;
            Ok ()
        | Parser.Command_resumed _ ->
            ignore
              (Parser.context_set_option (Stdlib.Option.get !root) ~bit_index:1L
                 false
              |> checked_control);
            Ok ()
        | _ -> Ok ()))
      with
      declaration =
        Some
          (function
          | Parser.Parameter_default_completed receipt ->
              let context =
                receipt.default_function.function_header.declaration_command
                  .command_context
              in
              ignore
                (Parser.context_set_option context ~bit_index:1L true
                |> checked_control);
              Ok ()
          | Parser.Function_header_completed header ->
              headers := header :: !headers;
              Ok ()
          | _ -> Ok ());
    }
  in
  ignore (parse_options session config "I64 F(I64 n=0);I64 G();" commands);
  let headers = List.rev !headers and starts = List.rev !starts in
  Alcotest.(check int) "original source headers" 2 (List.length headers);
  let first = List.hd headers in
  Alcotest.(check int64)
    "declaration keeps entry options" Option.initial_mask
    first.function_publication.function_header.declaration_compiler_options;
  Alcotest.(check int64)
    "header retains reached default changes"
    (Int64.logor Option.initial_mask 2L)
    first.header_compiler_options;
  Alcotest.(check int64)
    "later changes cannot rewrite old header"
    (Int64.logor Option.initial_mask 2L)
    first.header_compiler_options;
  Alcotest.(check int64)
    "next declaration uses restored current mask" Option.initial_mask
    (List.nth headers 1).header_compiler_options;
  List.iter
    (fun start ->
      Alcotest.(check int64)
        "command retains original entry options" Option.initial_mask
        start.Parser.command_compiler_options)
    starts

let original_directive_and_child_controls () =
  let module Diagnostic = Holyc_lib.Diagnostic in
  let module Span = Holyc_lib.Span in
  let module Source_file = Holyc_lib.Source_file in
  let session = Session.create () in
  let task = Session.task_frontend session in
  let aot = Config.create ~compilation_mode:Aot () |> checked_control in
  let jit = Config.create ~compilation_mode:Jit () |> checked_control in
  let root = ref None and entered = ref 0 in
  let emit context counted =
    let source = Parser.context_source context in
    let diagnostic =
      Diagnostic.make ~severity:Diagnostic.Warning ~code:"HCTESTWARNING"
        ~message:"original warning counter"
        ~primary:
          (Span.unsafe_make ~source:(Source_file.id source) ~start:0 ~stop:0)
        ()
    in
    (if counted then Parser.context_emit_counted_compiler_warning
     else Parser.context_emit_compiler_warning)
      context diagnostic
    |> checked_control
  in
  let child =
    Session.add_source session ~path:"option-child.hc" ~contents:"42;"
  in
  let commands =
    option_sink (function
      | Parser.Sequence_started context ->
          root := Some context;
          Alcotest.(check int64)
            "fresh compiler warning count" 0L
            (Parser.context_warning_count context |> checked_control);
          emit context true;
          emit context false;
          Alcotest.(check int64)
            "PrintWarn-only warning leaves count unchanged" 1L
            (Parser.context_warning_count context |> checked_control);
          ignore
            (Parser.context_set_option context ~bit_index:1L true
            |> checked_control);
          Ok ()
      | Parser.Command_resumed _ ->
          let root = Stdlib.Option.get !root in
          Alcotest.(check int64)
            "directive count is retained by its parent" 2L
            (Parser.context_warning_count root |> checked_control);
          Alcotest.(check bool)
            "directive shares original compiler control" false
            (Parser.context_get_option root ~bit_index:1L |> checked_control);
          Alcotest.(check bool)
            "ordinary child changes stay separate" false
            (Parser.context_get_option root ~bit_index:0L |> checked_control);
          Ok ()
      | _ -> Ok ())
  in
  let execute_stream _ =
    incr entered;
    let stream_commands =
      option_sink (function
        | Parser.Sequence_started context ->
            Alcotest.(check int64)
              "directive shares warning count" 1L
              (Parser.context_warning_count context |> checked_control);
            emit context true;
            Alcotest.(check bool)
              "directive inherits live caller options" true
              (Parser.context_get_option context ~bit_index:1L
              |> checked_control);
            ignore
              (Parser.context_set_option context ~bit_index:1L false
              |> checked_control);
            Alcotest.(check bool)
              "suspended ancestor cannot operate" true
              (Result.is_error
                 (Parser.context_compiler_options (Stdlib.Option.get !root)));
            Ok ()
        | Parser.Command_resumed completed ->
            let caller = completed.command_start.command_context in
            for _ = 1 to 2 do
              let suspension =
                Parser.suspend_context caller |> checked_control
              in
              let child_commands =
                option_sink (function
                  | Parser.Sequence_started context ->
                      Alcotest.(check int64)
                        "each ordinary child starts a fresh counter" 0L
                        (Parser.context_warning_count context |> checked_control);
                      emit context true;
                      Alcotest.(check bool)
                        "child copies live caller, not saved table" false
                        (Parser.context_get_option context ~bit_index:1L
                        |> checked_control);
                      Alcotest.(check bool)
                        "successive child starts independently" false
                        (Parser.context_get_option context ~bit_index:0L
                        |> checked_control);
                      ignore
                        (Parser.context_set_option context ~bit_index:0L true
                        |> checked_control);
                      Gc.full_major ();
                      Gc.compact ();
                      Ok ()
                  | _ -> Ok ())
              in
              let output =
                Parser.parse_suspended_enclosing suspension
                  ~enclosing:(Stdlib.Option.get !root) ~commands:child_commands
                  ~sources:(Session.sources session)
                  ~definitions:(Session.definitions session)
                  ~symbols:(Session.symbols session) ~config:jit child
                |> checked_control
              in
              if Parser.has_errors output then
                Alcotest.fail "original child options failed";
              Alcotest.(check bool)
                "parent control restored after child" false
                (Parser.context_get_option caller ~bit_index:0L
                |> checked_control);
              Alcotest.(check int64)
                "child warnings do not increment parent counter" 2L
                (Parser.context_warning_count caller |> checked_control)
            done;
            Ok ()
        | _ -> Ok ())
    in
    Ok
      Parser.
        {
          definitions = Session.definitions task;
          symbols = Session.symbols task;
          commands = stream_commands;
          finish = (fun () -> Ok "");
          abort = (fun () -> ());
        }
  in
  let parsed =
    parse_options ~execute_stream session aot "#exe {42;}42;" commands
  in
  Alcotest.(check int)
    "counted and uncounted child diagnostics all reach the parent" 5
    (List.length parsed.diagnostics);
  Alcotest.(check int) "original directive entered" 1 !entered

let original_body_option_snapshots () =
  let session = Session.create () in
  let config = Config.create () |> checked_control in
  let bodies = ref [] in
  let commands =
    {
      (option_sink (fun _ -> Ok ())) with
      declaration =
        Some
          (function
          | Parser.Function_header_completed header ->
              let context =
                header.function_publication.function_header.declaration_command
                  .command_context
              in
              ignore
                (Parser.context_set_option context ~bit_index:16L false
                |> checked_control);
              Ok ()
          | Parser.Function_body_completed (header, body) ->
              let context =
                header.function_publication.function_header.declaration_command
                  .command_context
              in
              let reached =
                Parser.function_body_compiler_options header body
                |> checked_control
              in
              Alcotest.(check int64)
                "body retains its reached mask" 0x80000L reached;
              ignore
                (Parser.context_set_option context ~bit_index:16L true
                |> checked_control);
              bodies := (header, body) :: !bodies;
              Ok ()
          | _ -> Ok ());
    }
  in
  ignore (parse_options session config "I64 F(I64 unused){return 42;}" commands);
  let header, body = List.hd !bodies in
  Gc.full_major ();
  Gc.compact ();
  Alcotest.(check int64)
    "header retains its earlier mask" Option.initial_mask
    header.header_compiler_options;
  Alcotest.(check int64)
    "expired snapshot remains immutable" 0x80000L
    (Parser.function_body_compiler_options header body |> checked_control);
  Alcotest.(check bool)
    "closed body has no execution authority" false
    (Parser.function_body_completion_is_current header body);
  let copied_body : Holyc_lib.Ast.function_definition =
    Obj.obj (Obj.dup (Obj.repr body))
  in
  Alcotest.(check bool)
    "shallow body copy has no reached mask" true
    (Result.is_error (Parser.function_body_compiler_options header copied_body));
  let copied_header : Parser.completed_function_header =
    Obj.obj (Obj.dup (Obj.repr header))
  in
  Alcotest.(check bool)
    "shallow header copy has no original mask" true
    (Result.is_error (Parser.function_body_compiler_options copied_header body))

let source_warnings () =
  let run mode text =
    let session = Session.create () in
    let config = Config.create ~compilation_mode:mode () |> checked_control in
    let source =
      Session.add_source session ~path:"source-warning-options.hc"
        ~contents:text
    in
    Holyc_lib.run_integer_program_report ~max_steps:10000 session ~config
      ~source
  in
  let success report =
    match Holyc_lib.integer_program_report_outcome report with
    | Ok _ -> ()
    | Error diagnostics ->
        Alcotest.fail
          (diagnostics
          |> List.map (fun diagnostic ->
              diagnostic.Holyc_lib.Diagnostic.message)
          |> String.concat "; ")
  in
  let diagnostics report =
    match Holyc_lib.integer_program_report_outcome report with
    | Ok checked -> checked.diagnostics
    | Error diagnostics -> diagnostics
  in
  let warnings report =
    diagnostics report
    |> List.filter (fun diagnostic ->
        diagnostic.Holyc_lib.Diagnostic.severity = Holyc_lib.Diagnostic.Warning)
    |> List.map (fun diagnostic ->
        (diagnostic.Holyc_lib.Diagnostic.code, diagnostic.message))
  in
  List.iter
    (fun mode ->
      let check text expected =
        let report = run mode text in
        success report;
        Alcotest.(check (list (pair string string)))
          "reached compiler warnings" expected (warnings report)
      in
      check
        "#exe {Option(16,0);I64 Quiet(I64 unused){return 42;}Option(16,1);I64 \
         Loud(I64 unused){return 42;}Option(16,0);}42;"
        [ ("HCSEMA0034", "unused variable \"unused\" in function \"Loud\"") ];
      check
        "#exe {Option(16,0);I64 Suppression(I64 used,I64 _anon_){no_warn \
         used;used;no_warn _anon_;_anon_;return 42;}}42;"
        [
          ( "HCSEMA0035",
            "unneeded no_warn for \"used\" in function \"Suppression\"" );
        ];
      check
        "#exe {I64 BodyOff(I64 unused){I64 a[Option(16,0)+1];return 42;}}42;" [];
      check
        "#exe {Option(16,0);I64 BodyOn(I64 unused){I64 \
         a[Option(16,1)+1];return 42;}}42;"
        [
          ("HCSEMA0034", "unused variable \"unused\" in function \"BodyOn\"");
          ("HCSEMA0034", "unused variable \"a\" in function \"BodyOn\"");
        ];
      check
        {|#exe {Option(16,0);StreamExePrint("Option(16,1);I64 Child(I64 unused){return 42;}");I64 Parent(I64 unused){return 42;}}42;|}
        [ ("HCSEMA0034", "unused variable \"unused\" in function \"Child\"") ];
      let report =
        run mode
          "#exe {I64 Earlier(I64 unused){return 42;}I64 zero=0;42/zero;}42;"
      in
      Alcotest.(check bool)
        "later execution fails" true
        (Result.is_error (Holyc_lib.integer_program_report_outcome report));
      Alcotest.(check (list (pair string string)))
        "reached warning survives later failure"
        [ ("HCSEMA0034", "unused variable \"unused\" in function \"Earlier\"") ]
        (warnings report);
      let report =
        run mode "#exe {I64 Broken(I64 unused){return missing;}}42;"
      in
      Alcotest.(check bool)
        "body compilation fails" true
        (Result.is_error (Holyc_lib.integer_program_report_outcome report));
      Alcotest.(check (list (pair string string)))
        "failed body emits no unused warning" [] (warnings report))
    [ Holyc_lib.Preprocessor.Jit; Holyc_lib.Preprocessor.Aot ];
  List.iter
    (fun mode ->
      let report =
        run mode "I64 Ordinary(I64 unused){return 42;}Ordinary(0);"
      in
      success report;
      Alcotest.(check (list (pair string string)))
        "default warning option is consumed"
        [
          ("HCSEMA0034", "unused variable \"unused\" in function \"Ordinary\"");
        ]
        (warnings report))
    [ Holyc_lib.Preprocessor.Jit; Holyc_lib.Preprocessor.Aot ]

let tests =
  [
    Alcotest.test_case "exact registry" `Quick exact_registry;
    Alcotest.test_case "typed lookups" `Quick typed_lookups;
    Alcotest.test_case "default mask" `Quick defaults_use_target_masks;
    Alcotest.test_case "pure set" `Quick pure_set_returns_previous_state;
    Alcotest.test_case "scope and source status" `Quick scopes_and_source_status;
    Alcotest.test_case "provenance" `Quick provenance;
    Alcotest.test_case "consumer lines" `Quick consumer_lines;
    Alcotest.test_case "original live parser control" `Quick
      original_live_control;
    Alcotest.test_case "original source option snapshots" `Quick
      original_source_snapshots;
    Alcotest.test_case "directive and child option controls" `Quick
      original_directive_and_child_controls;
    Alcotest.test_case "source warning option consumer" `Quick source_warnings;
    Alcotest.test_case "original body option snapshots" `Quick
      original_body_option_snapshots;
  ]
