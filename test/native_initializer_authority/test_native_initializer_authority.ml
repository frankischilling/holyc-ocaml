open Holyc_lib
module D = Task_declarations
module Preparation = Holyc_lib__Driver.Native_default_preparation
module Initializers = Holyc_lib__Driver.Integer_initializers
module Unit = Holyc_lib__Driver.Integer_unit
module Proof = Native_global_initializers
module Image = X86_64_program

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics = function
  | Ok value -> value
  | Error errors ->
      Alcotest.fail
        (String.concat "; "
           (List.map
              (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
              errors))

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let fixture ?contents ?(statics = false) mode =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-initializer-authority.hc"
      ~contents:
        (Option.value contents
           ~default:
             (if statics then
                "I8 A=255;I64 F(){static I8 n=40;return ++n;}I64 G(){static I8 \
                 n=6*7;return n;}F();F();"
              else "I8 A=255;I64 B=6*7;I64 F(){return B;}A+F();"))
  in
  let ledger = D.create_source session ~source |> checked in
  let preparation =
    Preparation.create ~compilation_mode:mode ~max_initializer_steps:100 session
    |> checked
  in
  let check_suspended context current =
    let token = Parser.suspend_context context |> checked in
    let child =
      Session.add_source session ~path:"empty-static-child.hc" ~contents:""
    in
    let commands : Parser.command_sink =
      {
        checkpoint =
          Some
            (fun _ ->
              Alcotest.(check bool)
                "suspended static receipt" false (current ());
              Ok ());
        query = None;
        reference = None;
        implicit_output = None;
        call = None;
        declaration = None;
        dimension_count = None;
        command = (fun _ -> Ok ());
        resume = (fun () -> Ok ());
      }
    in
    let config =
      Preprocessor.Config.create ~compilation_mode:mode () |> checked
    in
    let child =
      Parser.parse_suspended token ~commands ~sources:(Session.sources session)
        ~definitions:(Session.definitions session)
        ~symbols:(Parser.context_environment context)
        ~config child
      |> checked
    in
    Alcotest.(check bool) "empty nested source" false (Parser.has_errors child);
    Alcotest.(check bool) "restored static receipt" true (current ())
  in
  let receipts = ref [] in
  let static_receipts = ref [] in
  let commands : Parser.command_sink =
    {
      checkpoint = Some (D.observe_command ledger);
      query = Some (D.observe_query ledger);
      reference = Some (D.observe_reference ledger);
      implicit_output = None;
      call = None;
      declaration =
        Some
          (fun event ->
            Result.bind (D.observe ledger event) (fun () ->
                match event with
                | Parser.Global_initializer_leaf_completed receipt ->
                    (if !receipts <> [] then
                       let other =
                         Preparation.create ~compilation_mode:mode
                           ~max_initializer_steps:100 session
                         |> checked
                       in
                       reject "one ledger cannot split its preparation budget"
                         (Preparation.prepare_initializer other ~session ~ledger
                            receipt));
                    Result.map
                      (fun () ->
                        receipts := receipt :: !receipts;
                        reject "same callback cannot prepare twice"
                          (Preparation.prepare_initializer preparation ~session
                             ~ledger receipt))
                      (Preparation.prepare_initializer preparation ~session
                         ~ledger receipt)
                | Parser.Static_initializer_preparing receipt ->
                    List.iter
                      (fun (earlier_event, earlier) ->
                        let before = Preparation.work preparation in
                        reject "expired original leaf cannot reenter the ledger"
                          (D.observe ledger earlier_event);
                        reject
                          "expired original leaf cannot prepare during its \
                           successor"
                          (Preparation.prepare_static preparation ~session
                             ~ledger earlier);
                        Alcotest.(check int)
                          "rejected replay leaves work unchanged" before
                          (Preparation.work preparation))
                      !static_receipts;
                    check_suspended
                      receipt.static_allocation.allocation_function
                        .function_header
                        .declaration_command
                        .command_context (fun () ->
                        Parser.static_initializer_is_current receipt);
                    Alcotest.(check bool)
                      "original static callback" true
                      (Parser.static_initializer_is_current receipt);
                    let clone = Obj.obj (Obj.dup (Obj.repr receipt)) in
                    Alcotest.(check bool)
                      "cloned static receipt" false
                      (Parser.static_initializer_is_current clone);
                    reject "cloned receipt cannot prepare"
                      (Preparation.prepare_static preparation ~session ~ledger
                         clone);
                    Result.map
                      (fun () ->
                        static_receipts := (event, receipt) :: !static_receipts;
                        reject "static callback cannot prepare twice"
                          (Preparation.prepare_static preparation ~session
                             ~ledger receipt))
                      (Preparation.prepare_static preparation ~session ~ledger
                         receipt)
                | Parser.Static_initializer_completed receipt ->
                    check_suspended
                      receipt.static_completed_start.static_start_allocation
                        .allocation_function
                        .function_header
                        .declaration_command
                        .command_context (fun () ->
                        Parser.static_initializer_completion_is_current receipt);
                    Alcotest.(check bool)
                      "original static completion" true
                      (Parser.static_initializer_completion_is_current receipt);
                    let clone = Obj.obj (Obj.dup (Obj.repr receipt)) in
                    Alcotest.(check bool)
                      "cloned static completion" false
                      (Parser.static_initializer_completion_is_current clone);
                    Ok ()
                | _ -> Ok ()));
      dimension_count = Some (D.grammar_dimension_count ledger);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  let output =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  if Parser.has_errors output then
    ignore (diagnostics (Error output.diagnostics));
  List.iter
    (fun prepared ->
      let receipt = Initializers.native_static_receipt prepared in
      Alcotest.(check bool)
        "original static callback expired" false
        (Parser.static_initializer_is_current receipt);
      Alcotest.(check bool)
        "original static completed" true
        (Option.is_some
           (Parser.static_initializer_completed_declarator receipt));
      reject "expired static cannot prepare"
        (Preparation.prepare_static preparation ~session ~ledger receipt))
    (Preparation.static_initializers preparation);
  let ast = Option.get output.ast in
  let source_command = D.seal_source ledger ast |> diagnostics in
  let runtime =
    Ir_integer_interpreter.create_task_state ~max_initializer_steps:100
      ~table:(Session.semantic_symbols session)
      ()
    |> checked
  in
  List.iter
    (fun receipt ->
      reject "expired source callback"
        (D.native_initializer_fragment ledger ~runtime receipt))
    !receipts;
  let compile ?native_initializers ?native_static_initializers
      ?(max_initializer_steps = 100) () =
    Unit.compile_source_output ?native_initializers ?native_static_initializers
      ~source_command ~max_initializer_steps session ~config output
    |> Result.map (fun (unit_ : Unit.compiled Unit.checked) -> unit_.value)
  in
  (ast.span, preparation, compile)

let proof ?(static_completions = []) span completions unit_ =
  Proof.create ~span ~static_completions ~completions
    ~preparation:(Unit.initializer_preparation unit_)
    ~runtime_calls:(Unit.runtime_calls unit_)
    ~initialization:(Unit.initialization unit_)
    ~entry:(Unit.entry unit_) ~functions:(Unit.functions unit_)

let emit ?global_initializers ?status_abi unit_ =
  Image.compile_callable ?global_initializers ?status_abi
    ~max_ir_instructions:4096 ~max_code_bytes:65536
    ~runtime_calls:(Unit.runtime_calls unit_)
    ~initialization:(Unit.initialization unit_)
    ~entry:(Unit.entry unit_) ~functions:(Unit.functions unit_) ()

let ownership () =
  List.iter
    (fun mode ->
      let span, prepared, compile = fixture mode in
      let evidence = Preparation.initializers prepared in
      let unit_ = compile ~native_initializers:evidence () |> diagnostics in
      let completions = Preparation.initializer_completions prepared in
      let sealed = proof span completions unit_ |> checked in
      reject "missing charged completions" (proof span [] unit_);
      reject "reordered charged completions"
        (proof span (List.rev completions) unit_);
      List.iter
        (fun status_abi ->
          Alcotest.(check bool)
            "both ABI encodings" true
            (Result.is_ok (emit ~global_initializers:sealed ~status_abi unit_)))
        [ Image.Windows_x64; Image.System_v_x64 ];
      reject "prepared bits without a bundle proof" (emit unit_);
      let _, other_prepared, other_compile = fixture mode in
      let other =
        other_compile
          ~native_initializers:(Preparation.initializers other_prepared)
          ()
        |> diagnostics
      in
      reject "equal-source foreign bundle"
        (emit ~global_initializers:sealed other);
      let legacy_span, _, legacy_compile = fixture mode in
      let legacy = legacy_compile () |> diagnostics in
      reject "ordinary preparation cannot impersonate original native callbacks"
        (proof legacy_span completions legacy);
      List.iter
        (fun mutate ->
          let _, prepared, compile = fixture mode in
          reject "missing, repeated, reordered or substituted leaves"
            (compile
               ~native_initializers:(mutate (Preparation.initializers prepared))
               ()))
        [
          (fun _ -> []);
          (fun xs -> List.tl xs);
          (fun xs -> List.hd xs :: xs);
          List.rev;
          (fun _ -> evidence);
        ];
      let _, prepared, compile = fixture mode in
      let steps = Preparation.work prepared in
      Alcotest.(check int)
        "work charged once" steps
        (List.fold_left
           (fun n proof -> n + Initializers.native_steps proof)
           0
           (Preparation.initializers prepared));
      ignore
        (compile
           ~native_initializers:(Preparation.initializers prepared)
           ~max_initializer_steps:steps ()
        |> diagnostics);
      let _, prepared, compile = fixture mode in
      reject "one below preparation budget"
        (compile
           ~native_initializers:(Preparation.initializers prepared)
           ~max_initializer_steps:(steps - 1) ()))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let static_ownership () =
  List.iter
    (fun mode ->
      let span, prepared, compile = fixture ~statics:true mode in
      let evidence = Preparation.static_initializers prepared in
      let globals = Preparation.initializers prepared in
      let unit_ =
        compile ~native_initializers:globals
          ~native_static_initializers:evidence ()
        |> diagnostics
      in
      let completions = Preparation.static_completions prepared in
      let seal ?(static_completions = completions) unit_ =
        proof ~static_completions span
          (Preparation.initializer_completions prepared)
          unit_
      in
      let sealed = seal unit_ |> checked in
      List.iter
        (fun status_abi ->
          ignore
            (emit ~global_initializers:sealed ~status_abi unit_
            |> Result.map_error (fun _ -> "ABI encoding failed")
            |> checked))
        [ Image.Windows_x64; Image.System_v_x64 ];
      reject "static prepared bits without certificate" (emit unit_);
      let cloned_evidence =
        List.map (fun p -> Obj.obj (Obj.dup (Obj.repr p))) evidence
      in
      let reconstructed =
        compile ~native_initializers:globals
          ~native_static_initializers:cloned_evidence ()
        |> diagnostics
      in
      reject "reconstructed static preparation cannot match charged completion"
        (seal reconstructed);

      reject "missing static completions" (seal ~static_completions:[] unit_);
      reject "reordered static completions"
        (seal ~static_completions:(List.rev completions) unit_);
      List.iter
        (fun mutate ->
          let _, p, c = fixture ~statics:true mode in
          reject "missing repeated reordered or foreign static proof"
            (c
               ~native_initializers:(Preparation.initializers p)
               ~native_static_initializers:
                 (mutate (Preparation.static_initializers p))
               ()))
        [
          (fun _ -> []);
          List.tl;
          (fun xs -> List.hd xs :: xs);
          List.rev;
          (fun _ -> evidence);
        ];
      let _, p, c = fixture ~statics:true mode in
      let foreign =
        c
          ~native_initializers:(Preparation.initializers p)
          ~native_static_initializers:(Preparation.static_initializers p)
          ()
        |> diagnostics
      in
      reject "foreign static bundle" (emit ~global_initializers:sealed foreign);
      let _, _, c = fixture ~statics:true mode in
      let ordinary = c () |> diagnostics in
      reject "ordinary static preparation lacks source authority"
        (seal ordinary);
      let _, p, c = fixture ~statics:true mode in
      let steps = Preparation.work p in
      ignore
        (c
           ~native_initializers:(Preparation.initializers p)
           ~native_static_initializers:(Preparation.static_initializers p)
           ~max_initializer_steps:steps ()
        |> diagnostics);
      let _, p, c = fixture ~statics:true mode in
      reject "static shared budget one below"
        (c
           ~native_initializers:(Preparation.initializers p)
           ~native_static_initializers:(Preparation.static_initializers p)
           ~max_initializer_steps:(steps - 1) ()))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let array_ownership () =
  let contents =
    "I16 G[2][2]={{40,0},{0,2}};I64 F(){static U8 \
     S[2][3]={\"40\",\"12\"};return G[0][0]+G[1][1]+S[0][2]+S[1][2];}F();"
  in
  List.iter
    (fun mode ->
      let make () = fixture ~contents mode in
      let span, prepared, compile = make () in
      let globals = Preparation.initializers prepared in
      let statics = Preparation.static_initializers prepared in
      Alcotest.(check int)
        "four original numeric leaves" 4 (List.length globals);
      Alcotest.(check int) "two original copied rows" 2 (List.length statics);
      let unit_ =
        compile ~native_initializers:globals ~native_static_initializers:statics
          ()
        |> diagnostics
      in
      let seal unit_ =
        proof
          ~static_completions:(Preparation.static_completions prepared)
          span
          (Preparation.initializer_completions prepared)
          unit_
      in
      let sealed = seal unit_ |> checked in
      Alcotest.(check int)
        "array preparation is imported once"
        (Preparation.work prepared)
        (Initializers.executed_steps (Unit.initializer_preparation unit_));
      List.iter
        (fun status_abi ->
          ignore
            (emit ~global_initializers:sealed ~status_abi unit_
            |> Result.map_error (fun _ -> "array ABI encoding failed")
            |> checked))
        [ Image.Windows_x64; Image.System_v_x64 ];
      reject "array image requires original charged completions" (emit unit_);
      List.iter
        (fun mutation ->
          let change xs =
            match mutation with
            | `Missing -> []
            | `Tail -> List.tl xs
            | `Duplicate -> List.hd xs :: xs
            | `Reverse -> List.rev xs
          in
          let _, fresh, compile = make () in
          reject "numeric array leaves are complete and ordered"
            (compile
               ~native_initializers:(change (Preparation.initializers fresh))
               ~native_static_initializers:
                 (Preparation.static_initializers fresh)
               ());
          let _, fresh, compile = make () in
          reject "copied array rows are complete and ordered"
            (compile
               ~native_initializers:(Preparation.initializers fresh)
               ~native_static_initializers:
                 (change (Preparation.static_initializers fresh))
               ()))
        [ `Missing; `Tail; `Duplicate; `Reverse ];
      let reconstructed =
        compile ~native_initializers:globals
          ~native_static_initializers:
            (List.map (fun p -> Obj.obj (Obj.dup (Obj.repr p))) statics)
          ()
        |> diagnostics
      in
      reject "copied row proof cannot replace the charged preparation"
        (seal reconstructed);
      let _, fresh, compile = make () in
      let foreign =
        compile
          ~native_initializers:(Preparation.initializers fresh)
          ~native_static_initializers:(Preparation.static_initializers fresh)
          ()
        |> diagnostics
      in
      reject "equal-source array bundle cannot borrow another proof"
        (emit ~global_initializers:sealed foreign))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let array_publication_prefix () =
  List.iter
    (fun contents ->
      let span, prepared, compile = fixture ~contents Preprocessor.Jit in
      let unit_ =
        compile
          ~native_initializers:(Preparation.initializers prepared)
          ~native_static_initializers:(Preparation.static_initializers prepared)
          ()
        |> diagnostics
      in
      Alcotest.(check bool)
        "original interleaved publications exist" true
        (Ir_global_initialization.publications (Unit.initialization unit_) <> []);
      reject
        "native image cannot move a prepared publication before prior entry \
         work"
        (proof
           ~static_completions:(Preparation.static_completions prepared)
           span
           (Preparation.initializer_completions prepared)
           unit_))
    [
      "42;I8 G[2]={40,2};G[0]+G[1];";
      "42;I64 F(){static U8 a[2]={40,2};return a[0]+a[1];}F();";
    ]

let failed_preparation () =
  List.iter
    (fun mode ->
      List.iter
        (fun (contents, code) ->
          let session = Session.create () in
          let source =
            Session.add_source session ~path:"native-initializer-fault.hc"
              ~contents
          in
          let config =
            Preprocessor.Config.create ~compilation_mode:mode () |> checked
          in
          let report =
            Native_program.evaluate session ~config ~source ~max_steps:100
          in
          let errors =
            match Native_program.outcome report with
            | Error errors -> errors
            | Ok _ -> Alcotest.fail "invalid initializer reached native entry"
          in
          Alcotest.(check bool)
            ("expected " ^ code ^ " for " ^ contents ^ ": "
            ^ String.concat "; "
                (List.map
                   (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
                   errors))
            true
            (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors);
          Alcotest.(check bool)
            "no native image" true
            (Option.is_none (Native_program.image report));
          Alcotest.(check bool)
            "no runtime instruction attempt" true
            (Option.is_none (Native_program.executed_steps report));
          Alcotest.(check bool)
            "earlier declaration work survives" true
            (Native_program.preparation_steps report > 0))
        [
          ("I64 A=40;I64 B=1/0;42;", "HCIRVM0009");
          ("I64 F(I64 n=40){return n;}I64 B=1/0;42;", "HCIRVM0009");
          ("I64 A=40;I64 F(I64 n=1/0){return n;}42;", "HCIRVM0009");
          ("I64 A=40;I64 B=0&&(1/0);42;", "HCIRVM0009");
          ("I64 A=40;I64 B=1/0;I64 C=1<<2;42;", "HCIRVM0009");
          ("I64 A=40;I64 B=A;42;", "HCRUN0006");
          ("I64 A=40;I64 B=1<<2;42;", "HCRUN0006");
          ("I64 A=40;42;I64 B=2;", "HCRUN0001");
          ("I64 A=40;I64 F(){static I8 n=1/0;return n;}42;", "HCIRVM0009");
          ("I64 F(){static I8 n=40;return n;}I64 B=1/0;42;", "HCIRVM0009");
          ("I64 F(){return 1;static I8 n=1/0;}42;", "HCIRVM0009");
          ("I64 F(){static I8 n=40 junk;return n;}42;", "HCPARSE0102");
          ("I64 A=40;I64 F(){static I8 n=A;return n;}42;", "HCRUN0006");
          ( "I64 A=40;I64 H(){return 2;}I64 F(){static I8 n=H();return n;}42;",
            "HCRUN0006" );
          ("I64 A=40;I64 F(){static I8 n={2};return n;}42;", "HCRUN0001");
          ("I64 A=40;I64 F(){static I8 n=\"a\";return n;}42;", "HCRUN0006");
          ("I64 A=40;I64 F(){static I8 n=1<<2;return n;}42;", "HCRUN0006");
          ("I64 A=40;42;I64 F(){static I8 n=2;return n;}", "HCRUN0001");
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let array_failure_work () =
  let copy_rows =
    "I64 F(){static U8 s[2][3]={\"AB\",\"CD\"};return s[0][2]+s[1][2]+42;}42;"
  in
  List.iter
    (fun mode ->
      let inputs contents =
        let session = Session.create () in
        let source =
          Session.add_source session ~path:"native-array-initializer-fault.hc"
            ~contents
        in
        let config =
          Preprocessor.Config.create ~compilation_mode:mode () |> checked
        in
        (session, config, source)
      in
      let session, config, source = inputs copy_rows in
      ignore
        (Native_program.compile ~max_initializer_steps:6 session ~config ~source
        |> diagnostics);
      List.iter
        (fun (contents, limit, code, work) ->
          let session, config, source = inputs contents in
          let report =
            Native_program.evaluate ~max_initializer_steps:limit session ~config
              ~source ~max_steps:100
          in
          let errors =
            match Native_program.outcome report with
            | Error errors -> errors
            | Ok _ -> Alcotest.fail "array preparation failure reached entry"
          in
          Alcotest.(check bool)
            ("array preparation diagnostic " ^ code)
            true
            (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors);
          Alcotest.(check int)
            "exact earlier and faulting leaf work" work
            (Native_program.preparation_steps report);
          Alcotest.(check bool)
            "failed array preparation has no image" true
            (Option.is_none (Native_program.image report));
          Alcotest.(check bool)
            "failed array preparation has no entry attempts" true
            (Option.is_none (Native_program.executed_steps report)))
        [
          ( "I64 F(){static I16 a[2]={40 junk,2};return 0;}42;",
            100,
            "HCPARSE0139",
            3 );
          ( "I64 F(){static I16 a[2]={40,1/0};return 0;}42;",
            100,
            "HCIRVM0009",
            6 );
          ("I16 a[2]={40,1/0};42;", 100, "HCIRVM0009", 6);
          ("I64 F(){static I16 a[2]={40,2};return 0;}42;", 5, "HCIRVM0007", 5);
          (copy_rows, 5, "HCIRVM0007", 3);
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let () =
  Alcotest.run "native initializer authority"
    [
      ( "source",
        [
          Alcotest.test_case "original preparation and bundle ownership" `Quick
            ownership;
          Alcotest.test_case "static original preparation and bundle ownership"
            `Quick static_ownership;
          Alcotest.test_case "numeric and copied array leaf ownership" `Quick
            array_ownership;
          Alcotest.test_case "prepared arrays publish before native entry"
            `Quick array_publication_prefix;
          Alcotest.test_case "preparation failures precede entry" `Quick
            failed_preparation;
          Alcotest.test_case "static leaf and copy failures retain exact work"
            `Quick array_failure_work;
        ] );
    ]
