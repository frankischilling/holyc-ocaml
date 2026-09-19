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

let fixture mode =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-initializer-authority.hc"
      ~contents:"I8 A=255;I64 B=6*7;I64 F(){return B;}A+F();"
  in
  let ledger = D.create_source session ~source |> checked in
  let preparation =
    Preparation.create ~compilation_mode:mode ~max_initializer_steps:100 session
    |> checked
  in
  let receipts = ref [] in
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
  let compile ?native_initializers ?(max_initializer_steps = 100) () =
    Unit.compile_source_output ?native_initializers ~source_command
      ~max_initializer_steps session ~config output
    |> Result.map (fun (unit_ : Unit.compiled Unit.checked) -> unit_.value)
  in
  (ast.span, preparation, compile)

let proof span completions unit_ =
  Proof.create ~span ~completions
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
            "expected guard" true
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
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let () =
  Alcotest.run "native initializer authority"
    [
      ( "source",
        [
          Alcotest.test_case "original preparation and bundle ownership" `Quick
            ownership;
          Alcotest.test_case "preparation failures precede entry" `Quick
            failed_preparation;
        ] );
    ]
