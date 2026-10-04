open Holyc_lib
module D = Task_declarations
module Preparation = Holyc_lib__Driver.Native_default_preparation
module Unit = Holyc_lib__Driver.Integer_unit
module Saved = Holyc_lib__Ir.Prepared_parameter_default
module Callback_saved = Holyc_lib__Ir.Prepared_callback_default
module Default_program = Holyc_lib__Ir.Integer_interpreter
module Fragment = Holyc_lib__Sema.Default_fragment
module Proof = Native_parameter_defaults
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

let source_fixture mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-default-authority.hc" ~contents
  in
  let table = Session.semantic_symbols session in
  let ledger = D.create_source session ~source |> checked in
  let preparation =
    Preparation.create ~compilation_mode:mode ~max_initializer_steps:100 session
    |> checked
  in
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
            match D.observe ledger event with
            | Error _ as error -> error
            | Ok () -> (
                match event with
                | Parser.Parameter_default_completed receipt ->
                    Preparation.prepare preparation ~session ~ledger receipt
                | Parser.Callback_default_completed receipt ->
                    Preparation.prepare_callback preparation ~session ~ledger
                      receipt
                | Parser.Callback_signature_completed header ->
                    D.complete_source_callback_defaults ledger header
                | Parser.Function_header_completed header ->
                    D.complete_source_defaults ledger header
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
  let prepared =
    D.native_source_defaults ~table ~ast source_command |> diagnostics
  in
  let prepared_callbacks =
    D.native_source_callback_defaults ~table ~ast source_command |> diagnostics
  in
  let unit_ =
    Unit.compile_source_output ~source_command ~max_initializer_steps:100
      session ~config output
    |> diagnostics
  in
  ( unit_.value,
    prepared,
    prepared_callbacks,
    Preparation.completions preparation )

let fixture mode type_name default ending =
  let unit_, prepared, _, completions =
    source_fixture mode
      (Printf.sprintf
         "%s F(%s n=%s){return n;} I64 Unused(I64 x=7){return x;} %s" type_name
         type_name default ending)
  in
  (unit_, prepared, completions)

let seal ?(prepared_callbacks = []) unit_ prepared completions =
  Proof.create ~globals:(Unit.globals unit_)
    ~runtime_calls:(Unit.runtime_calls unit_)
    ~initialization:(Unit.initialization unit_)
    ~entry:(Unit.entry unit_) ~functions:(Unit.functions unit_) ~prepared
    ~prepared_callbacks ~completions

let compile ?parameter_defaults ?status_abi unit_ =
  Image.compile_callable ?parameter_defaults ?status_abi
    ~max_ir_instructions:4096 ~max_code_bytes:65536
    ~runtime_calls:(Unit.runtime_calls unit_)
    ~initialization:(Unit.initialization unit_)
    ~entry:(Unit.entry unit_) ~functions:(Unit.functions unit_) ()

let proof_ownership () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_name, default, expected_bits, stored_bits) ->
          List.iter
            (fun ending ->
              let unit_, prepared, executions =
                fixture mode type_name default ending
              in
              let foreign_unit, foreign_prepared, foreign_executions =
                fixture mode type_name default ending
              in
              Alcotest.(check int)
                "fixture includes an unused default owner" 2
                (List.length prepared);
              let proof = seal unit_ prepared executions |> checked in
              Alcotest.(check bool)
                "authentic preparation compiles without native execution" true
                (Result.is_ok (compile ~parameter_defaults:proof unit_));
              reject "even supplied and unused defaults need proof"
                (compile unit_);
              reject "equal-source foreign bundle cannot borrow proof"
                (compile ~parameter_defaults:proof foreign_unit);
              reject "saved values alone do not authorize preparation"
                (seal unit_ prepared []);
              List.iteri
                (fun index saved ->
                  let altered =
                    Saved.create ~publication:(Saved.publication saved)
                      ~header:(Saved.header saved)
                      ~receipt:(Saved.receipt saved)
                      ~bits:(Int64.succ (Saved.bits saved))
                    |> checked
                  in
                  reject "changed saved bits cannot borrow an actual completion"
                    (seal unit_
                       (List.map
                          (fun value ->
                            if value == saved then altered else value)
                          prepared)
                       executions);
                  let label =
                    Printf.sprintf "default %d with %s" index ending
                  in
                  let remaining_prepared =
                    List.filter (( != ) saved) prepared
                  in
                  let remaining_executions =
                    List.filter
                      (fun completion ->
                        completion |> Preparation.execution
                        |> Default_program.default_constant_authority
                        |> Fragment.authorized_fragment |> Fragment.receipt
                        |> fun receipt -> receipt != Saved.receipt saved)
                      executions
                  in
                  Alcotest.(check int)
                    (label ^ " removes only its prepared value")
                    1
                    (List.length remaining_prepared);
                  Alcotest.(check int)
                    (label ^ " removes only its matching completion")
                    1
                    (List.length remaining_executions);
                  reject
                    (label ^ " requires its prepared owner")
                    (seal unit_ remaining_prepared executions);
                  reject
                    (label ^ " requires its completed execution")
                    (seal unit_ prepared remaining_executions);
                  reject
                    (label ^ " cannot omit both matched proofs")
                    (seal unit_ remaining_prepared remaining_executions))
                prepared;
              reject "duplicate prepared receipts are rejected"
                (seal unit_ (List.hd prepared :: prepared) executions);
              reject "duplicate completed receipts are rejected"
                (seal unit_ prepared (List.hd executions :: executions));
              reject "extra foreign preparation is rejected"
                (seal unit_ (List.hd foreign_prepared :: prepared) executions);
              reject "equal-source foreign completed fragments are rejected"
                (seal unit_ prepared foreign_executions);
              let saved =
                List.find
                  (fun saved ->
                    (Saved.header saved).function_publication.function_name
                      .spelling = "F")
                  prepared
              in
              let remaining_prepared = List.filter (( != ) saved) prepared in
              Alcotest.(check int64)
                (type_name
               ^ " saves full register bits before parameter narrowing")
                expected_bits (Saved.bits saved);
              let reconstructed =
                Saved.create ~publication:(Saved.publication saved)
                  ~header:(Saved.header saved) ~receipt:(Saved.receipt saved)
                  ~bits:(Saved.bits saved)
                |> checked
              in
              reject "equal saved facts are not the original prepared object"
                (seal unit_ (reconstructed :: remaining_prepared) executions);
              let changed =
                Saved.create ~publication:(Saved.publication saved)
                  ~header:(Saved.header saved) ~receipt:(Saved.receipt saved)
                  ~bits:(Int64.logxor (Saved.bits saved) 1L)
                |> checked
              in
              reject "caller-chosen bits cannot borrow successful preparation"
                (seal unit_ (changed :: remaining_prepared) executions);
              if expected_bits <> stored_bits then
                let narrowed =
                  Saved.create ~publication:(Saved.publication saved)
                    ~header:(Saved.header saved) ~receipt:(Saved.receipt saved)
                    ~bits:stored_bits
                  |> checked
                in
                reject
                  (type_name
                 ^ " normalized storage bits cannot replace the saved word")
                  (seal unit_ (narrowed :: remaining_prepared) executions))
            [ "F();"; "F(1);"; "42;" ])
        [
          ("I8", "255", 255L, -1L);
          ("U8", "554", 554L, 42L);
          ("I16", "65535", 65535L, -1L);
          ("U16", "65578", 65578L, 42L);
          ("I32", "4294967295", 4294967295L, -1L);
          ("U32", "4294967338", 4294967338L, 42L);
          ("I64", "20+22", 42L, 42L);
          ("I64", "1<<3", 8L, 8L);
          ("I64", "(-7<<63)<<1", 0L, 0L);
          ("I8", "255<<1", 510L, -2L);
          ( "U64",
            "-7>>0x8000000000000001",
            9223372036854775804L,
            9223372036854775804L );
          ("U64", "0xffffffffffffffff", -1L, -1L);
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let callback_proof_ownership () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_name, literal, bits) ->
          let contents =
            Printf.sprintf
              "%s Echo(%s n=17){return n;} %s (*G)(%s n=%s); I64 Run(){%s \
               (*Unused)(%s n=%s),(*p)(%s n=%s);p=&Echo;return p();}Run();"
              type_name type_name type_name type_name literal type_name
              type_name literal type_name literal
          in
          let unit_, named, callbacks, completions =
            source_fixture mode contents
          in
          let foreign, _, foreign_callbacks, foreign_completions =
            source_fixture mode contents
          in
          Alcotest.(check int)
            "global and both local declarations are prepared" 3
            (List.length callbacks);
          let seal callbacks completions =
            seal ~prepared_callbacks:callbacks unit_ named completions
          in
          let proof = seal callbacks completions |> checked in
          List.iter
            (fun status_abi ->
              ignore
                (compile ~status_abi ~parameter_defaults:proof unit_
                |> Result.map_error (fun errors ->
                    String.concat "; "
                      (List.map
                         (fun (error : Image.error) -> error.message)
                         errors))
                |> checked);
              reject "callback saved bits alone cannot authorize code emission"
                (compile ~status_abi unit_);
              reject "callback proof cannot move to an equal-source bundle"
                (compile ~status_abi ~parameter_defaults:proof foreign))
            [ Image.Windows_x64; Image.System_v_x64 ];
          reject "callback preparation receipts cannot be omitted"
            (seal callbacks []);
          reject "foreign namespace receipts cannot replace current completions"
            (seal callbacks foreign_completions);
          reject "foreign prepared callback values cannot replace originals"
            (seal foreign_callbacks completions);
          reject "duplicate callback saved evidence is rejected"
            (seal (List.hd callbacks :: callbacks) completions);
          let callback_completion =
            List.find
              (fun completion ->
                match
                  completion |> Preparation.execution
                  |> Default_program.default_constant_authority
                  |> Fragment.authorized_fragment |> Fragment.source
                with
                | Fragment.Callback _ -> true
                | Named _ -> false)
              completions
          in
          reject "duplicate callback completion is rejected"
            (seal callbacks (callback_completion :: completions));
          List.iter
            (fun saved ->
              Alcotest.(check int64)
                "anonymous saved values retain full bits" bits
                (Callback_saved.bits saved);
              let replace value =
                List.map
                  (fun prior -> if prior == saved then value else prior)
                  callbacks
              in
              List.iter
                (fun replacement_bits ->
                  let rebuilt =
                    Callback_saved.create
                      ~namespace:(Callback_saved.namespace saved)
                      ~header:(Callback_saved.header saved)
                      ~receipt:(Callback_saved.receipt saved)
                      ~bits:replacement_bits
                    |> checked
                  in
                  reject
                    "reconstructed values cannot borrow original anonymous \
                     preparation"
                    (seal (replace rebuilt) completions))
                [ bits; Int64.succ bits ];
              let without_saved = List.filter (( != ) saved) callbacks in
              let without_completion =
                List.filter
                  (fun completion ->
                    match
                      completion |> Preparation.execution
                      |> Default_program.default_constant_authority
                      |> Fragment.authorized_fragment |> Fragment.source
                    with
                    | Callback (_, receipt) ->
                        receipt != Callback_saved.receipt saved
                    | Named _ -> true)
                  completions
              in
              reject "even unused anonymous defaults require their saved object"
                (seal without_saved completions);
              reject
                "each anonymous default requires its own charged completion"
                (seal callbacks without_completion);
              reject
                "omitting a matched anonymous pair does not remove its \
                 declaration"
                (seal without_saved without_completion))
            callbacks)
        [
          ("I8", "255", 255L);
          ("U8", "554", 554L);
          ("I64", "20+22", 42L);
          ("U64", "0xffffffffffffffff", -1L);
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let unused_callback_defaults_require_proof () =
  List.iter
    (fun mode ->
      List.iter
        (fun contents ->
          let unit_, named, callbacks, completions =
            source_fixture mode contents
          in
          Alcotest.(check int)
            "no named default can mask the callback guard" 0 (List.length named);
          let proof =
            seal ~prepared_callbacks:callbacks unit_ named completions
            |> checked
          in
          List.iter
            (fun status_abi ->
              reject
                "unused and explicit-only callback defaults require \
                 preparation authority"
                (compile ~status_abi unit_);
              Alcotest.(check bool)
                "original anonymous preparation admits both ABIs" true
                (Result.is_ok
                   (compile ~status_abi ~parameter_defaults:proof unit_)))
            [ Image.Windows_x64; Image.System_v_x64 ])
        [
          "I64 (*G)(I64 n=42);42;";
          "I64 F(){I64 (*p)(I64 n=42);return 42;}42;";
          "I64 Echo(I64 n){return n;}I64 F(){I64 (*p)(I64 n=42);p=&Echo;return \
           p(1);}F();";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let callback_word_proof_ownership () =
  let contents =
    "I64 Ignore(noreg U0 (*cb)()=0xffffffffffffffff){return cb==-1;}I64 \
     Unused(F64 (*cb)(F64 n)=17){return 42;}I64 (*G)(U0 \
     (*cb)()=0xffffffffffffffff)[2];I64 Run(){I64 (*p)(U0 \
     (*cb)()=0xffffffffffffffff);p=&Ignore;return p();}Run();"
  in
  List.iter
    (fun mode ->
      let unit_, named, callbacks, completions = source_fixture mode contents in
      let foreign, foreign_named, foreign_callbacks, foreign_completions =
        source_fixture mode contents
      in
      Alcotest.(check int)
        "named callback-word defaults including unused body" 2
        (List.length named);
      Alcotest.(check int)
        "global and local anonymous callback-word defaults" 2
        (List.length callbacks);
      let seal named callbacks completions =
        seal ~prepared_callbacks:callbacks unit_ named completions
      in
      let proof = seal named callbacks completions |> checked in
      List.iter
        (fun status_abi ->
          Alcotest.(check bool)
            "original callback word proof admits both ABIs" true
            (Result.is_ok (compile ~status_abi ~parameter_defaults:proof unit_));
          reject "callback words cannot compile from saved facts alone"
            (compile ~status_abi unit_);
          reject "callback word proof cannot move to an equal bundle"
            (compile ~status_abi ~parameter_defaults:proof foreign))
        [ Image.Windows_x64; Image.System_v_x64 ];
      reject "named callback words cannot borrow foreign saved owners"
        (seal foreign_named callbacks completions);
      reject "anonymous callback words cannot borrow foreign saved owners"
        (seal named foreign_callbacks completions);
      reject "callback words cannot borrow foreign charged executions"
        (seal named callbacks foreign_completions);
      List.iter
        (fun saved ->
          Alcotest.(check int)
            "saved named callback keeps physical pointer shape" 1
            (Semantic_type.pointer_depth (Saved.type_ saved));
          let remaining = List.filter (( != ) saved) named in
          reject "unused named callback word cannot omit saved proof"
            (seal remaining callbacks completions);
          let rebuilt =
            Saved.create ~publication:(Saved.publication saved)
              ~header:(Saved.header saved) ~receipt:(Saved.receipt saved)
              ~bits:(Saved.bits saved)
            |> checked
          in
          reject "reconstructed named callback word cannot borrow original work"
            (seal (rebuilt :: remaining) callbacks completions);
          reject "duplicated named callback word proof is rejected"
            (seal (saved :: named) callbacks completions))
        named;
      List.iter
        (fun saved ->
          Alcotest.(check int64)
            "anonymous callback word retains all bits" (-1L)
            (Callback_saved.bits saved);
          Alcotest.(check int)
            "saved anonymous callback keeps pointer shape" 1
            (Semantic_type.pointer_depth (Callback_saved.type_ saved));
          let remaining = List.filter (( != ) saved) callbacks in
          reject "unused anonymous callback word cannot omit saved proof"
            (seal named remaining completions);
          let rebuilt =
            Callback_saved.create
              ~namespace:(Callback_saved.namespace saved)
              ~header:(Callback_saved.header saved)
              ~receipt:(Callback_saved.receipt saved)
              ~bits:(Callback_saved.bits saved)
            |> checked
          in
          reject "reconstructed anonymous callback word cannot borrow work"
            (seal named (rebuilt :: remaining) completions);
          reject "duplicated anonymous callback word proof is rejected"
            (seal named (saved :: callbacks) completions))
        callbacks;
      List.iter
        (fun completion ->
          let remaining = List.filter (( != ) completion) completions in
          reject "every callback word requires original charged completion"
            (seal named callbacks remaining);
          reject "a callback word completion cannot be consumed twice"
            (seal named callbacks (completion :: completions)))
        completions)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let () =
  Alcotest.run "native default authority"
    [
      ( "ownership",
        [
          Alcotest.test_case
            "callback word defaults retain original charged owners" `Quick
            callback_word_proof_ownership;
          Alcotest.test_case
            "unused anonymous declarations cannot bypass preparation proof"
            `Quick unused_callback_defaults_require_proof;
          Alcotest.test_case
            "anonymous defaults retain their original saved and charged owners"
            `Quick callback_proof_ownership;
          Alcotest.test_case
            "exact original defaults bind the entire native bundle" `Quick
            proof_ownership;
        ] );
    ]
