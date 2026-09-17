open Holyc_lib
module D = Task_declarations
module Preparation = Holyc_lib__Driver.Native_default_preparation
module Unit = Holyc_lib__Driver.Integer_unit
module Saved = Holyc_lib__Ir.Prepared_parameter_default
module Default_program = Holyc_lib__Ir.Default_fragment_program
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

let fixture mode type_name default ending =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-default-authority.hc"
      ~contents:
        (Printf.sprintf
           "%s F(%s n=%s){return n;} I64 Unused(I64 x=7){return x;} %s"
           type_name type_name default ending)
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
  let unit_ =
    Unit.compile_source_output ~source_command ~max_initializer_steps:100
      session ~config output
    |> diagnostics
  in
  (unit_.value, prepared, Preparation.completions preparation)

let seal unit_ prepared completions =
  Proof.create ~globals:(Unit.globals unit_)
    ~runtime_calls:(Unit.runtime_calls unit_)
    ~initialization:(Unit.initialization unit_)
    ~entry:(Unit.entry unit_) ~functions:(Unit.functions unit_) ~prepared
    ~completions

let compile ?parameter_defaults unit_ =
  Image.compile_callable ?parameter_defaults ~max_ir_instructions:4096
    ~max_code_bytes:65536 ~runtime_calls:(Unit.runtime_calls unit_)
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
                        |> Default_program.authority
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
          ("U64", "0xffffffffffffffff", -1L, -1L);
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let () =
  Alcotest.run "native default authority"
    [
      ( "ownership",
        [
          Alcotest.test_case
            "exact original defaults bind the entire native bundle" `Quick
            proof_ownership;
        ] );
    ]
