open Holyc_lib
module D = Task_declarations
module N = Semantic_function_record_phase

let checked = Test_declaration_collection.checked
let expect = Test_integer_program.checked

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let parse text =
  let session = Session.create () in
  let ledger = D.create session |> checked in
  let starts = ref [] and emissions = ref [] in
  let commands : Parser.command_sink =
    {
      checkpoint = Some (D.observe_command ledger);
      declaration = Some (D.observe ledger);
      reference = Some (D.observe_reference ledger);
      call =
        Some
          {
            implicit = None;
            start =
              (fun receipt ->
                let result = D.observe_call_start ledger receipt in
                starts := receipt :: !starts;
                reject "duplicate live start cannot replace captured shape"
                  (D.observe_call_start ledger receipt);
                result);
            emit =
              (fun receipt ->
                let result = D.observe_call_emission ledger receipt in
                emissions := receipt :: !emissions;
                reject "duplicate live emission cannot replace selected record"
                  (D.observe_call_emission ledger receipt);
                result);
          };
      implicit_output = None;
      query = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true ~commands
      ~configure:(fun _ execution -> { execution with Parser.commands })
      text
  in
  (ledger, parsed, List.rev !starts, List.rev !emissions)

let captured_count () =
  let ledger, parsed, _, emissions =
    parse "extern I64 F();F#exe {extern I64 F(I64 n);}(40);"
  in
  ignore (Test_parser.expect_ast parsed);
  let receipt = List.hd emissions in
  let start, emission = D.call_record_snapshots ledger receipt |> expect in
  Alcotest.(check (option int))
    "count captured after name lookahead" (Some 1)
    (N.argument_count (Option.get start));
  Alcotest.(check bool)
    "emission retains selected identity" true
    (N.same_identity (Option.get start) (Option.get emission));
  reject "expired start cannot replay"
    (D.observe_call_start ledger receipt.call_start);
  reject "expired emission cannot replay"
    (D.observe_call_emission ledger receipt)

let frozen_cursor () =
  let _, parsed, _, emissions =
    parse "extern I64 F();F(#exe {extern I64 F(I64 n);}40);"
  in
  Alcotest.(check bool)
    "surplus argument rejects captured zero count" true
    (Parser.has_errors parsed);
  Alcotest.(check int) "rejected call has no emission" 0 (List.length emissions)

let selected_identity () =
  List.iter
    (fun (declaration, expected_same) ->
      let ledger, parsed, _, emissions =
        parse (declaration ^ "F()#exe {I64 F(I64 n){return n;}};")
      in
      ignore (Test_parser.expect_ast parsed);
      let first, last =
        D.call_record_snapshots ledger (List.hd emissions) |> expect
      in
      let first = Option.get first and last = Option.get last in
      Alcotest.(check bool)
        "selected record survives post-close declarations" true
        (N.same_identity first last);
      Alcotest.(check (option int))
        "post-close shared count follows exact record"
        (Some (if expected_same then 1 else 0))
        (N.argument_count last);
      Alcotest.(check (option int))
        "traversal count stays immutable" (Some 0) (N.argument_count first))
    [ ("extern I64 F();", true); ("I64 F(){return 17;}", false) ]

let legacy_provider () =
  let module T = Test_task_declarations in
  let session, runtime, ledger = T.runtime_setup () in
  let prime =
    Session.add_source session ~path:"legacy-provider.hc"
      ~contents:"extern I64 F(I64 n);"
  in
  Parser.parse ~sources:(Session.sources session)
    ~symbols:(Session.symbols session)
    ~definitions:(Session.definitions session)
    ~config:(T.config ()) prime
  |> Test_parser.expect_ast |> ignore;
  let emissions = ref [] in
  let commands : Parser.command_sink =
    {
      checkpoint = Some (D.observe_command ledger);
      reference = Some (D.observe_execution_reference ledger);
      declaration =
        Some
          (fun event ->
            Result.bind (D.observe ledger event) (fun () ->
                match event with
                | Parser.Function_header_completed header ->
                    D.admit_function_header ledger ~runtime header
                | _ -> Ok ()));
      call =
        Some
          {
            implicit = None;
            start = D.observe_call_start ledger;
            emit =
              (fun receipt ->
                Result.map
                  (fun () -> emissions := receipt :: !emissions)
                  (D.observe_call_emission ledger receipt));
          };
      implicit_output = None;
      query = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let parse contents =
    let source =
      Session.add_source session ~path:"observed-provider.hc" ~contents
    in
    Parser.parse ~commands ~sources:(Session.sources session)
      ~symbols:(Session.symbols session)
      ~definitions:(Session.definitions session)
      ~config:(T.config ()) source
    |> Test_parser.expect_ast
  in
  let ast = parse "I64 F(I64 n){if(0)F(42);return n;}" in
  let declaration_command = D.seal ledger ast |> expect in
  let program =
    (compile_integer_task_ast ~task:runtime ~declaration_command session
       ~config:(T.config ()) ast
    |> expect)
      .value
  in
  T.execute_runtime_ok runtime program;
  D.observe_admission ledger (T.admission runtime program |> Option.get)
  |> checked;
  ignore (parse "F(42);");
  Alcotest.(check int)
    "completed source and runtime alias calls observed" 2
    (List.length !emissions);
  List.iter
    (fun receipt ->
      let arguments, emission =
        D.call_record_snapshots ledger receipt |> expect
      in
      Alcotest.(check bool)
        "legacy grammar never fabricates native call evidence" true
        (Option.is_none arguments && Option.is_none emission))
    !emissions

let activation_declaration_snapshots ?(abort = false) text () =
  let module P = Test_source_promotion in
  let module VM = Ir_integer_interpreter in
  let session, source, ledger = P.inputs text in
  let observed = ref [] and replayed = ref 0 in
  let publication = function
    | Parser.Function_declared p -> Some p
    | Parser.Function_parameter_declared p -> Some p.parameter_function
    | Parser.Function_parameter_completed p ->
        Some p.parameter_publication.parameter_function
    | Parser.Function_header_completed h -> Some h.function_publication
    | Parser.Parameter_default_completed p -> Some p.default_function
    | Parser.Function_variadic_started p | Parser.Function_variadic_completed p
      -> Some p.variadic_function
    | Parser.Function_body_completed (h, _) -> Some h.function_publication
    | _ -> None
  in
  let declaration event =
    Result.map
      (fun () ->
        Option.iter
          (fun publication ->
            let snapshot =
              D.function_record_snapshot ledger publication |> expect
            in
            observed := (event, publication, snapshot) :: !observed)
          (publication event))
      (D.observe ledger event)
  in
  let enter span =
    let runtime =
      VM.create_task_state ~table:(Session.semantic_symbols session) ()
      |> checked
    in
    D.promote_source_for_activation ledger ~runtime session ~source |> checked;
    let errors =
      [
        Diagnostic.make ~code:"TEST" ~severity:Diagnostic.Error ~primary:span
          ~message:"stop after declaration replay checks" ();
      ]
    in
    List.iter
      (fun (_, publication, _) ->
        reject "unstarted activation grants no native declaration phase"
          (D.function_record_snapshot ledger publication))
      !observed;
    let result =
      D.activate_source ledger ~runtime ~span
        ~command:(fun _ -> Ok ())
        ~declaration:(fun event ->
          (match
             List.find_opt (fun (original, _, _) -> original == event) !observed
           with
          | None -> ()
          | Some (_, publication, original) ->
              incr replayed;
              let actual =
                D.function_record_snapshot ledger publication |> expect
              in
              Alcotest.(check (option int))
                "activation retains the original native argument count"
                (N.argument_count original)
                (N.argument_count actual);
              Alcotest.(check bool)
                "activation retains the exact immutable declaration phase" true
                (actual == original);
              List.iter
                (fun (_, other, _) ->
                  if other != publication then
                    reject "another source cannot borrow the current phase"
                      (D.function_record_snapshot ledger other))
                !observed);
          if abort then Error errors else Ok ())
    in
    if abort then (
      reject "activation failure remains an error" result;
      List.iter
        (fun (_, publication, _) ->
          reject "failed activation cannot read a later native phase"
            (D.function_record_snapshot ledger publication))
        !observed)
    else ignore (result |> expect);
    Alcotest.(check int)
      "snapshot reads execute no instructions" 0
      (VM.task_progress runtime).executed_steps;
    Error errors
  in
  let parsed =
    P.parse ~declaration ~execute_stream:enter session source ledger
  in
  Alcotest.(check bool)
    "controlled stop after replay" true (Parser.has_errors parsed);
  if not abort then
    Alcotest.(check int)
      "every original native declaration phase replays" (List.length !observed)
      !replayed

let tests =
  [
    Alcotest.test_case "task call count follows name lookahead" `Quick
      captured_count;
    Alcotest.test_case "task call cursor precedes opening lookahead" `Quick
      frozen_cursor;
    Alcotest.test_case "task call emission preserves native selected record"
      `Quick selected_identity;
    Alcotest.test_case
      "legacy providers retain completed grammar without native evidence" `Quick
      legacy_provider;
    Alcotest.test_case "activation preserves original native declaration phases"
      `Quick
      (activation_declaration_snapshots
         "extern I64 F(I64 n=40,...);extern I64 G();#exe {}");
    Alcotest.test_case "activation retains phases across native record reuse"
      `Quick
      (activation_declaration_snapshots
         "extern I64 F(I64 n);extern I64 F();#exe {}");
    Alcotest.test_case "failed activation revokes native declaration reads"
      `Quick
      (activation_declaration_snapshots ~abort:true
         "extern I64 F(I64 n);#exe {}");
  ]
