open Holyc_lib
module P = Preprocessor
module D = Task_declarations

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics = function
  | Ok value -> value
  | Error items ->
      Alcotest.fail
        (String.concat "; " (List.map (fun d -> d.Diagnostic.message) items))

let check = Alcotest.(check bool)
let word lookup = (P.lexical_lookup_token lookup).Token.raw

let source session text =
  Session.add_source session ~path:"lexical-consumers.hc" ~contents:text

let config mode = P.Config.create ~compilation_mode:mode () |> checked

let sink ?lexical_lookup ?checkpoint () : Parser.command_sink =
  {
    lexical_lookup;
    checkpoint;
    reference = None;
    call = None;
    implicit_output = None;
    query = None;
    declaration = None;
    dimension_count = None;
    command = (fun _ -> Ok ());
    resume = (fun () -> Ok ());
  }

let parse ?execute_stream ?lexical_lookup session input commands mode =
  Parser.parse ~commands ?execute_stream ?lexical_lookup
    ~sources:(Session.sources session) ~symbols:(Session.symbols session)
    ~definitions:(Session.definitions session)
    ~config:(config mode) input

let success output =
  if output.Parser.diagnostics <> [] then
    Alcotest.fail
      (String.concat "; "
         (List.map (fun d -> d.Diagnostic.message) output.diagnostics));
  check "accepted AST" true (Option.is_some output.ast)

let before_first_and_inspection () =
  let session = Session.create () in
  let input = source session "I64 Alpha; I64 Beta;" in
  let order = ref [] and retained = ref [] in
  let consume context lookup =
    check "focused original read" true
      (Parser.lexical_lookup_is_current context lookup);
    if !retained = [] then
      check "read does not advance lifecycle cursor" true
        (Parser.context_is_current context ~observed_events:1);
    retained := (context, lookup) :: !retained;
    order := ("consume:" ^ word lookup) :: !order;
    Ok ()
  in
  let inspect lookup =
    check "inspection retains original receipt" true
      (P.lexical_lookup_is_current lookup);
    order := ("inspect:" ^ word lookup) :: !order
  in
  parse session input
    (sink ~lexical_lookup:consume ())
    P.Jit ~lexical_lookup:inspect
  |> success;
  Alcotest.(check (list string))
    "same read, consumer before inspector"
    [
      "consume:I64";
      "inspect:I64";
      "consume:Alpha";
      "inspect:Alpha";
      "consume:I64";
      "inspect:I64";
      "consume:Beta";
      "inspect:Beta";
    ]
    (List.rev !order);
  List.iter
    (fun (context, lookup) ->
      check "expired context/receipt" false
        (Parser.lexical_lookup_is_current context lookup))
    !retained

let directive_routing child_enabled =
  let session = Session.create () in
  let child =
    Symbol_visibility.Environment.task_view (Session.symbols session)
  in
  let child_definitions =
    Definition.Environment.task_view (Session.definitions session)
  in
  let input = source session "I64 Alpha; #exe {I64 Beta;} I64 Gamma;" in
  let root_words = ref []
  and child_words = ref []
  and all_words = ref []
  and parent = ref None in
  let root context lookup =
    parent := Some context;
    check "root environment" true
      (P.lexical_lookup_environment lookup == Session.symbols session);
    root_words := word lookup :: !root_words;
    Ok ()
  in
  let nested context lookup =
    check "child focus" true (Parser.lexical_lookup_is_current context lookup);
    check "child environment" true (P.lexical_lookup_environment lookup == child);
    check "child mode" true (P.lexical_lookup_mode lookup = P.Jit);
    check "parent suspended" false
      (Parser.lexical_lookup_is_current (Option.get !parent) lookup);
    child_words := word lookup :: !child_words;
    Ok ()
  in
  let enter _ =
    Ok
      {
        Parser.definitions = child_definitions;
        symbols = child;
        commands =
          sink ?lexical_lookup:(if child_enabled then Some nested else None) ();
        finish = (fun () -> Ok "");
        abort = (fun () -> ());
      }
  in
  parse session input (sink ~lexical_lookup:root ()) P.Aot ~execute_stream:enter
    ~lexical_lookup:(fun lookup -> all_words := word lookup :: !all_words)
  |> success;
  Alcotest.(check (list string))
    "root restored, child masked"
    [ "I64"; "Alpha"; "exe"; "I64"; "Gamma" ]
    (List.rev !root_words);
  Alcotest.(check (list string))
    "child dispatch"
    (if child_enabled then [ "I64"; "Beta" ] else [])
    (List.rev !child_words);
  Alcotest.(check (list string))
    "inspection sees full input"
    [ "I64"; "Alpha"; "exe"; "I64"; "Beta"; "I64"; "Gamma" ]
    (List.rev !all_words)

let ledger_and_rejection () =
  let session = Session.create () in
  let input = source session "I64 Alpha; I64 Beta;" in
  let ledger = D.create_source session ~source:input |> checked in
  let foreign = D.create_source session ~source:input |> checked in
  let retained = ref None in
  let consume context lookup =
    D.observe_lexical_lookup ledger context lookup |> diagnostics;
    check "duplicate rejected" true
      (Result.is_error (D.observe_lexical_lookup ledger context lookup));
    check "foreign ledger rejected" true
      (Result.is_error (D.observe_lexical_lookup foreign context lookup));
    let cross_domain =
      Domain.spawn (fun () ->
          Result.is_error (D.observe_lexical_lookup ledger context lookup))
    in
    check "other domain rejected" true (Domain.join cross_domain);
    retained := Some (context, lookup);
    Ok ()
  in
  parse session input
    (sink ~lexical_lookup:consume ~checkpoint:(D.observe_command ledger) ())
    P.Jit
  |> success;
  Alcotest.(check int)
    "one mutation per original read" 4
    (D.lexical_read_count ledger);
  let context, lookup = Option.get !retained in
  check "expired read rejected" true
    (Result.is_error (D.observe_lexical_lookup ledger context lookup));
  Alcotest.(check int)
    "rejections preserve frontier" 4
    (D.lexical_read_count ledger)

let error_and_exception_restore () =
  let session = Session.create () in
  let input = source session "I64 Alpha; I64 Beta; I64 Gamma;" in
  let seen = ref [] and aborted = ref false and retained = ref None in
  let consume context lookup =
    seen := word lookup :: !seen;
    retained := Some (context, lookup);
    if word lookup = "Beta" then
      Error
        [
          Diagnostic.make ~code:"TESTLEX" ~severity:Diagnostic.Error
            ~primary:(P.lexical_lookup_token lookup).Token.span
            ~message:"original consumer failure" ();
        ]
    else Ok ()
  in
  let checkpoint = function
    | Parser.Sequence_aborted _ ->
        aborted := true;
        Ok ()
    | _ -> Ok ()
  in
  let output =
    parse session input (sink ~lexical_lookup:consume ~checkpoint ()) P.Jit
  in
  check "error aborts sequence" true !aborted;
  check "reached diagnostic preserved" true
    (List.exists (fun d -> d.Diagnostic.code = "TESTLEX") output.diagnostics);
  Alcotest.(check (list string))
    "no later read"
    [ "I64"; "Alpha"; "I64"; "Beta" ]
    (List.rev !seen);
  let context, lookup = Option.get !retained in
  check "failed receipt expires" false
    (Parser.lexical_lookup_is_current context lookup);
  let input = source session "I64 Delta;" in
  let raised =
    try
      ignore
        (parse session input
           (sink ~lexical_lookup:(fun _ _ -> failwith "lexical exception") ())
           P.Jit);
      false
    with Failure message when message = "lexical exception" -> true
  in
  check "exception propagates" true raised;
  parse session input (sink ()) P.Jit |> success

let tests =
  [
    Alcotest.test_case "first lookahead and original inspection order" `Quick
      before_first_and_inspection;
    Alcotest.test_case "directive selects child and restores root" `Quick
      (fun () -> directive_routing true);
    Alcotest.test_case "empty child service masks suspended root" `Quick
      (fun () -> directive_routing false);
    Alcotest.test_case "owned ledger, duplicate, expiry and domain checks"
      `Quick ledger_and_rejection;
    Alcotest.test_case "consumer error and exception release contexts" `Quick
      error_and_exception_restore;
  ]
