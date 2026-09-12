open Holyc_lib
module A = Holyc_lib__Sema.Source_activation
module C = Semantic_declaration_collection

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let reject label value =
  Alcotest.(check bool) label true (Result.is_error value)

type fixture = {
  session : Session.t;
  namespace : C.namespace;
  calls : A.call_journal;
  mutable events_rev : A.event list;
  mutable count : int;
  mutable context : Parser.command_context option;
  mutable starts : Parser.call_start list;
  mutable emissions : Parser.completed_call list;
  mutable capture : bool;
}

let fixture () =
  let session = Session.create () in
  let namespace =
    C.create_namespace ~table:(Session.semantic_symbols session) () |> checked
  in
  ignore
    (Symbol_visibility.Environment.add (Session.symbols session) ~name:"F"
       ~kind:Symbol_visibility.Function ());
  {
    session;
    namespace;
    calls = A.create_call_journal ~namespace ();
    events_rev = [];
    count = 0;
    context = None;
    starts = [];
    emissions = [];
    capture = true;
  }

let append state event = state.events_rev <- event :: state.events_rev

let activation state =
  A.create ~calls:state.calls ~namespace:state.namespace
    ~context:(Option.get state.context) ~observed_events:state.count
    (List.rev state.events_rev)

let parse ?(on_start = fun _ _ -> ()) ?(on_emit = fun _ _ -> ())
    ?(on_complete = fun _ -> ()) ?(on_enter = fun _ -> ())
    ?(on_reference = fun _ _ -> ()) ?(on_declaration = fun _ _ -> ()) state
    source =
  let commands : Parser.command_sink =
    {
      checkpoint =
        Some
          (fun event ->
            state.count <- state.count + 1;
            append state (A.Command event);
            (match event with
            | Parser.Sequence_started context -> state.context <- Some context
            | Parser.Command_completed _ -> on_complete state
            | _ -> ());
            Ok ());
      reference =
        Some
          (fun receipt ->
            append state (A.Reference receipt);
            on_reference state receipt;
            Ok ());
      call =
        Some
          {
            implicit = None;
            start =
              (fun receipt ->
                state.starts <- receipt :: state.starts;
                if state.capture then
                  append state
                    (A.capture_call_start state.calls
                       ~events_rev:state.events_rev receipt
                    |> checked);
                on_start state receipt;
                Ok None);
            emit =
              (fun receipt ->
                state.emissions <- receipt :: state.emissions;
                if state.capture then
                  append state
                    (A.capture_call_emission state.calls
                       ~events_rev:state.events_rev receipt
                    |> checked);
                on_emit state receipt;
                Ok ());
          };
      declaration =
        Some
          (fun event ->
            append state (A.Declaration event);
            on_declaration state event;
            Ok ());
      implicit_output = None;
      query = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session:state.session ~same_task:true ~commands
      ~on_enter:(fun () -> on_enter state)
      source
  in
  parsed

let current activation = function
  | A.Call_start receipt -> A.call_start activation receipt
  | A.Call_emission receipt -> A.call_emission activation receipt
  | _ -> false

let replay_identity () =
  let state = fixture () and replayed = ref [] and saved = ref None in
  let on_complete state =
    let journal = activation state |> checked in
    saved := Some journal;
    Alcotest.(check bool)
      "unused journal is available" true (A.available journal);
    List.iter
      (fun event ->
        Alcotest.(check bool)
          "inactive event has no authority" false
          (current (Some journal) event))
      state.events_rev;
    A.run journal ~invalid:"inactive journal" (fun event ->
        (match event with
        | A.Call_start receipt ->
            Alcotest.(check bool)
              "only current original start replays" true
              (A.call_start (Some journal) receipt);
            Alcotest.(check bool)
              "original live callback has ended" false
              (Parser.call_start_is_current receipt);
            reject "replay cannot recapture a start"
              (A.capture_call_start state.calls ~events_rev:state.events_rev
                 receipt);
            replayed := "start" :: !replayed
        | A.Call_emission receipt ->
            Alcotest.(check bool)
              "only current original emission replays" true
              (A.call_emission (Some journal) receipt);
            Alcotest.(check bool)
              "start is not active during emission" false
              (A.call_start (Some journal) receipt.call_start);
            Alcotest.(check bool)
              "original emission callback has ended" false
              (Parser.call_emission_is_current receipt);
            replayed := "emit" :: !replayed
        | _ -> ());
        List.iter
          (fun original ->
            if original != event then
              Alcotest.(check bool)
                "another event has no replay authority" false
                (current (Some journal) original))
          state.events_rev;
        Ok ())
    |> checked;
    reject "journal cannot replay twice"
      (A.run journal ~invalid:"consumed" (fun _ -> Ok ()));
    reject "call captures cannot create a second activation" (activation state)
  in
  ignore (parse ~on_complete state "F(F());" |> Test_parser.expect_ast);
  Alcotest.(check (list string))
    "nested calls preserve original phase order"
    [ "start"; "start"; "emit"; "emit" ]
    (List.rev !replayed);
  List.iter
    (fun event ->
      Alcotest.(check bool)
        "finished events have no authority" false (current !saved event))
    state.events_rev

let altered_journal () =
  let state = fixture () in
  let on_complete state =
    let context = Option.get state.context in
    let create ?(namespace = state.namespace) ?(calls = state.calls) events =
      A.create ~calls ~namespace ~context ~observed_events:state.count events
    in
    let events = List.rev state.events_rev in
    let calls =
      List.filter
        (function
          | A.Call_start _ | A.Call_emission _ -> true
          | _ -> false)
        events
    in
    reject "raw receipts lack live capture authority"
      (A.create ~namespace:state.namespace ~context ~observed_events:state.count
         events);
    reject "captured emission cannot be omitted"
      (create
         (List.filter
            (fun event ->
              match event with
              | A.Call_emission _ -> false
              | _ -> true)
            events));
    reject "captured event cannot repeat" (create (events @ [ List.hd calls ]));
    let first = List.nth calls 0 and second = List.nth calls 1 in
    reject "captured calls cannot swap"
      (create
         (List.map
            (fun event ->
              if event == first then second
              else if event == second then first
              else event)
            events));
    let namespace =
      C.create_namespace ~table:(Session.semantic_symbols state.session) ()
      |> checked
    in
    reject "foreign namespace cannot adopt captures" (create ~namespace events);
    let foreign = A.create_call_journal ~namespace:state.namespace () in
    reject "another journal cannot lend capture authority"
      (create ~calls:foreign events);
    ignore (create events |> checked)
  in
  ignore (parse ~on_complete state "F(F());" |> Test_parser.expect_ast)

let live_rejection () =
  let state = fixture () in
  let on_start state receipt =
    reject "the same live start cannot be captured twice"
      (A.capture_call_start state.calls ~events_rev:state.events_rev receipt)
  in
  let on_emit state receipt =
    reject "the same live emission cannot be captured twice"
      (A.capture_call_emission state.calls ~events_rev:state.events_rev receipt);
    let orphan = A.create_call_journal ~namespace:state.namespace () in
    reject "live emission requires its registered original start"
      (A.capture_call_emission orphan ~events_rev:state.events_rev receipt)
  in
  ignore (parse ~on_start ~on_emit state "F();" |> Test_parser.expect_ast);
  reject "start cannot be captured after parsing"
    (A.capture_call_start state.calls ~events_rev:state.events_rev
       (List.hd state.starts));
  reject "emission cannot be captured after parsing"
    (A.capture_call_emission state.calls ~events_rev:state.events_rev
       (List.hd state.emissions));
  reject "expired parser context cannot activate" (activation state)

let foreign_context () =
  let outer = fixture () in
  let on_start state _ =
    let inner = fixture () in
    let on_start _ receipt =
      Alcotest.(check bool)
        "foreign receipt is actually live" true
        (Parser.call_start_is_current receipt);
      reject "foreign live context cannot enter the outer journal"
        (A.capture_call_start state.calls ~events_rev:state.events_rev receipt)
    in
    ignore (parse ~on_start inner "F();" |> Test_parser.expect_ast)
  in
  ignore (parse ~on_start outer "F();" |> Test_parser.expect_ast)

let original_reference_and_prefix () =
  let state = fixture () in
  state.capture <- false;
  let without_references events =
    List.filter
      (function
        | A.Reference _ -> false
        | _ -> true)
      events
  in
  let on_start state receipt =
    reject "start requires its original registered reference"
      (A.capture_call_start state.calls
         ~events_rev:(without_references state.events_rev)
         receipt);
    append state
      (A.capture_call_start state.calls ~events_rev:state.events_rev receipt
      |> checked)
  in
  let on_emit state receipt =
    reject "later capture cannot replace the previously registered prefix"
      (A.capture_call_emission state.calls
         ~events_rev:(without_references state.events_rev)
         receipt);
    append state
      (A.capture_call_emission state.calls ~events_rev:state.events_rev receipt
      |> checked)
  in
  let on_complete state = ignore (activation state |> checked) in
  ignore
    (parse ~on_start ~on_emit ~on_complete state "F();"
    |> Test_parser.expect_ast)

let nested_phase target () =
  let state = fixture () and entries = ref 0 and replayed = ref 0 in
  let on_enter state =
    incr entries;
    if !entries = target then (
      let journal = activation state |> checked in
      A.run journal ~invalid:"nested activation expired" (fun event ->
          (match event with
          | A.Call_start receipt ->
              Alcotest.(check bool)
                "nested entry replays original start" true
                (A.call_start (Some journal) receipt);
              incr replayed
          | A.Call_emission _ ->
              Alcotest.fail "post-close lookahead still precedes emission"
          | _ -> ());
          Ok ())
      |> checked;
      state.capture <- false)
  in
  ignore
    (parse ~on_enter state "F#exe {}(#exe {}7)#exe {};"
    |> Test_parser.expect_ast);
  Alcotest.(check int)
    "all original nested directive phases occurred" 3 !entries;
  Alcotest.(check int)
    "only starts already read are replayed"
    (if target = 1 then 0 else 1)
    !replayed

let capture_advance_expires () =
  let state = fixture () and saved = ref None in
  let on_start state _ = saved := Some (activation state |> checked) in
  let on_emit state _ =
    let previous = Option.get !saved in
    Alcotest.(check bool)
      "new emission expires an earlier same-command snapshot" false
      (A.available previous);
    reject "advanced snapshot cannot replay"
      (A.run previous ~invalid:"advanced" (fun _ -> Ok ()));
    ignore (activation state |> checked)
  in
  ignore (parse ~on_start ~on_emit state "F();" |> Test_parser.expect_ast)

exception Replay_failure

let revoked exceptional () =
  let state = fixture () in
  let on_complete state =
    let journal = activation state |> checked in
    let run () =
      A.run journal ~invalid:"revoked" (fun event ->
          match event with
          | A.Call_start _ ->
              if exceptional then raise Replay_failure else Error "rejected"
          | _ -> Ok ())
    in
    if exceptional then
      match run () with
      | _ -> Alcotest.fail "exception must escape"
      | exception Replay_failure -> ()
    else reject "callback rejection revokes replay" (run ());
    List.iter
      (fun event ->
        Alcotest.(check bool)
          "failure releases all active authority" false
          (current (Some journal) event))
      state.events_rev;
    reject "failed activation cannot rerun"
      (A.run journal ~invalid:"revoked" (fun _ -> Ok ()));
    reject "failed activation cannot recreate authority" (activation state)
  in
  ignore (parse ~on_complete state "F();" |> Test_parser.expect_ast)

let parser_failure exceptional () =
  let state = fixture () and saved = ref None in
  let on_start state _ =
    saved := Some (activation state |> checked);
    if exceptional then raise Replay_failure
  in
  if exceptional then
    match parse ~on_start state "F();" with
    | _ -> Alcotest.fail "parser exception must escape"
    | exception Replay_failure -> ()
  else
    Alcotest.(check bool)
      "malformed call rejects parser" true
      (Parser.has_errors (parse ~on_start state "F(1:);"));
  let journal = Option.get !saved in
  Alcotest.(check bool)
    "parser failure expires captured journal" false (A.available journal);
  reject "failed parser cannot replay captured start"
    (A.run journal ~invalid:"expired" (fun _ -> Ok ()))

let single_original_capture () =
  let state = fixture () in
  state.capture <- false;
  let second = A.create_call_journal ~namespace:state.namespace () in
  let on_start state receipt =
    let prefix = state.events_rev in
    reject "failed preflight cannot claim original receipt"
      (A.capture_call_start state.calls ~events_rev:[] receipt);
    let captured =
      A.capture_call_start state.calls ~events_rev:prefix receipt |> checked
    in
    reject "another live journal cannot capture the same original start"
      (A.capture_call_start second ~events_rev:prefix receipt);
    append state captured
  in
  let on_emit state receipt =
    let prefix = state.events_rev in
    reject "failed emission preflight leaves original authority available"
      (A.capture_call_emission state.calls ~events_rev:[] receipt);
    let captured =
      A.capture_call_emission state.calls ~events_rev:prefix receipt |> checked
    in
    reject "another live journal cannot capture original emission"
      (A.capture_call_emission second ~events_rev:prefix receipt);
    append state captured
  in
  let on_complete state =
    A.run (activation state |> checked) ~invalid:"expired" (fun _ -> Ok ())
    |> checked
  in
  ignore
    (parse ~on_start ~on_emit ~on_complete state "F();"
    |> Test_parser.expect_ast)

let reference_named name = function
  | A.Reference receipt ->
      (Parser.selected_identifier receipt).Ast.spelling = name
  | _ -> false

let altered_reference_variants events =
  let x = List.find (reference_named "x") events in
  let y = List.find (reference_named "y") events in
  [
    List.filter (fun event -> event != x) events;
    x :: events;
    List.map
      (fun event -> if event == x then y else if event == y then x else event)
      events;
  ]

let original_argument_events () =
  let state = fixture () in
  state.capture <- false;
  let on_start state receipt =
    append state
      (A.capture_call_start state.calls ~events_rev:state.events_rev receipt
      |> checked)
  in
  let on_emit state receipt =
    List.iter
      (fun events_rev ->
        reject "argument references cannot be omitted duplicated or reordered"
          (A.capture_call_emission state.calls ~events_rev receipt))
      (altered_reference_variants state.events_rev);
    append state
      (A.capture_call_emission state.calls ~events_rev:state.events_rev receipt
      |> checked)
  in
  let on_complete state = ignore (activation state |> checked) in
  ignore
    (parse ~on_start ~on_emit ~on_complete state "F(x+y);"
    |> Test_parser.expect_ast)

let original_declaration_prefix () =
  let state = fixture () in
  state.capture <- false;
  let on_start state receipt =
    let declarations =
      List.filter
        (function
          | A.Declaration _ -> true
          | _ -> false)
        state.events_rev
    in
    let first = List.hd declarations and second = List.nth declarations 1 in
    List.iter
      (fun events_rev ->
        reject
          "original declaration prefix cannot be omitted duplicated or \
           reordered"
          (A.capture_call_start state.calls ~events_rev receipt))
      [
        List.filter (fun event -> event != first) state.events_rev;
        first :: state.events_rev;
        List.map
          (fun event ->
            if event == first then second
            else if event == second then first
            else event)
          state.events_rev;
      ];
    append state
      (A.capture_call_start state.calls ~events_rev:state.events_rev receipt
      |> checked)
  in
  let on_emit state receipt =
    append state
      (A.capture_call_emission state.calls ~events_rev:state.events_rev receipt
      |> checked)
  in
  ignore
    (parse ~on_start ~on_emit state "I64 x,y;F();" |> Test_parser.expect_ast)

let original_after_call_events () =
  let state = fixture () in
  let on_complete state =
    if List.exists (reference_named "y") state.events_rev then (
      List.iter
        (fun events_rev ->
          reject "activation cannot alter non-call events after last capture"
            (A.create ~calls:state.calls ~namespace:state.namespace
               ~context:(Option.get state.context) ~observed_events:state.count
               (List.rev events_rev)))
        (altered_reference_variants state.events_rev);
      ignore (activation state |> checked))
  in
  ignore (parse ~on_complete state "F();(x+y);" |> Test_parser.expect_ast)

let reference_advance_expires () =
  let state = fixture () and saved = ref None in
  let on_start state _ = saved := Some (activation state |> checked) in
  let on_reference _ receipt =
    if (Parser.selected_identifier receipt).Ast.spelling = "x" then (
      let journal = Option.get !saved in
      Alcotest.(check bool)
        "argument reference expires earlier original trace" false
        (A.available journal);
      reject "earlier trace cannot run after argument observation"
        (A.run journal ~invalid:"advanced" (fun _ -> Ok ())))
  in
  ignore (parse ~on_start ~on_reference state "F(x);" |> Test_parser.expect_ast)

let function_phase_revocation raises () =
  let state = fixture () and saved = ref None and original = ref [] in
  let later = ref false in
  let check_denied label journal =
    List.iter
      (fun event ->
        Alcotest.(check bool)
          label false
          (A.function_phase_admission (Some journal) event))
      !original
  in
  let on_complete state =
    if Option.is_none !saved then (
      original :=
        List.filter_map
          (function
            | A.Declaration event
              when Test_function_record_phase.admission_phase event ->
                Some event
            | _ -> None)
          (List.rev state.events_rev);
      Alcotest.(check int)
        "all original function phases journaled" 7 (List.length !original);
      let journal = activation state |> checked in
      saved := Some journal;
      check_denied "journaled phase is denied before replay" journal;
      let visit event =
        List.iter
          (fun candidate ->
            let expected =
              match event with
              | A.Declaration active -> active == candidate
              | _ -> false
            in
            Alcotest.(check bool)
              "only exact active phase admits" expected
              (A.function_phase_admission (Some journal) candidate))
          !original;
        match event with
        | A.Declaration (Parser.Function_header_completed _) ->
            if raises then raise Exit else Error "rejected header"
        | _ -> Ok ()
      in
      if raises then
        try
          ignore (A.run journal ~invalid:"inactive" visit);
          Alcotest.fail "expected replay exception"
        with Exit -> ()
      else
        reject "rejected header aborts replay"
          (A.run journal ~invalid:"inactive" visit);
      check_denied "failed replay permanently denies original phases" journal;
      reject "revoked phase journal cannot replay"
        (A.run journal ~invalid:"consumed" (fun _ -> Ok ())))
  in
  let on_declaration _ event =
    if Test_function_record_phase.admission_phase event then (
      Alcotest.(check bool)
        "unjournaled live phase remains eligible" true
        (A.function_phase_admission !saved event);
      if Option.is_some !saved then later := true)
  in
  ignore
    (parse ~on_complete ~on_declaration state
       "extern I64 G(I64 n=40,...);extern I64 H();"
    |> Test_parser.expect_ast);
  check_denied "later parsing cannot revive original phases" (Option.get !saved);
  Alcotest.(check bool) "future unjournaled phases exercised" true !later

let tests =
  [
    Alcotest.test_case "original nested calls replay once with exact authority"
      `Quick replay_identity;
    Alcotest.test_case "failed function-phase replay revokes admission" `Quick
      (function_phase_revocation false);
    Alcotest.test_case "exceptional function-phase replay revokes admission"
      `Quick
      (function_phase_revocation true);
    Alcotest.test_case "altered and foreign call journals are rejected" `Quick
      altered_journal;
    Alcotest.test_case "capture requires original live unrepeated phases" `Quick
      live_rejection;
    Alcotest.test_case "live foreign parser context cannot capture" `Quick
      foreign_context;
    Alcotest.test_case "live capture preserves original reference and prefix"
      `Quick original_reference_and_prefix;
    Alcotest.test_case "after-name nested activation precedes call start" `Quick
      (nested_phase 1);
    Alcotest.test_case "inside-call nested activation replays start only" `Quick
      (nested_phase 2);
    Alcotest.test_case "post-close nested activation precedes emission" `Quick
      (nested_phase 3);
    Alcotest.test_case "call advancement expires earlier activation snapshot"
      `Quick capture_advance_expires;
    Alcotest.test_case "callback rejection permanently revokes call replay"
      `Quick (revoked false);
    Alcotest.test_case "callback exception permanently revokes call replay"
      `Quick (revoked true);
    Alcotest.test_case "parser failure expires captured call start" `Quick
      (parser_failure false);
    Alcotest.test_case "parser exception expires captured call start" `Quick
      (parser_failure true);
    Alcotest.test_case
      "original call receipts have single journal capture authority" `Quick
      single_original_capture;
    Alcotest.test_case "argument observations preserve exact original order"
      `Quick original_argument_events;
    Alcotest.test_case "declaration prefix preserves exact original order"
      `Quick original_declaration_prefix;
    Alcotest.test_case "activation preserves non-call suffix after last capture"
      `Quick original_after_call_events;
    Alcotest.test_case "non-call observation expires earlier activation" `Quick
      reference_advance_expires;
  ]
