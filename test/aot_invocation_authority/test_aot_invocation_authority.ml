open Holyc_lib
module Parser = Holyc_lib__Frontend.Parser
module Symbols = Holyc_lib__Sema.Symbol_table
module Collection = Holyc_lib__Sema.Declaration_collection
module Invocation = Holyc_lib__Sema.Aot_invocation
module VM = Holyc_lib__Ir.Integer_interpreter
module D = Holyc_lib__Driver.Task_declarations
module Preparation = Holyc_lib__Driver.Native_default_preparation
module Unit = Holyc_lib__Driver.Integer_unit
module Initializers = Holyc_lib__Driver.Integer_initializers
module Task = Holyc_lib__Driver.Integer_task

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let reject label = function
  | Ok _ -> Alcotest.fail label
  | Error _ -> ()

let run ~consume ~foreign_namespace ~foreign_task =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"aot-invocation.hc" ~contents:"I64 value;"
  in
  let table = Session.semantic_symbols session in
  let namespace = Collection.create_namespace ~table () |> checked in
  let bound_namespace =
    if foreign_namespace then Collection.create_namespace ~table () |> checked
    else namespace
  in
  let fork, child = Symbols.fork_empty ~root_name:"session-task" table in
  let _, wrong_child = Symbols.fork_empty ~root_name:"session-task" table in
  let owner = VM.create_task_state ~table () |> checked in
  VM.bind_task_namespace owner bound_namespace |> checked;
  let other_owner = VM.create_task_state ~table () |> checked in
  VM.bind_task_namespace other_owner namespace |> checked;
  let saved = ref None in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint =
        Some
          (fun event ->
            (match event with
            | Parser.Sequence_started context ->
                if foreign_namespace then
                  reject "foreign namespace cannot begin"
                    (VM.begin_aot_invocation owner ~context
                       ~events_rev:[ event ] ~namespace ~fork)
                else
                  let proof =
                    VM.begin_aot_invocation owner ~context ~events_rev:[ event ]
                      ~namespace ~fork
                    |> checked
                  in
                  saved := Some proof;
                  reject "root start cannot mint twice"
                    (VM.begin_aot_invocation owner ~context
                       ~events_rev:[ event ] ~namespace ~fork);
                  if consume then (
                    reject "unreserved peer"
                      (VM.create_aot_peer_state owner ~invocation:proof);
                    reject "wrong fork child"
                      (Invocation.claim_peer proof ~parent:table
                         ~child:wrong_child);
                    Invocation.claim_peer proof ~parent:table ~child |> checked;
                    if foreign_task then (
                      reject "another task with the same namespace cannot pair"
                        (VM.create_aot_peer_state other_owner ~invocation:proof);
                      let peer =
                        VM.create_aot_peer_state owner ~invocation:proof
                        |> checked
                      in
                      Alcotest.(check bool)
                        "foreign rejection remains retryable" true
                        (VM.task_shares_resources owner peer))
                    else
                      let peer =
                        VM.create_aot_peer_state owner ~invocation:proof
                        |> checked
                      in
                      Alcotest.(check bool)
                        "same resources" true
                        (VM.task_shares_resources owner peer);
                      Alcotest.(check bool)
                        "original tasks share source invocation" true
                        (VM.task_same_source_invocation owner peer);
                      Alcotest.(check bool)
                        "foreign task is not source peer" false
                        (VM.task_same_source_invocation peer other_owner);
                      reject "peer cannot be consumed twice"
                        (VM.create_aot_peer_state owner ~invocation:proof))
            | _ -> ());
            Ok ());
      call = None;
      implicit_output = None;
      reference = None;
      query = None;
      dimension_count = None;
      declaration = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let config = Preprocessor.Config.create ~compilation_mode:Aot () |> checked in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  Alcotest.(check bool) "parse" false (Parser.has_errors parsed);
  if not consume then
    reject "expired invocation cannot reserve a peer"
      (Invocation.claim_peer (Option.get !saved) ~parent:table ~child)

let reject_reused_source ~reuse_owner () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"reuse.hc" ~contents:"I64 value;"
  in
  let table = Session.semantic_symbols session in
  let namespace = Collection.create_namespace ~table () |> checked in
  let fork, child = Symbols.fork_empty table in
  let other_fork, _ = Symbols.fork_empty table in
  let owner = VM.create_task_state ~table () |> checked in
  VM.bind_task_namespace owner namespace |> checked;
  let attempt first =
    let commands : Parser.command_sink =
      {
        lexical_lookup = None;
        checkpoint =
          Some
            (fun event ->
              (match event with
              | Parser.Sequence_started context ->
                  let chosen =
                    if first || not reuse_owner then fork else other_fork
                  in
                  let result =
                    VM.begin_aot_invocation owner ~context ~events_rev:[ event ]
                      ~namespace ~fork:chosen
                  in
                  if first then (
                    let proof = checked result in
                    if reuse_owner then (
                      Invocation.claim_peer proof ~parent:table ~child
                      |> checked;
                      ignore
                        (VM.create_aot_peer_state owner ~invocation:proof
                        |> checked)))
                  else
                    reject
                      "claimed fork or paired owner cannot serve another root"
                      result
              | _ -> ());
              Ok ());
        call = None;
        implicit_output = None;
        reference = None;
        query = None;
        dimension_count = None;
        declaration = None;
        command = (fun _ -> Ok ());
        resume = (fun () -> Ok ());
      }
    in
    let config =
      Preprocessor.Config.create ~compilation_mode:Aot () |> checked
    in
    let parsed =
      Parser.parse ~commands ~sources:(Session.sources session)
        ~definitions:(Session.definitions session)
        ~symbols:(Session.symbols session) ~config source
    in
    Alcotest.(check bool) "parse" false (Parser.has_errors parsed)
  in
  attempt true;
  attempt false

let diagnostics = function
  | Ok value -> value
  | Error errors ->
      Alcotest.fail
        (String.concat "; "
           (List.map
              (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
              errors))

let source_output_authority ~paired () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"unpaired-authority.hc"
      ~contents:"I64 n=42;U8 s[3]=\"AB\";n;"
  in
  let ledger = D.create_source session ~source |> checked in
  let preparation =
    Preparation.create ~compilation_mode:Preprocessor.Aot
      ~max_initializer_steps:100 session
    |> checked
  in
  let other =
    Preparation.create ~compilation_mode:Preprocessor.Aot
      ~max_initializer_steps:100 session
    |> checked
  in
  let forked = Session.fork_frontend session in
  let fork = Option.get (Session.semantic_fork forked) in
  let sequence = ref None in
  let peer = ref None in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint =
        Some
          (fun event ->
            Result.bind (D.observe_command ledger event) (fun () ->
                match event with
                | Parser.Sequence_started context ->
                    let invocation =
                      Preparation.begin_aot_invocation preparation
                        ~parent:session ~ledger ~context ~fork
                      |> diagnostics
                    in
                    if paired then (
                      D.reserve_aot_peer ledger invocation |> diagnostics;
                      let actual_peer =
                        Preparation.create_aot_peer preparation ~parent:session
                          ~child:forked ~invocation
                        |> checked
                      in
                      let producer = Preparation.task_state preparation in
                      reject "source producer is not a consumer"
                        (Task.create_aot_peer ~state:producer session);
                      let copied_peer =
                        Obj.obj (Obj.dup (Obj.repr actual_peer))
                      in
                      reject "copied peer cannot claim driver"
                        (Task.create_aot_peer ~state:copied_peer forked);
                      ignore
                        (Task.create_aot_peer ~state:actual_peer forked
                        |> checked);
                      reject "original peer driver is one-shot"
                        (Task.create_aot_peer ~state:actual_peer forked);
                      peer := Some actual_peer);
                    Ok ()
                | Parser.Sequence_completed done_ ->
                    sequence := Some done_;
                    Ok ()
                | _ -> Ok ()));
      query = Some (D.observe_query ledger);
      reference = Some (D.observe_reference ledger);
      implicit_output = None;
      call = None;
      declaration =
        Some
          (fun event ->
            Result.bind (D.observe ~isolated_initializers:true ledger event)
              (fun () ->
                match event with
                | Parser.Global_initializer_leaf_completed receipt ->
                    Preparation.prepare_initializer preparation ~session ~ledger
                      receipt
                | Parser.Static_initializer_preparing receipt ->
                    Preparation.prepare_static preparation ~session ~ledger
                      receipt
                | _ -> Ok ()));
      dimension_count = Some (D.grammar_dimension_count ledger);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let config = Preprocessor.Config.create ~compilation_mode:Aot () |> checked in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  if Parser.has_errors parsed then
    ignore (diagnostics (Error parsed.diagnostics));
  let command = D.seal_source ledger (Option.get parsed.ast) |> diagnostics in
  let unit_ =
    Unit.compile_source_output ~source_command:command
      ~native_initializers:(Preparation.initializers preparation)
      ~native_static_initializers:(Preparation.static_initializers preparation)
      ~max_initializer_steps:100 session ~config parsed
    |> diagnostics
  in
  let unit_ = unit_.Unit.value in
  let sequence = Option.get !sequence in
  let globals = Unit.globals unit_ and entry = Unit.entry unit_ in
  let task = Preparation.task_state preparation in
  let evidence = Preparation.initializers preparation in
  let receipts = List.filter_map Initializers.native_execution evidence in
  let copies = List.filter_map Initializers.native_copy_execution evidence in
  Alcotest.(check int) "one actual scalar receipt" 1 (List.length receipts);
  Alcotest.(check int) "one actual byte-copy receipt" 1 (List.length copies);
  let scalar = List.hd receipts and copy = List.hd copies in
  let cloned_scalar = Obj.obj (Obj.dup (Obj.repr scalar)) in
  let cloned_copy = Obj.obj (Obj.dup (Obj.repr copy)) in
  let with_changed_field value index replacement test =
    let object_ = Obj.repr value in
    let old = Obj.field object_ index in
    Fun.protect
      ~finally:(fun () -> Obj.set_field object_ index old)
      (fun () ->
        Obj.set_field object_ index replacement;
        test ())
  in
  let cloned_scalar_source =
    let some = Obj.field (Obj.repr scalar) 6 in
    let source = Obj.dup (Obj.field some 0) in
    Obj.repr (Some (Obj.obj source))
  in
  let cloned_copy_source = Obj.dup (Obj.field (Obj.repr copy) 0) in
  if paired then (
    let peer = Option.get !peer in
    Alcotest.(check bool)
      "producer and consumer share real resources" true
      (VM.task_shares_resources task peer);
    let invalid thunk =
      try
        ignore (thunk ());
        false
      with Invalid_argument _ -> true
    in
    Alcotest.(check bool)
      "cloned source receipt rejects" true
      (invalid (fun () ->
           VM.begin_isolated_preparation ~receipts:[ cloned_scalar ] ~copies
             peer));
    Alcotest.(check bool)
      "cloned copy receipt rejects" true
      (invalid (fun () ->
           VM.begin_isolated_preparation ~receipts ~copies:[ cloned_copy ] peer));
    with_changed_field scalar 6 cloned_scalar_source (fun () ->
        Alcotest.(check bool)
          "mutated original scalar source rejects" true
          (invalid (fun () ->
               VM.begin_isolated_preparation ~receipts ~copies peer)));
    with_changed_field copy 0 cloned_copy_source (fun () ->
        Alcotest.(check bool)
          "mutated original copy source rejects" true
          (invalid (fun () ->
               VM.begin_isolated_preparation ~receipts ~copies peer)));
    let copied_peer = Obj.obj (Obj.dup (Obj.repr peer)) in
    Alcotest.(check bool)
      "copied peer with identical resources rejects" true
      (invalid (fun () ->
           VM.begin_isolated_preparation ~receipts ~copies copied_peer));
    Alcotest.(check bool)
      "producer cannot act as its own peer" true
      (invalid (fun () -> VM.begin_isolated_preparation ~receipts ~copies task));
    let ticket = VM.begin_isolated_preparation ~receipts ~copies peer in
    let seal ticket =
      VM.finish_isolated_preparation peer ticket
        ~runtime_calls:(Unit.runtime_calls unit_) ~globals
        ~initialization:(Unit.initialization unit_)
        ~functions:(Unit.functions unit_) entry
    in
    let copied_ticket = Obj.obj (Obj.dup (Obj.repr ticket)) in
    reject "copied charged ticket cannot publish" (seal copied_ticket);
    let original_steps : int = Obj.obj (Obj.field (Obj.repr ticket) 3) in
    with_changed_field ticket 3
      (Obj.repr (original_steps + 1))
      (fun () ->
        reject "mutated charged-work count cannot publish" (seal ticket));
    with_changed_field scalar 6 cloned_scalar_source (fun () ->
        reject "mutated scalar after reservation cannot publish" (seal ticket));
    with_changed_field copy 0 cloned_copy_source (fun () ->
        reject "mutated copy after reservation cannot publish" (seal ticket));
    seal ticket |> checked;
    with_changed_field ticket 4 (Obj.repr false) (fun () ->
        reject "resetting ticket closed flag cannot replay" (seal ticket));
    with_changed_field scalar 5 (Obj.repr false) (fun () ->
        Alcotest.(check bool)
          "resetting public consumed bit cannot replay" true
          (invalid (fun () ->
               VM.begin_isolated_preparation ~receipts ~copies peer)));
    with_changed_field copy 9 (Obj.repr false) (fun () ->
        Alcotest.(check bool)
          "resetting copy consumed bit cannot replay" true
          (invalid (fun () ->
               VM.begin_isolated_preparation ~receipts ~copies peer)));
    reject "real charged ticket is once-only" (seal ticket);
    Alcotest.(check bool)
      "consumed scalar/copy receipts cannot be imported again" true
      (invalid (fun () -> VM.begin_isolated_preparation ~receipts ~copies peer)))
  else
    let finish preparation sequence globals entry =
      Preparation.finish_unpaired_aot_output preparation ~sequence ~globals
        ~initialization:(Unit.initialization unit_)
        ~entry
    in
    let copied_sequence = Obj.obj (Obj.dup (Obj.repr sequence)) in
    Alcotest.(check bool)
      "copied sequence has no parser acceptance" false
      (Parser.sequence_accepted copied_sequence);
    reject "copied source sequence cannot seal"
      (finish preparation copied_sequence globals entry);
    reject "foreign source task cannot seal"
      (finish other sequence globals entry);
    let copied_globals = Obj.obj (Obj.dup (Obj.repr globals)) in
    reject "copied compiled storage cannot seal"
      (finish preparation sequence copied_globals entry);
    let copied_entry = Obj.obj (Obj.dup (Obj.repr entry)) in
    reject "copied entry graph cannot seal"
      (finish preparation sequence globals copied_entry);
    let direct task receipts copies =
      VM.finish_unpaired_aot_output task ~sequence ~globals
        ~initialization:(Unit.initialization unit_)
        ~entry ~receipts ~copies
    in
    reject "missing all source executions" (direct task [] []);
    reject "missing original copy" (direct task receipts []);
    reject "missing original scalar" (direct task [] copies);
    reject "cloned scalar execution is not issued"
      (direct task [ cloned_scalar ] copies);
    reject "cloned copy execution is not issued"
      (direct task receipts [ cloned_copy ]);
    with_changed_field scalar 6 cloned_scalar_source (fun () ->
        reject "mutated original scalar source rejects"
          (direct task receipts copies));
    with_changed_field copy 0 cloned_copy_source (fun () ->
        reject "mutated original copy source rejects"
          (direct task receipts copies));
    reject "duplicated source execution"
      (direct task (receipts @ receipts) copies);
    reject "duplicated source copy" (direct task receipts (copies @ copies));
    let copied_task = Obj.obj (Obj.dup (Obj.repr task)) in
    reject "copied source task cannot publish"
      (direct copied_task receipts copies);
    finish preparation sequence globals entry |> checked;
    reject "completed output cannot be sealed twice"
      (finish preparation sequence globals entry)

let () =
  Alcotest.run "AOT invocation authority"
    [
      ( "pair",
        [
          Alcotest.test_case "original live fork is one-shot" `Quick (fun () ->
              run ~consume:true ~foreign_namespace:false ~foreign_task:false);
          Alcotest.test_case "expired source cannot claim" `Quick (fun () ->
              run ~consume:false ~foreign_namespace:false ~foreign_task:false);
          Alcotest.test_case "foreign namespace cannot pair" `Quick (fun () ->
              run ~consume:true ~foreign_namespace:true ~foreign_task:false);
          Alcotest.test_case "same namespace foreign task cannot pair" `Quick
            (fun () ->
              run ~consume:true ~foreign_namespace:false ~foreign_task:true);
          Alcotest.test_case "fork cannot serve two source roots" `Quick
            (reject_reused_source ~reuse_owner:false);
          Alcotest.test_case "paired task cannot serve two source roots" `Quick
            (reject_reused_source ~reuse_owner:true);
          Alcotest.test_case
            "unpaired output rejects substituted and replayed owners" `Quick
            (source_output_authority ~paired:false);
          Alcotest.test_case
            "paired output rejects cloned receipts, peers and tickets" `Quick
            (source_output_authority ~paired:true);
        ] );
    ]
