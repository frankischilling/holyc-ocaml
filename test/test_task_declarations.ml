open Holyc_lib
module D = Task_declarations
module C = Semantic_declaration_collection

let checked = Test_declaration_collection.checked
let expect = Test_integer_program.checked

let reject message result =
  Alcotest.(check bool) message true (Result.is_error result)

let config () = Preprocessor.Config.create () |> checked

let parse_source ?sources ?symbols ?observe ?checkpoint session ledger source =
  let sources = Option.value sources ~default:(Session.sources session) in
  let symbols = Option.value symbols ~default:(Session.symbols session) in
  let events = ref [] in
  let consume event =
    events := event :: !events;
    match observe with
    | Some observe -> observe event
    | None -> D.observe ledger event
  in
  let commands : Parser.command_sink =
    {
      checkpoint =
        Some (Option.value checkpoint ~default:(D.observe_command ledger));
      reference = None;
      declaration = Some consume;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let output =
    Parser.parse ~commands ~sources ~symbols
      ~definitions:(Session.definitions session)
      ~config:(config ()) source
  in
  (output, List.rev !events)

let parse ?observe ?checkpoint session ledger text =
  let source =
    Session.add_source session ~path:"declarations.hc" ~contents:text
  in
  parse_source ?observe ?checkpoint session ledger source

let setup () =
  let session = Session.create () in
  (session, D.create session |> checked)

let copy_module (ast : Ast.module_) items =
  Ast.make_module ~source:ast.source ~span:ast.span ~items

let retained_globals () =
  let session, ledger = setup () in
  let output, events = parse session ledger "I64 A=40,B=2;" in
  let ast = Test_parser.expect_ast output in
  let assigned =
    events
    |> List.filter_map (function
      | Parser.Global_declared publication ->
          D.symbol_for ledger publication.global_entry
      | _ -> None)
  in
  let before =
    List.length
      (Semantic_symbol_table.all_symbols (Session.semantic_symbols session))
  in
  let command = D.seal ledger ast |> expect in
  let collection =
    D.collection ~table:(Session.semantic_symbols session) ~ast command
    |> expect
  in
  Alcotest.(check bool)
    "final collection reuses exact early symbols" true
    (List.for_all2
       (fun symbol entry -> symbol == C.entry_symbol entry)
       assigned (C.entries collection));
  Alcotest.(check int)
    "completion creates no replacement symbol" before
    (List.length
       (Semantic_symbol_table.all_symbols (Session.semantic_symbols session)));
  Alcotest.(check bool)
    "same AST reuses its seal" true
    (D.seal ledger ast |> expect == command);
  let wrapper = copy_module ast ast.items in
  reject "foreign AST wrapper cannot use collection seal"
    (D.collection
       ~table:(Session.semantic_symbols session)
       ~ast:wrapper command);
  reject "overlapping wrapper cannot claim declarations again"
    (D.seal ledger wrapper);
  reject "foreign table cannot use collection seal"
    (D.collection
       ~table:(Session.semantic_symbols (Session.create ()))
       ~ast command);
  List.iter
    (fun event ->
      reject "event replay rejected after seal" (D.observe ledger event))
    events

let substitution_is_atomic () =
  let session, ledger = setup () in
  let output, _ = parse session ledger "I64 A=40,B=2;" in
  let ast = Test_parser.expect_ast output in
  match ast.items with
  | [ Ast.Global_declaration declaration ] ->
      let first = List.nth declaration.declarators 0 in
      let second = List.nth declaration.declarators 1 in
      let copied_name =
        Ast.make_identifier ~spelling:second.name.spelling
          ~location:second.name.location
      in
      let copied =
        Ast.make_global_declarator ~pointer_layers:second.pointer_layers
          ~function_pointer:second.function_pointer ~name:copied_name
          ~array_dimensions:second.array_dimensions
          ~initial_value:second.global_initial_value ~delimiter:second.delimiter
          ~location:second.location
      in
      let replacement declarators =
        copy_module ast
          [
            Ast.Global_declaration
              (Ast.make_global_declaration ~modifiers:declaration.modifiers
                 ~binding:declaration.binding
                 ~type_specifier:declaration.type_specifier ~declarators
                 ~trailing_semicolon:declaration.trailing_semicolon
                 ~location:declaration.location);
          ]
      in
      reject "equal-looking second name cannot claim publication"
        (D.seal ledger (replacement [ first; copied ]));
      reject "duplicate source declaration is rejected"
        (D.seal ledger (replacement [ first; first ]));
      reject "original declarators cannot be reordered"
        (D.seal ledger (replacement [ second; first ]));
      ignore (D.seal ledger ast |> expect)
  | _ -> Alcotest.fail "expected declaration list"

let function_phases () =
  List.iter
    (fun source ->
      let session, ledger = setup () in
      let output, events = parse session ledger source in
      let ast = Test_parser.expect_ast output in
      match events with
      | Parser.Function_declared publication
        :: Parser.Function_header_completed header
        :: rest ->
          let provisional =
            D.symbol_for ledger publication.function_entry |> Option.get
          in
          let completed =
            D.symbol_for ledger header.completed_entry |> Option.get
          in
          Alcotest.(check bool)
            "header completion keeps assigned symbol" true
            (provisional == completed);
          let command = D.seal ledger ast |> expect in
          let collection =
            D.collection ~table:(Session.semantic_symbols session) ~ast command
            |> expect
          in
          Alcotest.(check bool)
            "final typed collection keeps header symbol" true
            (C.entry_symbol (List.hd (C.entries collection)) == provisional);
          Alcotest.(check bool)
            "definition completion is separate"
            (String.starts_with ~prefix:"I64" source)
            (rest <> [])
      | _ -> Alcotest.fail "expected function publication phases")
    [ "extern I64 F(I64 n=42);"; "I64 F(I64 n){return n;}"; "I64 F()" ]

let unfinished_cannot_seal () =
  let session, ledger = setup () in
  let source =
    Session.add_source session ~path:"incomplete.hc" ~contents:"I64 N=;"
  in
  let output, events = parse_source session ledger source in
  Alcotest.(check bool)
    "source has syntax error" true (Parser.has_errors output);
  match events with
  | [ Parser.Global_declared publication ] ->
      let name = publication.global_name in
      let variable =
        Ast.make_global_variable ~modifiers:publication.global_header.modifiers
          ~binding:publication.global_header.binding
          ~type_specifier:publication.global_header.type_specifier
          ~pointer_layers:publication.global_pointer_layers ~name
          ~array_dimensions:publication.global_dimensions
          ~semicolon:name.location.span ~location:name.location
      in
      let ast =
        Ast.make_module ~source:(Source_file.id source) ~span:name.location.span
          ~items:[ Ast.Global_variable variable ]
      in
      reject "incomplete initialized global cannot become uninitialized storage"
        (D.seal ledger ast)
  | _ -> Alcotest.fail "expected only provisional global"

let foreign_source_owners () =
  let session, ledger = setup () in
  let original =
    Session.add_source session ~path:"same.hc" ~contents:"I64 A=42;"
  in
  let foreign = Session.create () in
  let foreign_source =
    Session.add_source foreign ~path:"same.hc" ~contents:"I64 A=42;"
  in
  Alcotest.(check bool)
    "source IDs collide" true
    (Source_id.equal (Source_file.id original) (Source_file.id foreign_source));
  let before =
    List.length
      (Semantic_symbol_table.all_symbols (Session.semantic_symbols session))
  in
  let output, _ =
    parse_source ~sources:(Session.sources foreign) session ledger
      foreign_source
  in
  Alcotest.(check bool)
    "different source manager rejected" true (Parser.has_errors output);
  let substitute =
    Source_file.create ~id:(Source_file.id original)
      ~path:(Source_file.path original)
      ~display_path:(Source_file.display_path original)
      ~contents:(Source_file.contents original)
  in
  let output, _ = parse_source session ledger substitute in
  Alcotest.(check bool)
    "reconstructed input rejected" true (Parser.has_errors output);
  let output, _ =
    parse_source ~symbols:(Session.symbols foreign) session ledger original
  in
  Alcotest.(check bool)
    "foreign symbol environment rejected" true (Parser.has_errors output);
  Alcotest.(check int)
    "foreign events assign no semantic symbols" before
    (List.length
       (Semantic_symbol_table.all_symbols (Session.semantic_symbols session)));
  let output, _ = parse_source session ledger original in
  ignore (D.seal ledger (Test_parser.expect_ast output) |> expect)

let phase_order_and_replay () =
  let session, ledger = setup () in
  let checkpoints = ref [] in
  let output, events =
    parse
      ~observe:(fun _ -> Ok ())
      ~checkpoint:(fun event ->
        checkpoints := event :: !checkpoints;
        Ok ())
      session ledger "I64 F(){return 42;}"
  in
  let ast = Test_parser.expect_ast output in
  match events with
  | [ declared; header; body ] ->
      let initial, completion =
        match List.rev !checkpoints with
        | first :: second :: rest -> ([ first; second ], rest)
        | _ -> Alcotest.fail "expected command context and start"
      in
      List.iter
        (fun event -> ignore (D.observe_command ledger event |> expect))
        initial;
      reject "body cannot precede declaration" (D.observe ledger body);
      reject "header cannot precede declaration" (D.observe ledger header);
      ignore (D.observe ledger declared |> expect);
      reject "provisional replay cannot allocate another symbol"
        (D.observe ledger declared);
      reject "body cannot precede header" (D.observe ledger body);
      ignore (D.observe ledger header |> expect);
      reject "header cannot complete twice" (D.observe ledger header);
      reject "unfinished function cannot seal" (D.seal ledger ast);
      ignore (D.observe ledger body |> expect);
      reject "body cannot complete twice" (D.observe ledger body);
      List.iter
        (fun event -> ignore (D.observe_command ledger event |> expect))
        completion;
      ignore (D.seal ledger ast |> expect)
  | _ -> Alcotest.fail "expected three function events"

let nested_publication_views () =
  let session, ledger = setup () in
  let source =
    Session.add_source session ~path:"nested.hc"
      ~contents:"I64 A=#exe {I64 A=100;};"
  in
  let nested = ref [] in
  let outer_publication = ref None in
  let saw_assigned_outer = ref false in
  let consume event =
    (match event with
    | Parser.Global_declared publication when Option.is_none !outer_publication
      -> outer_publication := Some publication
    | _ -> ());
    D.observe ledger event
  in
  let sink checkpoint : Parser.command_sink =
    {
      checkpoint =
        Some
          (fun event ->
            Result.bind (D.observe_command ledger event) (fun () ->
                checkpoint event));
      reference = None;
      declaration = Some consume;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let execute_stream _ =
    saw_assigned_outer :=
      Option.is_some
        (D.symbol_for ledger (Option.get !outer_publication).global_entry);
    Ok
      Parser.
        {
          definitions = Session.definitions session;
          symbols = Session.symbols session;
          commands =
            sink (function
              | Parser.Command_completed receipt ->
                  let ast = receipt.command_ast in
                  D.seal ledger ast
                  |> Result.map (fun command ->
                      nested := (ast, command) :: !nested)
              | _ -> Ok ());
          finish = (fun () -> Ok "42");
          abort = (fun () -> ());
        }
  in
  let output =
    Parser.parse
      ~commands:(sink (fun _ -> Ok ()))
      ~execute_stream ~sources:(Session.sources session)
      ~symbols:(Session.symbols session)
      ~definitions:(Session.definitions session)
      ~config:(config ()) source
  in
  let ast = Test_parser.expect_ast output in
  let outer = D.seal ledger ast |> expect in
  let entry command ast =
    D.collection ~table:(Session.semantic_symbols session) ~ast command
    |> expect |> C.entries |> List.hd |> C.entry_symbol
  in
  let outer_symbol = entry outer ast in
  let inner_ast, inner = List.hd !nested in
  let inner_symbol = entry inner inner_ast in
  Alcotest.(check bool)
    "outer symbol exists before its nested initializer" true !saw_assigned_outer;
  Alcotest.(check bool)
    "nested shadow has distinct retained identity" true
    (outer_symbol != inner_symbol);
  Alcotest.(check bool)
    "nested commands retain the shared task scope" true
    (Semantic_symbol.Scope_id.equal
       (Semantic_symbol.scope_id outer_symbol)
       (Semantic_symbol.scope_id inner_symbol));
  Alcotest.(check bool)
    "outer seal keeps its pre-directive symbol" true
    (outer_symbol
    == Option.get
         (D.symbol_for ledger (Option.get !outer_publication).global_entry))

let singleton_and_callback_sources () =
  List.iter
    (fun source ->
      let session, ledger = setup () in
      let output, _ = parse session ledger source in
      let ast = Test_parser.expect_ast output in
      let command = D.seal ledger ast |> expect in
      let collection =
        D.collection ~table:(Session.semantic_symbols session) ~ast command
        |> expect
      in
      Alcotest.(check int)
        "single source has one semantic declaration" 1
        (List.length (C.entries collection)))
    [ "I64 N;"; "I64 A[2];"; "I64 (*Callback)(I64 n);" ]

let whole_statement_membership () =
  let session, ledger = setup () in
  let output, _ = parse session ledger "1;42;" in
  let ast = Test_parser.expect_ast output in
  Alcotest.(check int) "two original commands" 2 (List.length ast.items);
  reject "statement subset has no parser command receipt"
    (D.seal ledger (copy_module ast [ List.hd ast.items ]));
  reject "reordered statements have no parser sequence receipt"
    (D.seal ledger (copy_module ast (List.rev ast.items)));
  reject "equal wrapper has no parser sequence receipt"
    (D.seal ledger (copy_module ast ast.items));
  ignore (D.seal ledger ast |> expect)

let command_protocol () =
  let session, ledger = setup () in
  let events = ref [] in
  let output, _ =
    parse
      ~checkpoint:(fun event ->
        events := event :: !events;
        Ok ())
      session ledger "1;42;"
  in
  let ast = Test_parser.expect_ast output in
  let events = List.rev !events in
  let accept event = ignore (D.observe_command ledger event |> expect) in
  (match events with
  | [ opened; started; completed; resumed; next; last; final_resume; closed ] ->
      reject "completion cannot precede context"
        (D.observe_command ledger completed);
      accept opened;
      reject "sequence cannot finish before its commands"
        (D.observe_command ledger closed);
      reject "second command cannot become first"
        (D.observe_command ledger next);
      accept started;
      reject "resume cannot precede completion"
        (D.observe_command ledger resumed);
      accept completed;
      reject "next command cannot bypass pending resume"
        (D.observe_command ledger next);
      accept resumed;
      accept next;
      reject "old resume cannot resume a new command"
        (D.observe_command ledger resumed);
      accept last;
      reject "sequence cannot bypass final resume"
        (D.observe_command ledger closed);
      accept final_resume;
      accept closed;
      List.iter
        (fun event ->
          reject "command event replay" (D.observe_command ledger event))
        events
  | _ -> Alcotest.fail "expected two complete command lifecycles");
  ignore (D.seal ledger ast |> expect)

let overlapping_statement_views () =
  List.iter
    (fun seal_sequence ->
      let session, ledger = setup () in
      let views = ref [] in
      let checkpoint event =
        Result.map
          (fun () ->
            match event with
            | Parser.Command_completed receipt ->
                views := receipt.command_ast :: !views
            | _ -> ())
          (D.observe_command ledger event)
      in
      let output, _ = parse ~checkpoint session ledger "1;42;" in
      let ast = Test_parser.expect_ast output in
      let first, second =
        match List.rev !views with
        | [ first; second ] -> (first, second)
        | _ -> Alcotest.fail "expected two original command views"
      in
      if seal_sequence then (
        ignore (D.seal ledger ast |> expect);
        reject "sequence claims first statement" (D.seal ledger first);
        reject "sequence claims second statement" (D.seal ledger second))
      else (
        ignore (D.seal ledger first |> expect);
        reject "single statement blocks overlapping sequence"
          (D.seal ledger ast);
        ignore (D.seal ledger second |> expect)))
    [ false; true ]

let failed_sequences_release_context () =
  let session, ledger = setup () in
  let views = ref [] in
  let aborted = ref 0 in
  let checkpoint event =
    Result.map
      (fun () ->
        match event with
        | Parser.Command_completed receipt ->
            views := receipt.command_ast :: !views
        | Parser.Sequence_aborted _ -> incr aborted
        | _ -> ())
      (D.observe_command ledger event)
  in
  let output, _ = parse ~checkpoint session ledger "42;I64 N=;" in
  Alcotest.(check bool)
    "unfinished sequence fails" true (Parser.has_errors output);
  Alcotest.(check int) "one completed command survives" 1 (List.length !views);
  Alcotest.(check int) "syntax failure closes context" 1 !aborted;
  ignore (D.seal ledger (List.hd !views) |> expect);
  (try
     ignore
       (parse ~checkpoint
          ~observe:(fun event ->
            ignore (D.observe ledger event |> expect);
            raise Exit)
          session ledger "I64 M=42;");
     Alcotest.fail "expected consumer exception"
   with Exit -> ());
  Alcotest.(check int) "exception closes context" 2 !aborted;
  let output, _ = parse session ledger "42;" in
  ignore (D.seal ledger (Test_parser.expect_ast output) |> expect)

let empty_sequence_identity () =
  let session, ledger = setup () in
  let output, _ = parse session ledger "" in
  let ast = Test_parser.expect_ast output in
  reject "empty wrapper still needs a sequence witness"
    (D.seal ledger (copy_module ast []));
  ignore (D.seal ledger ast |> expect)

let rejected_sequence_cannot_seal () =
  List.iter
    (fun throws ->
      let session, ledger = setup () in
      let sequence = ref None in
      let commands = ref [] in
      let abort = ref None in
      let checkpoint event =
        let result = D.observe_command ledger event in
        match (result, event) with
        | Ok (), Parser.Command_completed receipt ->
            commands := receipt.command_ast :: !commands;
            Ok ()
        | Ok (), Parser.Sequence_completed receipt ->
            sequence := Some receipt.sequence_ast;
            reject "sequence cannot seal during its unaccepted callback"
              (D.seal ledger receipt.sequence_ast);
            if throws then raise Exit
            else
              Error
                [
                  Diagnostic.make ~code:"TESTREJECT" ~severity:Diagnostic.Error
                    ~primary:receipt.sequence_ast.span
                    ~message:"late completion rejected" ();
                ]
        | Ok (), Parser.Sequence_aborted _ ->
            abort := Some event;
            result
        | _ -> result
      in
      (try
         let output, _ = parse ~checkpoint session ledger "42;" in
         Alcotest.(check bool)
           "late rejection fails parsing" true (Parser.has_errors output);
         Alcotest.(check bool) "expected normal rejection" false throws
       with Exit -> Alcotest.(check bool) "expected exception" true throws);
      reject "rejected sequence cannot acquire a whole-source seal"
        (D.seal ledger (Option.get !sequence));
      reject "late abort is consumed once"
        (D.observe_command ledger (Option.get !abort));
      ignore (D.seal ledger (List.hd !commands) |> expect);
      let output, _ = parse session ledger "1;" in
      ignore (D.seal ledger (Test_parser.expect_ast output) |> expect))
    [ false; true ]

let nested_receipt_views () =
  let cases =
    [
      ("#exe {42;} 1;", "", true, false, `None, true, Some 0);
      ("I64 N=#exe {42;};", "42", true, false, `None, true, Some 1);
      ("I64 N;#exe {42;}1;", "", true, false, `None, true, Some 2);
      ("#exe {42;}1;", "", false, false, `None, false, None);
      ("#exe {42;}1;", "", false, true, `None, true, Some 0);
      ("#exe {42;I64 N=;}1;", "", true, false, `None, false, Some 0);
      ("#exe {42;}1;", "", true, false, `Finish, false, Some 0);
      ("I64 N=#exe {42;};", "42", true, false, `Completion, false, Some 1);
    ]
  in
  List.iter
    (fun ( contents,
           generated,
           observe_outer,
           foreign_outer,
           failure,
           succeeds,
           parent_phase ) ->
      let session, ledger = setup () in
      let outer = if foreign_outer then Session.create () else session in
      let source =
        Session.add_source session ~path:"nested-receipts.hc" ~contents
      in
      let child_sequence = ref None in
      let child_context = ref None in
      let sink checkpoint : Parser.command_sink =
        {
          checkpoint = Some checkpoint;
          reference = None;
          declaration = Some (D.observe ledger);
          command = (fun _ -> Ok ());
          resume = (fun () -> Ok ());
        }
      in
      let inner_checkpoint event =
        Result.bind (D.observe_command ledger event) (fun () ->
            (match event with
            | Parser.Sequence_started context -> child_context := Some context
            | Parser.Sequence_completed receipt ->
                child_sequence := Some receipt
            | _ -> ());
            match event with
            | Parser.Sequence_completed receipt when failure = `Completion ->
                Error
                  [
                    Diagnostic.make ~code:"TESTCOMPLETE"
                      ~severity:Diagnostic.Error
                      ~primary:receipt.sequence_ast.span
                      ~message:"child completion failed" ();
                  ]
            | _ -> Ok ())
      in
      let execute_stream span =
        Ok
          Parser.
            {
              definitions = Session.definitions session;
              symbols = Session.symbols session;
              commands = sink inner_checkpoint;
              finish =
                (fun () ->
                  if failure = `Finish then
                    Error
                      [
                        Diagnostic.make ~code:"TESTFINISH"
                          ~severity:Diagnostic.Error ~primary:span
                          ~message:"generation failed" ();
                      ]
                  else Ok generated);
              abort = (fun () -> ());
            }
      in
      let commands =
        if observe_outer then Some (sink (D.observe_command ledger)) else None
      in
      let output =
        Parser.parse ?commands ~execute_stream
          ~sources:(Session.sources session)
          ~definitions:(Session.definitions outer)
          ~symbols:(Session.symbols outer) ~config:(config ()) source
      in
      Alcotest.(check bool)
        "nested parser/ledger outcome" succeeds
        (not (Parser.has_errors output));
      let actual_phase =
        Option.map
          (fun context ->
            match Parser.context_parent context with
            | Some (Parser.Before_first_command _) -> 0
            | Some (Parser.Reading_command _) -> 1
            | Some (Parser.Awaiting_resume _) -> 2
            | None -> Alcotest.fail "child lost suspended parent")
          !child_context
      in
      Alcotest.(check (option int))
        "ledger consumes exact parent phase" parent_phase actual_phase;
      Option.iter
        (fun (receipt : Parser.completed_sequence) ->
          Alcotest.(check bool)
            "only accepted child syntax survives later generation failure"
            (failure <> `Completion)
            (Parser.sequence_accepted receipt);
          if failure = `Completion then
            reject "rejected child has no sequence seal"
              (D.seal ledger receipt.sequence_ast)
          else ignore (D.seal ledger receipt.sequence_ast |> expect))
        !child_sequence;
      if succeeds && observe_outer then
        ignore (D.seal ledger (Test_parser.expect_ast output) |> expect);
      let output, _ = parse session ledger "42;" in
      ignore (D.seal ledger (Test_parser.expect_ast output) |> expect))
    cases

let tests =
  [
    Alcotest.test_case "nested receipts validate parent ownership and cleanup"
      `Quick nested_receipt_views;
    Alcotest.test_case "late completion rejection cannot seal a failed sequence"
      `Quick rejected_sequence_cannot_seal;
    Alcotest.test_case "command lifecycle rejects replay and reordered phases"
      `Quick command_protocol;
    Alcotest.test_case "statement command and sequence views cannot overlap"
      `Quick overlapping_statement_views;
    Alcotest.test_case "failed and exceptional parsing release their context"
      `Quick failed_sequences_release_context;
    Alcotest.test_case "empty sequence has exact parser ownership" `Quick
      empty_sequence_identity;
    Alcotest.test_case "statement membership requires original commands" `Quick
      whole_statement_membership;
    Alcotest.test_case
      "nested publications keep command-local declaration views" `Quick
      nested_publication_views;
    Alcotest.test_case
      "singleton and callback globals retain source association" `Quick
      singleton_and_callback_sources;
    Alcotest.test_case "completed globals reuse assigned symbols" `Quick
      retained_globals;
    Alcotest.test_case
      "source substitution rejects before claiming any publication" `Quick
      substitution_is_atomic;
    Alcotest.test_case "function phases retain one declaration symbol" `Quick
      function_phases;
    Alcotest.test_case
      "unfinished initializer cannot become an uninitialized global" `Quick
      unfinished_cannot_seal;
    Alcotest.test_case "parser source and environment ownership is exact" `Quick
      foreign_source_owners;
    Alcotest.test_case "completion phase and replay checks" `Quick
      phase_order_and_replay;
  ]
