open Holyc_lib
module D = Task_declarations
module C = Semantic_declaration_collection

let checked = Test_declaration_collection.checked
let expect = Test_integer_program.checked

let reject message result =
  Alcotest.(check bool) message true (Result.is_error result)

let config () = Preprocessor.Config.create () |> checked

let parse_source ?sources ?symbols ?observe ?checkpoint ?reference ?query
    session ledger source =
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
      reference;
      query = Some (Option.value query ~default:(D.observe_query ledger));
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

let parse ?observe ?checkpoint ?reference ?query session ledger text =
  let source =
    Session.add_source session ~path:"declarations.hc" ~contents:text
  in
  parse_source ?observe ?checkpoint ?reference ?query session ledger source

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
      query = None;
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
          query = None;
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

module VM = Ir_integer_interpreter

module Program = struct
  let compile_task_ast = compile_integer_task_ast
  let runtime_calls = integer_program_runtime_calls
  let globals = integer_program_globals
  let initialization = integer_program_initialization
  let functions = integer_program_functions
  let entry = integer_program_entry
end

let runtime_setup ?max_global_bytes ?max_initializer_steps () =
  let session = Session.create () in
  let runtime =
    VM.create_task_state ?max_global_bytes ?max_initializer_steps
      ~table:(Session.semantic_symbols session)
      ()
    |> checked
  in
  let ledger = D.create ~runtime session |> checked in
  (session, runtime, ledger)

let compile_runtime session runtime source =
  let ast = Test_integer_task.parse (Session.fork_frontend session) source in
  (Program.compile_task_ast ~task:runtime session ~config:(config ()) ast
  |> expect)
    .value

let execute_runtime runtime program =
  VM.execute_task_program runtime
    ~runtime_calls:(Program.runtime_calls program)
    ~globals:(Program.globals program)
    ~initialization:(Program.initialization program)
    ~functions:(Program.functions program)
    (Program.entry program)

let execute_runtime_ok runtime program =
  match execute_runtime runtime program with
  | Ok _ -> ()
  | Error errors ->
      Alcotest.fail
        (String.concat "; "
           (List.map (fun (error : VM.error) -> error.message) errors))

let runtime_compilation_budget () =
  let session, runtime, _ = runtime_setup ~max_initializer_steps:6 () in
  let first = compile_runtime session runtime "I64 N=40;" in
  Alcotest.(check int)
    "pending compilation charges task preparation" 3
    (VM.task_initializer_steps runtime);
  let second = compile_runtime session runtime "I64 M=2;" in
  Alcotest.(check int)
    "pending units share the task preparation limit" 6
    (VM.task_initializer_steps runtime);
  let ast =
    Test_integer_task.parse (Session.fork_frontend session) "I64 Excess=1;"
  in
  Test_integer_task.fault "HCIRVM0007"
    (Program.compile_task_ast ~task:runtime session ~config:(config ()) ast);
  execute_runtime_ok runtime first;
  execute_runtime_ok runtime second;
  execute_runtime_ok runtime (compile_runtime session runtime "42;");
  let session, runtime, _ = runtime_setup ~max_initializer_steps:6 () in
  let ast =
    Test_integer_task.parse (Session.fork_frontend session) "I64 Failed=1/0;"
  in
  Test_integer_task.fault "HCIRVM0009"
    (Program.compile_task_ast ~task:runtime session ~config:(config ()) ast);
  Alcotest.(check int)
    "failed preparation retains reached work" 3
    (VM.task_initializer_steps runtime);
  ignore (compile_runtime session runtime "I64 N=42;");
  Alcotest.(check int)
    "later preparation uses remaining budget" 6
    (VM.task_initializer_steps runtime)

let runtime_compilation_owner () =
  let session, runtime, _ = runtime_setup () in
  let foreign = Session.create () in
  let check target config =
    let ast =
      Test_integer_task.parse (Session.fork_frontend target) "I64 N=42;"
    in
    let table = Session.semantic_symbols target in
    let symbols = Semantic_symbol_table.all_symbols table in
    let scopes = Semantic_symbol_table.all_scopes table in
    Test_integer_task.fault "HCRUN0004"
      (Program.compile_task_ast ~task:runtime target ~config ast);
    Alcotest.(check bool)
      "rejection allocates no symbols" true
      (Semantic_symbol_table.all_symbols table = symbols);
    Alcotest.(check bool)
      "rejection allocates no scopes" true
      (Semantic_symbol_table.all_scopes table = scopes);
    Alcotest.(check int)
      "rejection charges no preparation" 0
      (VM.task_initializer_steps runtime)
  in
  check foreign (config ());
  check session (Preprocessor.Config.create ~compilation_mode:Aot () |> checked)

let admission runtime program =
  VM.task_admission runtime ~globals:(Program.globals program)
    ~entry:(Program.entry program)

let visible session name =
  match
    Symbol_visibility.Environment.find_preprocessor (Session.symbols session)
      name
  with
  | Symbol_visibility.Present entry -> entry
  | _ -> Alcotest.fail "expected published frontend entry"

let runtime_admission_order () =
  let session, runtime, ledger = runtime_setup () in
  let first = compile_runtime session runtime "I64 F(){return 40;}" in
  execute_runtime_ok runtime first;
  let first_receipt = admission runtime first |> Option.get in
  let second = compile_runtime session runtime "I64 F(I64 n){return n;}" in
  execute_runtime_ok runtime second;
  let second_receipt = admission runtime second |> Option.get in
  reject "older receipt is stale before newer delivery"
    (D.observe_admission ledger first_receipt);
  ignore (D.observe_admission ledger second_receipt |> checked);
  let newest = visible session "F" in
  reject "older admission cannot replace the current frontend header"
    (D.observe_admission ledger first_receipt);
  Alcotest.(check bool)
    "rejection preserves newest entry" true
    (visible session "F" == newest)

let runtime_admission_ownership () =
  let session, runtime, ledger = runtime_setup () in
  let other =
    VM.create_task_state ~table:(Session.semantic_symbols session) () |> checked
  in
  let other_ledger = D.create ~runtime:other session |> checked in
  let unbound = D.create session |> checked in
  let foreign = Session.create () in
  let foreign_runtime =
    VM.create_task_state ~table:(Session.semantic_symbols foreign) () |> checked
  in
  reject "runtime must own ledger semantic table"
    (D.create ~runtime:foreign_runtime session);
  let program =
    compile_runtime session runtime
      "I64 A=40;I64 F(I64 n=42){return n;}I64 B=2;extern U0 Print(U8 *fmt,...);"
  in
  let separate = compile_runtime session runtime "1;" in
  Alcotest.(check bool)
    "compilation creates no admission" true
    (Option.is_none (admission runtime program));
  execute_runtime_ok runtime program;
  let receipt = admission runtime program |> Option.get in
  Alcotest.(check bool)
    "receipt retains exact owning task" true
    (VM.owns_task_admission runtime receipt);
  Alcotest.(check bool)
    "same-table foreign task has no authority" false
    (VM.owns_task_admission other receipt);
  Alcotest.(check bool)
    "foreign task cannot retrieve receipt" true
    (Option.is_none (admission other program));
  Alcotest.(check bool)
    "substituted entry cannot retrieve receipt" true
    (Option.is_none
       (VM.task_admission runtime ~globals:(Program.globals program)
          ~entry:(Program.entry separate)));
  Alcotest.(check bool)
    "substituted storage cannot retrieve receipt" true
    (Option.is_none
       (VM.task_admission runtime ~globals:(Program.globals separate)
          ~entry:(Program.entry program)));
  reject "foreign ledger cannot publish receipt"
    (D.observe_admission other_ledger receipt);
  reject "unbound ledger cannot publish receipt"
    (D.observe_admission unbound receipt);
  ignore (D.observe_admission ledger receipt |> checked);
  (match VM.admission_publications receipt with
  | [
   (VM.Admitted_global (_, first) as first_link);
   VM.Admitted_function _;
   VM.Admitted_global (_, second);
   VM.Admitted_function _;
  ] ->
      Alcotest.(check string)
        "first global retains source order" "A"
        (Semantic_symbol.name (Ir_integer_globals.slot_symbol first));
      Alcotest.(check string)
        "second global retains source order" "B"
        (Semantic_symbol.name (Ir_integer_globals.slot_symbol second));
      let entry = visible session "A" in
      Alcotest.(check bool)
        "frontend entry keeps exact retained publication" true
        (D.retained_for ledger entry |> Option.get == first_link);
      Alcotest.(check bool)
        "frontend entry reuses admitted semantic symbol" true
        (D.symbol_for ledger entry |> Option.get
        == Ir_integer_globals.slot_symbol first)
  | _ -> Alcotest.fail "expected original global/function publication order");
  let shape name =
    Symbol_visibility.function_call_shape (visible session name) |> Option.get
  in
  Alcotest.(check bool)
    "function default shape retained" true
    (match (shape "F").parameters with
    | [ { has_default = true; parameter_name = Some "n" } ] -> true
    | _ -> false);
  Alcotest.(check bool)
    "provider variadic shape retained" true (shape "Print").variadic;
  let saved = visible session "F" in
  reject "receipt cannot publish twice" (D.observe_admission ledger receipt);
  reject "executed program cannot issue another receipt"
    (execute_runtime runtime program);
  Alcotest.(check bool)
    "replay retains original receipt" true
    (admission runtime program |> Option.get == receipt);
  Alcotest.(check bool)
    "replay preserves frontend entry" true
    (visible session "F" == saved);
  execute_runtime_ok runtime separate;
  let next = compile_runtime session runtime "I64 G(){return 42;}" in
  execute_runtime_ok runtime next;
  ignore
    (D.observe_admission ledger (admission runtime next |> Option.get)
    |> checked);
  ignore (visible session "G")

let runtime_admission_boundaries () =
  let session, runtime, _ = runtime_setup ~max_global_bytes:1 () in
  let program = compile_runtime session runtime "I64 Excess=42;" in
  reject "late storage preflight fails" (execute_runtime runtime program);
  Alcotest.(check bool)
    "failed preflight has no admission" true
    (Option.is_none (admission runtime program));
  Alcotest.(check bool)
    "failed preflight has no latest admission" true
    (Option.is_none (VM.latest_task_admission runtime));
  let session, runtime, ledger = runtime_setup () in
  let program =
    compile_runtime session runtime "I64 N=40;I64 F(I64 n){return n;}N=42;1/0;"
  in
  (match execute_runtime runtime program with
  | Error (error :: _) ->
      Alcotest.(check string) "fault follows admission" "HCIRVM0009" error.code
  | _ -> Alcotest.fail "expected reached division fault");
  ignore
    (D.observe_admission ledger (admission runtime program |> Option.get)
    |> checked);
  let next = compile_runtime session runtime "F(N);" in
  match execute_runtime runtime next with
  | Ok result ->
      Alcotest.(check int64)
        "admitted function and reached global survive" 42L
        (VM.final_value result |> Option.get).bits
  | Error _ -> Alcotest.fail "retained call failed"

(* Exercise real nested parser receipts and checked task compilation without
   claiming the production source-execution facade is connected. *)
let selected_runtime_source session runtime ledger contents =
  let execute ast =
    let ( let* ) = Result.bind in
    let* declaration_command = D.seal ledger ast in
    let* program =
      Program.compile_task_ast ~task:runtime ~declaration_command session
        ~config:(config ()) ast
    in
    execute_runtime runtime program.value
    |> Result.map_error
         (List.map (fun (error : VM.error) ->
              Diagnostic.make ~code:error.code ~severity:Diagnostic.Error
                ~primary:ast.Ast.span ~message:error.message ()))
  in
  let sink run : Parser.command_sink =
    {
      checkpoint =
        Some
          (fun event ->
            Result.bind (D.observe_command ledger event) (fun () ->
                match event with
                | Parser.Command_completed receipt when run ->
                    execute receipt.command_ast |> Result.map ignore
                | _ -> Ok ()));
      query = Some (D.observe_query ledger);
      reference = Some (D.observe_reference ledger);
      declaration = Some (D.observe ledger);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let source =
    Session.add_source session ~path:"selected-runtime.hc" ~contents
  in
  let parsed =
    Parser.parse ~commands:(sink false)
      ~execute_stream:(fun _ ->
        Ok
          Parser.
            {
              definitions = Session.definitions session;
              symbols = Session.symbols session;
              commands = sink true;
              finish = (fun () -> Ok "");
              abort = (fun () -> ());
            })
      ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config:(config ()) source
  in
  match parsed.Parser.ast with
  | Some ast -> execute ast
  | None -> Error parsed.diagnostics

let selected_runtime_global () =
  let session, runtime, ledger = runtime_setup () in
  let initial = compile_runtime session runtime "I64 N=40;" in
  execute_runtime_ok runtime initial;
  ignore
    (D.observe_admission ledger (admission runtime initial |> Option.get)
    |> checked);
  let result =
    selected_runtime_source session runtime ledger "N #exe {I64 N=100;} +2;"
    |> expect
  in
  Alcotest.(check int64)
    "original selected global survives nested shadow" 42L
    (VM.final_value result |> Option.get).bits

let selected_runtime_function () =
  let session, runtime, ledger = runtime_setup () in
  let initial = compile_runtime session runtime "I64 F(){return 42;}" in
  execute_runtime_ok runtime initial;
  ignore
    (D.observe_admission ledger (admission runtime initial |> Option.get)
    |> checked);
  let result =
    selected_runtime_source session runtime ledger
      "F #exe {I64 F(I64 n){return n;}};"
    |> expect
  in
  Alcotest.(check int64)
    "selected function retains original callable and argument shape" 42L
    (VM.final_value result |> Option.get).bits

let selected_runtime_expression_contexts () =
  List.iter
    (fun source ->
      let session, runtime, ledger = runtime_setup () in
      let initial =
        compile_runtime session runtime "I64 N=40;I64 F(){return 40;}"
      in
      execute_runtime_ok runtime initial;
      ignore
        (D.observe_admission ledger (admission runtime initial |> Option.get)
        |> checked);
      let result =
        selected_runtime_source session runtime ledger source |> expect
      in
      Alcotest.(check int64)
        source 42L (VM.final_value result |> Option.get).bits)
    [
      "I64 G(){return N #exe {I64 N=100;} +2;}G;";
      "I64 G(){return F #exe {I64 F(){return 100;}} +2;}G;";
      "I64 X=N #exe {I64 N=100;} +2;X;";
      "I64 X=F #exe {I64 F(){return 100;}} +2;X;";
      "I64 G(I64 N){return N #exe {I64 N=100;} +2;}G(40);";
    ]

let selected_absence_stays_absent () =
  List.iter
    (fun source ->
      let session, runtime, ledger = runtime_setup () in
      reject "later admitted same-name entry cannot fill selected absence"
        (selected_runtime_source session runtime ledger source);
      let later =
        selected_runtime_source session runtime ledger "Missing;" |> expect
      in
      Alcotest.(check int64)
        "reached nested publication survives the outer binding failure" 42L
        (VM.final_value later |> Option.get).bits)
    [
      "Missing #exe {I64 Missing=42;};";
      "I64 G(){return Missing #exe {I64 Missing=42;};}G;";
      "I64 X=Missing #exe {I64 Missing=42;};X;";
      "I64 X[Missing #exe {I64 Missing=42;}];";
    ]

let selected_defined_query () =
  List.iter
    (fun source ->
      let session, runtime, ledger = runtime_setup () in
      let result =
        selected_runtime_source session runtime ledger source |> expect
      in
      Alcotest.(check int64)
        "defined preserves selected absence" 0L
        (VM.final_value result |> Option.get).bits;
      let later =
        selected_runtime_source session runtime ledger "defined Missing;"
        |> expect
      in
      Alcotest.(check int64)
        "later query sees reached publication" 1L
        (VM.final_value later |> Option.get).bits)
    [
      "defined Missing #exe {I64 Missing=42;};";
      "I64 G(){return defined Missing #exe {I64 Missing=42;};}G;";
      "I64 N=defined Missing #exe {I64 Missing=42;};N;";
    ]

let selected_sizeof_query () =
  List.iter
    (fun source ->
      let session, runtime, ledger = runtime_setup () in
      let initial = compile_runtime session runtime "I64 N=40;" in
      execute_runtime_ok runtime initial;
      ignore
        (D.observe_admission ledger (admission runtime initial |> Option.get)
        |> checked);
      let result =
        selected_runtime_source session runtime ledger source |> expect
      in
      Alcotest.(check int64)
        "sizeof retains selected I64 record" 8L
        (VM.final_value result |> Option.get).bits)
    [
      "sizeof N #exe {U8 N=1;};";
      "I64 G(){return sizeof N #exe {U8 N=1;};}G;";
      "I64 Result=sizeof N #exe {U8 N=1;};Result;";
    ]

let selected_dimension_query () =
  let session, runtime, ledger = runtime_setup () in
  let initial = compile_runtime session runtime "I64 N=40;" in
  execute_runtime_ok runtime initial;
  ignore
    (D.observe_admission ledger (admission runtime initial |> Option.get)
    |> checked);
  let result =
    selected_runtime_source session runtime ledger
      "I64 A[sizeof N #exe {U8 N=1;}];A[7]=42;A[7];"
    |> expect
  in
  Alcotest.(check int64)
    "dimension consumes the selected constant size" 42L
    (VM.final_value result |> Option.get).bits

let selected_query_ownership () =
  let session, ledger = setup () in
  let _, foreign = setup () in
  let completed = ref [] in
  let observed = ref [] in
  let query event =
    reject "foreign ledger cannot consume query receipts"
      (D.observe_query foreign event);
    Result.map
      (fun () ->
        observed := event :: !observed;
        reject "each query phase rejects replay" (D.observe_query ledger event);
        match event with
        | Parser.Query_completed query -> completed := query :: !completed
        | _ -> ())
      (D.observe_query ledger event)
  in
  let output, _ =
    parse ~query session ledger
      "I64 N=sizeof N;defined N;sizeof I64.one.two;offset I64.one.two;defined \
       Missing;"
  in
  let ast = Test_parser.expect_ast output in
  let command = D.seal ledger ast |> expect in
  let table = Session.semantic_symbols session in
  let receipts = List.rev !completed in
  let read receipt =
    D.query_for ~table ~ast command receipt.Parser.query_expression |> expect
  in
  Alcotest.(check (list bool))
    "presence is independent of runtime admission"
    [ true; true; true; true; false ]
    (List.map (fun receipt -> D.query_presence (read receipt)) receipts);
  let first = List.hd receipts in
  let evidence = D.query_selection (read first) in
  let selection_origin =
    match first.query_root.query_node with
    | Parser.Sizeof_target identifier ->
        Semantic_symbol.Source_location
          {
            span = identifier.location.span;
            source_segments = identifier.location.source_segments;
            generated_from = identifier.location.generated_from;
            defined_at = identifier.location.defined_at;
          }
    | _ -> Alcotest.fail "expected sizeof root"
  in
  ignore
    (Semantic_query_selection.validate ~table
       ~role:Semantic_query_selection.Sizeof_root ~name:"N"
       ~origin:selection_origin evidence
    |> checked);
  reject "semantic query evidence rejects foreign table"
    (Semantic_query_selection.validate
       ~table:(Session.semantic_symbols (Session.create ()))
       ~role:Semantic_query_selection.Sizeof_root ~name:"N"
       ~origin:selection_origin evidence);
  reject "semantic query evidence rejects a different role"
    (Semantic_query_selection.validate ~table
       ~role:Semantic_query_selection.Defined_operand ~name:"N"
       ~origin:selection_origin evidence);
  Alcotest.(check bool)
    "query evidence keeps original expression" true
    (Semantic_query_selection.expression evidence == first.query_expression);
  ignore
    (Semantic_query_selection.validate_manifest ~table
       ~expression:first.query_expression [ evidence ]
    |> checked);
  reject "query manifest requires the original read"
    (Semantic_query_selection.validate_manifest ~table
       ~expression:first.query_expression []);
  reject "query manifest rejects foreign table"
    (Semantic_query_selection.validate_manifest
       ~table:(Session.semantic_symbols (Session.create ()))
       ~expression:first.query_expression [ evidence ]);
  reject "query manifest rejects another source occurrence"
    (Semantic_query_selection.validate_manifest ~table
       ~expression:(List.nth receipts 1).query_expression [ evidence ]);
  reject "query manifest rejects a repeated read"
    (Semantic_query_selection.validate_manifest ~table
       ~expression:first.query_expression [ evidence; evidence ]);
  Alcotest.(check bool)
    "repeated walks keep exact semantic evidence" true
    (evidence == D.query_selection (read first));
  Alcotest.(check bool)
    "self query retains provisional global metadata" true
    (match D.query_target (read first) with
    | D.Selected_source
        { stage = D.Global_selection (_, None); admitted = None; _ } -> true
    | _ -> false);
  List.iter
    (fun receipt ->
      let original = read receipt in
      Alcotest.(check bool)
        "repeat reads retain exact capability" true
        (original == read receipt);
      Alcotest.(check bool)
        "original parser completion retained" true
        (D.query_receipt original == receipt);
      reject "foreign table cannot borrow query receipt"
        (D.query_for
           ~table:(Session.semantic_symbols (Session.create ()))
           ~ast command receipt.query_expression);
      reject "rebuilt AST cannot borrow query receipt"
        (D.query_for ~table
           ~ast:(copy_module ast ast.items)
           command receipt.query_expression))
    receipts;
  List.iter
    (fun event ->
      reject "closed query cannot replay" (D.observe_query ledger event))
    !observed;
  let expression =
    match first.query_expression with
    | Ast.Sizeof_expression value ->
        Ast.Sizeof_expression
          (Ast.make_sizeof_expression
             ~keyword_spelling:value.sizeof_keyword_spelling
             ~keyword_location:value.sizeof_keyword_location
             ~opening_parentheses:value.sizeof_opening_parentheses
             ~target:value.sizeof_target ~members:value.sizeof_members
             ~pointer_layers:value.sizeof_pointer_layers
             ~closing_parentheses:value.sizeof_closing_parentheses
             ~location:value.sizeof_location)
    | _ -> Alcotest.fail "expected sizeof initializer"
  in
  reject "equal rebuilt expression cannot borrow receipt"
    (D.query_for ~table ~ast command expression)

let missing_query_phases () =
  List.iter
    (fun skip_root ->
      let session, ledger = setup () in
      let query event =
        match event with
        | Parser.Query_root _ when skip_root -> Ok ()
        | Parser.Query_root _ -> D.observe_query ledger event
        | Parser.Query_member_started start ->
            if skip_root then
              reject "dot requires its observed root"
                (D.observe_query ledger event)
            else if start.member_start_ordinal = 0 then
              ignore (D.observe_query ledger event |> expect)
            else
              reject "next dot requires the previous completed member"
                (D.observe_query ledger event);
            Ok ()
        | Parser.Query_member _ ->
            if skip_root then
              reject "member requires its observed root"
                (D.observe_query ledger event);
            Ok ()
        | Parser.Query_completed _ ->
            reject "completion requires every original read"
              (D.observe_query ledger event);
            Ok ()
      in
      let output, _ = parse ~query session ledger "sizeof I64.one.two;" in
      let ast = Test_parser.expect_ast output in
      let command = D.seal ledger ast |> expect in
      match ast.items with
      | [ Ast.Top_level_statement (Ast.Expression_statement statement) ] ->
          reject "missing completion has no sealed query capability"
            (D.query_for
               ~table:(Session.semantic_symbols session)
               ~ast command statement.expression_statement_expression)
      | _ -> Alcotest.fail "expected query statement")
    [ true; false ]

let rejected_query_completion () =
  List.iter
    (fun throws ->
      let session, ledger = setup () in
      let completed = ref None in
      let commands = ref [] in
      let checkpoint event =
        Result.map
          (fun () ->
            match event with
            | Parser.Command_completed command ->
                commands := command.command_ast :: !commands
            | _ -> ())
          (D.observe_command ledger event)
      in
      let query event =
        Result.bind (D.observe_query ledger event) (fun () ->
            match event with
            | Parser.Query_completed query ->
                completed := Some query;
                if throws then raise Exit
                else
                  Error
                    [
                      Diagnostic.make ~code:"TESTQUERY"
                        ~severity:Diagnostic.Error
                        ~message:"late query rejection"
                        ~primary:query.query_root.query_location.span ();
                    ]
            | _ -> Ok ())
      in
      let source =
        Session.add_source session ~path:"query-rejected.hc"
          ~contents:"40;defined Missing;"
      in
      (try
         let output, _ =
           parse_source ~query ~checkpoint session ledger source
         in
         Alcotest.(check bool)
           "late query rejection fails parsing" true (Parser.has_errors output);
         Alcotest.(check bool) "normal rejection expected" false throws
       with Exit -> Alcotest.(check bool) "exception expected" true throws);
      let completed = Option.get !completed in
      let expression = completed.query_expression in
      let item =
        Ast.Top_level_statement
          (Ast.Expression_statement
             (Ast.make_expression_statement ~expression ~semicolon:None
                ~location:(Ast.expression_location expression)))
      in
      let forged =
        Ast.make_module ~source:(Source_file.id source)
          ~span:(Ast.expression_location expression).span ~items:[ item ]
      in
      reject "recorded query cannot seal without its completed command"
        (D.seal ledger forged);
      Alcotest.(check int)
        "only preceding command completed" 1 (List.length !commands);
      ignore (D.seal ledger (List.hd !commands) |> expect);
      let output, _ = parse session ledger "defined Missing;" in
      ignore (D.seal ledger (Test_parser.expect_ast output) |> expect))
    [ false; true ]

let selected_reference_ownership () =
  let session, ledger = setup () in
  let selections = ref [] in
  let reference selection =
    Result.map
      (fun () ->
        reject "reference replay rejects while command is active"
          (D.observe_reference ledger selection);
        selections := selection :: !selections)
      (D.observe_reference ledger selection)
  in
  let output, _ =
    parse ~reference session ledger "I64 N=N;I64 F(I64 n=F()){return n+N;}F(2);"
  in
  let ast = Test_parser.expect_ast output in
  let command = D.seal ledger ast |> expect in
  let table = Session.semantic_symbols session in
  let target selection =
    D.reference_for ~table ~ast command (Parser.selected_identifier selection)
    |> expect
  in
  match List.rev !selections with
  | [ initial; provisional; local; completed_global; completed_function ] ->
      Alcotest.(check bool)
        "initializer keeps incomplete global stage" true
        (match target initial with
        | D.Selected_source
            { stage = D.Global_selection (_, None); admitted = None; _ } -> true
        | _ -> false);
      Alcotest.(check bool)
        "consumed provisional function cannot upgrade" true
        (match target provisional with
        | D.Selected_source { stage = D.Provisional_function_selection _; _ } ->
            true
        | _ -> false);
      Alcotest.(check bool)
        "local selection stays explicit" true
        (match target local with
        | D.Selected_local -> true
        | _ -> false);
      Alcotest.(check bool)
        "later occurrence sees completed global" true
        (match target completed_global with
        | D.Selected_source { stage = D.Global_selection (_, Some _); _ } ->
            true
        | _ -> false);
      Alcotest.(check bool)
        "completed function keeps its original body" true
        (match target completed_function with
        | D.Selected_source { stage = D.Function_selection (_, Some _); _ } ->
            true
        | _ -> false);
      let identifier = Parser.selected_identifier initial in
      let copied =
        Ast.make_identifier ~spelling:identifier.spelling
          ~location:identifier.location
      in
      reject "equal rebuilt identifier cannot borrow selection"
        (D.reference_for ~table ~ast command copied);
      reject "rebuilt module cannot borrow selection"
        (D.reference_for ~table
           ~ast:(copy_module ast ast.items)
           command identifier);
      reject "foreign table cannot borrow selection"
        (D.reference_for
           ~table:(Session.semantic_symbols (Session.create ()))
           ~ast command identifier);
      Alcotest.(check bool)
        "repeat reads keep the exact frozen target" true
        (target initial == target initial)
  | _ -> Alcotest.fail "expected five selected identifiers"

let command_runtime_ownership () =
  let session, runtime, ledger = runtime_setup () in
  let output, _ = parse session ledger "40+2;" in
  let ast = Test_parser.expect_ast output in
  let declaration_command = D.seal ledger ast |> expect in
  let other =
    VM.create_task_state ~table:(Session.semantic_symbols session) () |> checked
  in
  let before =
    Semantic_symbol_table.all_symbols (Session.semantic_symbols session)
    |> List.length
  in
  reject
    "same semantic table cannot transfer a parser command to another runtime"
    (Program.compile_task_ast ~task:other ~declaration_command session
       ~config:(config ()) ast);
  Alcotest.(check int)
    "foreign command creates no semantic artifacts" before
    (Semantic_symbol_table.all_symbols (Session.semantic_symbols session)
    |> List.length);
  Alcotest.(check int)
    "foreign command charges no preparation" 0
    (VM.task_initializer_steps other);
  reject "public reference resolver rejects a sibling runtime snapshot"
    (D.reference_resolver
       ~table:(Session.semantic_symbols session)
       ~ast
       ~task_view:(VM.task_snapshot other |> checked)
       declaration_command);
  let earlier_view = VM.task_snapshot runtime |> checked in
  let program =
    Program.compile_task_ast ~task:runtime ~declaration_command session
      ~config:(config ()) ast
    |> expect
  in
  execute_runtime_ok runtime program.value;
  List.iter
    (fun task_view ->
      Alcotest.(check bool)
        "owning catalog permits earlier and later snapshots" true
        (D.reference_resolver
           ~table:(Session.semantic_symbols session)
           ~ast ~task_view declaration_command
        |> Result.is_ok))
    [ earlier_view; VM.task_snapshot runtime |> checked ];
  let unbound = D.create session |> checked in
  let output, _ = parse session unbound "42;" in
  let ast = Test_parser.expect_ast output in
  let declaration_command = D.seal unbound ast |> expect in
  reject "semantic-only command grants no runtime authority"
    (Program.compile_task_ast ~task:runtime ~declaration_command session
       ~config:(config ()) ast)

let missing_reference_rejects_before_execution () =
  let session, runtime, ledger = runtime_setup () in
  let output, _ = parse session ledger "I64 N=40;N+=2;" in
  let ast = Test_parser.expect_ast output in
  let declaration_command = D.seal ledger ast |> expect in
  reject "parser-aware compilation requires every original reference receipt"
    (Program.compile_task_ast ~task:runtime ~declaration_command session
       ~config:(config ()) ast);
  Alcotest.(check int)
    "missing evidence executes no instructions" 0
    (VM.task_executed_steps runtime);
  Alcotest.(check bool)
    "missing evidence admits no declaration" true
    (Option.is_none (VM.latest_task_admission runtime))

let selected_reference_admission_stage () =
  let session, runtime, ledger = runtime_setup () in
  let output, _ = parse session ledger "I64 N=40;" in
  let ast = Test_parser.expect_ast output in
  let declaration_command = D.seal ledger ast |> expect in
  let pending =
    Program.compile_task_ast ~task:runtime ~declaration_command session
      ~config:(config ()) ast
    |> expect
  in
  let reference () =
    let selected = ref None in
    let output, _ =
      parse session ledger "N;" ~reference:(fun selection ->
          Result.map
            (fun () -> selected := Some selection)
            (D.observe_reference ledger selection))
    in
    let ast = Test_parser.expect_ast output in
    let command = D.seal ledger ast |> expect in
    fun () ->
      D.reference_for
        ~table:(Session.semantic_symbols session)
        ~ast command
        (Parser.selected_identifier (Option.get !selected))
      |> expect
  in
  let before = reference () in
  execute_runtime_ok runtime pending.value;
  Alcotest.(check bool)
    "later admission cannot upgrade consumed reference" true
    (match before () with
    | D.Selected_source { admitted = None; _ } -> true
    | _ -> false);
  let after = reference () in
  Alcotest.(check bool)
    "later reference captures exact reached admission" true
    (match after () with
    | D.Selected_source { publication; admitted = Some admitted; _ } ->
        C.publication_symbol publication == VM.admitted_source_symbol admitted
    | _ -> false)

let selected_reference_resume_admission () =
  let session, runtime, ledger = runtime_setup () in
  let pending = ref None in
  let selected = ref None in
  let checkpoint event =
    Result.bind (D.observe_command ledger event) (fun () ->
        match event with
        | Parser.Command_completed receipt when Option.is_none !pending ->
            let ast = receipt.command_ast in
            Result.bind (D.seal ledger ast) (fun declaration_command ->
                Program.compile_task_ast ~task:runtime ~declaration_command
                  session ~config:(config ()) ast
                |> Result.map (fun program -> pending := Some program.value))
        | Parser.Command_resumed receipt
          when receipt.command_start.command_ordinal = 0 ->
            Alcotest.(check bool)
              "resume follows lookahead before reference delivery" true
              (Option.is_none !selected);
            Alcotest.(check bool)
              "first unit is still unadmitted before resume" true
              (Option.is_none (admission runtime (Option.get !pending)));
            execute_runtime_ok runtime (Option.get !pending);
            Ok ()
        | _ -> Ok ())
  in
  let reference selection =
    Result.map
      (fun () -> selected := Some selection)
      (D.observe_reference ledger selection)
  in
  let command = ref None in
  let output, _ =
    parse ~reference
      ~checkpoint:(fun event ->
        Result.bind (checkpoint event) (fun () ->
            match event with
            | Parser.Command_completed receipt
              when receipt.command_start.command_ordinal = 1 ->
                command := Some receipt.command_ast;
                Ok ()
            | _ -> Ok ()))
      session ledger "I64 N=40;N;"
  in
  ignore (Test_parser.expect_ast output);
  let ast = Option.get !command in
  let command = D.seal ledger ast |> expect in
  let target =
    D.reference_for
      ~table:(Session.semantic_symbols session)
      ~ast command
      (Parser.selected_identifier (Option.get !selected))
    |> expect
  in
  Alcotest.(check bool)
    "buffered identifier consumes the admitted selected record" true
    (match target with
    | D.Selected_source { admitted = Some _; _ } -> true
    | _ -> false)

let selected_sizeof_constructor_shape () =
  let module F = Semantic_function_collection in
  let module B = Semantic_function_binding_index in
  let module E = Semantic_function_expression_binding in
  let module M = Semantic_module_expression_binding in
  let module Q = Semantic_function_call_resolution in
  let session, ledger = setup () in
  let receipts = ref [] in
  let query event =
    Result.map
      (fun () ->
        match event with
        | Parser.Query_completed receipt -> receipts := receipt :: !receipts
        | _ -> ())
      (D.observe_query ledger event)
  in
  let output, _ =
    parse ~query session ledger "I64 F(){sizeof U8;sizeof U8*;}"
  in
  let ast = Test_parser.expect_ast output in
  let command = D.seal ledger ast |> expect in
  let table = Session.semantic_symbols session in
  let declarations = D.collection ~table ~ast command |> expect in
  let parent = C.scope declarations in
  let functions = collect_functions session ~declarations ast |> checked in
  let function_ = List.hd (F.functions functions) in
  let symbol = F.function_symbol function_ in
  let scope = F.function_scope function_ in
  let item_index = F.function_item_index function_ in
  let bindings =
    B.build ~table ~parent
      [
        {
          B.function_symbol = symbol;
          function_scope = scope;
          function_item_index = item_index;
          function_bindings = [];
        };
      ]
    |> Result.map_error B.error_to_string
    |> checked
  in
  let source_origin (location : Ast.location) =
    Semantic_symbol.Source_location
      {
        span = location.span;
        source_segments = location.source_segments;
        generated_from = location.generated_from;
        defined_at = location.defined_at;
      }
  in
  let receipts = List.rev !receipts in
  let events =
    List.map
      (fun receipt ->
        let selection =
          D.query_for ~table ~ast command receipt.Parser.query_expression
          |> expect |> D.query_selection
        in
        match receipt.query_expression with
        | Ast.Sizeof_expression expression ->
            E.make_selected_name_query ~selection ~role:E.Sizeof_root
              ~name:expression.sizeof_target.spelling
              ~origin:(source_origin expression.sizeof_target.location)
            |> checked
        | _ -> Alcotest.fail "expected original sizeof expression")
      receipts
  in
  let input = E.make_function ~symbol ~scope ~item_index events |> checked in
  let expressions =
    E.resolve ~table ~parent ~bindings [ input ]
    |> Result.map_error E.error_to_string
    |> checked
  in
  let publication =
    M.make_publication ~source_symbol:symbol ~canonical_symbol:symbol
      ~publication_kind:M.Function ~declaration_index:0 ~item_index ()
    |> checked
  in
  let module_ =
    M.resolve ~table ~parent ~compilation_mode:Semantic_function_resolution.Jit
      ~expressions [ publication ]
    |> Result.map_error M.error_to_string
    |> checked
  in
  let queries = M.functions module_ |> List.hd |> M.function_queries in
  let originals =
    List.map
      (fun receipt ->
        match receipt.Parser.query_expression with
        | Ast.Sizeof_expression expression -> expression
        | _ -> Alcotest.fail "expected sizeof")
      receipts
  in
  let make expression query ?(members = []) ?pointer_layers () =
    let layers =
      Option.value pointer_layers
        ~default:
          (List.map
             (fun (layer : Ast.pointer_layer) ->
               Q.make_sizeof_pointer_layer ~depth:layer.depth
                 ~spelling:layer.spelling
                 ~origin:(source_origin layer.location)
               |> checked)
             expression.Ast.sizeof_pointer_layers)
    in
    Q.make_sizeof_argument_expression
      ~keyword_spelling:expression.sizeof_keyword_spelling
      ~keyword_origin:(source_origin expression.sizeof_keyword_location)
      ~opening_origins:
        (List.map source_origin expression.sizeof_opening_parentheses)
      ~target_spelling:expression.sizeof_target.spelling
      ~target_origin:(source_origin expression.sizeof_target.location)
      ~members ~pointer_layers:layers
      ~closing_origins:
        (List.map source_origin expression.sizeof_closing_parentheses)
      ~root_resolution:(Q.Sizeof_function_query (Q.Module_query query))
      ~bound_aggregate_size:None ~bound_target:None
  in
  List.iter2
    (fun (expression, query) expected ->
      match make expression query () |> checked with
      | Q.Sizeof_expression result ->
          Alcotest.(check (option int64))
            "original selected query constant" (Some expected)
            (Q.sizeof_known_value result)
      | _ -> Alcotest.fail "expected semantic sizeof")
    (List.combine originals queries)
    [ 1L; 8L ];
  let scalar = List.nth originals 0 and pointer = List.nth originals 1 in
  let scalar_query = List.nth queries 0
  and pointer_query = List.nth queries 1 in
  reject "selected pointer suffix cannot be removed"
    (make pointer pointer_query ~pointer_layers:[] ());
  let layer = List.hd pointer.sizeof_pointer_layers in
  let semantic_layer =
    Q.make_sizeof_pointer_layer ~depth:layer.depth ~spelling:layer.spelling
      ~origin:(source_origin layer.location)
    |> checked
  in
  reject "selected scalar cannot acquire a pointer suffix"
    (make scalar scalar_query ~pointer_layers:[ semantic_layer ] ());
  let member_origin = source_origin scalar.sizeof_target.location in
  let member =
    Q.make_sizeof_member ~lookup:None ~dot_origin:member_origin ~name:"extra"
      ~name_origin:member_origin ~origin:member_origin
    |> checked
  in
  reject "selected query cannot acquire a member"
    (make scalar scalar_query ~members:[ member ] ())

let sizeof_native_member_boundaries () =
  List.iter
    (fun (source, expected) ->
      let session, runtime, ledger = runtime_setup () in
      ignore
        (selected_runtime_source session runtime ledger "I64 N=40;" |> expect);
      reject "invalid sizeof member rejects"
        (selected_runtime_source session runtime ledger source);
      let result =
        selected_runtime_source session runtime ledger "N;" |> expect
      in
      Alcotest.(check int64)
        "only effects before the native member boundary run" expected
        (VM.final_value result |> Option.get).bits)
    [
      ("sizeof Missing #exe {N=1;} *;", 40L);
      ("sizeof I64i . #exe {N=1;} bad;", 40L);
      ("sizeof N . #exe {N=1;} bad #exe {N=2;};", 1L);
    ]

let aggregate_query_table_ownership () =
  let module L = Semantic_aggregate_layout in
  let capture () =
    let session, ledger = setup () in
    let receipt = ref None in
    let query event =
      Result.map
        (fun () ->
          match event with
          | Parser.Query_completed value -> receipt := Some value
          | _ -> ())
        (D.observe_query ledger event)
    in
    let output, _ = parse ~query session ledger "sizeof U8;" in
    let ast = Test_parser.expect_ast output in
    let command = D.seal ledger ast |> expect in
    let table = Session.semantic_symbols session in
    let query =
      D.query_for ~table ~ast command (Option.get !receipt).query_expression
      |> expect |> D.query_selection
    in
    (table, query)
  in
  let table, own = capture () in
  let _, foreign = capture () in
  let namespace = C.create_namespace ~table () |> checked in
  let parent = C.namespace_scope namespace in
  let origin = Semantic_symbol.Synthesized "query layout owner" in
  let symbol =
    Semantic_symbol_table.add table ~scope:parent ~name:"Owner"
      ~kind:Semantic_symbol.Aggregate_type ~origin
    |> checked
  in
  let scope =
    Semantic_symbol_table.create_scope table ~parent
      ~kind:Semantic_symbol_table.Aggregate ()
    |> checked
  in
  let member_symbol =
    Semantic_symbol_table.add table ~scope ~name:"values"
      ~kind:Semantic_symbol.Member ~origin
    |> checked
  in
  let type_ =
    Semantic_type.make_primitive ~form:Semantic_type.Public_spelling
      ~primitive:Primitive_type.U8 ~pointer_depth:0
    |> checked
  in
  let accepts query wrap =
    let expression = L.Selected_query_expression query in
    let field =
      L.Field
        {
          L.member_symbol;
          member_path = [ 0 ];
          member_declarator_index = 0;
          member_origin = origin;
          member_type = type_;
          member_is_function_pointer = false;
          member_dimensions =
            [
              {
                dimension_expression = Some expression;
                dimension_origin = origin;
              };
            ];
        }
    in
    let items =
      match wrap with
      | 0 -> [ L.Offset_directive expression ]
      | 1 ->
          [
            L.Offset_directive
              (L.Unary_expression
                 { operator = L.Identity; operand = expression; origin });
          ]
      | 2 ->
          [
            L.Offset_directive
              (L.Binary_expression
                 {
                   operator = L.Add;
                   left = expression;
                   right = L.Integer_expression { value = 1L; origin };
                   origin;
                 });
          ]
      | 3 -> [ field ]
      | _ ->
          [
            L.Anonymous_union { union_origin = origin; union_items = [ field ] };
          ]
    in
    L.layout ~table ~parent
      [
        {
          L.aggregate_symbol = symbol;
          aggregate_scope = scope;
          aggregate_kind = L.Class;
          aggregate_item_index = 0;
          aggregate_origin = origin;
          aggregate_base = None;
          aggregate_items = items;
        };
      ]
    |> Result.is_ok
  in
  let shapes = [ 0; 1; 2; 3; 4 ] in
  Alcotest.(check (list bool))
    "own query metadata works in every layout expression"
    [ true; true; true; true; true ]
    (List.map (accepts own) shapes);
  Alcotest.(check (list bool))
    "foreign query metadata cannot enter a checked layout"
    [ false; false; false; false; false ]
    (List.map (accepts foreign) shapes)

let selected_local_sizeof () =
  List.iter
    (fun source ->
      let session, runtime, ledger = runtime_setup () in
      let result =
        selected_runtime_source session runtime ledger source |> expect
      in
      Alcotest.(check int64)
        "selected scalar local uses its original declared size" 42L
        (VM.final_value result |> Option.get).bits)
    [
      "I64 F(){I64 N;return sizeof N+34;}F();";
      "I64 F(U8 N){return sizeof N+41;}F(0);";
      "I64 F(){I64 N=sizeof N;return N+34;}F();";
      "I64 F(){U8 *P;return sizeof P+34;}F();";
      "I64 F(){U8 N;return sizeof N #exe {I64 N=7;}+41;}F();";
    ]

let seeded_public_sizeof () =
  List.iter
    (fun (spelling, size, line) ->
      let original = Session.create () in
      List.iter
        (fun session ->
          let symbols = Session.symbols session in
          let entry =
            match
              Symbol_visibility.Environment.find_preprocessor symbols spelling
            with
            | Symbol_visibility.Present entry -> entry
            | _ -> Alcotest.fail "public union is missing"
          in
          Alcotest.(check bool)
            "public primitive is a class with its original union origin" true
            (Symbol_visibility.kind entry = Symbol_visibility.Class
            && Symbol_visibility.origin entry
               = Symbol_visibility.Pinned_source
                   { path = "Kernel/KernelA.HH"; line });
          let binding = Session.primitive_for session entry |> Option.get in
          let symbol = Session.primitive_symbol binding in
          Alcotest.(check bool)
            "exact seeded symbol belongs to this semantic table" true
            (Semantic_symbol.kind symbol = Semantic_symbol.Aggregate_type
            && Semantic_symbol_table.owns_symbol
                 (Session.semantic_symbols session)
                 symbol);
          let ledger = D.create session |> checked in
          let output, _ = parse session ledger ("sizeof " ^ spelling ^ ";") in
          let ast = Test_parser.expect_ast output in
          let command = D.seal ledger ast |> expect in
          let expression =
            match ast.items with
            | [ Ast.Top_level_statement (Ast.Expression_statement item) ] ->
                item.expression_statement_expression
            | _ -> Alcotest.fail "expected sizeof expression"
          in
          let query =
            D.query_for
              ~table:(Session.semantic_symbols session)
              ~ast command expression
            |> expect
          in
          Alcotest.(check (option int64))
            "public union size comes from its seeded record" (Some size)
            (D.query_selection query |> Semantic_query_selection.constant))
        [
          original;
          Session.fork_frontend original;
          Session.task_frontend original;
        ])
    [
      ("U16", 2L, 67);
      ("I16", 2L, 73);
      ("U32", 4L, 79);
      ("I32", 4L, 87);
      ("U64", 8L, 95);
      ("I64", 8L, 105);
    ]

let local_query_source_children () =
  let session, ledger = setup () in
  let receipts = ref [] in
  let query event =
    Result.map
      (fun () ->
        match event with
        | Parser.Query_completed receipt -> receipts := receipt :: !receipts
        | _ -> ())
      (D.observe_query ledger event)
  in
  let output, _ =
    parse ~query session ledger
      "I64 F(U8 P){I64 N=sizeof N;sizeof P;defined N;}I64 V(...){sizeof \
       argc;sizeof argv;sizeof argv*;}"
  in
  let ast = Test_parser.expect_ast output in
  let command = D.seal ledger ast |> expect in
  let table = Session.semantic_symbols session in
  let definitions =
    List.filter_map
      (function
        | Ast.Function_definition definition -> Some definition
        | _ -> None)
      ast.items
  in
  let function_ = List.hd definitions in
  let rec find_local = function
    | Ast.Local_declaration_statement declaration -> Some declaration
    | Ast.Block_statement body -> List.find_map find_local body.block_statements
    | Ast.Sequence_statement sequence ->
        List.find_map
          (fun (element : Ast.statement_sequence_element) ->
            find_local element.sequence_statement)
          sequence.sequence_elements
    | _ -> None
  in
  let declaration = Option.bind function_.body find_local |> Option.get in
  let local = List.hd declaration.local_declarators in
  let receipts = List.rev !receipts in
  let sources =
    List.map
      (fun (receipt : Parser.completed_query) ->
        let source = receipt.query_root.query_local |> Option.get in
        Alcotest.(check bool)
          "local publication owns the query command and environment" true
          (source.local_command == receipt.query_root.query_command
          && source.local_environment == receipt.query_root.query_environment);
        source.local_source)
      receipts
  in
  (match (List.nth sources 0, List.nth sources 1, List.nth sources 2) with
  | ( Parser.Local_variable first,
      Parser.Local_parameter parameter,
      Parser.Local_variable again ) ->
      Alcotest.(check bool)
        "local metadata preserves exact completed source children" true
        (first.local_name == local.local_name
        && first.local_type_specifier == declaration.local_type_specifier
        && first.local_pointer_layers == local.local_pointer_layers
        && first.local_array_dimensions == local.local_array_dimensions
        && parameter == List.hd function_.parameters
        && first.local_name == again.local_name
        && List.nth sources 0 == List.nth sources 2)
  | _ -> Alcotest.fail "expected original local and parameter sources");
  let variadic = (List.nth definitions 1).variadic |> Option.get in
  (match (List.nth sources 3, List.nth sources 4) with
  | Parser.Variadic_count count, Parser.Variadic_vector vector ->
      Alcotest.(check bool)
        "implicit locals retain the actual variadic marker" true
        (count == variadic && vector == variadic)
  | _ -> Alcotest.fail "expected original variadic sources");
  Alcotest.(check (list (option int64)))
    "original local sizes and presence remain available after scope exit"
    [ Some 8L; Some 1L; Some 1L; Some 8L; Some 1016L; Some 8L ]
    (List.map
       (fun (receipt : Parser.completed_query) ->
         D.query_for ~table ~ast command receipt.query_expression
         |> expect |> D.query_selection |> Semantic_query_selection.constant)
       receipts)

let local_query_dot_boundary () =
  List.iter
    (fun (declaration, expected) ->
      let session, runtime, ledger = runtime_setup () in
      ignore
        (selected_runtime_source session runtime ledger "I64 N=40;" |> expect);
      reject "local member lookup requires checked member metadata"
        (selected_runtime_source session runtime ledger
           ("I64 F(){" ^ declaration
          ^ ";return sizeof Local. #exe {N=1;} member;}"));
      let result =
        selected_runtime_source session runtime ledger "N;" |> expect
      in
      Alcotest.(check int64)
        "declared class kind controls member-token effects" expected
        (VM.final_value result |> Option.get).bits)
    [
      ("I64i Local", 40L);
      ("U8 Local", 40L);
      ("I64 Local", 1L);
      ("U8 *Local", 1L);
      ("I64 (*Local)()", 1L);
    ]

let source_command_ownership () =
  let session, runtime, runtime_ledger = runtime_setup () in
  let source =
    Session.add_source session ~path:"declarations.hc"
      ~contents:"I64 N=sizeof N;"
  in
  let foreign_source =
    Session.add_source (Session.create ()) ~path:"declarations.hc"
      ~contents:"I64 N=sizeof N;"
  in
  let scope_count =
    Session.semantic_symbols session
    |> Semantic_symbol_table.all_scopes |> List.length
  in
  reject "source factory rejects equal input from another source manager"
    (D.create_source session ~source:foreign_source);
  Alcotest.(check int)
    "rejected source does not allocate a namespace" scope_count
    (Session.semantic_symbols session
    |> Semantic_symbol_table.all_scopes |> List.length);
  let ledger = D.create_source session ~source |> checked in
  let output, _ = parse_source session ledger source in
  let ast = Test_parser.expect_ast output in
  let command = D.seal_source ledger ast |> expect in
  let table = Session.semantic_symbols session in
  let original = D.source_collection ~table ~ast command |> expect in
  Alcotest.(check (option string))
    "ordinary source retains its module scope name" (Some "declarations.hc")
    (C.scope original |> Semantic_symbol_table.scope_name);
  let task_command = D.seal ledger ast |> expect in
  Alcotest.(check bool)
    "source collection preserves original publications" true
    (D.collection ~table ~ast task_command |> expect == original);
  reject "ordinary source grants no task execution authority"
    (Program.compile_task_ast ~task:runtime ~declaration_command:task_command
       session ~config:(config ()) ast);
  let copied = copy_module ast ast.items in
  reject "source collection rejects rebuilt module"
    (D.source_collection ~table ~ast:copied command);
  reject "source collection rejects foreign table"
    (D.source_collection
       ~table:(Session.semantic_symbols (Session.create ()))
       ~ast command);
  reject "source seal rejects rebuilt module" (D.seal_source ledger copied);
  let expression =
    match ast.items with
    | [ Ast.Global_declaration declaration ] -> (
        match
          Option.map
            (fun (initial : Ast.global_initializer) ->
              initial.global_initializer_value)
            (List.hd declaration.declarators).global_initial_value
        with
        | Some (Ast.Scalar_initializer expression) -> expression
        | _ -> Alcotest.fail "expected expression initializer")
    | _ -> Alcotest.fail "expected original declaration"
  in
  ignore (D.source_query_for ~table ~ast command expression |> expect);
  reject "source query rejects a foreign AST"
    (D.source_query_for ~table ~ast:copied command expression);
  reject "source query rejects a foreign table"
    (D.source_query_for
       ~table:(Session.semantic_symbols (Session.create ()))
       ~ast command expression);
  let copied_expression =
    match expression with
    | Ast.Sizeof_expression sizeof -> Ast.Sizeof_expression sizeof
    | _ -> Alcotest.fail "expected sizeof query"
  in
  reject "source query rejects a reconstructed occurrence"
    (D.source_query_for ~table ~ast command copied_expression);
  let other_source =
    Session.add_source session ~path:"other.hc" ~contents:"42;"
  in
  let output, _ = parse_source session ledger other_source in
  Alcotest.(check bool)
    "source ledger rejects another root input" true
    (Option.is_none output.ast);
  List.iter
    (fun other ->
      let output, _ = parse session other "42;" in
      let ast = Test_parser.expect_ast output in
      reject "analysis and runtime ledgers cannot acquire ordinary source seals"
        (D.seal_source other ast))
    [ D.create session |> checked; runtime_ledger ]

let tests =
  [
    Alcotest.test_case "ordinary source seals retain distinct ownership" `Quick
      source_command_ownership;
    Alcotest.test_case "public union sizeof owns exact seeded metadata" `Quick
      seeded_public_sizeof;
    Alcotest.test_case "local query metadata owns original source children"
      `Quick local_query_source_children;
    Alcotest.test_case "local query dot precedes member-token effects" `Quick
      local_query_dot_boundary;
    Alcotest.test_case "local sizeof retains selected source metadata" `Quick
      selected_local_sizeof;
    Alcotest.test_case "query seals own original reads and AST children" `Quick
      selected_query_ownership;
    Alcotest.test_case "query seals require root, members and completion" `Quick
      missing_query_phases;
    Alcotest.test_case "rejected query completion cannot acquire command seal"
      `Quick rejected_query_completion;
    Alcotest.test_case "defined retains presence at query consumption" `Quick
      selected_defined_query;
    Alcotest.test_case "sizeof retains its selected compiler record" `Quick
      selected_sizeof_query;
    Alcotest.test_case "constant dimensions consume selected queries" `Quick
      selected_dimension_query;
    Alcotest.test_case
      "selected sizeof constructor preserves its original suffix" `Quick
      selected_sizeof_constructor_shape;
    Alcotest.test_case "sizeof member validation precedes its next native Lex"
      `Quick sizeof_native_member_boundaries;
    Alcotest.test_case "aggregate layout owns all selected query metadata"
      `Quick aggregate_query_table_ownership;
    Alcotest.test_case "parser command seals retain their exact runtime owner"
      `Quick command_runtime_ownership;
    Alcotest.test_case "missing selections reject before runtime effects" `Quick
      missing_reference_rejects_before_execution;
    Alcotest.test_case "selected absence survives later nested publication"
      `Quick selected_absence_stays_absent;
    Alcotest.test_case
      "nested shadow preserves selected function and call shape" `Quick
      selected_runtime_function;
    Alcotest.test_case "selected bindings cross body and initializer walks"
      `Quick selected_runtime_expression_contexts;
    Alcotest.test_case
      "buffered reference sees same-record admission before consumption" `Quick
      selected_reference_resume_admission;
    Alcotest.test_case
      "reference seals freeze consumed source stage and ownership" `Quick
      selected_reference_ownership;
    Alcotest.test_case "consumed references cannot gain later runtime admission"
      `Quick selected_reference_admission_stage;
    Alcotest.test_case
      "selected runtime global survives nested parser execution" `Quick
      selected_runtime_global;
    Alcotest.test_case "checked runtime compilation owns preparation charges"
      `Quick runtime_compilation_budget;
    Alcotest.test_case
      "checked runtime compilation rejects foreign owner and mode" `Quick
      runtime_compilation_owner;
    Alcotest.test_case
      "runtime admission receipts own exact publications and task" `Quick
      runtime_admission_ownership;
    Alcotest.test_case
      "runtime admission separates preflight from reached faults" `Quick
      runtime_admission_boundaries;
    Alcotest.test_case "runtime receipt delivery cannot reverse admission order"
      `Quick runtime_admission_order;
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
