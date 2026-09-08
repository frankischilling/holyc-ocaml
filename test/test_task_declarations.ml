open Holyc_lib
module D = Task_declarations
module C = Semantic_declaration_collection

let checked = Test_declaration_collection.checked
let expect = Test_integer_program.checked

let reject message result =
  Alcotest.(check bool) message true (Result.is_error result)

let config () = Preprocessor.Config.create () |> checked

let parse_source ?sources ?symbols ?observe session ledger source =
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

let parse ?observe session ledger text =
  let source =
    Session.add_source session ~path:"declarations.hc" ~contents:text
  in
  parse_source ?observe session ledger source

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
  let output, events =
    parse ~observe:(fun _ -> Ok ()) session ledger "I64 F(){return 42;}"
  in
  let ast = Test_parser.expect_ast output in
  match events with
  | [ declared; header; body ] ->
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
  let sink command : Parser.command_sink =
    {
      reference = None;
      declaration = Some consume;
      command;
      resume = (fun () -> Ok ());
    }
  in
  let span =
    Span.unsafe_make ~source:(Source_file.id source) ~start:0
      ~stop:(Source_file.length source)
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
            sink (fun item ->
                let ast =
                  Ast.make_module ~source:(Source_file.id source) ~span
                    ~items:[ item ]
                in
                D.seal ledger ast
                |> Result.map (fun command ->
                    nested := (ast, command) :: !nested));
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

let tests =
  [
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
