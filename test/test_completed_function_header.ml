open Holyc_lib
module C = Semantic_declaration_collection
module R = Semantic_compiler_record
module H = Semantic_function_type_resolution

let checked = Test_declaration_collection.checked

let parse ?(observe = fun _ _ _ -> ())
    ?(type_header =
      fun session namespace declared ->
        resolve_completed_function_header session ~namespace declared |> checked)
    text =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let source =
    Session.add_source session ~path:"completed-header.hc" ~contents:text
  in
  let publications = ref [] and headers = ref [] in
  let commands : Parser.command_sink =
    {
      checkpoint = None;
      reference = None;
      implicit_output = None;
      query = None;
      dimension_count = None;
      declaration =
        Some
          (fun event ->
            (match event with
            | Parser.Function_declared source ->
                publications :=
                  (source, C.publish_function namespace source |> checked)
                  :: !publications
            | Parser.Function_header_completed header ->
                let publication =
                  List.assq header.function_publication !publications
                in
                Alcotest.(check bool)
                  "exact header callback is active" true
                  (Parser.function_header_is_current header);
                let declared =
                  R.declare_function ~table ~namespace publication header
                  |> checked
                in
                let typed = type_header session namespace declared in
                headers := (header, publication, declared, typed) :: !headers;
                observe session namespace (header, publication, declared)
            | _ -> ());
            Ok ());
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let result =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create () |> checked)
      source
  in
  Alcotest.(check bool) "header fixture parses" true (Option.is_some result.ast);
  (session, namespace, List.rev !headers)

let source_children () =
  let session, namespace, headers =
    parse "I64 F(I8 n=40,U8 *p,...){return n;}"
  in
  let original, publication, declared, typed = List.hd headers in
  Alcotest.(check bool)
    "original completed header" true
    (R.declared_function_source declared == original);
  Alcotest.(check bool)
    "original publication symbol" true
    (H.function_symbol typed == C.publication_symbol publication);
  Alcotest.(check bool)
    "owned namespace" true
    (R.declared_function_owns_namespace declared namespace);
  Alcotest.(check bool)
    "owned table" true
    (R.declared_function_owns_table declared (Session.semantic_symbols session));
  let parameters = H.function_signature typed |> H.signature_parameters in
  List.iter2
    (fun source parameter ->
      Alcotest.(check bool)
        "exact parameter AST" true
        (Option.get (H.parameter_source parameter) == source))
    original.parameters parameters;
  Alcotest.(check bool)
    "variadic bindings before a body" true
    (Option.is_some (H.function_variadic_bindings typed));
  Alcotest.(check bool)
    "callback expires" false
    (Parser.function_header_is_current original);
  Alcotest.(check bool)
    "stale callback cannot create proof" true
    (Result.is_error
       (R.declare_function
          ~table:(Session.semantic_symbols session)
          ~namespace publication original))

let foreign () =
  ignore
    (parse
       ~observe:(fun session namespace (header, publication, declared) ->
         let table = Session.semantic_symbols session in
         let other = C.create_namespace ~table () |> checked in
         Alcotest.(check bool)
           "foreign namespace" true
           (Result.is_error
              (R.declare_function ~table ~namespace:other publication header));
         Alcotest.(check bool)
           "typing cannot borrow another namespace" true
           (Result.is_error
              (resolve_completed_function_header session ~namespace:other
                 declared));
         let foreign = Session.create () in
         Alcotest.(check bool)
           "foreign table" true
           (Result.is_error
              (R.declare_function
                 ~table:(Session.semantic_symbols foreign)
                 ~namespace publication header));
         let fake =
           C.publish namespace ~name:"F" ~kind:Semantic_symbol.Function
             ~origin:(Semantic_symbol.origin (C.publication_symbol publication))
           |> checked
         in
         Alcotest.(check bool)
           "matching unowned publication" true
           (Result.is_error (R.declare_function ~table ~namespace fake header)))
       "I64 F(){return 42;}")

let callback_exception () =
  let saved = ref None in
  (try
     ignore
       (parse
          ~observe:(fun _ _ (header, _, _) ->
            saved := Some header;
            raise Exit)
          "I64 F(){return 42;}")
   with Exit -> ());
  Alcotest.(check bool)
    "exception closes header callback" false
    (Parser.function_header_is_current (Option.get !saved))

let signature_parity () =
  let module T = Test_function_type_resolution in
  List.iter
    (fun text ->
      let _, _, headers = parse text in
      let original, _, _, typed = List.hd headers in
      let session = Session.create () in
      let ast = T.parse session ~path:"complete-signature.hc" text in
      let complete =
        T.resolve session ast |> fun results -> T.function_named results "F"
      in
      Alcotest.(check string)
        "completed header agrees with ordinary signature"
        (T.type_signature complete)
        (T.type_signature typed);
      let rec compare_metadata expected actual =
        Alcotest.(check bool)
          "nested variadic marker"
          (Option.is_some (H.signature_variadic_origin expected))
          (Option.is_some (H.signature_variadic_origin actual));
        Alcotest.(check (list int))
          "variadic register requests"
          (H.signature_variadic_register_requests expected |> T.request_codes)
          (H.signature_variadic_register_requests actual |> T.request_codes);
        List.iter2
          (fun expected actual ->
            Alcotest.(check string)
              "default kind" (T.default_kind expected) (T.default_kind actual);
            Alcotest.(check (list int))
              "parameter register requests"
              (H.parameter_register_requests expected |> T.request_codes)
              (H.parameter_register_requests actual |> T.request_codes);
            match
              ( H.parameter_declarator_kind expected,
                H.parameter_declarator_kind actual )
            with
            | H.Function_pointer expected, H.Function_pointer actual ->
                compare_metadata
                  (H.function_pointer_signature expected)
                  (H.function_pointer_signature actual)
            | H.Object, H.Object -> ()
            | _ -> Alcotest.fail "nested signature kind changed")
          (H.signature_parameters expected)
          (H.signature_parameters actual)
      in
      compare_metadata
        (H.function_signature complete)
        (H.function_signature typed);
      let rec source_parameters sources parameters =
        List.iter2
          (fun (source : Ast.function_parameter) parameter ->
            Alcotest.(check bool)
              "nested original parameter" true
              (Option.get (H.parameter_source parameter) == source);
            match
              (source.function_pointer, H.parameter_declarator_kind parameter)
            with
            | Some source, H.Function_pointer pointer ->
                source_parameters source.signature_parameters
                  (H.function_pointer_signature pointer
                  |> H.signature_parameters)
            | None, H.Object -> ()
            | _ -> Alcotest.fail "callback shape changed")
          sources parameters
      in
      source_parameters original.parameters
        (H.function_signature typed |> H.signature_parameters))
    [
      "extern I64i *F(I64 plain,I64,U8 *byte,I16i **storage,F64 ****wide);";
      "extern U8 *F(I64 first=1,I64 middle,I64 last=lastclass,U0 \
       (**handler)(U8 *node,I64=lastclass,U8 *(*nested)(I64 \
       value,...),...),...);";
      "extern U0 F(I64 plain,reg I64 allocate,noreg U8 stack,I64 reg R15 \
       exact,reg RAX U16,reg R14 noreg I32 reg R13 last,I64 (*callback)(reg \
       R11 noreg U8 **value));";
      "extern U0 F(reg R12 noreg ...);";
    ]

let unsupported_types () =
  List.iter
    (fun text ->
      try
        ignore
          (parse
             ~type_header:(fun session namespace declared ->
               let table = Session.semantic_symbols session in
               let scopes = Semantic_symbol_table.all_scopes table in
               let symbols = Semantic_symbol_table.all_symbols table in
               Alcotest.(check bool)
                 "missing aggregate visibility is explicit" true
                 (Result.is_error
                    (resolve_completed_function_header session ~namespace
                       declared));
               Alcotest.(check int)
                 "no scope allocated on unsupported type" (List.length scopes)
                 (List.length (Semantic_symbol_table.all_scopes table));
               Alcotest.(check int)
                 "no parameter allocated on unsupported type"
                 (List.length symbols)
                 (List.length (Semantic_symbol_table.all_symbols table));
               raise Exit)
             text);
        Alcotest.fail "missing completed header"
      with Exit -> ())
    [
      "class Node {}; extern Node *F();";
      "class Node {}; extern I64 F(Node *node);";
      "class Node {}; extern I64 F(U0 (*callback)(Node *node));";
    ]

let lookahead () =
  List.iter
    (fun (text, expected_headers, expected_bodies) ->
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"header-lookahead.hc" ~contents:text
      in
      let headers = ref [] and bodies = ref 0 and reached = ref false in
      let commands : Parser.command_sink =
        {
          checkpoint = None;
          reference = None;
          implicit_output = None;
          query = None;
          dimension_count = None;
          declaration =
            Some
              (fun event ->
                (match event with
                | Parser.Function_header_completed header ->
                    headers := header :: !headers
                | Parser.Function_body_completed _ -> incr bodies
                | _ -> ());
                Ok ());
          command = (fun _ -> Ok ());
          resume = (fun () -> Ok ());
        }
      in
      let enter span =
        reached := true;
        Alcotest.(check int)
          "completed headers at directive" expected_headers
          (List.length !headers);
        Alcotest.(check int)
          "completed bodies at directive" expected_bodies !bodies;
        List.iter
          (fun header ->
            Alcotest.(check bool)
              "earlier callback is closed" false
              (Parser.function_header_is_current header))
          !headers;
        Error
          [
            Diagnostic.make ~code:"TEST" ~severity:Diagnostic.Error
              ~primary:span ~message:"stop at lookahead" ();
          ]
      in
      ignore
        (Parser.parse ~commands ~execute_stream:enter
           ~sources:(Session.sources session)
           ~definitions:(Session.definitions session)
           ~symbols:(Session.symbols session)
           ~config:(Preprocessor.Config.create () |> checked)
           source);
      Alcotest.(check bool) "directive reached" true !reached)
    [
      ("I64 F(I64 n)#exe {}{return n;}", 0, 0);
      ("I64 F(I64 n){return n;}#exe {}", 1, 0);
      ("I64 F(I64 n){return n;};#exe {}", 1, 1);
    ]

let ledger () =
  let session, source, ledger =
    Test_source_promotion.inputs "I64 F(I64 n=42){return n;}"
  in
  let saved = ref None in
  let declaration event =
    Result.map
      (fun () ->
        match event with
        | Parser.Function_header_completed header ->
            let first =
              Task_declarations.declared_function_header ledger header
              |> Test_integer_program.checked
            in
            let second =
              Task_declarations.declared_function_header ledger header
              |> Test_integer_program.checked
            in
            Alcotest.(check bool)
              "ledger reuses source proof" true (first == second);
            saved := Some (header, first)
        | _ -> ())
      (Task_declarations.observe ledger event)
  in
  ignore
    (Test_source_promotion.parse ~declaration session source ledger
    |> Test_parser.expect_ast);
  let header, original = Option.get !saved in
  let retained =
    Task_declarations.declared_function_header ledger header
    |> Test_integer_program.checked
  in
  Alcotest.(check bool)
    "completed source retains original proof" true (retained == original)

let activation () =
  let module A = Holyc_lib__Sema.Source_activation in
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let source =
    Session.add_source session ~path:"header-activation.hc"
      ~contents:"I64 F(){return 42;}#exe {}"
  in
  let events = ref [] and count = ref 0 and context = ref None in
  let publication = ref None and header = ref None and reached = ref false in
  let commands : Parser.command_sink =
    {
      checkpoint =
        Some
          (fun event ->
            incr count;
            events := A.Command event :: !events;
            (match event with
            | Parser.Sequence_started original -> context := Some original
            | _ -> ());
            Ok ());
      declaration =
        Some
          (fun event ->
            events := A.Declaration event :: !events;
            (match event with
            | Parser.Function_declared source ->
                publication :=
                  Some (C.publish_function namespace source |> checked)
            | Parser.Function_header_completed original ->
                header := Some original
            | _ -> ());
            Ok ());
      reference = None;
      implicit_output = None;
      query = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let enter span =
    reached := true;
    let header = Option.get !header and publication = Option.get !publication in
    let create events =
      A.create ~namespace ~context:(Option.get !context) ~observed_events:!count
        events
      |> checked
    in
    let unrelated =
      create
        (List.rev !events
        |> List.filter (function
          | A.Declaration _ -> false
          | _ -> true))
    in
    let declare activation =
      R.declare_function ~activation ~table ~namespace publication header
    in
    Alcotest.(check bool)
      "unrelated inactive journal cannot authorize stale header" true
      (Result.is_error (declare unrelated));
    let original = create (List.rev !events) in
    Alcotest.(check bool)
      "pending exact event is not active" true
      (Result.is_error (declare original));
    A.run original ~invalid:"invalid activation" (fun event ->
        match event with
        | A.Declaration (Parser.Function_header_completed source)
          when source == header ->
            ignore (declare original |> checked);
            Ok ()
        | _ ->
            Alcotest.(check bool)
              "other active events cannot authorize header" true
              (Result.is_error (declare original));
            Ok ())
    |> checked;
    Alcotest.(check bool)
      "consumed journal cannot authorize header" true
      (Result.is_error (declare original));
    Error
      [
        Diagnostic.make ~code:"TEST" ~severity:Diagnostic.Error ~primary:span
          ~message:"stop after activation" ();
      ]
  in
  ignore
    (Parser.parse ~commands ~execute_stream:enter
       ~sources:(Session.sources session)
       ~definitions:(Session.definitions session)
       ~symbols:(Session.symbols session)
       ~config:(Preprocessor.Config.create () |> checked)
       source);
  Alcotest.(check bool) "activation probe reached" true !reached

let tests =
  [
    Alcotest.test_case "original completed header children and lifetime" `Quick
      source_children;
    Alcotest.test_case "table namespace and publication ownership" `Quick
      foreign;
    Alcotest.test_case "exception revokes original callback" `Quick
      callback_exception;
    Alcotest.test_case "recursive types defaults and register parity" `Quick
      signature_parity;
    Alcotest.test_case "unsupported aggregate visibility allocates nothing"
      `Quick unsupported_types;
    Alcotest.test_case "native header and body lookahead phases" `Quick
      lookahead;
    Alcotest.test_case "declaration ledger retains one header witness" `Quick
      ledger;
    Alcotest.test_case "exact active journal event" `Quick activation;
  ]
