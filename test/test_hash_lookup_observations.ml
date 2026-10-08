open Holyc_lib
module V = Symbol_visibility
module P = Preprocessor

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let check = Alcotest.(check bool)

let commands ?(declaration = fun _ -> Ok ()) () : Parser.command_sink =
  {
    checkpoint = None;
    reference = None;
    call = None;
    implicit_output = None;
    query = None;
    declaration = Some declaration;
    dimension_count = None;
    command = (fun _ -> Ok ());
    resume = (fun () -> Ok ());
  }

let source session text =
  Session.add_source session ~path:"hash-lookups.hc" ~contents:text

let config mode = P.Config.create ~compilation_mode:mode () |> checked

let drain stream =
  let rec read tokens diagnostics =
    match P.next stream with
    | Lexer.Token token when token.Token.kind = Token_kind.Eof ->
        (List.rev tokens, List.rev diagnostics)
    | Lexer.Token token -> read (token :: tokens) diagnostics
    | Lexer.Diagnostic diagnostic -> read tokens (diagnostic :: diagnostics)
  in
  read [] []

let create ?(mode = P.Jit) ?symbols ?(observe = fun _ -> ()) session text =
  P.create ~lexical_lookup:observe ~sources:(Session.sources session)
    ~definitions:(Session.definitions session)
    ~symbols:(Option.value symbols ~default:(Session.symbols session))
    ~config:(config mode) (source session text)

let retain observations lookup =
  check "original lexical callback" true (P.lexical_lookup_is_current lookup);
  observations := lookup :: !observations

let words observations =
  List.rev observations
  |> List.map (fun lookup -> (P.lexical_lookup_token lookup).Token.raw)

let selected lookup entry =
  match P.lexical_lookup_selection lookup with
  | V.Present candidate -> candidate == entry
  | _ -> false

let expired observations =
  List.iter
    (fun lookup ->
      check "remembered lexical receipt expires" false
        (P.lexical_lookup_is_current lookup))
    observations

let lexical_identity_and_purity () =
  let session = Session.create () in
  let symbols = Session.symbols session in
  let original = V.Environment.add symbols ~name:"F" ~kind:V.Function () in
  ignore (V.Environment.add symbols ~name:"F" ~kind:V.Import_system_symbol ());
  let observations = ref [] in
  let stream =
    create session "F F" ~observe:(fun lookup ->
        retain observations lookup;
        for _ = 1 to 10 do
          ignore (V.Environment.find_preprocessor symbols "F");
          ignore (V.Environment.find_function symbols "F")
        done)
  in
  let tokens, diagnostics = drain stream in
  Alcotest.(check int) "no diagnostics" 0 (List.length diagnostics);
  Alcotest.(check (list int))
    "metadata reads create no extra observations" [ 0; 1 ]
    (List.rev_map P.lexical_lookup_ordinal !observations);
  List.iter2
    (fun lookup token ->
      check "physical original token" true
        (P.lexical_lookup_token lookup == token);
      check "import-system symbol excluded" true (selected lookup original);
      check "physical selected environment" true
        (P.lexical_lookup_environment lookup == symbols))
    (List.rev !observations) tokens;
  expired !observations

let local_shadow_and_mutation () =
  let session = Session.create () in
  let symbols = Session.symbols session in
  let original = V.Environment.add symbols ~name:"F" ~kind:V.Function () in
  let observations = ref [] in
  let stream = create session "F F F" ~observe:(retain observations) in
  ignore (P.next stream);
  let saved = List.hd !observations in
  let context = V.Environment.begin_local_context symbols in
  V.Environment.add_local symbols context ~name:"F" |> checked;
  ignore (P.next stream);
  check "local suppresses hash selection" true
    (P.lexical_lookup_selection (List.hd !observations) = V.Shadowed_by_local);
  V.Environment.end_local_context symbols context |> checked;
  let replacement =
    V.Environment.add symbols ~name:"F" ~kind:V.Global_variable ()
  in
  ignore (P.next stream);
  check "later lexical read selects replacement" true
    (selected (List.hd !observations) replacement);
  check "earlier observation retains original selection" true
    (selected saved original);
  expired !observations

let directives_definitions_and_inactive_input () =
  let session = Session.create () in
  let observations = ref [] in
  let stream =
    create session "#define M F\n#if 0\nF\n#ifdef F\nF\n#endif\n#endif\nM F"
      ~observe:(retain observations)
  in
  let tokens, diagnostics = drain stream in
  Alcotest.(check int) "no diagnostics" 0 (List.length diagnostics);
  Alcotest.(check (list string))
    "raw skipped and captured bytes are not lexed"
    [ "define"; "M"; "if"; "F"; "ifdef"; "endif"; "endif"; "M"; "F"; "F" ]
    (words !observations);
  Alcotest.(check (list string))
    "expanded output" [ "F"; "F" ]
    (List.map (fun token -> token.Token.raw) tokens);
  let invocation = List.nth (List.rev !observations) 7 in
  let definition =
    Definition.Environment.find (Session.definitions session) "M" |> Option.get
  in
  check "exact definition candidate retained" true
    (Option.fold ~none:false ~some:(( == ) definition)
       (P.lexical_lookup_definition invocation));
  let expanded = List.nth (List.rev !observations) 8 in
  check "definition frame has original source" true
    (not
       (Source_id.equal (P.lexical_lookup_token invocation).span.source
          (P.lexical_lookup_token expanded).span.source));
  expired !observations

let conditional_metadata_reads () =
  let session = Session.create () in
  ignore
    (V.Environment.add (Session.symbols session) ~name:"F" ~kind:V.Function ());
  let observations = ref [] in
  let stream =
    create session "#ifdef F\nF\n#endif\n#if defined(F)\nF\n#endif"
      ~observe:(retain observations)
  in
  let _, diagnostics = drain stream in
  Alcotest.(check int) "no diagnostics" 0 (List.length diagnostics);
  Alcotest.(check (list string))
    "condition queries do not create lexical reads"
    [ "ifdef"; "F"; "F"; "endif"; "if"; "defined"; "F"; "F"; "endif" ]
    (words !observations)

let foreign_streams_and_exception () =
  let session = Session.create () in
  let first = ref [] and second = ref [] in
  let one = create session "F" ~observe:(retain first) in
  let two = create session "F" ~observe:(retain second) in
  ignore (P.next one);
  ignore (P.next two);
  check "same ordinal does not identify stream" false
    (P.same_lexical_lookup_stream (List.hd !first) (List.hd !second));
  expired (!first @ !second);
  let saved = ref None in
  let failed =
    create session "F G" ~observe:(fun lookup ->
        saved := Some lookup;
        raise Exit)
  in
  (try
     ignore (P.next failed);
     Alcotest.fail "expected observer exception"
   with Exit -> ());
  check "exception closes receipt" false
    (P.lexical_lookup_is_current (Option.get !saved))

let reentrant_read_expires_original () =
  let session = Session.create () in
  let stream = ref None and first = ref None and observations = ref [] in
  let value =
    create session "F G" ~observe:(fun lookup ->
        retain observations lookup;
        if (P.lexical_lookup_token lookup).raw = "F" then (
          first := Some lookup;
          ignore (P.next (Option.get !stream));
          check "nested read expires earlier callback" false
            (P.lexical_lookup_is_current lookup))
        else
          check "nested callback cannot revive original" false
            (P.lexical_lookup_is_current (Option.get !first)))
  in
  stream := Some value;
  ignore (P.next value);
  Alcotest.(check (list string))
    "each actual read observed once" [ "F"; "G" ] (words !observations);
  expired !observations

let environment_switch_expires_original () =
  let session = Session.create () in
  let stream = ref None in
  let value =
    create session "F" ~observe:(fun lookup ->
        check "initial callback current" true
          (P.lexical_lookup_is_current lookup);
        P.with_environment (Option.get !stream)
          ~definitions:(Definition.Environment.create ())
          ~symbols:(V.Environment.create ()) ~compilation_mode:P.Aot (fun () ->
            check "foreign environment rejects" false
              (P.lexical_lookup_is_current lookup));
        check "restoration cannot revive original" false
          (P.lexical_lookup_is_current lookup))
  in
  stream := Some value;
  ignore (P.next value)

let nonidentifiers_and_predefined () =
  let session = Session.create () in
  let observations = ref [] in
  let stream =
    create session "/* F */ \"F\" 42 @ __LINE__" ~observe:(retain observations)
  in
  ignore (drain stream);
  Alcotest.(check (list string))
    "strings, comments and numbers have no lookup" [ "@"; "__LINE__" ]
    (words !observations);
  check "predefined candidate retained" true
    (P.lexical_lookup_predefined (List.hd !observations) = Some Predefined.Line)

let parse ?(mode = P.Jit) ?symbols ?execute_stream ?(observe = fun _ -> ())
    ?(declaration = fun _ -> Ok ()) session text =
  Parser.parse ~commands:(commands ~declaration ()) ~lexical_lookup:observe
    ?execute_stream ~sources:(Session.sources session)
    ~definitions:(Session.definitions session)
    ~symbols:(Option.value symbols ~default:(Session.symbols session))
    ~config:(config mode) (source session text)

let function_join_before_failed_parameters mode () =
  let session = Session.create () in
  let joins = ref [] and trace = ref [] and headers = ref [] in
  let output =
    parse session ~mode
      "extern I64 F(I64 n);I64 Earlier(){return F(42);}I64 F(I64 n,"
      ~observe:(fun lookup ->
        trace := (P.lexical_lookup_token lookup).raw :: !trace)
      ~declaration:(function
        | Parser.Function_declared publication ->
            let lookup = publication.function_join_lookup in
            check "original join callback" true
              (Parser.join_lookup_is_current lookup);
            check "join owns exact source name" true
              (Parser.join_lookup_name lookup == publication.function_name);
            check "function lookup kind" true
              (Parser.join_lookup_kind lookup = V.Function);
            joins := lookup :: !joins;
            trace := ("join:" ^ publication.function_name.spelling) :: !trace;
            Ok ()
        | Parser.Function_header_completed header ->
            headers := header :: !headers;
            Ok ()
        | _ -> Ok ())
  in
  check "parameter failure reached" true (Parser.has_errors output);
  Alcotest.(check int) "all joins precede failure" 3 (List.length !joins);
  let first_header = List.hd (List.rev !headers) in
  let last = List.hd !joins in
  check "selected original completed extern entry" true
    (Option.fold ~none:false
       ~some:(( == ) first_header.completed_entry)
       (Parser.join_lookup_selection last));
  let chronological = List.rev !trace in
  let rec after_last_join = function
    | "join:F" :: rest when not (List.mem "join:F" rest) -> rest
    | _ :: rest -> after_last_join rest
    | [] -> Alcotest.fail "missing last join"
  in
  Alcotest.(check (list string))
    "parameter lexer reads follow join" [ "I64"; "n" ]
    (after_last_join chronological);
  List.iter
    (fun lookup ->
      check "saved join expires" false (Parser.join_lookup_is_current lookup))
    !joins

let class_join_and_extern_freshness mode () =
  let session = Session.create () in
  let publications = ref [] and saved = ref None in
  let output =
    parse session ~mode "extern class C;extern class C;class C{I64 n;};42;"
      ~declaration:(function
      | Parser.Aggregate_declared publication ->
          (match publication.aggregate_join_lookup with
          | None ->
              check "only forward publications lack joins" true
                (List.length !publications < 2)
          | Some lookup ->
              check "class join current" true
                (Parser.join_lookup_is_current lookup);
              check "class lookup kind" true
                (Parser.join_lookup_kind lookup = V.Class);
              check "newest exact forward publication" true
                (Option.fold ~none:false
                   ~some:(( == ) (List.hd !publications).Parser.aggregate_entry)
                   (Parser.join_lookup_selection lookup));
              saved := Some lookup);
          publications := publication :: !publications;
          Ok ()
      | _ -> Ok ())
  in
  check "class input accepted" false (Parser.has_errors output);
  Alcotest.(check int) "three publications" 3 (List.length !publications);
  check "class receipt expires" false
    (Parser.join_lookup_is_current (Option.get !saved))

let join_scope_and_kind mode () =
  let session = Session.create () in
  let baseline = Session.symbols session in
  let inherited = V.Environment.add baseline ~name:"F" ~kind:V.Function () in
  let symbols = V.Environment.task_view baseline in
  let other = V.Environment.task_view baseline in
  ignore (V.Environment.add other ~name:"F" ~kind:V.Function ());
  ignore (V.Environment.add symbols ~name:"F" ~kind:V.Global_variable ());
  let observed = ref false in
  let output =
    parse session ~mode ~symbols "extern I64 F();" ~declaration:(function
      | Parser.Function_declared publication ->
          observed := true;
          let lookup = publication.function_join_lookup in
          check "own declaration environment" true
            (Parser.join_lookup_environment lookup == symbols);
          check "original compilation mode" true
            (Parser.join_lookup_mode lookup = mode);
          check "proper table scope" true
            (Parser.join_lookup_scope lookup
            = if mode = P.Jit then V.Current_table else V.Visible_tables);
          (match mode with
          | P.Jit ->
              check "current table excludes baseline and other owner" true
                (Option.is_none (Parser.join_lookup_selection lookup))
          | P.Aot ->
              check "visible table selects baseline function past other kind"
                true
                (Option.fold ~none:false ~some:(( == ) inherited)
                   (Parser.join_lookup_selection lookup)));
          Ok ()
      | _ -> Ok ())
  in
  check "scope input accepted" false (Parser.has_errors output);
  check "scope join observed" true !observed

let directive_environment_restoration () =
  let session = Session.create () in
  let outer = Session.symbols session in
  let inner = V.Environment.task_view outer in
  let observations = ref [] and joins = ref [] and finished = ref false in
  let declaration = function
    | Parser.Function_declared publication ->
        joins := publication.function_join_lookup :: !joins;
        Ok ()
    | _ -> Ok ()
  in
  let execute_stream _ =
    Ok
      ({
         Parser.definitions =
           Definition.Environment.task_view (Session.definitions session);
         symbols = inner;
         commands = commands ~declaration ();
         finish =
           (fun () ->
             finished := true;
             Ok "");
         abort = (fun () -> Alcotest.fail "unexpected stream abort");
       }
        : Parser.stream_execution)
  in
  let output =
    parse session ~mode:P.Aot ~execute_stream ~declaration
      "#exe {extern I64 F();}extern I64 G();" ~observe:(retain observations)
  in
  check "directive parse accepted" false (Parser.has_errors output);
  check "directive finished" true !finished;
  List.iter
    (fun lookup ->
      let token = P.lexical_lookup_token lookup in
      if List.mem token.raw [ "F"; "G" ] then (
        let child = token.raw = "F" in
        check "original lexical writer" true
          (P.lexical_lookup_environment lookup == if child then inner else outer);
        check "original lexical mode" true
          (P.lexical_lookup_mode lookup = if child then P.Jit else P.Aot)))
    !observations;
  Alcotest.(check (list string))
    "child and outer joins observed" [ "F"; "G" ]
    (List.rev_map
       (fun lookup -> (Parser.join_lookup_name lookup).spelling)
       !joins);
  expired !observations

let join_exception_closes_receipt () =
  let session = Session.create () in
  let saved = ref None in
  (try
     ignore
       (parse session "extern I64 F();" ~declaration:(function
         | Parser.Function_declared publication ->
             saved := Some publication.function_join_lookup;
             raise Exit
         | _ -> Ok ()));
     Alcotest.fail "expected join observer exception"
   with Exit -> ());
  check "exception closes original join" false
    (Parser.join_lookup_is_current (Option.get !saved))

let suspended_child_preserves_join_focus () =
  let session = Session.create () in
  let observations = ref [] and parent = ref None and child = ref None in
  let output =
    parse session "extern I64 F();" ~declaration:(function
      | Parser.Function_declared publication ->
          let lookup = publication.function_join_lookup in
          parent := Some lookup;
          let context =
            publication.function_header.declaration_command.command_context
          in
          let suspension = Parser.suspend_context context |> checked in
          let nested =
            Parser.parse_suspended suspension
              ~commands:
                (commands
                   ~declaration:(function
                     | Parser.Function_declared nested ->
                         child := Some nested.function_join_lookup;
                         check "child join current" true
                           (Parser.join_lookup_is_current
                              nested.function_join_lookup);
                         check "suspended ancestor join has no focus" false
                           (Parser.join_lookup_is_current lookup);
                         Ok ()
                     | _ -> Ok ())
                   ())
              ~lexical_lookup:(retain observations)
              ~sources:(Session.sources session)
              ~definitions:(Session.definitions session)
              ~symbols:(Session.symbols session) ~config:(config P.Jit)
              (source session "extern I64 G();")
            |> checked
          in
          check "nested input accepted" false (Parser.has_errors nested);
          check "original callback regains focus" true
            (Parser.join_lookup_is_current lookup);
          Ok ()
      | _ -> Ok ())
  in
  check "parent input accepted" false (Parser.has_errors output);
  check "parent receipt finally expires" false
    (Parser.join_lookup_is_current (Option.get !parent));
  check "child receipt expires" false
    (Parser.join_lookup_is_current (Option.get !child));
  Alcotest.(check (list string))
    "suspended entry forwards lexical observer" [ "extern"; "I64"; "G" ]
    (words !observations);
  expired !observations

let aliases_and_copies_retain_selected_entries () =
  let session = Session.create () in
  let original_symbols = Session.symbols session in
  let original =
    V.Environment.add original_symbols ~name:"F" ~kind:V.Function ()
  in
  let alias =
    V.Environment.add_function_alias original_symbols ~original_entry:original
      ()
    |> checked
  in
  let copied = V.Environment.copy original_symbols in
  let observations = ref [] in
  ignore
    (drain (create session "F" ~symbols:copied ~observe:(retain observations)));
  let lookup = List.hd !observations in
  check "copy's actual selected alias" true (selected lookup alias);
  check "copy is its own original lookup environment" true
    (P.lexical_lookup_environment lookup == copied);
  check "immutable alias ancestry" true
    (Option.fold ~none:false ~some:(( == ) original)
       (V.function_alias_original alias));
  let replacement =
    V.Environment.add original_symbols ~name:"F" ~kind:V.Function ()
  in
  check "original environment changed independently" true
    (Option.fold ~none:false ~some:(( == ) replacement)
       (V.Environment.find_function original_symbols "F"));
  check "saved alias selection unchanged" true (selected lookup alias);
  expired !observations

let foreign_domain_has_no_current_lookup () =
  let session = Session.create () in
  ignore
    (P.next
       (create session "F" ~observe:(fun lookup ->
            check "original domain current" true
              (P.lexical_lookup_is_current lookup);
            let foreign =
              Domain.spawn (fun () -> P.lexical_lookup_is_current lookup)
            in
            check "foreign domain cannot borrow active receipt" false
              (Domain.join foreign);
            check "original domain still current" true
              (P.lexical_lookup_is_current lookup))))

let tests =
  [
    Alcotest.test_case "lexical identity and pure metadata" `Quick
      lexical_identity_and_purity;
    Alcotest.test_case "local shadow and saved selection" `Quick
      local_shadow_and_mutation;
    Alcotest.test_case "definition frames and inactive raw input" `Quick
      directives_definitions_and_inactive_input;
    Alcotest.test_case "conditional metadata is not lexer input" `Quick
      conditional_metadata_reads;
    Alcotest.test_case "foreign streams and callback exception" `Quick
      foreign_streams_and_exception;
    Alcotest.test_case "reentrant read expires original" `Quick
      reentrant_read_expires_original;
    Alcotest.test_case "environment restoration does not revive receipt" `Quick
      environment_switch_expires_original;
    Alcotest.test_case "nonidentifiers and predefined candidate" `Quick
      nonidentifiers_and_predefined;
    Alcotest.test_case "JIT join before parameter failure" `Quick
      (function_join_before_failed_parameters P.Jit);
    Alcotest.test_case "AOT join before parameter failure" `Quick
      (function_join_before_failed_parameters P.Aot);
    Alcotest.test_case "JIT extern class has no join" `Quick
      (class_join_and_extern_freshness P.Jit);
    Alcotest.test_case "AOT extern class has no join" `Quick
      (class_join_and_extern_freshness P.Aot);
    Alcotest.test_case "JIT current table and kind mask" `Quick
      (join_scope_and_kind P.Jit);
    Alcotest.test_case "AOT parent tables and kind mask" `Quick
      (join_scope_and_kind P.Aot);
    Alcotest.test_case "directive JIT writer and outer restoration" `Quick
      directive_environment_restoration;
    Alcotest.test_case "join exception closes original receipt" `Quick
      join_exception_closes_receipt;
    Alcotest.test_case "suspended child and original join focus" `Quick
      suspended_child_preserves_join_focus;
    Alcotest.test_case "alias selection and independent copied environment"
      `Quick aliases_and_copies_retain_selected_entries;
    Alcotest.test_case "foreign domain cannot borrow active lookup" `Quick
      foreign_domain_has_no_current_lookup;
  ]
