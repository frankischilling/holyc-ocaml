open Holyc_lib

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let without_eof tokens =
  List.filter (fun token -> token.Token.kind <> Token_kind.Eof) tokens

let token_words tokens =
  without_eof tokens
  |> List.map (fun token ->
      match token.Token.value with
      | Token.Text value -> value
      | _ -> Token_kind.name token.kind)

let config ?(compilation_mode = Preprocessor.Jit) ?(max_conditional_depth = 64)
    ?(working_directory = ".") conditional_recovery =
  Preprocessor.Config.create ~working_directory ~compilation_mode
    ~max_conditional_depth ~conditional_recovery ()
  |> checked

let collect ?compilation_mode ?max_conditional_depth ?working_directory
    conditional_recovery contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"conditional-recovery.hc" ~contents
  in
  let config =
    config ?compilation_mode ?max_conditional_depth ?working_directory
      conditional_recovery
  in
  let output =
    Preprocessor.collect_all ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  (session, output)

let diagnostic_position session diagnostic =
  let source =
    Source_manager.find (Session.sources session)
      diagnostic.Diagnostic.primary.source
    |> Option.get
  in
  Source_file.position source diagnostic.primary.start |> checked

let diagnostics_with_positions session diagnostics =
  List.map
    (fun diagnostic ->
      let position = diagnostic_position session diagnostic in
      (diagnostic.Diagnostic.code, position.Source_file.line, position.column))
    diagnostics

let boundary_recovery () =
  let cases =
    [
      ( "stray else",
        "#else dropped #endif kept",
        [ "dropped"; "kept" ],
        [ ("HCPP0015", 1, 2); ("HCPP0017", 1, 16) ],
        [ "kept" ] );
      ( "stray endif",
        "#endif kept",
        [ "kept" ],
        [ ("HCPP0017", 1, 2) ],
        [ "kept" ] );
      ( "duplicate else",
        "#ifjit selected #else discarded #else dropped #endif kept",
        [ "selected"; "kept" ],
        [ ("HCPP0016", 1, 34) ],
        [ "selected"; "kept" ] );
      ("EOF before endif", "#ifaot dropped", [], [ ("HCPP0018", 1, 1) ], []);
    ]
  in
  List.iter
    (fun (name, source, strict_words, strict_diagnostics, permissive_words) ->
      let strict_session, strict = collect Preprocessor.Hosted_strict source in
      Alcotest.(check (list string))
        (name ^ " strict tokens") strict_words
        (token_words strict.tokens);
      Alcotest.(check (list (triple string int int)))
        (name ^ " strict diagnostic positions")
        strict_diagnostics
        (diagnostics_with_positions strict_session strict.diagnostics);
      let _, permissive = collect Preprocessor.Templeos_permissive source in
      Alcotest.(check (list string))
        (name ^ " permissive tokens")
        permissive_words
        (token_words permissive.tokens);
      Alcotest.(check int)
        (name ^ " permissive diagnostics")
        0
        (List.length permissive.diagnostics))
    cases

let nested_openers_are_counted () =
  [ "#if ignored"; "#ifdef ignored"; "#ifndef ignored"; "#ifaot"; "#ifjit" ]
  |> List.iter (fun opener ->
      let source =
        Printf.sprintf "#else outer %s nested #endif outer_tail #endif kept"
          opener
      in
      let _, output = collect Preprocessor.Templeos_permissive source in
      Alcotest.(check (list string))
        (opener ^ " remains nested during recovery")
        [ "kept" ]
        (token_words output.tokens);
      Alcotest.(check int)
        (opener ^ " recovery diagnostics")
        0
        (List.length output.diagnostics))

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temp_directory run =
  let path = Filename.temp_dir "holyc-conditional-recovery-" "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> run path)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let collect_file root contents_by_path =
  List.iter
    (fun (path, contents) -> write_file (Filename.concat root path) contents)
    contents_by_path;
  let session = Session.create () in
  let source =
    Session.load_source session ~path:(Filename.concat root "root.HC")
    |> checked
  in
  let config =
    config ~working_directory:root Preprocessor.Templeos_permissive
  in
  let output =
    Preprocessor.collect_all ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  (session, output)

let recovery_crosses_frames () =
  with_temp_directory (fun root ->
      let _, included =
        collect_file root
          [
            ("root.HC", "#include \"open\" caller_dropped #endif include_kept");
            ("open.HC", "#else included_dropped");
          ]
      in
      Alcotest.(check (list string))
        "include frame resumes at caller" [ "include_kept" ]
        (token_words included.tokens);
      Alcotest.(check int)
        "include frame diagnostics" 0
        (List.length included.diagnostics));
  let _, defined =
    collect Preprocessor.Templeos_permissive
      "#define INNER else definition_dropped\n\
       #define OUTER INNER\n\
       #OUTER caller_dropped #endif definition_kept"
  in
  Alcotest.(check (list string))
    "nested definition frames resume at caller" [ "definition_kept" ]
    (token_words defined.tokens);
  Alcotest.(check int)
    "definition frame diagnostics" 0
    (List.length defined.diagnostics)

let discarded_exe_is_inert () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"conditional-recovery-exe.hc"
      ~contents:"#else #exe {1/0;} #endif kept"
  in
  let config = config Preprocessor.Templeos_permissive in
  let executions = ref 0 in
  let execute_stream _ _ =
    incr executions;
    Ok { Preprocessor.generated = "leaked"; diagnostics = [] }
  in
  let stream =
    Preprocessor.create ~execute_stream ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  let rec drain tokens diagnostics =
    match Preprocessor.next stream with
    | Lexer.Token token when token.Token.kind = Token_kind.Eof ->
        (List.rev (token :: tokens), List.rev diagnostics)
    | Lexer.Token token -> drain (token :: tokens) diagnostics
    | Lexer.Diagnostic diagnostic -> drain tokens (diagnostic :: diagnostics)
  in
  let tokens, diagnostics = drain [] [] in
  Alcotest.(check int) "discarded #exe callback count" 0 !executions;
  Alcotest.(check (list string))
    "discarded #exe text" [ "kept" ] (token_words tokens);
  Alcotest.(check int) "discarded #exe diagnostics" 0 (List.length diagnostics)

let recovery_respects_depth_limit () =
  let session, output =
    collect ~max_conditional_depth:1 Preprocessor.Templeos_permissive
      "#else discarded #ifjit #define HIDDEN value #endif #endif visible"
  in
  Alcotest.(check (list string))
    "poisoned recovery emits no remaining source" []
    (token_words output.tokens);
  Alcotest.(check (list string))
    "depth diagnostic is retained" [ "HCPP0019" ]
    (List.map (fun diagnostic -> diagnostic.Diagnostic.code) output.diagnostics);
  Alcotest.(check bool)
    "poisoned recovery has no definition side effect" true
    (Definition.Environment.find (Session.definitions session) "HIDDEN"
    |> Option.is_none)

let eof_selection_in_both_modes () =
  List.iter
    (fun compilation_mode ->
      let cases =
        [
          ("#ifjit", compilation_mode = Preprocessor.Jit);
          ("#ifaot", compilation_mode = Preprocessor.Aot);
          ("#if 1", true);
          ("#if 0", false);
          ("#ifdef NotPresent", false);
          ("#ifndef NotPresent", true);
        ]
      in
      List.iter
        (fun (opener, selected) ->
          let contents = opener ^ "\nkept" in
          let _, permissive =
            collect ~compilation_mode Preprocessor.Templeos_permissive contents
          in
          Alcotest.(check (list string))
            "EOF retains only selected source"
            (if selected then [ "kept" ] else [])
            (token_words permissive.tokens);
          Alcotest.(check int)
            "EOF has no permissive mismatch" 0
            (List.length permissive.diagnostics);
          let _, strict =
            collect ~compilation_mode Preprocessor.Hosted_strict contents
          in
          Alcotest.(check (list string))
            "strict EOF still diagnoses the opener" [ "HCPP0018" ]
            (List.map
               (fun diagnostic -> diagnostic.Diagnostic.code)
               strict.diagnostics))
        cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let exact_depth_and_nul_guard () =
  List.iter
    (fun (max_conditional_depth, contents) ->
      let _, output =
        collect ~max_conditional_depth Preprocessor.Templeos_permissive contents
      in
      Alcotest.(check (list string))
        "exact depth resumes at caller" [ "kept" ]
        (token_words output.tokens);
      Alcotest.(check int)
        "exact depth has no error" 0
        (List.length output.diagnostics))
    [
      (2, "#else outer #ifjit inner #endif outer #endif kept");
      (1, "#else first #else second #else third #endif kept");
    ];
  let _, output =
    collect Preprocessor.Templeos_permissive "#else ignored\x00tail #endif kept"
  in
  Alcotest.(check (list string))
    "raw scan still rejects an embedded NUL" [ "HCLEX0006" ]
    (List.map (fun diagnostic -> diagnostic.Diagnostic.code) output.diagnostics);
  Alcotest.(check (list string))
    "NUL recovery preserves the closing boundary" [ "kept" ]
    (token_words output.tokens)

let native_capture_projection () =
  let open Yojson.Safe.Util in
  let fixture_path =
    [
      "oracle/conditional-recovery.json";
      "test/oracle/conditional-recovery.json";
      "../test/oracle/conditional-recovery.json";
    ]
    |> List.find Sys.file_exists
  in
  let fixture = Yojson.Safe.from_file fixture_path in
  Alcotest.(check string)
    "native capture is complete" "captured"
    (fixture |> member "status" |> to_string);
  Alcotest.(check string)
    "native capture source pin" Version.reference_commit
    (fixture |> member "reference" |> member "commit" |> to_string);
  Alcotest.(check string)
    "native capture compilation mode" "jit"
    (fixture |> member "environment" |> member "compilation_mode" |> to_string);
  let cases = fixture |> member "cases" |> to_list in
  Alcotest.(check int) "native boundary and control cases" 6 (List.length cases);
  List.iter
    (fun case ->
      let name = case |> member "id" |> to_string in
      let source = case |> member "source" |> to_string in
      let records pass =
        case |> member pass |> to_list
        |> List.map (fun record ->
            match record |> to_string |> String.split_on_char ' ' with
            | _label :: fields -> fields
            | [] -> Alcotest.fail "missing captured record label")
      in
      let observed = records "pass_a" in
      Alcotest.(check (list (list string)))
        (name ^ " repeated native record")
        observed (records "pass_b");
      let captured_tokens =
        List.map
          (function
            | [ token; last; line; errors; warnings ] ->
                Alcotest.(check string) (name ^ " native errors") "err=0" errors;
                Alcotest.(check string)
                  (name ^ " native warnings")
                  "warn=0" warnings;
                let last_line = Scanf.sscanf last "last=%d%!" Fun.id in
                let current_line = Scanf.sscanf line "line=%d%!" Fun.id in
                Alcotest.(check bool)
                  (name ^ " native cursor retained")
                  true
                  (last_line >= 1 && current_line >= last_line);
                if token = "EOF=0" then ("eof", "")
                else
                  Scanf.sscanf token "T%d=100:%s%!" (fun _ordinal spelling ->
                      ("identifier", spelling))
            | _ -> Alcotest.fail (name ^ " malformed native record"))
          observed
      in
      let _, output = collect Preprocessor.Templeos_permissive source in
      let actual_tokens =
        List.map
          (fun token ->
            match (token.Token.kind, token.value) with
            | Token_kind.Identifier, Token.Text text -> ("identifier", text)
            | Token_kind.Eof, _ -> ("eof", "")
            | _ -> Alcotest.fail (name ^ " unexpected hosted token kind"))
          output.tokens
      in
      Alcotest.(check (list (pair string string)))
        (name ^ " captured token stream")
        captured_tokens actual_tokens;
      Alcotest.(check int)
        (name ^ " hosted permissive diagnostics")
        0
        (List.length output.diagnostics);
      let strict = member "hosted_strict" case in
      let session, output = collect Preprocessor.Hosted_strict source in
      Alcotest.(check (list string))
        (name ^ " separate strict tokens")
        (strict |> member "tokens_without_eof" |> to_list |> List.map to_string)
        (token_words output.tokens);
      let diagnostics =
        strict |> member "diagnostics" |> to_list
        |> List.map (fun item ->
            ( item |> member "code" |> to_string,
              item |> member "line" |> to_int,
              item |> member "column" |> to_int ))
      in
      Alcotest.(check (list (triple string int int)))
        (name ^ " separate strict positions")
        diagnostics
        (diagnostics_with_positions session output.diagnostics))
    cases

let tests =
  [
    Alcotest.test_case "four malformed boundaries" `Quick boundary_recovery;
    Alcotest.test_case "nested opener accounting" `Quick
      nested_openers_are_counted;
    Alcotest.test_case "include and definition frames" `Quick
      recovery_crosses_frames;
    Alcotest.test_case "discarded exe is inert" `Quick discarded_exe_is_inert;
    Alcotest.test_case "conditional depth limit" `Quick
      recovery_respects_depth_limit;
    Alcotest.test_case "EOF selection in both modes" `Quick
      eof_selection_in_both_modes;
    Alcotest.test_case "exact depth and NUL guard" `Quick
      exact_depth_and_nul_guard;
    Alcotest.test_case "captured native recovery projection" `Quick
      native_capture_projection;
  ]
