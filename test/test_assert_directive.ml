open Holyc_lib

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let config ?max_expression_nodes working_directory =
  Preprocessor.Config.create ~working_directory ?max_expression_nodes ()
  |> checked

let run ?max_expression_nodes ?(prepare = fun _ -> ()) contents =
  let session = Session.create () in
  prepare session;
  let source = Session.add_source session ~path:"assertions.HC" ~contents in
  let config = config ?max_expression_nodes (Sys.getcwd ()) in
  let output = Holyc_lib.preprocess_detailed session ~config ~source in
  (session, source, config, output)

let without_eof tokens =
  List.filter (fun token -> token.Token.kind <> Token_kind.Eof) tokens

let words (output : Preprocessor.output) =
  output.tokens |> without_eof
  |> List.map (fun token ->
      match token.Token.value with
      | Token.Text value -> value
      | _ -> token.Token.raw)

let diagnostic_with_code code (output : Preprocessor.output) =
  match
    List.find_opt
      (fun item -> String.equal item.Diagnostic.code code)
      output.diagnostics
  with
  | Some item -> item
  | None ->
      Alcotest.failf "expected diagnostic %s, got %s" code
        (output.diagnostics
        |> List.map (fun item -> item.Diagnostic.code)
        |> String.concat ", ")

let contains_text text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec search offset =
    if offset + needle_length > text_length then false
    else if String.sub text offset needle_length = needle then true
    else search (offset + 1)
  in
  search 0

let true_and_false_assertions () =
  let session, source, config, output =
    run "#assert 1 first #assert 0 second"
  in
  Alcotest.(check (list string))
    "lookahead token order" [ "first"; "second" ] (words output);
  Alcotest.(check int) "one failed assertion" 1 (List.length output.diagnostics);
  let warning = diagnostic_with_code "HCPP0024" output in
  Alcotest.(check bool)
    "warning severity" true
    (warning.Diagnostic.severity = Diagnostic.Warning);
  Alcotest.(check bool)
    "warning is nonfatal" false
    (Preprocessor.has_errors output);
  let second =
    output.tokens
    |> List.find (fun token -> String.equal token.Token.raw "second")
  in
  let resume = List.hd warning.secondary in
  Alcotest.(check string)
    "resume note" "preprocessing resumes with this token" resume.message;
  Alcotest.(check bool)
    "resume span" true
    (Span.compare resume.span second.span = 0);
  match Holyc_lib.preprocess session ~config ~source with
  | Error _ -> Alcotest.fail "a warning must not fail the convenience API"
  | Ok tokens ->
      Alcotest.(check (list string))
        "convenience tokens" [ "first"; "second" ]
        (words
           ({ tokens; diagnostics = []; help_metadata = Help_metadata.empty }
             : Preprocessor.output))

let definitions_and_symbols () =
  let prepare session =
    ignore
      (Symbol_visibility.Environment.add (Session.symbols session)
         ~name:"KnownFunction" ~kind:Symbol_visibility.Function ())
  in
  let _, _, _, output =
    run ~prepare
      "#define FALSE 0\n\
       #assert FALSE after_false #assert defined(KnownFunction) after_true"
  in
  Alcotest.(check (list string))
    "expanded and defined assertions"
    [ "after_false"; "after_true" ]
    (words output);
  Alcotest.(check (list string))
    "only FALSE warns" [ "HCPP0024" ]
    (List.map (fun item -> item.Diagnostic.code) output.diagnostics);
  let warning = diagnostic_with_code "HCPP0024" output in
  Alcotest.(check bool)
    "expanded assertion provenance" true
    (List.exists
       (fun (related : Diagnostic.related) ->
         String.equal related.message "definition \"FALSE\" was declared here")
       warning.secondary)

let expression_errors () =
  let _, _, _, missing = run "#assert" in
  let item = diagnostic_with_code "HCPP0021" missing in
  Alcotest.(check bool)
    "missing term names assert" true
    (contains_text item.message "#assert");
  let _, _, _, parenthesis = run "#assert (1 tail" in
  let item = diagnostic_with_code "HCPP0023" parenthesis in
  Alcotest.(check bool)
    "parenthesis message names assert" true
    (contains_text item.message "#assert");
  Alcotest.(check (list string))
    "parenthesis lookahead" [ "tail" ] (words parenthesis);
  let _, _, _, division = run "#assert 1/0 tail" in
  let item = diagnostic_with_code "HCPP0025" division in
  Alcotest.(check bool)
    "division is fatal" true
    (Preprocessor.has_errors division);
  Alcotest.(check bool)
    "directive in message" true
    (contains_text item.message "#assert");
  Alcotest.(check (list string))
    "lookahead after error" [ "tail" ] (words division);
  let _, _, _, expanded = run "#define ZERO 0\n#assert 1/ZERO expanded_tail" in
  let item = diagnostic_with_code "HCPP0025" expanded in
  Alcotest.(check bool)
    "expanded divisor provenance" true
    (List.exists
       (fun (related : Diagnostic.related) ->
         String.equal related.message "definition \"ZERO\" was declared here")
       item.secondary);
  let _, _, _, limited = run ~max_expression_nodes:1 "#assert 1+1 tail" in
  let item = diagnostic_with_code "HCPP0026" limited in
  Alcotest.(check bool)
    "limit message names assert" true
    (contains_text item.message "#assert")

let inactive_assertions_are_not_evaluated () =
  let _, _, _, output =
    run "#if 0 #assert MissingCall( #else selected #endif"
  in
  Alcotest.(check (list string)) "selected branch" [ "selected" ] (words output);
  Alcotest.(check int)
    "no inactive diagnostics" 0
    (List.length output.diagnostics)

let definition_directive_provenance () =
  let _, _, _, output = run "#define CHECK #assert 0\nCHECK resumed" in
  let warning = diagnostic_with_code "HCPP0024" output in
  Alcotest.(check (list string)) "caller resumes" [ "resumed" ] (words output);
  let messages =
    List.map (fun (item : Diagnostic.related) -> item.message) warning.secondary
  in
  List.iter
    (fun expected ->
      Alcotest.(check bool)
        expected true
        (List.exists (String.equal expected) messages))
    [
      "definition \"CHECK\" was expanded here";
      "definition \"CHECK\" was declared here";
    ]

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temp_directory run =
  let path = Filename.temp_dir "holyc-assert-" "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> run path)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let include_provenance () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#include \"child\" root_tail";
      write_file (Filename.concat root "child.HC") "#assert 0 child_tail";
      let session = Session.create () in
      let source = Session.load_source session ~path:root_file |> checked in
      let output =
        Holyc_lib.preprocess_detailed session ~config:(config root) ~source
      in
      let warning = diagnostic_with_code "HCPP0024" output in
      Alcotest.(check (list string))
        "include token order"
        [ "child_tail"; "root_tail" ]
        (words output);
      Alcotest.(check int) "include stack" 1 (List.length warning.include_stack))

let diagnostic_rendering () =
  let session, _, _, output = run "#assert 0 tail" in
  let warning = diagnostic_with_code "HCPP0024" output in
  let rendered = Diagnostic_render.human (Session.sources session) warning in
  Alcotest.(check bool)
    "human warning" true
    (String.starts_with
       ~prefix:
         "assertions.HC:1:1: warning[HCPP0024]: #assert expression evaluated \
          to false"
       rendered);
  let json =
    Diagnostic_render.json (Session.sources session) [ warning ]
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "JSON severity" "warning"
    (json |> index 0 |> member "severity" |> to_string)

let tests =
  [
    Alcotest.test_case "true, false, and lookahead" `Quick
      true_and_false_assertions;
    Alcotest.test_case "definitions and symbols" `Quick definitions_and_symbols;
    Alcotest.test_case "expression errors" `Quick expression_errors;
    Alcotest.test_case "inactive assertions" `Quick
      inactive_assertions_are_not_evaluated;
    Alcotest.test_case "definition directive provenance" `Quick
      definition_directive_provenance;
    Alcotest.test_case "include provenance" `Quick include_provenance;
    Alcotest.test_case "diagnostic rendering" `Quick diagnostic_rendering;
  ]
