open Holyc_lib

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temp_directory run =
  let path = Filename.temp_dir "holyc-corpus-" "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> run path)

let make_directory path = Unix.mkdir path 0o700

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let file report path =
  Corpus.files report
  |> List.find_opt (fun (item : Corpus.file_result) ->
      String.equal item.path path)
  |> function
  | Some item -> item
  | None -> Alcotest.fail ("missing corpus result for " ^ path)

let parse_file report path =
  Corpus.Parse.files report
  |> List.find_opt (fun (item : Corpus.Parse.file_result) ->
      String.equal item.path path)
  |> function
  | Some item -> item
  | None -> Alcotest.fail ("missing parser corpus result for " ^ path)

let synthetic_tree () =
  with_temp_directory (fun root ->
      let nested = Filename.concat root "nested" in
      make_directory nested;
      write_file (Filename.concat root "z.PRJ") "public";
      write_file (Filename.concat root "a.HC") "one\\\ntwo";
      write_file (Filename.concat nested "b.HH") "x\x00PAY";
      write_file (Filename.concat root "ignored.txt") "not HolyC";
      let scan () =
        Corpus.lex_tree ~reference_commit:"synthetic-reference" ~root ()
        |> checked
      in
      let report = scan () in
      Alcotest.(check (list string))
        "sorted source paths"
        [ "a.HC"; "nested/b.HH"; "z.PRJ" ]
        (Corpus.files report
        |> List.map (fun (item : Corpus.file_result) -> item.path));
      Alcotest.(check int) "source count" 3 (Corpus.file_count report);
      Alcotest.(check int) "tokenized count" 3 (Corpus.tokenizes_count report);
      Alcotest.(check int) "failure count" 0 (Corpus.failure_count report);
      Alcotest.(check int64) "source bytes" 19L (Corpus.total_bytes report);
      Alcotest.(check int64) "lexed bytes" 15L (Corpus.total_lexed_bytes report);
      Alcotest.(check int64) "token count" 4L (Corpus.total_tokens report);
      Alcotest.(check int)
        "NUL-terminated files" 1
        (Corpus.nul_terminated_count report);
      Alcotest.(check int64)
        "binary payload bytes" 3L
        (Corpus.total_binary_payload_bytes report);
      Alcotest.(check int64)
        "all source bytes accounted for"
        (Corpus.total_bytes report)
        (Int64.add
           (Corpus.total_lexed_bytes report)
           (Int64.add
              (Int64.of_int (Corpus.nul_terminated_count report))
              (Corpus.total_binary_payload_bytes report)));
      let binary = file report "nested/b.HH" in
      Alcotest.(check bool) "NUL termination" true binary.nul_terminated;
      Alcotest.(check (option int64))
        "bytes before NUL" (Some 1L) binary.lexed_bytes;
      Alcotest.(check int64) "file payload bytes" 3L binary.binary_payload_bytes;
      let second = scan () in
      Alcotest.(check string)
        "deterministic JSON" (Corpus.json report) (Corpus.json second);
      Alcotest.(check string)
        "deterministic human report" (Corpus.human report) (Corpus.human second);
      Alcotest.(check string)
        "human report"
        "holyc-corpus-lex-v1\n\
         phase lex\n\
         input filesystem-tree\n\
         templeos-reference synthetic-reference\n\
         files 3\n\
         tokenizes 3\n\
         failed 0\n\
         bytes 19\n\
         lexed-bytes 15\n\
         tokens 4\n\
         nul-terminated-files 1\n\
         binary-payload-bytes 3\n"
        (Corpus.human report))

let failures_are_recorded () =
  with_temp_directory (fun root ->
      write_file (Filename.concat root "bad.HC") "\"open";
      let report =
        Corpus.lex_tree ~reference_commit:"synthetic-reference" ~root ()
        |> checked
      in
      Alcotest.(check int) "failure count" 1 (Corpus.failure_count report);
      let result = file report "bad.HC" in
      Alcotest.(check string)
        "status" "lexer-diagnostics"
        (Corpus.status_name result.status);
      Alcotest.(check int) "diagnostic count" 1 result.diagnostic_count;
      match result.first_diagnostic with
      | Some diagnostic ->
          Alcotest.(check string) "diagnostic code" "HCLEX0003" diagnostic.code;
          Alcotest.(check int) "diagnostic line" 1 diagnostic.line;
          Alcotest.(check int) "diagnostic column" 1 diagnostic.column
      | None -> Alcotest.fail "missing first lexer diagnostic")

let file_limit () =
  with_temp_directory (fun root ->
      write_file (Filename.concat root "large.HC") "12345";
      let report =
        Corpus.lex_tree ~max_file_bytes:4
          ~reference_commit:"synthetic-reference" ~root ()
        |> checked
      in
      let result = file report "large.HC" in
      Alcotest.(check string)
        "status" "read-error"
        (Corpus.status_name result.status);
      Alcotest.(check int) "failure count" 1 (Corpus.failure_count report);
      Alcotest.(check bool) "report fails" true (Corpus.has_failures report))

let parser_synthetic_tree () =
  with_temp_directory (fun root ->
      let nested = Filename.concat root "nested" in
      make_directory nested;
      write_file (Filename.concat root "a.HC") "#define FLAG I64\nI64 a;";
      write_file
        (Filename.concat root "b.HC")
        "#ifdef FLAG\n}\n#else\nI64 b;\n#endif";
      write_file (Filename.concat nested "inc.HH") "I64 inc;";
      write_file
        (Filename.concat root "z.PRJ")
        "#include \"nested/inc.HH\"\nI64 z;";
      let scan () =
        Corpus.Parse.tree ~reference_commit:"synthetic-reference"
          ~compilation_mode:Preprocessor.Aot ~root ()
        |> checked
      in
      let report = scan () in
      Alcotest.(check (list string))
        "sorted source paths"
        [ "a.HC"; "b.HC"; "nested/inc.HH"; "z.PRJ" ]
        (Corpus.Parse.files report
        |> List.map (fun (item : Corpus.Parse.file_result) -> item.path));
      Alcotest.(check int) "source count" 4 (Corpus.Parse.file_count report);
      Alcotest.(check int) "parsed count" 4 (Corpus.Parse.parses_count report);
      Alcotest.(check int) "failure count" 0 (Corpus.Parse.failure_count report);
      Alcotest.(check int64)
        "source bytes" 95L
        (Corpus.Parse.total_bytes report);
      Alcotest.(check int)
        "diagnostic count" 0
        (Corpus.Parse.diagnostic_count report);
      Alcotest.(check bool)
        "report passes" false
        (Corpus.Parse.has_failures report);
      let second = scan () in
      Alcotest.(check string)
        "deterministic JSON" (Corpus.Parse.json report)
        (Corpus.Parse.json second);
      Alcotest.(check string)
        "deterministic human report"
        (Corpus.Parse.human report)
        (Corpus.Parse.human second);
      Alcotest.(check string)
        "human report"
        "holyc-corpus-parse-v1\n\
         phase parse\n\
         input filesystem-tree\n\
         templeos-reference synthetic-reference\n\
         compilation-mode aot\n\
         files 4\n\
         parses 4\n\
         failed 0\n\
         frontend-diagnostics 0\n\
         parser-diagnostics 0\n\
         read-errors 0\n\
         internal-errors 0\n\
         bytes 95\n\
         diagnostics 0\n\
         errors 0\n\
         warnings 0\n\
         notes 0\n"
        (Corpus.Parse.human report))

let parser_physical_nul_termination () =
  with_temp_directory (fun root ->
      write_file (Filename.concat root "child.HH") "I64 included;\x00}";
      write_file (Filename.concat root "direct.HC") "I64 direct;\x00}";
      write_file
        (Filename.concat root "main.HC")
        "#include \"child.HH\"\nI64 root;";
      let report =
        Corpus.Parse.tree ~reference_commit:"synthetic-reference"
          ~compilation_mode:Preprocessor.Aot ~root ()
        |> checked
      in
      Alcotest.(check int) "source count" 3 (Corpus.Parse.file_count report);
      Alcotest.(check int) "parsed count" 3 (Corpus.Parse.parses_count report);
      Alcotest.(check int)
        "diagnostics" 0
        (Corpus.Parse.diagnostic_count report);
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"strict.HC" ~contents:"I64 value;\x00}"
      in
      let config =
        Preprocessor.Config.create ~working_directory:root () |> checked
      in
      let output =
        Parser.parse ~sources:(Session.sources session)
          ~definitions:(Session.definitions session)
          ~symbols:(Session.symbols session) ~config source
      in
      Alcotest.(check bool) "standalone AST" true (Option.is_none output.ast);
      match output.diagnostics with
      | first :: _ ->
          Alcotest.(check string) "standalone NUL" "HCLEX0006" first.code
      | [] -> Alcotest.fail "standalone parse accepted an embedded NUL")

let parser_diagnostics_are_classified () =
  with_temp_directory (fun root ->
      write_file (Filename.concat root "bad.HC") "}";
      write_file (Filename.concat root "front.HH") "\"open";
      write_file (Filename.concat root "good.HC") "I64 good;";
      write_file (Filename.concat root "warning.PRJ") "#assert 0\nI64 warned;";
      let report =
        Corpus.Parse.tree ~reference_commit:"synthetic-reference"
          ~compilation_mode:Preprocessor.Jit ~root ()
        |> checked
      in
      Alcotest.(check int) "parsed count" 2 (Corpus.Parse.parses_count report);
      Alcotest.(check int)
        "frontend failures" 1
        (Corpus.Parse.frontend_diagnostic_count report);
      Alcotest.(check int)
        "parser failures" 1
        (Corpus.Parse.parser_diagnostic_count report);
      Alcotest.(check int) "failure count" 2 (Corpus.Parse.failure_count report);
      Alcotest.(check int) "error count" 2 (Corpus.Parse.error_count report);
      Alcotest.(check int) "warning count" 1 (Corpus.Parse.warning_count report);
      let bad = parse_file report "bad.HC" in
      Alcotest.(check string)
        "parser status" "parser-diagnostics"
        (Corpus.Parse.status_name bad.status);
      (match bad.first_error with
      | Some diagnostic ->
          Alcotest.(check string)
            "parser diagnostic" "HCPARSE0050" diagnostic.code;
          Alcotest.(check string) "parser path" "bad.HC" diagnostic.path
      | None -> Alcotest.fail "missing parser diagnostic");
      let front = parse_file report "front.HH" in
      Alcotest.(check string)
        "frontend status" "frontend-diagnostics"
        (Corpus.Parse.status_name front.status);
      (match front.first_error with
      | Some diagnostic ->
          Alcotest.(check string)
            "frontend diagnostic" "HCLEX0003" diagnostic.code
      | None -> Alcotest.fail "missing frontend diagnostic");
      let warning = parse_file report "warning.PRJ" in
      Alcotest.(check string)
        "warning file parses" "parses"
        (Corpus.Parse.status_name warning.status);
      Alcotest.(check int) "warning file count" 1 warning.warning_count;
      Alcotest.(check bool)
        "report fails" true
        (Corpus.Parse.has_failures report))

let parser_file_limit_and_redaction () =
  with_temp_directory (fun root ->
      write_file (Filename.concat root "child.HC") "123456789012345678901";
      write_file (Filename.concat root "main.HC") "#include \"child.HC\"";
      let report =
        Corpus.Parse.tree ~max_file_bytes:20
          ~reference_commit:"synthetic-reference"
          ~compilation_mode:Preprocessor.Aot ~root ()
        |> checked
      in
      let child = parse_file report "child.HC" in
      Alcotest.(check string)
        "oversized root" "read-error"
        (Corpus.Parse.status_name child.status);
      let main = parse_file report "main.HC" in
      Alcotest.(check string)
        "include failure" "frontend-diagnostics"
        (Corpus.Parse.status_name main.status);
      (match main.first_error with
      | Some diagnostic ->
          Alcotest.(check bool)
            "reference marker" true
            (String.starts_with
               ~prefix:
                 "could not read included source \"child.HC\" at \
                  <reference>/child.HC"
               diagnostic.message)
      | None -> Alcotest.fail "missing include diagnostic");
      let normalized_root =
        String.map (fun byte -> if Char.equal byte '\\' then '/' else byte) root
      in
      Alcotest.(check bool)
        "JSON omits root" false
        (let json = Corpus.Parse.json report in
         let root_length = String.length normalized_root in
         let rec contains offset =
           offset + root_length <= String.length json
           && (String.sub json offset root_length = normalized_root
              || contains (offset + 1))
         in
         contains 0))

let rec find_workspace directory =
  if
    Sys.file_exists (Filename.concat directory "dune-project")
    && Sys.file_exists (Filename.concat directory ".git")
  then directory
  else
    let parent = Filename.dirname directory in
    if String.equal parent directory then
      Alcotest.fail "could not locate the repository workspace"
    else find_workspace parent

let workspace () =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some directory
    when Sys.file_exists (Filename.concat directory "dune-project")
         && Sys.file_exists (Filename.concat directory ".git") -> directory
  | _ -> find_workspace (Sys.getcwd ())

let pinned_reference () =
  let root = Filename.concat (workspace ()) "third_party/TempleOS" in
  let report =
    Corpus.lex_reference ~expected_commit:Version.reference_commit ~root ()
    |> checked
  in
  Alcotest.(check string)
    "reference commit" Version.reference_commit
    (Corpus.reference_commit report);
  Alcotest.(check int) "source count" 528 (Corpus.file_count report);
  Alcotest.(check int) "tokenized count" 528 (Corpus.tokenizes_count report);
  Alcotest.(check int) "failure count" 0 (Corpus.failure_count report);
  Alcotest.(check int64)
    "canonical source bytes" 4_190_323L
    (Corpus.total_bytes report);
  Alcotest.(check int64)
    "canonical lexed bytes" 2_923_417L
    (Corpus.total_lexed_bytes report);
  Alcotest.(check int64) "token count" 719_304L (Corpus.total_tokens report);
  Alcotest.(check int)
    "NUL-terminated files" 54
    (Corpus.nul_terminated_count report);
  Alcotest.(check int64)
    "binary payload bytes" 1_266_852L
    (Corpus.total_binary_payload_bytes report);
  Alcotest.(check int64)
    "all source bytes accounted for"
    (Corpus.total_bytes report)
    (Int64.add
       (Corpus.total_lexed_bytes report)
       (Int64.add
          (Int64.of_int (Corpus.nul_terminated_count report))
          (Corpus.total_binary_payload_bytes report)));
  let json = Corpus.json report |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "JSON schema" "holyc-corpus-lex-v1"
    (json |> member "schema" |> to_string);
  Alcotest.(check string)
    "JSON input" "verified-git-tree"
    (json |> member "input" |> to_string);
  Alcotest.(check int)
    "JSON file count" 528
    (json |> member "files" |> to_list |> List.length)

let pinned_parser_reference () =
  let root = Filename.concat (workspace ()) "third_party/TempleOS" in
  let report =
    Corpus.Parse.reference ~expected_commit:Version.reference_commit
      ~compilation_mode:Preprocessor.Aot ~root ()
    |> checked
  in
  Alcotest.(check string)
    "reference commit" Version.reference_commit
    (Corpus.Parse.reference_commit report);
  Alcotest.(check string)
    "compilation mode" "aot"
    (Preprocessor.compilation_mode_name (Corpus.Parse.compilation_mode report));
  Alcotest.(check int) "source count" 528 (Corpus.Parse.file_count report);
  Alcotest.(check int) "parsed count" 21 (Corpus.Parse.parses_count report);
  Alcotest.(check int)
    "frontend failures" 17
    (Corpus.Parse.frontend_diagnostic_count report);
  Alcotest.(check int)
    "parser failures" 490
    (Corpus.Parse.parser_diagnostic_count report);
  Alcotest.(check int) "read errors" 0 (Corpus.Parse.read_error_count report);
  Alcotest.(check int)
    "internal errors" 0
    (Corpus.Parse.internal_error_count report);
  Alcotest.(check int) "failure count" 507 (Corpus.Parse.failure_count report);
  Alcotest.(check int64)
    "canonical source bytes" 4_190_323L
    (Corpus.Parse.total_bytes report);
  Alcotest.(check int)
    "diagnostics" 37_194
    (Corpus.Parse.diagnostic_count report);
  Alcotest.(check int) "errors" 37_194 (Corpus.Parse.error_count report);
  Alcotest.(check int) "warnings" 0 (Corpus.Parse.warning_count report);
  Alcotest.(check int) "notes" 0 (Corpus.Parse.note_count report);
  Alcotest.(check bool)
    "report has failures" true
    (Corpus.Parse.has_failures report);
  let expected =
    Filename.concat (workspace ()) "reference/parser-corpus-aot.json"
    |> read_file |> String.trim
  in
  Alcotest.(check string)
    "reviewed parser baseline" expected (Corpus.Parse.json report)

let reference_mismatch () =
  let root = Filename.concat (workspace ()) "third_party/TempleOS" in
  match
    Corpus.lex_reference
      ~expected_commit:"0000000000000000000000000000000000000000" ~root ()
  with
  | Error message ->
      Alcotest.(check bool)
        "mismatch is named" true
        (String.starts_with ~prefix:"reference commit mismatch" message)
  | Ok _ -> Alcotest.fail "expected a reference commit mismatch"

let read_channel channel =
  let buffer = Buffer.create 128 in
  let bytes = Bytes.create 256 in
  let rec loop () =
    match input channel bytes 0 (Bytes.length bytes) with
    | 0 -> Buffer.contents buffer
    | count ->
        Buffer.add_subbytes buffer bytes 0 count;
        loop ()
  in
  loop ()

let initialize_repository root =
  let arguments = [| "git"; "-C"; root; "init"; "--quiet" |] in
  let stdout, stdin, stderr =
    Unix.open_process_args_full "git" arguments (Unix.environment ())
  in
  ignore (read_channel stdout);
  let error_output = read_channel stderr in
  match Unix.close_process_full (stdout, stdin, stderr) with
  | Unix.WEXITED 0 -> ()
  | _ -> Alcotest.fail ("could not initialize test repository: " ^ error_output)

let dirty_reference () =
  with_temp_directory (fun root ->
      initialize_repository root;
      write_file (Filename.concat root "untracked.HC") "public";
      match
        Corpus.lex_reference
          ~expected_commit:"0000000000000000000000000000000000000000" ~root ()
      with
      | Error message ->
          Alcotest.(check string)
            "dirty checkout rejection"
            "reference checkout has uncommitted or untracked files" message
      | Ok _ -> Alcotest.fail "expected a dirty reference rejection")

let tests =
  [
    Alcotest.test_case "synthetic tree" `Quick synthetic_tree;
    Alcotest.test_case "failure records" `Quick failures_are_recorded;
    Alcotest.test_case "file limit" `Quick file_limit;
    Alcotest.test_case "parser synthetic tree" `Quick parser_synthetic_tree;
    Alcotest.test_case "parser physical NUL" `Quick
      parser_physical_nul_termination;
    Alcotest.test_case "parser diagnostics" `Quick
      parser_diagnostics_are_classified;
    Alcotest.test_case "parser file limit and redaction" `Quick
      parser_file_limit_and_redaction;
    Alcotest.test_case "dirty reference" `Quick dirty_reference;
    Alcotest.test_case "reference mismatch" `Quick reference_mismatch;
    Alcotest.test_case "pinned reference" `Slow pinned_reference;
    Alcotest.test_case "pinned parser reference" `Slow pinned_parser_reference;
  ]
