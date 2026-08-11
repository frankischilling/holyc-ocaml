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
    Alcotest.test_case "dirty reference" `Quick dirty_reference;
    Alcotest.test_case "reference mismatch" `Quick reference_mismatch;
    Alcotest.test_case "pinned reference" `Slow pinned_reference;
  ]
