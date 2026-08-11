type status = Tokenizes | Lexer_diagnostics | Read_error | Internal_error

type diagnostic = {
  code : string;
  severity : string;
  message : string;
  line : int;
  column : int;
  start : int;
  stop : int;
}

type file_result = {
  path : string;
  bytes : int64 option;
  lexed_bytes : int64 option;
  tokens : int64;
  diagnostic_count : int;
  nul_terminated : bool;
  binary_payload_bytes : int64;
  status : status;
  first_diagnostic : diagnostic option;
  failure_message : string option;
}

type t = {
  input : string;
  reference_commit : string;
  files : file_result list;
  tokenizes_count : int;
  lexer_diagnostic_count : int;
  read_error_count : int;
  internal_error_count : int;
  total_bytes : int64;
  total_lexed_bytes : int64;
  total_tokens : int64;
  nul_terminated_count : int;
  total_binary_payload_bytes : int64;
}

let schema = "holyc-corpus-lex-v1"

let status_name = function
  | Tokenizes -> "tokenizes"
  | Lexer_diagnostics -> "lexer-diagnostics"
  | Read_error -> "read-error"
  | Internal_error -> "internal-error"

let reference_commit report = report.reference_commit
let files report = report.files
let file_count report = List.length report.files
let tokenizes_count report = report.tokenizes_count

let failure_count report =
  report.lexer_diagnostic_count + report.read_error_count
  + report.internal_error_count

let total_bytes report = report.total_bytes
let total_lexed_bytes report = report.total_lexed_bytes
let total_tokens report = report.total_tokens
let nul_terminated_count report = report.nul_terminated_count
let total_binary_payload_bytes report = report.total_binary_payload_bytes
let has_failures report = failure_count report <> 0

let is_source_name name =
  match String.uppercase_ascii (Filename.extension name) with
  | ".HC" | ".HH" | ".PRJ" -> true
  | _ -> false

let path_message error operation path =
  Printf.sprintf "%s %s: %s" operation path (Unix.error_message error)

let canonical_directory root =
  try
    let canonical = Unix.realpath root in
    let status = Unix.stat canonical in
    if status.st_kind = Unix.S_DIR then Ok canonical
    else Error (Printf.sprintf "reference root is not a directory: %s" root)
  with
  | Unix.Unix_error (error, operation, path) ->
      Error (path_message error operation path)
  | Sys_error message -> Error message

let relative_child parent name =
  if String.equal parent "" then name else parent ^ "/" ^ name

let discover root =
  let rec visit directory relative found =
    try
      let names =
        Sys.readdir directory |> Array.to_list |> List.sort String.compare
      in
      List.fold_left
        (fun result name ->
          match result with
          | Error _ as error -> error
          | Ok found -> (
              if String.equal name ".git" then Ok found
              else
                let absolute = Filename.concat directory name in
                let child = relative_child relative name in
                try
                  match (Unix.lstat absolute).st_kind with
                  | Unix.S_DIR -> visit absolute child found
                  | Unix.S_REG ->
                      if is_source_name name then Ok ((child, absolute) :: found)
                      else Ok found
                  | Unix.S_LNK ->
                      Error
                        (Printf.sprintf
                           "reference tree contains a symbolic link at %s" child)
                  | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO | Unix.S_SOCK ->
                      Error
                        (Printf.sprintf
                           "reference tree contains a non-regular entry at %s"
                           child)
                with
                | Unix.Unix_error (error, operation, path) ->
                    Error (path_message error operation path)
                | Sys_error message -> Error message))
        (Ok found) names
    with
    | Sys_error message -> Error message
    | Unix.Unix_error (error, operation, path) ->
        Error (path_message error operation path)
  in
  match visit root "" [] with
  | Error _ as error -> error
  | Ok files ->
      Ok
        (List.sort
           (fun (left, _) (right, _) -> String.compare left right)
           files)

let read_all channel =
  let buffer = Buffer.create 256 in
  let bytes = Bytes.create 4096 in
  let rec loop () =
    match input channel bytes 0 (Bytes.length bytes) with
    | 0 -> Buffer.contents buffer
    | count ->
        Buffer.add_subbytes buffer bytes 0 count;
        loop ()
  in
  loop ()

let git_raw root arguments =
  let arguments = Array.of_list ("git" :: "-C" :: root :: arguments) in
  try
    let stdout, stdin, stderr =
      Unix.open_process_args_full "git" arguments (Unix.environment ())
    in
    let output = read_all stdout in
    let error_output = read_all stderr in
    match Unix.close_process_full (stdout, stdin, stderr) with
    | Unix.WEXITED 0 -> Ok output
    | Unix.WEXITED code ->
        Error
          (Printf.sprintf "git exited with status %d: %s" code
             (String.trim error_output))
    | Unix.WSIGNALED signal ->
        Error (Printf.sprintf "git was terminated by signal %d" signal)
    | Unix.WSTOPPED signal ->
        Error (Printf.sprintf "git was stopped by signal %d" signal)
  with
  | Unix.Unix_error (error, operation, path) ->
      Error (path_message error operation path)
  | Sys_error message -> Error message

let git root arguments =
  match git_raw root arguments with
  | Ok output -> Ok (String.trim output)
  | Error _ as error -> error

type reference_entry = { path : string; object_id : string; bytes : int64 }

let valid_object_id value =
  String.length value = 40
  && String.for_all
       (function
         | '0' .. '9' | 'a' .. 'f' -> true
         | _ -> false)
       value

let safe_git_path path =
  (not (String.equal path ""))
  && Filename.is_relative path
  && (not (String.contains path '\\'))
  && path |> String.split_on_char '/'
     |> List.for_all (fun part ->
         not
           (String.equal part "" || String.equal part "."
          || String.equal part ".."))

let parse_reference_entry record =
  match String.index_opt record '\t' with
  | None -> Error "Git returned a malformed tree entry without a path"
  | Some separator -> (
      let header = String.sub record 0 separator in
      let path =
        String.sub record (separator + 1) (String.length record - separator - 1)
      in
      let fields =
        header |> String.split_on_char ' '
        |> List.filter (fun field -> not (String.equal field ""))
      in
      match fields with
      | [ mode; kind; object_id; bytes ] -> (
          if not (String.equal kind "blob") then
            Error (Printf.sprintf "reference tree entry is not a blob: %s" path)
          else if String.equal mode "120000" then
            Error
              (Printf.sprintf "reference tree contains a symbolic link at %s"
                 path)
          else if not (String.equal mode "100644" || String.equal mode "100755")
          then
            Error
              (Printf.sprintf "reference tree has unsupported mode %s at %s"
                 mode path)
          else if not (safe_git_path path) then
            Error (Printf.sprintf "reference tree has an unsafe path: %s" path)
          else if not (valid_object_id object_id) then
            Error
              (Printf.sprintf "Git returned an invalid object ID for %s" path)
          else
            match Int64.of_string_opt bytes with
            | Some bytes when bytes >= 0L -> Ok { path; object_id; bytes }
            | _ ->
                Error
                  (Printf.sprintf "Git returned an invalid byte count for %s"
                     path))
      | _ -> Error "Git returned a malformed tree entry header")

let split_nul_records output =
  let rec loop start records =
    if start = String.length output then Ok (List.rev records)
    else
      match String.index_from_opt output start '\x00' with
      | None -> Error "Git returned a tree listing without a NUL terminator"
      | Some stop ->
          let record = String.sub output start (stop - start) in
          if String.equal record "" then
            Error "Git returned an empty tree entry"
          else loop (stop + 1) (record :: records)
  in
  loop 0 []

let discover_reference root =
  match git_raw root [ "ls-tree"; "-rlz"; "--full-tree"; "HEAD" ] with
  | Error message -> Error ("could not read the reference tree: " ^ message)
  | Ok output -> (
      match split_nul_records output with
      | Error _ as error -> error
      | Ok records ->
          let rec parse entries = function
            | [] ->
                Ok
                  (entries
                  |> List.filter (fun entry -> is_source_name entry.path)
                  |> List.sort (fun left right ->
                      String.compare left.path right.path))
            | record :: rest -> (
                match parse_reference_entry record with
                | Error _ as error -> error
                | Ok entry -> parse (entry :: entries) rest)
          in
          parse [] records)

let ensure_clean root =
  match git root [ "status"; "--porcelain=v1"; "--untracked-files=all" ] with
  | Error message -> Error ("could not inspect reference status: " ^ message)
  | Ok "" -> Ok ()
  | Ok _ -> Error "reference checkout has uncommitted or untracked files"

let validate_reference root expected_commit =
  match canonical_directory root with
  | Error _ as error -> error
  | Ok root -> (
      match git root [ "rev-parse"; "--show-toplevel" ] with
      | Error message ->
          Error ("could not locate the reference Git worktree: " ^ message)
      | Ok top -> (
          match canonical_directory top with
          | Error _ as error -> error
          | Ok top when not (Frontend.Include_resolver.equal_path top root) ->
              Error "reference root must be the Git worktree root"
          | Ok _ -> (
              match ensure_clean root with
              | Error _ as error -> error
              | Ok () -> (
                  match git root [ "rev-parse"; "--verify"; "HEAD" ] with
                  | Error message ->
                      Error ("could not read the reference commit: " ^ message)
                  | Ok actual when String.equal actual expected_commit ->
                      Ok root
                  | Ok actual ->
                      Error
                        (Printf.sprintf
                           "reference commit mismatch: expected %s, found %s"
                           expected_commit actual)))))

let redact_root root message =
  let replace text pattern =
    if String.equal pattern "" then text
    else
      let pattern_length = String.length pattern in
      let buffer = Buffer.create (String.length text) in
      let rec copy offset =
        if offset >= String.length text then Buffer.contents buffer
        else if
          offset + pattern_length <= String.length text
          && String.sub text offset pattern_length = pattern
        then (
          Buffer.add_string buffer "<reference>";
          copy (offset + pattern_length))
        else (
          Buffer.add_char buffer text.[offset];
          copy (offset + 1))
      in
      copy 0
  in
  message |> replace root
  |> replace
       (String.map
          (fun byte -> if Char.equal byte '\\' then '/' else byte)
          root)

let diagnostic_summary source (item : Common.Diagnostic.t) =
  let line, column =
    match Common.Source_file.position source item.primary.start with
    | Ok position -> (position.line, position.column)
    | Error _ -> (0, 0)
  in
  {
    code = item.code;
    severity = Common.Diagnostic.severity_name item.severity;
    message = item.message;
    line;
    column;
    start = item.primary.start;
    stop = item.primary.stop;
  }

let lex_source source =
  let lexer = Frontend.Lexer.create ~nul_terminates:true source in
  let rec loop tokens diagnostic_count first_diagnostic =
    match Frontend.Lexer.next lexer with
    | Frontend.Lexer.Token token
      when token.Frontend.Token.kind = Frontend.Token_kind.Eof ->
        ( tokens,
          diagnostic_count,
          first_diagnostic,
          Frontend.Lexer.termination lexer )
    | Frontend.Lexer.Token _ ->
        loop (Int64.succ tokens) diagnostic_count first_diagnostic
    | Frontend.Lexer.Diagnostic item ->
        let first_diagnostic =
          match first_diagnostic with
          | Some _ -> first_diagnostic
          | None -> Some (diagnostic_summary source item)
        in
        loop tokens (diagnostic_count + 1) first_diagnostic
  in
  loop 0L 0 None

let read_error ?bytes ~path message =
  {
    path;
    bytes;
    lexed_bytes = None;
    tokens = 0L;
    diagnostic_count = 0;
    nul_terminated = false;
    binary_payload_bytes = 0L;
    status = Read_error;
    first_diagnostic = None;
    failure_message = Some message;
  }

let scan_source ~root ~path source =
  let bytes = Int64.of_int (Common.Source_file.length source) in
  try
    let tokens, diagnostic_count, first_diagnostic, termination =
      lex_source source
    in
    let lexed_bytes, nul_terminated, binary_payload_bytes =
      match termination with
      | Some Frontend.Lexer.Physical_eof -> (bytes, false, 0L)
      | Some
          (Frontend.Lexer.Nul_terminated { terminator_offset; trailing_bytes })
        -> (Int64.of_int terminator_offset, true, Int64.of_int trailing_bytes)
      | None -> failwith "lexer reached EOF without recording termination"
    in
    {
      path;
      bytes = Some bytes;
      lexed_bytes = Some lexed_bytes;
      tokens;
      diagnostic_count;
      nul_terminated;
      binary_payload_bytes;
      status = (if diagnostic_count = 0 then Tokenizes else Lexer_diagnostics);
      first_diagnostic;
      failure_message = None;
    }
  with error ->
    {
      path;
      bytes = Some bytes;
      lexed_bytes = None;
      tokens = 0L;
      diagnostic_count = 0;
      nul_terminated = false;
      binary_payload_bytes = 0L;
      status = Internal_error;
      first_diagnostic = None;
      failure_message = Some (redact_root root (Printexc.to_string error));
    }

let scan_file session ~root ~max_file_bytes (relative, absolute) =
  match
    Session.load_source ~max_bytes:max_file_bytes ~display_path:relative session
      ~path:absolute
  with
  | Error message -> read_error ~path:relative (redact_root root message)
  | Ok source -> scan_source ~root ~path:relative source

let scan_reference_file session ~root ~max_file_bytes entry =
  if entry.bytes > Int64.of_int max_file_bytes then
    read_error ~bytes:entry.bytes ~path:entry.path
      (Printf.sprintf "source exceeds the %d-byte corpus file limit"
         max_file_bytes)
  else
    match git_raw root [ "cat-file"; "blob"; entry.object_id ] with
    | Error message ->
        read_error ~bytes:entry.bytes ~path:entry.path
          (redact_root root ("could not read reference object: " ^ message))
    | Ok contents ->
        if Int64.of_int (String.length contents) <> entry.bytes then
          read_error ~bytes:entry.bytes ~path:entry.path
            "Git returned a reference object with an unexpected byte count"
        else
          let source = Session.add_source session ~path:entry.path ~contents in
          scan_source ~root ~path:entry.path source

let checked_add label left right =
  if right < 0L || left > Int64.sub Int64.max_int right then
    Error (label ^ " exceeds the supported 64-bit report range")
  else Ok (Int64.add left right)

let summarize ~input reference_commit files =
  let rec fold tokenizes lexer_diagnostics read_errors internal_errors bytes
      lexed_bytes tokens nul_terminated binary_payload_bytes = function
    | [] ->
        Ok
          {
            input;
            reference_commit;
            files;
            tokenizes_count = tokenizes;
            lexer_diagnostic_count = lexer_diagnostics;
            read_error_count = read_errors;
            internal_error_count = internal_errors;
            total_bytes = bytes;
            total_lexed_bytes = lexed_bytes;
            total_tokens = tokens;
            nul_terminated_count = nul_terminated;
            total_binary_payload_bytes = binary_payload_bytes;
          }
    | file :: rest -> (
        let tokenizes, lexer_diagnostics, read_errors, internal_errors =
          match file.status with
          | Tokenizes ->
              (tokenizes + 1, lexer_diagnostics, read_errors, internal_errors)
          | Lexer_diagnostics ->
              (tokenizes, lexer_diagnostics + 1, read_errors, internal_errors)
          | Read_error ->
              (tokenizes, lexer_diagnostics, read_errors + 1, internal_errors)
          | Internal_error ->
              (tokenizes, lexer_diagnostics, read_errors, internal_errors + 1)
        in
        let file_bytes = Option.value file.bytes ~default:0L in
        match checked_add "corpus byte count" bytes file_bytes with
        | Error _ as error -> error
        | Ok bytes -> (
            let file_lexed_bytes = Option.value file.lexed_bytes ~default:0L in
            match
              checked_add "corpus lexed byte count" lexed_bytes file_lexed_bytes
            with
            | Error _ as error -> error
            | Ok lexed_bytes -> (
                match checked_add "corpus token count" tokens file.tokens with
                | Error _ as error -> error
                | Ok tokens -> (
                    match
                      checked_add "corpus binary payload byte count"
                        binary_payload_bytes file.binary_payload_bytes
                    with
                    | Error _ as error -> error
                    | Ok binary_payload_bytes ->
                        fold tokenizes lexer_diagnostics read_errors
                          internal_errors bytes lexed_bytes tokens
                          (nul_terminated + if file.nul_terminated then 1 else 0)
                          binary_payload_bytes rest))))
  in
  fold 0 0 0 0 0L 0L 0L 0 0L files

let lex_canonical_tree ?(max_file_bytes = 64 * 1024 * 1024) ~reference_commit
    root =
  if max_file_bytes < 0 then Error "corpus file byte limit must be nonnegative"
  else
    match discover root with
    | Error _ as error -> error
    | Ok paths ->
        let session = Session.create () in
        paths
        |> List.map (scan_file session ~root ~max_file_bytes)
        |> summarize ~input:"filesystem-tree" reference_commit

let lex_tree ?max_file_bytes ~reference_commit ~root () =
  match canonical_directory root with
  | Error _ as error -> error
  | Ok root -> lex_canonical_tree ?max_file_bytes ~reference_commit root

let lex_reference_tree ?(max_file_bytes = 64 * 1024 * 1024) ~reference_commit
    root =
  if max_file_bytes < 0 then Error "corpus file byte limit must be nonnegative"
  else
    match discover_reference root with
    | Error _ as error -> error
    | Ok entries ->
        let session = Session.create () in
        entries
        |> List.map (scan_reference_file session ~root ~max_file_bytes)
        |> summarize ~input:"verified-git-tree" reference_commit

let lex_reference ?max_file_bytes ~expected_commit ~root () =
  match validate_reference root expected_commit with
  | Error _ as error -> error
  | Ok root -> (
      match
        lex_reference_tree ?max_file_bytes ~reference_commit:expected_commit
          root
      with
      | Error _ as error -> error
      | Ok report -> (
          match validate_reference root expected_commit with
          | Error _ as error -> error
          | Ok _ -> Ok report))

let int64_json value = `Intlit (Int64.to_string value)

let diagnostic_json diagnostic =
  `Assoc
    [
      ("code", `String diagnostic.code);
      ("severity", `String diagnostic.severity);
      ("message", `String diagnostic.message);
      ("line", `Int diagnostic.line);
      ("column", `Int diagnostic.column);
      ("start", `Int diagnostic.start);
      ("stop", `Int diagnostic.stop);
    ]

let file_json (file : file_result) =
  `Assoc
    [
      ("path", `String file.path);
      ("status", `String (status_name file.status));
      ( "bytes",
        match file.bytes with
        | None -> `Null
        | Some bytes -> int64_json bytes );
      ( "lexed_bytes",
        match file.lexed_bytes with
        | None -> `Null
        | Some bytes -> int64_json bytes );
      ("tokens", int64_json file.tokens);
      ("diagnostics", `Int file.diagnostic_count);
      ("nul_terminated", `Bool file.nul_terminated);
      ("binary_payload_bytes", int64_json file.binary_payload_bytes);
      ( "first_diagnostic",
        match file.first_diagnostic with
        | None -> `Null
        | Some diagnostic -> diagnostic_json diagnostic );
      ( "failure_message",
        match file.failure_message with
        | None -> `Null
        | Some message -> `String message );
    ]

let to_yojson report =
  `Assoc
    [
      ("schema", `String schema);
      ("phase", `String "lex");
      ("input", `String report.input);
      ("reference_commit", `String report.reference_commit);
      ( "summary",
        `Assoc
          [
            ("files", `Int (file_count report));
            ("tokenizes", `Int report.tokenizes_count);
            ("lexer_diagnostics", `Int report.lexer_diagnostic_count);
            ("read_errors", `Int report.read_error_count);
            ("internal_errors", `Int report.internal_error_count);
            ("failed", `Int (failure_count report));
            ("bytes", int64_json report.total_bytes);
            ("lexed_bytes", int64_json report.total_lexed_bytes);
            ("tokens", int64_json report.total_tokens);
            ("nul_terminated_files", `Int report.nul_terminated_count);
            ( "binary_payload_bytes",
              int64_json report.total_binary_payload_bytes );
          ] );
      ("files", `List (List.map file_json report.files));
    ]

let json report = to_yojson report |> Yojson.Safe.pretty_to_string

let error_json message =
  `Assoc
    [
      ("schema", `String "holyc-corpus-lex-error-v1");
      ("phase", `String "lex");
      ("message", `String message);
    ]
  |> Yojson.Safe.pretty_to_string

let write_failure buffer file =
  match (file.first_diagnostic, file.failure_message) with
  | Some diagnostic, _ ->
      Printf.bprintf buffer "failure %s:%d:%d %s %s\n" file.path diagnostic.line
        diagnostic.column diagnostic.code diagnostic.message
  | None, Some message ->
      Printf.bprintf buffer "failure %s %s %s\n" file.path
        (status_name file.status) message
  | None, None ->
      Printf.bprintf buffer "failure %s %s\n" file.path
        (status_name file.status)

let human report =
  let buffer = Buffer.create 256 in
  Printf.bprintf buffer "%s\n" schema;
  Printf.bprintf buffer "phase lex\n";
  Printf.bprintf buffer "input %s\n" report.input;
  Printf.bprintf buffer "templeos-reference %s\n" report.reference_commit;
  Printf.bprintf buffer "files %d\n" (file_count report);
  Printf.bprintf buffer "tokenizes %d\n" report.tokenizes_count;
  Printf.bprintf buffer "failed %d\n" (failure_count report);
  Printf.bprintf buffer "bytes %Ld\n" report.total_bytes;
  Printf.bprintf buffer "lexed-bytes %Ld\n" report.total_lexed_bytes;
  Printf.bprintf buffer "tokens %Ld\n" report.total_tokens;
  Printf.bprintf buffer "nul-terminated-files %d\n" report.nul_terminated_count;
  Printf.bprintf buffer "binary-payload-bytes %Ld\n"
    report.total_binary_payload_bytes;
  report.files
  |> List.filter (fun file -> file.status <> Tokenizes)
  |> List.iter (write_failure buffer);
  Buffer.contents buffer
