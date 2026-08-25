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
    set_binary_mode_in stdout true;
    set_binary_mode_out stdin true;
    set_binary_mode_in stderr true;
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
  let replace pattern text =
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
  let lexer =
    Frontend.Lexer.create ~nul_terminates:true ~recover_normalized_doldoc:true
      source
  in
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

module Parse = struct
  type status =
    | Parses
    | Frontend_diagnostics
    | Parser_diagnostics
    | Read_error
    | Internal_error

  type diagnostic = {
    code : string;
    severity : string;
    message : string;
    path : string;
    line : int;
    column : int;
  }

  type file_result = {
    path : string;
    bytes : int64 option;
    diagnostic_count : int;
    error_count : int;
    warning_count : int;
    note_count : int;
    diagnostic_codes : (string * int) list;
    status : status;
    first_error : diagnostic option;
    failure_message : string option;
  }

  type t = {
    input : string;
    reference_commit : string;
    compilation_mode : Frontend.Preprocessor.compilation_mode;
    files : file_result list;
    parses_count : int;
    frontend_diagnostic_count : int;
    parser_diagnostic_count : int;
    read_error_count : int;
    internal_error_count : int;
    total_bytes : int64;
    total_diagnostic_count : int;
    total_error_count : int;
    total_warning_count : int;
    total_note_count : int;
    diagnostic_codes : (string * int) list;
  }

  module Code_map = Map.Make (String)

  let schema = "holyc-corpus-parse-v1"

  let status_name = function
    | Parses -> "parses"
    | Frontend_diagnostics -> "frontend-diagnostics"
    | Parser_diagnostics -> "parser-diagnostics"
    | Read_error -> "read-error"
    | Internal_error -> "internal-error"

  let reference_commit report = report.reference_commit
  let compilation_mode report = report.compilation_mode
  let files report = report.files
  let file_count report = List.length report.files
  let parses_count report = report.parses_count
  let frontend_diagnostic_count report = report.frontend_diagnostic_count
  let parser_diagnostic_count report = report.parser_diagnostic_count
  let read_error_count report = report.read_error_count
  let internal_error_count report = report.internal_error_count

  let failure_count report =
    report.frontend_diagnostic_count + report.parser_diagnostic_count
    + report.read_error_count + report.internal_error_count

  let total_bytes report = report.total_bytes
  let diagnostic_count report = report.total_diagnostic_count
  let error_count report = report.total_error_count
  let warning_count report = report.total_warning_count
  let note_count report = report.total_note_count
  let diagnostic_codes report = report.diagnostic_codes
  let has_failures report = failure_count report <> 0

  let normalize_path path =
    String.map (fun byte -> if Char.equal byte '\\' then '/' else byte) path

  let trim_trailing_slashes path =
    let rec stop index =
      if index <= 1 then index
      else if Char.equal path.[index - 1] '/' then stop (index - 1)
      else index
    in
    String.sub path 0 (stop (String.length path))

  let replace_all text ~pattern ~replacement =
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
          Buffer.add_string buffer replacement;
          copy (offset + pattern_length))
        else (
          Buffer.add_char buffer text.[offset];
          copy (offset + 1))
      in
      copy 0

  let report_message root message =
    redact_root root message
    |> replace_all ~pattern:"<reference>\\" ~replacement:"<reference>/"

  let source_path ~root ~fallback source =
    let raw = Common.Source_file.path source in
    if Filename.is_relative raw then normalize_path raw
    else
      let root = normalize_path root |> trim_trailing_slashes in
      let path = normalize_path raw in
      let prefix = root ^ "/" in
      if String.equal path root then "."
      else if String.starts_with ~prefix path then
        String.sub path (String.length prefix)
          (String.length path - String.length prefix)
      else
        let display = Common.Source_file.display_path source in
        if Filename.is_relative display then normalize_path display
        else fallback

  let diagnostic_summary ~root ~fallback sources (item : Common.Diagnostic.t) =
    let path, line, column =
      match Common.Source_manager.find sources item.primary.source with
      | None -> (fallback, 0, 0)
      | Some source ->
          let path = source_path ~root ~fallback source in
          let line, column =
            match Common.Source_file.position source item.primary.start with
            | Ok position -> (position.line, position.column)
            | Error _ -> (0, 0)
          in
          (path, line, column)
    in
    {
      code = item.code;
      severity = Common.Diagnostic.severity_name item.severity;
      message = report_message root item.message;
      path;
      line;
      column;
    }

  let count_diagnostics diagnostics =
    List.fold_left
      (fun (errors, warnings, notes) (item : Common.Diagnostic.t) ->
        match item.severity with
        | Common.Diagnostic.Error -> (errors + 1, warnings, notes)
        | Common.Diagnostic.Warning -> (errors, warnings + 1, notes)
        | Common.Diagnostic.Note -> (errors, warnings, notes + 1))
      (0, 0, 0) diagnostics

  let code_counts diagnostics =
    diagnostics
    |> List.fold_left
         (fun counts (item : Common.Diagnostic.t) ->
           Code_map.update item.code
             (function
               | None -> Some 1
               | Some count -> Some (count + 1))
             counts)
         Code_map.empty
    |> Code_map.bindings

  let classify_diagnostic code =
    if String.starts_with ~prefix:"HCPARSE" code then Parser_diagnostics
    else Frontend_diagnostics

  let internal_result ~root ~path ~bytes message =
    {
      path;
      bytes = Some bytes;
      diagnostic_count = 0;
      error_count = 0;
      warning_count = 0;
      note_count = 0;
      diagnostic_codes = [];
      status = Internal_error;
      first_error = None;
      failure_message = Some (report_message root message);
    }

  let parse_source session ~config ~root ~path source =
    let bytes = Int64.of_int (Common.Source_file.length source) in
    try
      let output =
        Frontend.Parser.parse ~sources:(Session.sources session)
          ~definitions:(Session.definitions session)
          ~symbols:(Session.symbols session) ~config source
      in
      let diagnostics = output.diagnostics in
      let diagnostic_count = List.length diagnostics in
      let error_count, warning_count, note_count =
        count_diagnostics diagnostics
      in
      let first_error_item =
        List.find_opt
          (fun (item : Common.Diagnostic.t) ->
            item.severity = Common.Diagnostic.Error)
          diagnostics
      in
      let first_error =
        Option.map
          (diagnostic_summary ~root ~fallback:path (Session.sources session))
          first_error_item
      in
      let status, failure_message =
        match (output.ast, first_error) with
        | Some _, None -> (Parses, None)
        | None, Some diagnostic -> (classify_diagnostic diagnostic.code, None)
        | None, None ->
            ( Internal_error,
              Some "the parser returned no syntax tree and no error diagnostic"
            )
        | Some _, Some _ ->
            ( Internal_error,
              Some "the parser returned a syntax tree while reporting an error"
            )
      in
      {
        path;
        bytes = Some bytes;
        diagnostic_count;
        error_count;
        warning_count;
        note_count;
        diagnostic_codes = code_counts diagnostics;
        status;
        first_error;
        failure_message;
      }
    with error ->
      internal_result ~root ~path ~bytes (Printexc.to_string error)

  let read_error ?bytes ~root ~path message =
    {
      path;
      bytes;
      diagnostic_count = 0;
      error_count = 0;
      warning_count = 0;
      note_count = 0;
      diagnostic_codes = [];
      status = Read_error;
      first_error = None;
      failure_message = Some (report_message root message);
    }

  let make_config ~root ~working_directory ~max_file_bytes ~compilation_mode =
    match
      Frontend.Preprocessor.Config.create ~working_directory ~templeos_root:root
        ~compilation_mode ~max_source_bytes:max_file_bytes
        ~physical_nul_terminates:true ~predefined_date:"01/01/70"
        ~recover_normalized_doldoc:true ~predefined_time:"00:00:00"
        ~command_line_source:false ()
    with
    | Ok config -> Ok config
    | Error message -> Error (report_message root message)

  let scan_file ~config ~root ~max_file_bytes (path, absolute) =
    let session = Session.create () in
    match
      Session.load_source ~max_bytes:max_file_bytes ~display_path:path session
        ~path:absolute
    with
    | Error message -> read_error ~root ~path (report_message root message)
    | Ok source -> parse_source session ~config ~root ~path source

  let scan_reference_file ~config ~root ~max_file_bytes
      (entry : reference_entry) =
    if entry.bytes > Int64.of_int max_file_bytes then
      read_error ~bytes:entry.bytes ~root ~path:entry.path
        (Printf.sprintf "source exceeds the %d-byte corpus file limit"
           max_file_bytes)
    else
      match git_raw root [ "cat-file"; "blob"; entry.object_id ] with
      | Error message ->
          read_error ~bytes:entry.bytes ~root ~path:entry.path
            ("could not read reference object: " ^ message)
      | Ok contents ->
          if Int64.of_int (String.length contents) <> entry.bytes then
            read_error ~bytes:entry.bytes ~root ~path:entry.path
              "Git returned a reference object with an unexpected byte count"
          else
            let session = Session.create () in
            let source =
              Session.add_source session
                ~path:(Filename.concat root entry.path)
                ~contents
            in
            parse_source session ~config ~root ~path:entry.path source

  let checked_sum_int label values =
    let rec loop total = function
      | [] -> Ok total
      | value :: rest ->
          if value < 0 || total > Stdlib.max_int - value then
            Error (label ^ " exceeds the supported report range")
          else loop (total + value) rest
    in
    loop 0 values

  let checked_sum_int64 label values =
    let rec loop total = function
      | [] -> Ok total
      | value :: rest -> (
          match checked_add label total value with
          | Error _ as error -> error
          | Ok total -> loop total rest)
    in
    loop 0L values

  let merged_code_counts files =
    let add_file counts (file : file_result) =
      List.fold_left
        (fun counts (code, count) ->
          Code_map.update code
            (function
              | None -> Some count
              | Some prior -> Some (prior + count))
            counts)
        counts file.diagnostic_codes
    in
    List.fold_left add_file Code_map.empty files |> Code_map.bindings

  let summarize ~input ~reference_commit ~compilation_mode files =
    let ( let* ) = Result.bind in
    let status_count status =
      List.fold_left
        (fun count file -> if file.status = status then count + 1 else count)
        0 files
    in
    let* total_bytes =
      checked_sum_int64 "parser corpus byte count"
        (List.map (fun file -> Option.value file.bytes ~default:0L) files)
    in
    let* total_diagnostic_count =
      checked_sum_int "parser corpus diagnostic count"
        (List.map (fun file -> file.diagnostic_count) files)
    in
    let* total_error_count =
      checked_sum_int "parser corpus error count"
        (List.map (fun file -> file.error_count) files)
    in
    let* total_warning_count =
      checked_sum_int "parser corpus warning count"
        (List.map (fun file -> file.warning_count) files)
    in
    let* total_note_count =
      checked_sum_int "parser corpus note count"
        (List.map (fun file -> file.note_count) files)
    in
    Ok
      {
        input;
        reference_commit;
        compilation_mode;
        files;
        parses_count = status_count Parses;
        frontend_diagnostic_count = status_count Frontend_diagnostics;
        parser_diagnostic_count = status_count Parser_diagnostics;
        read_error_count = status_count Read_error;
        internal_error_count = status_count Internal_error;
        total_bytes;
        total_diagnostic_count;
        total_error_count;
        total_warning_count;
        total_note_count;
        diagnostic_codes = merged_code_counts files;
      }

  let tree ?(max_file_bytes = 64 * 1024 * 1024) ~reference_commit
      ~compilation_mode ~root () =
    if max_file_bytes < 0 then
      Error "parser corpus file byte limit must be nonnegative"
    else
      match canonical_directory root with
      | Error _ as error -> error
      | Ok root -> (
          match
            make_config ~root ~working_directory:root ~max_file_bytes
              ~compilation_mode
          with
          | Error _ as error -> error
          | Ok config -> (
              match discover root with
              | Error _ as error -> error
              | Ok paths ->
                  paths
                  |> List.map (scan_file ~config ~root ~max_file_bytes)
                  |> summarize ~input:"filesystem-tree" ~reference_commit
                       ~compilation_mode))

  let reference ?(max_file_bytes = 64 * 1024 * 1024) ~expected_commit
      ~compilation_mode ~root () =
    if max_file_bytes < 0 then
      Error "parser corpus file byte limit must be nonnegative"
    else
      match validate_reference root expected_commit with
      | Error _ as error -> error
      | Ok root -> (
          match
            make_config ~root ~working_directory:root ~max_file_bytes
              ~compilation_mode
          with
          | Error _ as error -> error
          | Ok config -> (
              match discover_reference root with
              | Error _ as error -> error
              | Ok entries -> (
                  let result =
                    entries
                    |> List.map
                         (scan_reference_file ~config ~root ~max_file_bytes)
                    |> summarize ~input:"verified-git-tree"
                         ~reference_commit:expected_commit ~compilation_mode
                  in
                  match result with
                  | Error _ as error -> error
                  | Ok report -> (
                      match validate_reference root expected_commit with
                      | Error _ as error -> error
                      | Ok _ -> Ok report))))

  let diagnostic_json diagnostic =
    `Assoc
      [
        ("code", `String diagnostic.code);
        ("severity", `String diagnostic.severity);
        ("message", `String diagnostic.message);
        ("path", `String diagnostic.path);
        ("line", `Int diagnostic.line);
        ("column", `Int diagnostic.column);
      ]

  let code_counts_json counts =
    `Assoc (List.map (fun (code, count) -> (code, `Int count)) counts)

  let file_json file =
    `Assoc
      [
        ("path", `String file.path);
        ("status", `String (status_name file.status));
        ( "bytes",
          match file.bytes with
          | None -> `Null
          | Some bytes -> int64_json bytes );
        ("diagnostics", `Int file.diagnostic_count);
        ("errors", `Int file.error_count);
        ("warnings", `Int file.warning_count);
        ("notes", `Int file.note_count);
        ("diagnostic_codes", code_counts_json file.diagnostic_codes);
        ( "first_error",
          match file.first_error with
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
        ("phase", `String "parse");
        ("input", `String report.input);
        ("reference_commit", `String report.reference_commit);
        ( "compilation_mode",
          `String
            (Frontend.Preprocessor.compilation_mode_name report.compilation_mode)
        );
        ( "summary",
          `Assoc
            [
              ("files", `Int (file_count report));
              ("parses", `Int report.parses_count);
              ("frontend_diagnostics", `Int report.frontend_diagnostic_count);
              ("parser_diagnostics", `Int report.parser_diagnostic_count);
              ("read_errors", `Int report.read_error_count);
              ("internal_errors", `Int report.internal_error_count);
              ("failed", `Int (failure_count report));
              ("bytes", int64_json report.total_bytes);
              ("diagnostics", `Int report.total_diagnostic_count);
              ("errors", `Int report.total_error_count);
              ("warnings", `Int report.total_warning_count);
              ("notes", `Int report.total_note_count);
              ("diagnostic_codes", code_counts_json report.diagnostic_codes);
            ] );
        ("files", `List (List.map file_json report.files));
      ]

  let json report = to_yojson report |> Yojson.Safe.pretty_to_string

  let error_json message =
    `Assoc
      [
        ("schema", `String "holyc-corpus-parse-error-v1");
        ("phase", `String "parse");
        ("message", `String message);
      ]
    |> Yojson.Safe.pretty_to_string

  let write_failure buffer file =
    match (file.first_error, file.failure_message) with
    | Some diagnostic, _ ->
        Printf.bprintf buffer "failure %s %s at %s:%d:%d %s %s\n" file.path
          (status_name file.status) diagnostic.path diagnostic.line
          diagnostic.column diagnostic.code diagnostic.message
    | None, Some message ->
        Printf.bprintf buffer "failure %s %s %s\n" file.path
          (status_name file.status) message
    | None, None ->
        Printf.bprintf buffer "failure %s %s\n" file.path
          (status_name file.status)

  let human report =
    let buffer = Buffer.create 512 in
    Printf.bprintf buffer "%s\n" schema;
    Printf.bprintf buffer "phase parse\n";
    Printf.bprintf buffer "input %s\n" report.input;
    Printf.bprintf buffer "templeos-reference %s\n" report.reference_commit;
    Printf.bprintf buffer "compilation-mode %s\n"
      (Frontend.Preprocessor.compilation_mode_name report.compilation_mode);
    Printf.bprintf buffer "files %d\n" (file_count report);
    Printf.bprintf buffer "parses %d\n" report.parses_count;
    Printf.bprintf buffer "failed %d\n" (failure_count report);
    Printf.bprintf buffer "frontend-diagnostics %d\n"
      report.frontend_diagnostic_count;
    Printf.bprintf buffer "parser-diagnostics %d\n"
      report.parser_diagnostic_count;
    Printf.bprintf buffer "read-errors %d\n" report.read_error_count;
    Printf.bprintf buffer "internal-errors %d\n" report.internal_error_count;
    Printf.bprintf buffer "bytes %Ld\n" report.total_bytes;
    Printf.bprintf buffer "diagnostics %d\n" report.total_diagnostic_count;
    Printf.bprintf buffer "errors %d\n" report.total_error_count;
    Printf.bprintf buffer "warnings %d\n" report.total_warning_count;
    Printf.bprintf buffer "notes %d\n" report.total_note_count;
    List.iter
      (fun (code, count) ->
        Printf.bprintf buffer "diagnostic-code %s %d\n" code count)
      report.diagnostic_codes;
    report.files
    |> List.filter (fun file -> file.status <> Parses)
    |> List.iter (write_failure buffer);
    Buffer.contents buffer

  module Comparison = struct
    type outcome = file_result
    type parse_report = t

    type project_order = {
      path : string;
      directory : string;
      includes : string list;
    }

    type comparison =
      | Both_parse
      | Standalone_only
      | Project_prelude_only
      | Neither_parses

    type compared_file = {
      path : string;
      effective_project_directory : string;
      comparison : comparison;
      standalone : outcome;
      project_prelude : outcome;
    }

    type comparison_report = {
      standalone : parse_report;
      project_prelude : parse_report;
      project_orders : project_order list;
      prelude_source : string;
      prelude_files : string list;
      prelude_diagnostic_count : int;
      files : compared_file list;
    }

    type nonrec t = comparison_report

    let schema = "holyc-corpus-parse-comparison-v2"
    let directory_policy = "source-directory"
    let project_paths = [ "Compiler/Compiler.PRJ"; "Kernel/Kernel.PRJ" ]

    let source_directory path =
      let directory = Filename.dirname path |> normalize_path in
      if String.equal directory "" then "." else directory

    let source_working_directory ~root path =
      match source_directory path with
      | "." -> root
      | directory -> Filename.concat root directory

    let starts_with_include line =
      let line = String.trim line in
      if not (String.starts_with ~prefix:"#include" line) then None
      else
        let after_keyword = String.sub line 8 (String.length line - 8) in
        let after_keyword = String.trim after_keyword in
        if String.length after_keyword < 2 || after_keyword.[0] <> '"' then None
        else
          match String.index_from_opt after_keyword 1 '"' with
          | None -> None
          | Some closing -> Some (String.sub after_keyword 1 (closing - 1))

    let project_includes contents =
      contents |> String.split_on_char '\n'
      |> List.filter_map starts_with_include

    let project_order read path =
      Result.map
        (fun contents ->
          {
            path;
            directory = source_directory path;
            includes = project_includes contents;
          })
        (read path)

    let read_orders read =
      let rec loop found = function
        | [] -> Ok (List.rev found)
        | path :: rest -> (
            match project_order read path with
            | Error _ as error -> error
            | Ok order -> loop (order :: found) rest)
      in
      loop [] project_paths

    let header_include spelling =
      String.equal (String.uppercase_ascii (Filename.extension spelling)) ".HH"

    let compiler_prelude orders =
      match
        List.find_opt
          (fun (order : project_order) ->
            String.equal order.path "Compiler/Compiler.PRJ")
          orders
      with
      | None -> Error "the parser corpus did not read Compiler/Compiler.PRJ"
      | Some order ->
          let headers = List.filter header_include order.includes in
          if headers = [] then
            Error
              "Compiler/Compiler.PRJ does not contain a direct header include"
          else Ok headers

    let prelude_contents headers =
      headers
      |> List.map (fun spelling -> Printf.sprintf "#include %S\n" spelling)
      |> String.concat ""

    let build_prelude ~config ~root headers =
      let session = Session.create () in
      let path = Filename.concat root ".holyc-corpus-project-prelude.PRJ" in
      let source =
        Session.add_source session ~path ~contents:(prelude_contents headers)
      in
      try
        let output =
          Frontend.Parser.parse ~sources:(Session.sources session)
            ~definitions:(Session.definitions session)
            ~symbols:(Session.symbols session) ~config source
        in
        Ok (session, List.length output.diagnostics)
      with error ->
        Error
          ("could not build the parser corpus project prelude: "
         ^ Printexc.to_string error)

    let scan_file_with_prelude seed ~root ~max_file_bytes ~compilation_mode
        (path, absolute) =
      let working_directory = source_working_directory ~root path in
      match
        make_config ~root ~working_directory ~max_file_bytes ~compilation_mode
      with
      | Error message -> read_error ~root ~path message
      | Ok config -> (
          let session = Session.fork_frontend seed in
          match
            Session.load_source ~max_bytes:max_file_bytes ~display_path:path
              session ~path:absolute
          with
          | Error message ->
              read_error ~root ~path (report_message root message)
          | Ok source -> parse_source session ~config ~root ~path source)

    let scan_reference_file_with_prelude seed ~root ~max_file_bytes
        ~compilation_mode (entry : reference_entry) =
      if entry.bytes > Int64.of_int max_file_bytes then
        read_error ~bytes:entry.bytes ~root ~path:entry.path
          (Printf.sprintf "source exceeds the %d-byte corpus file limit"
             max_file_bytes)
      else
        match git_raw root [ "cat-file"; "blob"; entry.object_id ] with
        | Error message ->
            read_error ~bytes:entry.bytes ~root ~path:entry.path
              ("could not read reference object: " ^ message)
        | Ok contents -> (
            if Int64.of_int (String.length contents) <> entry.bytes then
              read_error ~bytes:entry.bytes ~root ~path:entry.path
                "Git returned a reference object with an unexpected byte count"
            else
              let working_directory =
                source_working_directory ~root entry.path
              in
              match
                make_config ~root ~working_directory ~max_file_bytes
                  ~compilation_mode
              with
              | Error message -> read_error ~root ~path:entry.path message
              | Ok config ->
                  let session = Session.fork_frontend seed in
                  let source =
                    Session.add_source session
                      ~path:(Filename.concat root entry.path)
                      ~contents
                  in
                  parse_source session ~config ~root ~path:entry.path source)

    let comparison left right =
      match (left.status = Parses, right.status = Parses) with
      | true, true -> Both_parse
      | true, false -> Standalone_only
      | false, true -> Project_prelude_only
      | false, false -> Neither_parses

    let comparison_name = function
      | Both_parse -> "both-parse"
      | Standalone_only -> "standalone-only"
      | Project_prelude_only -> "project-prelude-only"
      | Neither_parses -> "neither-parses"

    let compare_files (standalone : parse_report)
        (project_prelude : parse_report) =
      let rec loop (found : compared_file list) (left : outcome list)
          (right : outcome list) =
        match (left, right) with
        | [], [] -> Ok (List.rev found)
        | standalone :: left, project_prelude :: right
          when String.equal standalone.path project_prelude.path ->
            let item =
              {
                path = standalone.path;
                effective_project_directory = source_directory standalone.path;
                comparison = comparison standalone project_prelude;
                standalone;
                project_prelude;
              }
            in
            loop (item :: found) left right
        | standalone :: _, project_prelude :: _ ->
            Error
              (Printf.sprintf
                 "parser corpus reports disagree on file order at %s and %s"
                 standalone.path project_prelude.path)
        | [], _ :: _ | _ :: _, [] ->
            Error "parser corpus reports contain different file counts"
      in
      loop [] standalone.files project_prelude.files

    let make ~standalone ~project_prelude ~project_orders ~prelude_source
        ~prelude_files ~prelude_diagnostic_count =
      Result.map
        (fun files ->
          {
            standalone;
            project_prelude;
            project_orders;
            prelude_source;
            prelude_files;
            prelude_diagnostic_count;
            files;
          })
        (compare_files standalone project_prelude)

    let read_tree_project ~root ~max_file_bytes path =
      let absolute = Filename.concat root path in
      let session = Session.create () in
      match
        Session.load_source ~max_bytes:max_file_bytes session ~path:absolute
      with
      | Error message ->
          Error (Printf.sprintf "could not read %s: %s" path message)
      | Ok source -> Ok (Common.Source_file.contents source)

    let find_reference_entry (entries : reference_entry list) path =
      match
        List.find_opt
          (fun (entry : reference_entry) -> String.equal entry.path path)
          entries
      with
      | None ->
          Error (Printf.sprintf "the reference tree does not contain %s" path)
      | Some entry -> Ok entry

    let read_reference_project ~root ~max_file_bytes entries path =
      match find_reference_entry entries path with
      | Error _ as error -> error
      | Ok entry when entry.bytes > Int64.of_int max_file_bytes ->
          Error
            (Printf.sprintf "%s exceeds the %d-byte corpus file limit" path
               max_file_bytes)
      | Ok entry -> (
          match git_raw root [ "cat-file"; "blob"; entry.object_id ] with
          | Error message ->
              Error (Printf.sprintf "could not read %s: %s" path message)
          | Ok contents when Int64.of_int (String.length contents) = entry.bytes
            -> Ok contents
          | Ok _ ->
              Error
                (Printf.sprintf
                   "Git returned an unexpected byte count while reading %s" path)
          )

    let tree ?(max_file_bytes = 64 * 1024 * 1024) ~reference_commit
        ~compilation_mode ~root () =
      let ( let* ) = Result.bind in
      if max_file_bytes < 0 then
        Error "parser corpus file byte limit must be nonnegative"
      else
        let* root = canonical_directory root in
        let* config =
          make_config ~root ~working_directory:root ~max_file_bytes
            ~compilation_mode
        in
        let* paths = discover root in
        let* standalone =
          tree ~max_file_bytes ~reference_commit ~compilation_mode ~root ()
        in
        let* project_orders =
          read_orders (read_tree_project ~root ~max_file_bytes)
        in
        let* prelude_files = compiler_prelude project_orders in
        let* seed, prelude_diagnostic_count =
          build_prelude ~config ~root prelude_files
        in
        let* project_prelude =
          paths
          |> List.map
               (scan_file_with_prelude seed ~root ~max_file_bytes
                  ~compilation_mode)
          |> summarize ~input:"filesystem-tree+project-prelude"
               ~reference_commit ~compilation_mode
        in
        make ~standalone ~project_prelude ~project_orders
          ~prelude_source:"Compiler/Compiler.PRJ" ~prelude_files
          ~prelude_diagnostic_count

    let reference ?(max_file_bytes = 64 * 1024 * 1024) ~expected_commit
        ~compilation_mode ~root () =
      let ( let* ) = Result.bind in
      if max_file_bytes < 0 then
        Error "parser corpus file byte limit must be nonnegative"
      else
        let* root = validate_reference root expected_commit in
        let* config =
          make_config ~root ~working_directory:root ~max_file_bytes
            ~compilation_mode
        in
        let* entries = discover_reference root in
        let* standalone =
          reference ~max_file_bytes ~expected_commit ~compilation_mode ~root ()
        in
        let* project_orders =
          read_orders (read_reference_project ~root ~max_file_bytes entries)
        in
        let* prelude_files = compiler_prelude project_orders in
        let* seed, prelude_diagnostic_count =
          build_prelude ~config ~root prelude_files
        in
        let* project_prelude =
          entries
          |> List.map
               (scan_reference_file_with_prelude seed ~root ~max_file_bytes
                  ~compilation_mode)
          |> summarize ~input:"verified-git-tree+project-prelude"
               ~reference_commit:expected_commit ~compilation_mode
        in
        let* report =
          make ~standalone ~project_prelude ~project_orders
            ~prelude_source:"Compiler/Compiler.PRJ" ~prelude_files
            ~prelude_diagnostic_count
        in
        let* _ = validate_reference root expected_commit in
        Ok report

    let standalone report = report.standalone
    let project_prelude report = report.project_prelude
    let project_orders report = report.project_orders
    let prelude_source report = report.prelude_source
    let prelude_files report = report.prelude_files
    let prelude_diagnostic_count report = report.prelude_diagnostic_count
    let files report = report.files

    let count comparison report =
      List.fold_left
        (fun total file ->
          if file.comparison = comparison then total + 1 else total)
        0 report.files

    let both_parse_count report = count Both_parse report
    let standalone_only_count report = count Standalone_only report
    let project_prelude_only_count report = count Project_prelude_only report
    let neither_parses_count report = count Neither_parses report

    let unresolved_name_codes =
      [ "HCPARSE0001"; "HCPARSE0009"; "HCPARSE0048"; "HCPARSE0112" ]

    let unresolved_name_failure file =
      match file.first_error with
      | Some diagnostic -> List.mem diagnostic.code unresolved_name_codes
      | None -> false

    let unresolved_name_count (report : parse_report) =
      List.fold_left
        (fun count file ->
          if unresolved_name_failure file then count + 1 else count)
        0 report.files

    let other_failure_count (report : parse_report) =
      failure_count report - unresolved_name_count report

    let outcome_json file =
      `Assoc
        [
          ("status", `String (status_name file.status));
          ("diagnostics", `Int file.diagnostic_count);
          ("errors", `Int file.error_count);
          ("warnings", `Int file.warning_count);
          ("notes", `Int file.note_count);
          ("diagnostic_codes", code_counts_json file.diagnostic_codes);
          ( "first_error",
            match file.first_error with
            | None -> `Null
            | Some diagnostic -> diagnostic_json diagnostic );
          ( "failure_message",
            match file.failure_message with
            | None -> `Null
            | Some message -> `String message );
        ]

    let summary_json (report : parse_report) =
      `Assoc
        [
          ("parses", `Int report.parses_count);
          ("frontend_diagnostics", `Int report.frontend_diagnostic_count);
          ("parser_diagnostics", `Int report.parser_diagnostic_count);
          ("read_errors", `Int report.read_error_count);
          ("internal_errors", `Int report.internal_error_count);
          ("failed", `Int (failure_count report));
          ("unresolved_name_failures", `Int (unresolved_name_count report));
          ("other_failures", `Int (other_failure_count report));
          ("diagnostics", `Int report.total_diagnostic_count);
          ("errors", `Int report.total_error_count);
          ("warnings", `Int report.total_warning_count);
          ("notes", `Int report.total_note_count);
          ("diagnostic_codes", code_counts_json report.diagnostic_codes);
        ]

    let project_order_json (order : project_order) =
      `Assoc
        [
          ("path", `String order.path);
          ("directory", `String order.directory);
          ( "includes",
            `List (List.map (fun path -> `String path) order.includes) );
        ]

    let compared_file_json (file : compared_file) =
      `Assoc
        [
          ("path", `String file.path);
          ( "effective_project_directory",
            `String file.effective_project_directory );
          ("comparison", `String (comparison_name file.comparison));
          ( "bytes",
            match file.standalone.bytes with
            | None -> `Null
            | Some bytes -> int64_json bytes );
          ("standalone", outcome_json file.standalone);
          ("project_prelude", outcome_json file.project_prelude);
        ]

    let to_yojson report =
      `Assoc
        [
          ("schema", `String schema);
          ("phase", `String "parse");
          ("input", `String report.standalone.input);
          ("reference_commit", `String report.standalone.reference_commit);
          ( "compilation_mode",
            `String
              (Frontend.Preprocessor.compilation_mode_name
                 report.standalone.compilation_mode) );
          ("directory_policy", `String directory_policy);
          ( "project_orders",
            `List (List.map project_order_json report.project_orders) );
          ( "prelude",
            `Assoc
              [
                ("source", `String report.prelude_source);
                ( "files",
                  `List
                    (List.map (fun path -> `String path) report.prelude_files)
                );
                ("diagnostics", `Int report.prelude_diagnostic_count);
              ] );
          ( "summary",
            `Assoc
              [
                ("files", `Int (List.length report.files));
                ("both_parse", `Int (both_parse_count report));
                ("standalone_only", `Int (standalone_only_count report));
                ( "project_prelude_only",
                  `Int (project_prelude_only_count report) );
                ("neither_parses", `Int (neither_parses_count report));
                ("standalone", summary_json report.standalone);
                ("project_prelude", summary_json report.project_prelude);
              ] );
          ("files", `List (List.map compared_file_json report.files));
        ]

    let json report = to_yojson report |> Yojson.Safe.pretty_to_string

    let write_summary buffer label (report : parse_report) =
      Printf.bprintf buffer "%s-parses %d\n" label report.parses_count;
      Printf.bprintf buffer "%s-failed %d\n" label (failure_count report);
      Printf.bprintf buffer "%s-unresolved-name-failures %d\n" label
        (unresolved_name_count report);
      Printf.bprintf buffer "%s-other-failures %d\n" label
        (other_failure_count report)

    let write_outcome_failure buffer label file =
      if file.status <> Parses then
        match (file.first_error, file.failure_message) with
        | Some diagnostic, _ ->
            Printf.bprintf buffer "%s-failure %s %s at %s:%d:%d %s %s\n" label
              file.path (status_name file.status) diagnostic.path
              diagnostic.line diagnostic.column diagnostic.code
              diagnostic.message
        | None, Some message ->
            Printf.bprintf buffer "%s-failure %s %s %s\n" label file.path
              (status_name file.status) message
        | None, None ->
            Printf.bprintf buffer "%s-failure %s %s\n" label file.path
              (status_name file.status)

    let human report =
      let buffer = Buffer.create 1024 in
      Printf.bprintf buffer "%s\n" schema;
      Printf.bprintf buffer "phase parse\n";
      Printf.bprintf buffer "input %s\n" report.standalone.input;
      Printf.bprintf buffer "templeos-reference %s\n"
        report.standalone.reference_commit;
      Printf.bprintf buffer "compilation-mode %s\n"
        (Frontend.Preprocessor.compilation_mode_name
           report.standalone.compilation_mode);
      Printf.bprintf buffer "directory-policy %s\n" directory_policy;
      Printf.bprintf buffer "files %d\n" (List.length report.files);
      Printf.bprintf buffer "prelude-source %s\n" report.prelude_source;
      List.iter (Printf.bprintf buffer "prelude-file %s\n") report.prelude_files;
      Printf.bprintf buffer "prelude-diagnostics %d\n"
        report.prelude_diagnostic_count;
      Printf.bprintf buffer "both-parse %d\n" (both_parse_count report);
      Printf.bprintf buffer "standalone-only %d\n"
        (standalone_only_count report);
      Printf.bprintf buffer "project-prelude-only %d\n"
        (project_prelude_only_count report);
      Printf.bprintf buffer "neither-parses %d\n" (neither_parses_count report);
      write_summary buffer "standalone" report.standalone;
      write_summary buffer "project-prelude" report.project_prelude;
      List.iter
        (fun file ->
          Printf.bprintf buffer "project-directory %s %s\n" file.path
            file.effective_project_directory;
          if file.comparison <> Both_parse then
            Printf.bprintf buffer "comparison %s %s standalone=%s prelude=%s\n"
              file.path
              (comparison_name file.comparison)
              (status_name file.standalone.status)
              (status_name file.project_prelude.status);
          write_outcome_failure buffer "standalone" file.standalone;
          write_outcome_failure buffer "project-prelude" file.project_prelude)
        report.files;
      Buffer.contents buffer

    let has_failures report = has_failures report.project_prelude
  end
end
