open Cmdliner

type output_format = Human | Json

let output_format =
  let values = [ ("human", Human); ("json", Json) ] in
  Arg.enum values

let format_argument =
  let documentation = "Select human or JSON output." in
  Arg.(
    value & opt output_format Human
    & info [ "format" ] ~docv:"FORMAT" ~doc:documentation)

let file_argument =
  let documentation = "HolyC source file to read." in
  Arg.(
    required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc:documentation)

let print_diagnostics format session diagnostics =
  match format with
  | Human ->
      List.iter
        (fun diagnostic ->
          Holyc_lib.Diagnostic_render.human
            (Holyc_lib.Session.sources session)
            diagnostic
          |> output_string stderr)
        diagnostics
  | Json ->
      Holyc_lib.Diagnostic_render.json
        (Holyc_lib.Session.sources session)
        diagnostics
      |> output_string stderr;
      output_char stderr '\n'

let print_tokens format session tokens =
  match format with
  | Human ->
      List.iter
        (fun token ->
          Holyc_lib.Token.human (Holyc_lib.Session.sources session) token
          |> print_endline)
        tokens
  | Json ->
      Holyc_lib.Token.json (Holyc_lib.Session.sources session) tokens
      |> print_endline

let print_help_metadata format session metadata =
  match format with
  | Human ->
      Holyc_lib.Help_metadata.human (Holyc_lib.Session.sources session) metadata
      |> output_string stdout
  | Json ->
      Holyc_lib.Help_metadata.json (Holyc_lib.Session.sources session) metadata
      |> print_endline

let lex_file format path =
  let session = Holyc_lib.Session.create () in
  match Holyc_lib.Session.load_source session ~path with
  | Error message ->
      Printf.eprintf "holyc: could not read %s: %s\n" path message;
      1
  | Ok source -> (
      match Holyc_lib.lex session ~source with
      | Error diagnostics ->
          print_diagnostics format session diagnostics;
          1
      | Ok tokens ->
          print_tokens format session tokens;
          0)

let lex_command =
  let documentation =
    "Tokenize a file without executing directives or later compiler stages."
  in
  let info = Cmd.info "lex" ~doc:documentation in
  Cmd.v info Term.(const lex_file $ format_argument $ file_argument)

let include_roots_argument =
  let documentation =
    "Search this directory after the compiler working directory. Repeat the \
     option to add roots in order."
  in
  Arg.(
    value & opt_all dir []
    & info [ "I"; "include" ] ~docv:"DIR" ~doc:documentation)

let templeos_root_argument =
  let documentation =
    "Map TempleOS paths beginning with / or ::/ to this source-tree root."
  in
  Arg.(
    value
    & opt (some dir) None
    & info [ "templeos-root" ] ~docv:"DIR" ~doc:documentation)

let include_depth_argument =
  let documentation =
    "Allow at most this many simultaneously included files. The root file does \
     not count toward the limit."
  in
  Arg.(
    value & opt int 64
    & info [ "include-depth-limit" ] ~docv:"COUNT" ~doc:documentation)

let include_bytes_argument =
  let documentation =
    "Reject an included source larger than this many bytes."
  in
  Arg.(
    value
    & opt int (64 * 1024 * 1024)
    & info [ "include-byte-limit" ] ~docv:"BYTES" ~doc:documentation)

let definition_depth_argument =
  let documentation =
    "Allow at most this many active definition or predefined-value expansions."
  in
  Arg.(
    value & opt int 64
    & info [ "definition-depth-limit" ] ~docv:"COUNT" ~doc:documentation)

let generated_bytes_argument =
  let documentation =
    "Allow at most this many generated replacement bytes during one \
     preprocessing run. Definitions and predefined values share the budget."
  in
  Arg.(
    value
    & opt int (16 * 1024 * 1024)
    & info
        [ "generated-byte-limit"; "generated-definition-byte-limit" ]
        ~docv:"BYTES" ~doc:documentation)

let conditional_depth_argument =
  let documentation =
    "Allow at most this many nested conditional directives."
  in
  Arg.(
    value & opt int 64
    & info [ "conditional-depth-limit" ] ~docv:"COUNT" ~doc:documentation)

let expression_nodes_argument =
  let documentation =
    "Allow at most this many terms and operators in one #if or #assert \
     expression."
  in
  Arg.(
    value & opt int 512
    & info
        [ "conditional-expression-node-limit" ]
        ~docv:"COUNT" ~doc:documentation)

let compilation_mode_argument =
  let values =
    [ ("jit", Holyc_lib.Preprocessor.Jit); ("aot", Holyc_lib.Preprocessor.Aot) ]
  in
  let documentation =
    "Select which #ifjit or #ifaot branch the preprocessor returns."
  in
  Arg.(
    value
    & opt (enum values) Holyc_lib.Preprocessor.Jit
    & info [ "mode" ] ~docv:"MODE" ~doc:documentation)

let predefined_date_argument =
  let documentation =
    "Set the deterministic MM/DD/YY string returned by __DATE__."
  in
  Arg.(
    value & opt string "01/01/70"
    & info [ "predefined-date" ] ~docv:"MM/DD/YY" ~doc:documentation)

let predefined_time_argument =
  let documentation =
    "Set the deterministic HH:MM:SS string returned by __TIME__."
  in
  Arg.(
    value & opt string "00:00:00"
    & info [ "predefined-time" ] ~docv:"HH:MM:SS" ~doc:documentation)

let command_line_source_argument =
  let documentation =
    "Make __CMD_LINE__ true at TempleOS source depths below one."
  in
  Arg.(value & flag & info [ "command-line-source" ] ~doc:documentation)

let dump_help_metadata_argument =
  let documentation =
    "Print the versioned #help_index and #help_file metadata dump instead of \
     tokens."
  in
  Arg.(value & flag & info [ "dump-help-metadata" ] ~doc:documentation)

let make_preprocessor_config include_roots templeos_root max_include_depth
    max_source_bytes max_definition_depth max_generated_bytes
    max_conditional_depth max_expression_nodes compilation_mode predefined_date
    predefined_time command_line_source =
  Holyc_lib.Preprocessor.Config.create ~working_directory:(Sys.getcwd ())
    ~include_roots ?templeos_root ~compilation_mode ~max_include_depth
    ~max_source_bytes ~max_definition_depth ~max_generated_bytes
    ~max_conditional_depth ~max_expression_nodes ~predefined_date
    ~predefined_time ~command_line_source ()

let preprocess_file format dump_help_metadata include_roots templeos_root
    max_include_depth max_source_bytes max_definition_depth max_generated_bytes
    max_conditional_depth max_expression_nodes compilation_mode predefined_date
    predefined_time command_line_source path =
  let session = Holyc_lib.Session.create () in
  match Holyc_lib.Session.load_source session ~path with
  | Error message ->
      Printf.eprintf "holyc: could not read %s: %s\n" path message;
      1
  | Ok source -> (
      match
        make_preprocessor_config include_roots templeos_root max_include_depth
          max_source_bytes max_definition_depth max_generated_bytes
          max_conditional_depth max_expression_nodes compilation_mode
          predefined_date predefined_time command_line_source
      with
      | Error message ->
          Printf.eprintf "holyc: invalid preprocessor configuration: %s\n"
            message;
          1
      | Ok config ->
          let output = Holyc_lib.preprocess_detailed session ~config ~source in
          if output.diagnostics <> [] then
            print_diagnostics format session output.diagnostics;
          if Holyc_lib.Preprocessor.has_errors output then 1
          else (
            if dump_help_metadata then
              print_help_metadata format session output.help_metadata
            else print_tokens format session output.tokens;
            0))

let preprocess_command =
  let documentation =
    "Tokenize a file while resolving bounded includes, definition expansions, \
     deterministic predefined values, constant #if and #assert expressions, \
     help metadata, and JIT/AOT conditional frames. Unsupported directives \
     produce diagnostics."
  in
  let info = Cmd.info "preprocess" ~doc:documentation in
  Cmd.v info
    Term.(
      const preprocess_file $ format_argument $ dump_help_metadata_argument
      $ include_roots_argument $ templeos_root_argument $ include_depth_argument
      $ include_bytes_argument $ definition_depth_argument
      $ generated_bytes_argument $ conditional_depth_argument
      $ expression_nodes_argument $ compilation_mode_argument
      $ predefined_date_argument $ predefined_time_argument
      $ command_line_source_argument $ file_argument)

let print_ast format session ast =
  match format with
  | Human ->
      Holyc_lib.Ast_dump.human (Holyc_lib.Session.sources session) ast
      |> output_string stdout
  | Json ->
      Holyc_lib.Ast_dump.json (Holyc_lib.Session.sources session) ast
      |> print_endline

let parse_file format include_roots templeos_root max_include_depth
    max_source_bytes max_definition_depth max_generated_bytes
    max_conditional_depth max_expression_nodes compilation_mode predefined_date
    predefined_time command_line_source path =
  let session = Holyc_lib.Session.create () in
  match Holyc_lib.Session.load_source session ~path with
  | Error message ->
      Printf.eprintf "holyc: could not read %s: %s\n" path message;
      1
  | Ok source -> (
      match
        make_preprocessor_config include_roots templeos_root max_include_depth
          max_source_bytes max_definition_depth max_generated_bytes
          max_conditional_depth max_expression_nodes compilation_mode
          predefined_date predefined_time command_line_source
      with
      | Error message ->
          Printf.eprintf "holyc: invalid preprocessor configuration: %s\n"
            message;
          1
      | Ok config -> (
          let output = Holyc_lib.parse_detailed session ~config ~source in
          if output.diagnostics <> [] then
            print_diagnostics format session output.diagnostics;
          match output.ast with
          | None -> 1
          | Some ast ->
              print_ast format session ast;
              0))

let parser_term =
  Term.(
    const parse_file $ format_argument $ include_roots_argument
    $ templeos_root_argument $ include_depth_argument $ include_bytes_argument
    $ definition_depth_argument $ generated_bytes_argument
    $ conditional_depth_argument $ expression_nodes_argument
    $ compilation_mode_argument $ predefined_date_argument
    $ predefined_time_argument $ command_line_source_argument $ file_argument)

let parse_command =
  let documentation =
    "Parse the currently supported HolyC grammar and print the versioned AST."
  in
  Cmd.v (Cmd.info "parse" ~doc:documentation) parser_term

let dump_ast_command =
  let documentation =
    "Print the versioned AST for the currently supported HolyC grammar."
  in
  Cmd.v (Cmd.info "dump-ast" ~doc:documentation) parser_term

let corpus_root_argument =
  let documentation =
    "Verify the TempleOS checkout at this root and read its exact committed \
     source objects."
  in
  Arg.(
    required
    & opt (some dir) None
    & info [ "reference-root" ] ~docv:"DIR" ~doc:documentation)

let corpus_file_bytes_argument =
  let documentation =
    "Reject one corpus source when it exceeds this many bytes."
  in
  Arg.(
    value
    & opt int (64 * 1024 * 1024)
    & info [ "file-byte-limit" ] ~docv:"BYTES" ~doc:documentation)

let corpus_lex format max_file_bytes root =
  match
    Holyc_lib.Corpus.lex_reference ~max_file_bytes
      ~expected_commit:Holyc_lib.Version.reference_commit ~root ()
  with
  | Error message ->
      (match format with
      | Human -> Printf.eprintf "holyc: corpus lex: %s\n" message
      | Json ->
          Holyc_lib.Corpus.error_json message |> output_string stderr;
          output_char stderr '\n');
      1
  | Ok report ->
      (match format with
      | Human -> Holyc_lib.Corpus.human report |> output_string stdout
      | Json -> Holyc_lib.Corpus.json report |> print_endline);
      if Holyc_lib.Corpus.has_failures report then 1 else 0

let corpus_lex_command =
  let documentation =
    "Lex every .HC, .HH, and .PRJ object in the pinned reference tree. The \
     deterministic report records NUL terminators and trailing payload bytes."
  in
  let info = Cmd.info "lex" ~doc:documentation in
  Cmd.v info
    Term.(
      const corpus_lex $ format_argument $ corpus_file_bytes_argument
      $ corpus_root_argument)

let corpus_command =
  let documentation =
    "Measure compatibility stages against a verified TempleOS source tree."
  in
  Cmd.group (Cmd.info "corpus" ~doc:documentation) [ corpus_lex_command ]

let version_command =
  let documentation = "Print compiler and reference revisions." in
  let run () =
    Holyc_lib.Version.render () |> print_endline;
    0
  in
  Cmd.v (Cmd.info "version" ~doc:documentation) Term.(const run $ const ())

let root_command =
  let documentation =
    "Compile HolyC using behavior audited from a pinned TempleOS source tree."
  in
  let info =
    Cmd.info "holyc"
      ~version:(Holyc_lib.Version.package_version ())
      ~doc:documentation
  in
  Cmd.group info
    [
      lex_command;
      preprocess_command;
      parse_command;
      dump_ast_command;
      corpus_command;
      version_command;
    ]

let () = exit (Cmd.eval' root_command)
