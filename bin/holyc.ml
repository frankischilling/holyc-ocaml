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
    "Allow at most this many simultaneously active definition expansions."
  in
  Arg.(
    value & opt int 64
    & info [ "definition-depth-limit" ] ~docv:"COUNT" ~doc:documentation)

let generated_definition_bytes_argument =
  let documentation =
    "Allow at most this many definition replacement bytes during one \
     preprocessing run."
  in
  Arg.(
    value
    & opt int (16 * 1024 * 1024)
    & info
        [ "generated-definition-byte-limit" ]
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
    "Allow at most this many terms and operators in one #if expression."
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

let preprocess_file format include_roots templeos_root max_include_depth
    max_source_bytes max_definition_depth max_generated_bytes
    max_conditional_depth max_expression_nodes compilation_mode path =
  let session = Holyc_lib.Session.create () in
  match Holyc_lib.Session.load_source session ~path with
  | Error message ->
      Printf.eprintf "holyc: could not read %s: %s\n" path message;
      1
  | Ok source -> (
      match
        Holyc_lib.Preprocessor.Config.create ~working_directory:(Sys.getcwd ())
          ~include_roots ?templeos_root ~compilation_mode ~max_include_depth
          ~max_source_bytes ~max_definition_depth ~max_generated_bytes
          ~max_conditional_depth ~max_expression_nodes ()
      with
      | Error message ->
          Printf.eprintf "holyc: invalid preprocessor configuration: %s\n"
            message;
          1
      | Ok config -> (
          match Holyc_lib.preprocess session ~config ~source with
          | Error diagnostics ->
              print_diagnostics format session diagnostics;
              1
          | Ok tokens ->
              print_tokens format session tokens;
              0))

let preprocess_command =
  let documentation =
    "Tokenize a file while resolving bounded includes, definition expansions, \
     constant #if expressions, and JIT/AOT conditional frames. Unsupported \
     directives produce diagnostics."
  in
  let info = Cmd.info "preprocess" ~doc:documentation in
  Cmd.v info
    Term.(
      const preprocess_file $ format_argument $ include_roots_argument
      $ templeos_root_argument $ include_depth_argument $ include_bytes_argument
      $ definition_depth_argument $ generated_definition_bytes_argument
      $ conditional_depth_argument $ expression_nodes_argument
      $ compilation_mode_argument $ file_argument)

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
  Cmd.group info [ lex_command; preprocess_command; version_command ]

let () = exit (Cmd.eval' root_command)
