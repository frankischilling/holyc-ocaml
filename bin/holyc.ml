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

let source_only_argument =
  let documentation =
    "Show declarations published from the input stream and omit the pinned \
     keyword, type, register, opcode, and directive seed."
  in
  Arg.(value & flag & info [ "source-only" ] ~doc:documentation)

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

let print_symbols format source_only session =
  let sources = Holyc_lib.Session.sources session in
  let symbols = Holyc_lib.Session.symbols session in
  match format with
  | Human ->
      Holyc_lib.Symbol_visibility.Environment.human ~source_only sources symbols
      |> output_string stdout
  | Json ->
      Holyc_lib.Symbol_visibility.Environment.json ~source_only sources symbols
      |> print_endline

let print_aggregate_layouts format session layouts =
  let sources = Holyc_lib.Session.sources session in
  match format with
  | Human ->
      Holyc_lib.Semantic_aggregate_layout_dump.human sources layouts
      |> output_string stdout
  | Json ->
      Holyc_lib.Semantic_aggregate_layout_dump.json sources layouts
      |> print_endline

let compiler_error_code message =
  match String.index_opt message ':' with
  | Some separator ->
      let candidate = String.sub message 0 separator in
      if String.starts_with ~prefix:"HC" candidate then Some candidate else None
  | None -> None

let print_command_error format ~command message =
  match format with
  | Human -> Printf.eprintf "holyc: %s: %s\n" command message
  | Json ->
      `Assoc
        ([
           ("schema", `String "holyc-command-error-v1");
           ("command", `String command);
           ("message", `String message);
         ]
        @
        match compiler_error_code message with
        | None -> []
        | Some code -> [ ("code", `String code) ])
      |> Yojson.Safe.pretty_to_string |> output_string stderr;
      output_char stderr '\n'

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

let source_parser_options run =
  Term.(
    run $ format_argument $ include_roots_argument $ templeos_root_argument
    $ include_depth_argument $ include_bytes_argument
    $ definition_depth_argument $ generated_bytes_argument
    $ conditional_depth_argument $ expression_nodes_argument
    $ compilation_mode_argument $ predefined_date_argument
    $ predefined_time_argument $ command_line_source_argument $ file_argument)

let source_parser_term run = source_parser_options Term.(const run)

let print_integer_result format result =
  let module VM = Holyc_lib.Ir_integer_interpreter in
  match VM.termination result with
  | VM.Returned (Some word) ->
      let word_type, decimal =
        match word.type_ with
        | VM.I64 -> ("I64", Int64.to_string word.bits)
        | VM.U64 -> ("U64", Printf.sprintf "%Lu" word.bits)
      in
      (match format with
      | Human -> print_endline decimal
      | Json ->
          `Assoc
            [
              ("schema", `String "holyc-integer-expression-v1");
              ("reference_commit", `String VM.reference_commit);
              ("word_type", `String word_type);
              ("word", `String decimal);
              ("executed_steps", `Int (VM.executed_steps result));
            ]
          |> Yojson.Safe.pretty_to_string |> print_endline);
      0
  | VM.Stream_end | VM.Returned None ->
      print_command_error format ~command:"eval"
        "HCEVAL0003: expression execution did not return an integer word";
      1

let print_integer_program_result format mode max_steps max_frame_bytes
    max_call_depth result =
  let module VM = Holyc_lib.Ir_integer_interpreter in
  let mode =
    match mode with
    | Holyc_lib.Preprocessor.Jit -> "jit"
    | Aot -> "aot"
  in
  let termination =
    match VM.termination result with
    | VM.Stream_end -> "stream-end"
    | VM.Returned _ -> "returned"
  in
  let final_value = VM.final_value result in
  let decimal (word : VM.word) =
    match word.type_ with
    | VM.I64 -> Int64.to_string word.bits
    | VM.U64 -> Printf.sprintf "%Lu" word.bits
  in
  (match format with
  | Human -> (
      Printf.printf
        "holyc-integer-program-v1 implementation=%s reference=%s\n\
         mode=%s target=ir arithmetic=runtime-ir\n\
         step-limit=%d\n\
         steps=%d\n\
         termination=%s\n"
        Holyc_lib.Version.implementation_commit VM.reference_commit mode
        max_steps (VM.executed_steps result) termination;
      Printf.printf "frame-byte-limit=%d\ncall-depth-limit=%d\n" max_frame_bytes
        max_call_depth;
      match final_value with
      | None -> print_endline "final-value=none"
      | Some word ->
          Printf.printf "final-value=%s type=%s bits=0x%016Lx\n" (decimal word)
            (match word.type_ with
            | VM.I64 -> "i64"
            | VM.U64 -> "u64")
            word.bits)
  | Json ->
      `Assoc
        [
          ("schema", `String "holyc-integer-program-v1");
          ( "implementation_commit",
            `String Holyc_lib.Version.implementation_commit );
          ("reference_commit", `String VM.reference_commit);
          ("mode", `String mode);
          ("target", `String "ir");
          ("arithmetic", `String "runtime-ir");
          ("step_limit", `Int max_steps);
          ("executed_steps", `Int (VM.executed_steps result));
          ("termination", `String termination);
          ("frame_byte_limit", `Int max_frame_bytes);
          ("call_depth_limit", `Int max_call_depth);
          ( "final_value",
            match final_value with
            | None -> `Null
            | Some word ->
                `Assoc
                  [
                    ( "type",
                      `String
                        (match word.type_ with
                        | VM.I64 -> "i64"
                        | VM.U64 -> "u64") );
                    ("value", `String (decimal word));
                    ("bits", `String (Printf.sprintf "0x%016Lx" word.bits));
                  ] );
        ]
      |> Yojson.Safe.pretty_to_string |> print_endline);
  0

let integer_expression_file ?(max_frame_bytes = 1_048_576)
    ?(max_call_depth = 128) program target dump max_steps format include_roots
    templeos_root max_include_depth max_source_bytes max_definition_depth
    max_generated_bytes max_conditional_depth max_expression_nodes
    compilation_mode predefined_date predefined_time command_line_source path =
  let command = if dump then "dump-ir" else if program then "run" else "eval" in
  let fail message =
    print_command_error format ~command message;
    1
  in
  if target <> "ir" then
    fail "HCRUN0005: only the ir execution target is implemented"
  else if dump && format = Json then
    fail "JSON graph output is not supported; use --format=human"
  else if (not dump) && max_steps <= 0 then
    fail "HCIRVM0001: max_steps must be greater than zero"
  else if program && (max_frame_bytes <= 0 || max_call_depth <= 0) then
    fail
      "HCIRVM0001: max_frame_bytes and max_call_depth must be greater than zero"
  else
    let session = Holyc_lib.Session.create () in
    match Holyc_lib.Session.load_source session ~path with
    | Error message ->
        fail (Printf.sprintf "could not read %s: %s" path message)
    | Ok source -> (
        match
          make_preprocessor_config include_roots templeos_root max_include_depth
            max_source_bytes max_definition_depth max_generated_bytes
            max_conditional_depth max_expression_nodes compilation_mode
            predefined_date predefined_time command_line_source
        with
        | Error message ->
            fail ("invalid preprocessor configuration: " ^ message)
        | Ok config -> (
            let program_value (result : _ Holyc_lib.integer_program_result) =
              if result.diagnostics <> [] then
                print_diagnostics format session result.diagnostics;
              result.value
            in
            let output =
              if dump then
                (if program then
                   Holyc_lib.compile_integer_program session ~config ~source
                   |> Result.map program_value
                   |> Result.map Holyc_lib.integer_program_human
                 else
                   Holyc_lib.lower_integer_expression session ~config ~source
                   |> Result.map (fun graph ->
                       Holyc_lib.Ir_x87_stack.graph graph
                       |> Holyc_lib.Ir_block_graph.human))
                |> Result.map (fun text ->
                    output_string stdout text;
                    0)
              else if program then
                Holyc_lib.run_integer_program ~max_frame_bytes ~max_call_depth
                  session ~config ~source ~max_steps
                |> Result.map program_value
                |> Result.map
                     (print_integer_program_result format compilation_mode
                        max_steps max_frame_bytes max_call_depth)
              else
                Holyc_lib.evaluate_integer_expression session ~config ~source
                  ~max_steps
                |> Result.map (print_integer_result format)
            in
            match output with
            | Ok status -> status
            | Error diagnostics ->
                print_diagnostics format session diagnostics;
                1))

let step_limit_argument =
  Arg.(
    value & opt int 100000
    & info [ "step-limit" ] ~docv:"COUNT"
        ~doc:
          "Execute at most this many IR instructions, including terminators \
           and control-flow instructions. Must be positive.")

let expression_exits =
  Cmd.Exit.info 1
    ~doc:"on an input, configuration, lowering, or evaluation error"
  :: Cmd.Exit.defaults

let eval_command =
  Cmd.v
    (Cmd.info "eval" ~exits:expression_exits
       ~doc:
         "Evaluate one ordinary integer expression statement (EXPR;) at \
          runtime IR semantics with a bounded instruction budget.")
    (source_parser_options
       Term.(
         const (integer_expression_file false "ir" false) $ step_limit_argument))

let run_target_argument =
  Arg.(
    value & opt string "ir"
    & info [ "target" ] ~docv:"TARGET"
        ~doc:"Execution target. Only ir is currently implemented.")

let run_command =
  let frame_limit =
    Arg.(
      value & opt int 1_048_576
      & info [ "frame-byte-limit" ] ~docv:"BYTES"
          ~doc:
            "Maximum simultaneous parameter and local frame bytes. Must be \
             positive.")
  in
  let call_depth =
    Arg.(
      value & opt int 128
      & info [ "call-depth-limit" ] ~docv:"COUNT"
          ~doc:
            "Maximum simultaneously active integer function calls. Must be \
             positive.")
  in
  Cmd.v
    (Cmd.info "run" ~exits:expression_exits
       ~doc:
         "Run checked integer functions, expressions and structured control \
          flow in the bounded IR interpreter.")
    (source_parser_options
       Term.(
         const (fun target steps bytes depth ->
             integer_expression_file ~max_frame_bytes:bytes
               ~max_call_depth:depth true target false steps)
         $ run_target_argument $ step_limit_argument $ frame_limit $ call_depth))

let program_ir_argument =
  Arg.(
    value & flag
    & info [ "program" ]
        ~doc:
          "Lower a batch of integer top-level statements and structured \
           control flow.")

let dump_ir_command =
  Cmd.v
    (Cmd.info "dump-ir" ~exits:expression_exits
       ~doc:
         "Lower one ordinary expression statement (EXPR;) into a verified \
          return harness and print its deterministic IR without executing it.")
    (source_parser_options
       Term.(
         const (fun program -> integer_expression_file program "ir" true 0)
         $ program_ir_argument))

let parser_term = source_parser_term parse_file

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

let dump_symbols_file format source_only include_roots templeos_root
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
          let output = Holyc_lib.parse_detailed session ~config ~source in
          if output.diagnostics <> [] then
            print_diagnostics format session output.diagnostics;
          print_symbols format source_only session;
          if Option.is_none output.ast then 1 else 0)

let dump_symbols_term =
  Term.(
    const dump_symbols_file $ format_argument $ source_only_argument
    $ include_roots_argument $ templeos_root_argument $ include_depth_argument
    $ include_bytes_argument $ definition_depth_argument
    $ generated_bytes_argument $ conditional_depth_argument
    $ expression_nodes_argument $ compilation_mode_argument
    $ predefined_date_argument $ predefined_time_argument
    $ command_line_source_argument $ file_argument)

let dump_symbols_command =
  let documentation =
    "Print the versioned parser visibility state after consuming a HolyC \
     source file. This is not a semantic name-resolution result."
  in
  Cmd.v (Cmd.info "dump-symbols" ~doc:documentation) dump_symbols_term

let dump_layout_file format include_roots templeos_root max_include_depth
    max_source_bytes max_definition_depth max_generated_bytes
    max_conditional_depth max_expression_nodes compilation_mode predefined_date
    predefined_time command_line_source path =
  let session = Holyc_lib.Session.create () in
  match Holyc_lib.Session.load_source session ~path with
  | Error message ->
      print_command_error format ~command:"dump-layout"
        (Printf.sprintf "could not read %s: %s" path message);
      1
  | Ok source -> (
      match
        make_preprocessor_config include_roots templeos_root max_include_depth
          max_source_bytes max_definition_depth max_generated_bytes
          max_conditional_depth max_expression_nodes compilation_mode
          predefined_date predefined_time command_line_source
      with
      | Error message ->
          print_command_error format ~command:"dump-layout"
            ("invalid preprocessor configuration: " ^ message);
          1
      | Ok config -> (
          let output = Holyc_lib.parse_detailed session ~config ~source in
          if output.diagnostics <> [] then
            print_diagnostics format session output.diagnostics;
          match output.ast with
          | None -> 1
          | Some ast -> (
              match Holyc_lib.analyze_aggregate_layouts session ast with
              | Error message ->
                  print_command_error format ~command:"dump-layout" message;
                  1
              | Ok layouts ->
                  print_aggregate_layouts format session layouts;
                  0)))

let dump_layout_command =
  let documentation =
    "Calculate every closed aggregate layout and print its stable identities, \
     base, direct members, target byte facts, resolved types, and provenance."
  in
  let exits =
    Cmd.Exit.info 1
      ~doc:
        "on a source-read, preprocessing, parsing, or aggregate semantic error"
    :: Cmd.Exit.defaults
  in
  Cmd.v
    (Cmd.info "dump-layout" ~doc:documentation ~exits)
    (source_parser_term dump_layout_file)

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

let corpus_reference_commit_argument =
  let documentation =
    "Verify and record this exact TempleOS commit. The default is the \
     compiler's pinned reference; pass a full object ID rather than a branch \
     name."
  in
  Arg.(
    value
    & opt string Holyc_lib.Version.reference_commit
    & info [ "reference-commit" ] ~docv:"COMMIT" ~doc:documentation)

let corpus_compilation_mode_argument =
  let values =
    [ ("jit", Holyc_lib.Preprocessor.Jit); ("aot", Holyc_lib.Preprocessor.Aot) ]
  in
  let documentation =
    "Select which #ifjit or #ifaot branch the parser corpus measures."
  in
  Arg.(
    value
    & opt (enum values) Holyc_lib.Preprocessor.Aot
    & info [ "mode" ] ~docv:"MODE" ~doc:documentation)

let corpus_require_all_argument =
  let documentation =
    "Return a failure status unless every corpus file completes this phase."
  in
  Arg.(value & flag & info [ "require-all" ] ~doc:documentation)

let corpus_lex format max_file_bytes expected_commit root =
  match
    Holyc_lib.Corpus.lex_reference ~max_file_bytes ~expected_commit ~root ()
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
      $ corpus_reference_commit_argument $ corpus_root_argument)

let corpus_parse format max_file_bytes expected_commit compilation_mode
    require_all root =
  match
    Holyc_lib.Corpus.Parse.Comparison.reference ~max_file_bytes ~expected_commit
      ~compilation_mode ~root ()
  with
  | Error message ->
      (match format with
      | Human -> Printf.eprintf "holyc: corpus parse: %s\n" message
      | Json ->
          Holyc_lib.Corpus.Parse.error_json message |> output_string stderr;
          output_char stderr '\n');
      1
  | Ok report ->
      (match format with
      | Human ->
          Holyc_lib.Corpus.Parse.Comparison.human report |> output_string stdout
      | Json -> Holyc_lib.Corpus.Parse.Comparison.json report |> print_endline);
      if require_all && Holyc_lib.Corpus.Parse.Comparison.has_failures report
      then 1
      else 0

let corpus_parse_command =
  let documentation =
    "Parse every .HC, .HH, and .PRJ object in the pinned reference tree both \
     in a fresh session and with the project-header prelude. Known \
     incompatibilities remain visible in the report; --require-all gates the \
     prelude result."
  in
  let info = Cmd.info "parse" ~doc:documentation in
  Cmd.v info
    Term.(
      const corpus_parse $ format_argument $ corpus_file_bytes_argument
      $ corpus_reference_commit_argument $ corpus_compilation_mode_argument
      $ corpus_require_all_argument $ corpus_root_argument)

let corpus_command =
  let documentation =
    "Measure compatibility stages against a verified TempleOS source tree."
  in
  Cmd.group
    (Cmd.info "corpus" ~doc:documentation)
    [ corpus_lex_command; corpus_parse_command ]

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
      dump_symbols_command;
      dump_layout_command;
      eval_command;
      run_command;
      dump_ir_command;
      corpus_command;
      version_command;
    ]

let () = exit (Cmd.eval' root_command)
