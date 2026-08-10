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
          (match format with
          | Human ->
              List.iter
                (fun token ->
                  Holyc_lib.Token.human
                    (Holyc_lib.Session.sources session)
                    token
                  |> print_endline)
                tokens
          | Json ->
              Holyc_lib.Token.json (Holyc_lib.Session.sources session) tokens
              |> print_endline);
          0)

let lex_command =
  let documentation =
    "Tokenize a file without executing directives or later compiler stages."
  in
  let info = Cmd.info "lex" ~doc:documentation in
  Cmd.v info Term.(const lex_file $ format_argument $ file_argument)

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
  Cmd.group info [ lex_command; version_command ]

let () = exit (Cmd.eval' root_command)
