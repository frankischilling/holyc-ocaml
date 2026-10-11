open Cmdliner
open Holyc_lib
module Bin = Templeos_bin

type format = Human | Json

exception Failed of string

let fail message = raise (Failed message)

let unwrap = function
  | Ok value -> value
  | Error message -> fail message

let bin_error (e : Bin.error) =
  (match (e.offset, e.record_index) with
    | Some offset, _ -> Printf.sprintf "byte %d: " offset
    | _, Some index -> Printf.sprintf "record %d: " index
    | _ -> "")
  ^ e.message

let read ~limit path =
  if (Unix.stat path).Unix.st_kind <> Unix.S_REG then
    fail "input must be a regular file";
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      if length > limit then fail "input exceeds byte-limit";
      really_input_string channel length)

let canonical path =
  Filename.concat
    (Unix.realpath (Filename.dirname path))
    (Filename.basename path)

let save ~inputs files =
  let equal = Include_resolver.equal_path in
  let protected =
    List.concat_map (fun path -> [ canonical path; Unix.realpath path ]) inputs
  in
  let targets = List.map (fun (path, _) -> canonical path) files in
  List.iteri
    (fun index target ->
      if List.exists (equal target) protected then
        fail "BIN outputs must not overwrite an input";
      if
        List.exists (equal target) (List.filteri (fun i _ -> i < index) targets)
      then fail "BIN output and map paths must be distinct";
      let path = fst (List.nth files index) in
      if
        Sys.file_exists path
        && List.exists (equal (Unix.realpath path)) protected
      then fail "BIN outputs must not overwrite an input alias";
      if Sys.file_exists path && (Unix.stat path).Unix.st_kind <> Unix.S_REG
      then fail "BIN outputs must be regular files")
    targets;
  let staged = ref [] in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (temporary, _) ->
          if Sys.file_exists temporary then Sys.remove temporary)
        !staged)
    (fun () ->
      List.iter
        (fun (path, bytes) ->
          let temporary, channel =
            Filename.open_temp_file ~mode:[ Open_binary ]
              ~temp_dir:(Filename.dirname path) ".holyc-bin-" ".tmp"
          in
          staged := (temporary, path) :: !staged;
          Fun.protect
            ~finally:(fun () -> close_out_noerr channel)
            (fun () ->
              output_string channel bytes;
              close_out channel))
        files;
      List.iter
        (fun (temporary, path) -> Sys.rename temporary path)
        (List.rev !staged))

let run inspect bits main record_path output map byte_limit work_limit roots
    templeos_root format path =
  let session = Session.create () in
  let command_error message =
    (match format with
    | Human -> Printf.eprintf "holyc bin: %s\n" message
    | Json ->
        `Assoc
          [
            ("schema", `String "holyc-command-error-v1");
            ("command", `String "bin");
            ("message", `String message);
          ]
        |> Yojson.Safe.to_string |> prerr_endline);
    1
  in
  try
    if byte_limit <= 0 || byte_limit > Bin.max_file_bytes then
      fail "byte-limit must be between 1 and 16777216";
    if work_limit <= 0 || work_limit > 100_000_000 then
      fail "work-limit must be between 1 and 100000000";
    if
      inspect
      && (main <> [] || record_path <> None || roots <> []
        || templeos_root <> None)
    then fail "--inspect does not accept source compilation options";
    let module_, inputs =
      if inspect then
        let bytes = read ~limit:byte_limit path in
        match Bin.read ~max_bytes:byte_limit ~max_work:work_limit bytes with
        | Ok module_ -> (module_, [ path ])
        | Error error -> fail (bin_error error)
      else
        let config =
          unwrap
            (Preprocessor.Config.create ~working_directory:(Sys.getcwd ())
               ~include_roots:roots ?templeos_root
               ~compilation_mode:Preprocessor.Aot ~max_source_bytes:byte_limit
               ())
        in
        let source =
          unwrap (Session.load_source ~max_bytes:byte_limit session ~path)
        in
        let parsed =
          parse_assembly_detailed ~mode:bits session ~config ~source
        in
        (if parsed.diagnostics <> [] then
           let sources = Session.sources session in
           List.iter
             (fun diagnostic ->
               (match format with
                 | Human -> Diagnostic_render.human sources diagnostic
                 | Json ->
                     Diagnostic.to_yojson sources diagnostic
                     |> Yojson.Safe.to_string)
               |> prerr_endline)
             parsed.diagnostics);
        if
          List.exists
            (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error)
            parsed.diagnostics
        then fail "assembly did not compile";
        let assembly =
          match parsed.assembly with
          | Some value -> value
          | None -> fail "assembly did not compile"
        in
        let encoded =
          match
            Assembly_encoding.encode ~config ~max_code_bytes:byte_limit
              ~max_work:work_limit ~max_binary_input_bytes:byte_limit assembly
          with
          | Ok value -> value
          | Error diagnostic ->
              fail (diagnostic.code ^ ": " ^ diagnostic.message)
        in
        let records =
          match record_path with
          | None -> []
          | Some path -> (
              let json =
                Yojson.Safe.from_string (read ~limit:byte_limit path)
              in
              match Bin.records_of_yojson ~max_work:work_limit json with
              | Ok records -> records
              | Error error -> fail (bin_error error))
        in
        let module_ =
          match
            Assembly_bin.emit ~max_bytes:byte_limit ~max_work:work_limit ~main
              ~records encoded
          with
          | Ok value -> value
          | Error diagnostic ->
              fail (diagnostic.code ^ ": " ^ diagnostic.message)
        in
        let included =
          Source_manager.sources (Session.sources session)
          |> List.map Source_file.path
        in
        let binary =
          List.map
            (fun (input : Assembly_encoding.binary_input) ->
              input.provenance.resolved_path)
            encoded.binary_inputs
        in
        ( module_,
          List.sort_uniq String.compare
            ((path :: List.filter Sys.file_exists included)
            @ binary @ Option.to_list record_path) )
    in
    let files =
      List.filter_map Fun.id
        [
          Option.map (fun path -> (path, Bin.bytes module_)) output;
          Option.map (fun path -> (path, Bin.map module_)) map;
        ]
    in
    save ~inputs files;
    (match format with
    | Human -> output_string stdout (Bin.map module_)
    | Json -> Bin.to_yojson module_ |> Yojson.Safe.to_string |> print_endline);
    0
  with
  | Failed message
  | Sys_error message
  | Invalid_argument message
  | Yojson.Json_error message -> command_error message
  | End_of_file -> command_error "input changed while reading"
  | Unix.Unix_error (error, operation, argument) ->
      command_error
        (Printf.sprintf "%s %s: %s" operation argument
           (Unix.error_message error))

let command =
  let open Arg in
  let inspect =
    value & flag
    & info [ "inspect" ]
        ~doc:"Validate and inspect an existing uncompressed TempleOS BIN file."
  in
  let bits =
    value
    & opt
        (enum [ ("16", Asm_operand.Use16); ("32", Use32); ("64", Use64) ])
        Asm_operand.Use64
    & info [ "bits" ] ~doc:"Initial assembly mode (default 64)."
  in
  let main =
    value & opt_all string []
    & info [ "main" ] ~docv:"LABEL"
        ~doc:
          "Emit a MAIN record for this assembly label. Repeat to preserve \
           entry order."
  in
  let records =
    value
    & opt (some string) None
    & info [ "records" ] ~docv:"JSON"
        ~doc:"Add an explicit IET record array before generated import groups."
  in
  let output =
    value
    & opt (some string) None
    & info [ "o"; "output" ] ~docv:"BIN"
        ~doc:"Write the complete uncompressed TempleOS BIN module."
  in
  let map_argument =
    value
    & opt (some string) None
    & info [ "map" ] ~docv:"FILE"
        ~doc:"Write a deterministic module and patch-record map."
  in
  let bytes =
    value & opt int Bin.max_file_bytes
    & info [ "byte-limit" ] ~docv:"BYTES"
        ~doc:"Bound input and complete padded output (default 16 MiB)."
  in
  let work =
    value & opt int 1_000_000
    & info [ "work-limit" ] ~docv:"WORK"
        ~doc:
          "Bound each encoding, record and module phase independently (default \
           1000000)."
  in
  let roots =
    value & opt_all string []
    & info [ "I"; "include-root" ] ~docv:"DIR"
        ~doc:"Allow includes and BINFILE inputs under this root."
  in
  let templeos =
    value
    & opt (some string) None
    & info [ "templeos-root" ] ~docv:"DIR"
        ~doc:"Map TempleOS absolute source paths under this directory."
  in
  let format =
    value
    & opt (enum [ ("human", Human); ("json", Json) ]) Human
    & info [ "format" ] ~docv:"FORMAT"
        ~doc:"Select the human map or JSON artifact report."
  in
  let file = required & pos 0 (some string) None & info [] ~docv:"FILE" in
  Cmd.v
    (Cmd.info "bin"
       ~doc:
         "Compile standalone AOT assembly to a TempleOS BIN module, or inspect \
          a BIN file.")
    Term.(
      const run $ inspect $ bits $ main $ records $ output $ map_argument
      $ bytes $ work $ roots $ templeos $ format $ file)
