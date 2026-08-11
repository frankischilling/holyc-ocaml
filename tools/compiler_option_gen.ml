module Source = Compiler_option_source
module Json_util = Yojson.Basic.Util

let expected_reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"
let kernel_source_path = "Kernel/KernelA.HH"
let lex_source_path = "Compiler/Lex.HC"
let cmisc_source_path = "Compiler/CMisc.HC"
let bequ_source_path = "Kernel/KUtils.HC"

let consumer_paths =
  [
    "Compiler/Lex.HC";
    "Compiler/CMisc.HC";
    "Compiler/CExcept.HC";
    "Compiler/CMain.HC";
    "Compiler/LexLib.HC";
    "Compiler/PrsLib.HC";
    "Compiler/PrsStmt.HC";
    "Compiler/PrsVar.HC";
    "Compiler/OptPass3.HC";
    "Compiler/OptPass6.HC";
    "Compiler/OptPass789A.HC";
    "Compiler/BackFA.HC";
    "Compiler/BackLib.HC";
    "Kernel/KHashB.HC";
  ]

let source_paths =
  kernel_source_path :: bequ_source_path :: consumer_paths
  |> List.sort_uniq String.compare

type mode = Write | Check

type config = {
  reference_root : string;
  manifest : string;
  output : string;
  mode : mode;
}

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline ("compiler option generator: " ^ message);
      exit 2)
    format

let read_file path =
  try
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> really_input_string channel (in_channel_length channel))
  with Sys_error message -> fail "cannot read %s: %s" path message

let write_file path contents =
  try
    let channel = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () -> output_string channel contents)
  with Sys_error message -> fail "cannot write %s: %s" path message

let parse_arguments () =
  let rec collect reference_root manifest output mode = function
    | [] -> (reference_root, manifest, output, mode)
    | "--reference-root" :: path :: rest ->
        collect (Some path) manifest output mode rest
    | "--manifest" :: path :: rest ->
        collect reference_root (Some path) output mode rest
    | "--output" :: path :: rest ->
        collect reference_root manifest (Some path) mode rest
    | "--check" :: rest -> collect reference_root manifest output Check rest
    | option :: _ -> fail "unknown or incomplete option %S" option
  in
  let arguments = List.tl (Array.to_list Sys.argv) in
  let reference_root, manifest, output, mode =
    collect None None None Write arguments
  in
  let required name = function
    | Some value -> value
    | None -> fail "%s is required" name
  in
  {
    reference_root = required "--reference-root" reference_root;
    manifest = required "--manifest" manifest;
    output = required "--output" output;
    mode;
  }

let valid_checksum checksum =
  String.length checksum = 64
  && String.for_all
       (function
         | '0' .. '9' | 'a' .. 'f' -> true
         | _ -> false)
       checksum

let manifest_metadata path =
  try
    let manifest = Yojson.Basic.from_file path in
    let commit = manifest |> Json_util.member "commit" |> Json_util.to_string in
    if not (String.equal commit expected_reference_commit) then
      fail "manifest pins %s, but this generator audits %s" commit
        expected_reference_commit;
    let files = manifest |> Json_util.member "files" |> Json_util.to_list in
    let checksum source_path =
      let matching =
        List.find_opt
          (fun file ->
            file |> Json_util.member "path" |> Json_util.to_string
            |> String.equal source_path)
          files
      in
      let checksum =
        match matching with
        | Some file -> file |> Json_util.member "sha256" |> Json_util.to_string
        | None -> fail "manifest does not contain %s" source_path
      in
      if not (valid_checksum checksum) then
        fail "manifest has an invalid SHA-256 for %s" source_path;
      (source_path, checksum)
    in
    (commit, List.map checksum source_paths)
  with
  | Sys_error message -> fail "cannot read %s: %s" path message
  | Yojson.Json_error message -> fail "cannot parse %s: %s" path message
  | Json_util.Type_error (message, _) ->
      fail "manifest %s has the wrong shape: %s" path message

let source_file reference_root logical_path =
  read_file (Filename.concat reference_root logical_path)

let verify_source path checksum source =
  match Source.verify_sha256 ~expected:checksum source with
  | Ok () -> ()
  | Error problem -> fail "%s: %s" path (Source.error_to_string problem)

let add_option buffer = function
  | None -> Buffer.add_string buffer "None"
  | Some value -> Printf.bprintf buffer "Some %S" value

let add_source buffer (path, checksum) =
  Printf.bprintf buffer "    { path = %S; sha256 = %S };\n" path checksum

let add_reference buffer (reference : Source.source_reference) =
  Printf.bprintf buffer "        { path = %S; line = %d };\n" reference.path
    reference.line

let add_compiler_option buffer (entry : Source.option_entry) =
  Printf.bprintf buffer
    "    { name = %S; bit_index = 0x%X; initially_enabled = %b; \
     definition_line = %d; source_comment = "
    entry.name entry.bit_index entry.initially_enabled entry.definition_line;
  add_option buffer entry.source_comment;
  Buffer.add_string buffer "; consumers = [\n";
  List.iter (add_reference buffer) entry.consumers;
  Buffer.add_string buffer "      ] };\n"

let add_gap buffer (gap : Source.gap) =
  Printf.bprintf buffer "    { first = 0x%X; last = 0x%X };\n" gap.first
    gap.last

let render ~commit ~checksums tables =
  let buffer = Buffer.create 16384 in
  Buffer.add_string buffer
    "(* Generated from the pinned TempleOS compiler-option definitions and \
     consumers.\n";
  Buffer.add_string buffer
    "   Regenerate this file only as part of a reviewed reference or table \
     update. *)\n\n";
  Buffer.add_string buffer "[@@@ocamlformat \"disable\"]\n\n";
  Printf.bprintf buffer "let reference_commit = %S\n\n" commit;
  Buffer.add_string buffer
    "type source = { path : string; sha256 : string }\n\n";
  Buffer.add_string buffer
    "type source_reference = { path : string; line : int }\n\n";
  Buffer.add_string buffer
    "type option_entry = {\n\
    \  name : string;\n\
    \  bit_index : int;\n\
    \  initially_enabled : bool;\n\
    \  definition_line : int;\n\
    \  source_comment : string option;\n\
    \  consumers : source_reference list;\n\
     }\n\n";
  Buffer.add_string buffer "type gap = { first : int; last : int }\n\n";
  Buffer.add_string buffer
    "type api = {\n\
    \  option_line : int;\n\
    \  get_option_line : int;\n\
    \  bequ_line : int;\n\
    \  state_expression : string;\n\
    \  set_returns_previous : bool;\n\
     }\n\n";
  Buffer.add_string buffer "let sources =\n  [\n";
  List.iter (add_source buffer) checksums;
  Buffer.add_string buffer "  ]\n\nlet options =\n  [\n";
  List.iter (add_compiler_option buffer) tables.Source.options;
  Buffer.add_string buffer "  ]\n\nlet gaps =\n  [\n";
  List.iter (add_gap buffer) tables.gaps;
  Buffer.add_string buffer "  ]\n\n";
  Printf.bprintf buffer "let default_source_line = %d\n"
    tables.default_source_line;
  Printf.bprintf buffer "let echo_mask_line = %d\n\n" tables.echo_mask_line;
  Printf.bprintf buffer
    "let api =\n\
    \  { option_line = %d; get_option_line = %d; bequ_line = %d; \
     state_expression = %S; set_returns_previous = %b }\n"
    tables.api.option_line tables.api.get_option_line tables.api.bequ_line
    tables.api.state_expression tables.api.set_returns_previous;
  Buffer.contents buffer

let check_output path expected =
  let actual = read_file path in
  if not (String.equal actual expected) then
    fail
      "%s is stale; regenerate it with `dune exec \
       tools/compiler_option_gen.exe -- --reference-root third_party/TempleOS \
       --manifest reference/manifest.json --output \
       src/generated/compiler_options.ml`"
      path

let () =
  let config = parse_arguments () in
  let commit, checksums = manifest_metadata config.manifest in
  let sources =
    List.map
      (fun (path, checksum) ->
        let source = source_file config.reference_root path in
        verify_source path checksum source;
        (path, source))
      checksums
  in
  let source path = List.assoc path sources in
  let consumers = List.map (fun path -> (path, source path)) consumer_paths in
  let tables =
    match
      Source.parse
        ~kernel_source:(source kernel_source_path)
        ~lex_source:(source lex_source_path)
        ~cmisc_source:(source cmisc_source_path)
        ~bequ_source:(source bequ_source_path) ~consumers
    with
    | Ok tables -> tables
    | Error problem -> fail "%s" (Source.error_to_string problem)
  in
  let generated = render ~commit ~checksums tables in
  match config.mode with
  | Write -> write_file config.output generated
  | Check -> check_output config.output generated
