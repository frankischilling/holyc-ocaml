module Source = Opcode_table_source
module Json_util = Yojson.Basic.Util

let expected_reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"
let source_path = "Compiler/OpCodes.DD"

type mode = Write | Check

type config = {
  source : string;
  manifest : string;
  output : string;
  mode : mode;
}

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline ("opcode table generator: " ^ message);
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
  let rec collect source manifest output mode = function
    | [] -> (source, manifest, output, mode)
    | "--source" :: path :: rest ->
        collect (Some path) manifest output mode rest
    | "--manifest" :: path :: rest ->
        collect source (Some path) output mode rest
    | "--output" :: path :: rest ->
        collect source manifest (Some path) mode rest
    | "--check" :: rest -> collect source manifest output Check rest
    | option :: _ -> fail "unknown or incomplete option %S" option
  in
  let arguments = List.tl (Array.to_list Sys.argv) in
  let source, manifest, output, mode = collect None None None Write arguments in
  let required name = function
    | Some value -> value
    | None -> fail "%s is required" name
  in
  {
    source = required "--source" source;
    manifest = required "--manifest" manifest;
    output = required "--output" output;
    mode;
  }

let manifest_metadata path =
  try
    let manifest = Yojson.Basic.from_file path in
    let commit = manifest |> Json_util.member "commit" |> Json_util.to_string in
    if not (String.equal commit expected_reference_commit) then
      fail "manifest pins %s, but this generator audits %s" commit
        expected_reference_commit;
    let files = manifest |> Json_util.member "files" |> Json_util.to_list in
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
    if
      String.length checksum <> 64
      || not
           (String.for_all
              (function
                | '0' .. '9' | 'a' .. 'f' -> true
                | _ -> false)
              checksum)
    then fail "manifest has an invalid SHA-256 for %s" source_path;
    (commit, checksum)
  with
  | Sys_error message -> fail "cannot read %s: %s" path message
  | Yojson.Json_error message -> fail "cannot parse %s: %s" path message
  | Json_util.Type_error (message, _) ->
      fail "manifest %s has the wrong shape: %s" path message

let add_entry buffer kind entry =
  Printf.bprintf buffer
    "    { kind = %s; spelling = %S; templeos_id = %d; source_line = %d };\n"
    kind entry.Source.spelling entry.templeos_id entry.source_line

let add_table buffer name kind entries =
  Printf.bprintf buffer "let %s =\n  [\n" name;
  List.iter (add_entry buffer kind) entries;
  Buffer.add_string buffer "  ]\n"

let render ~commit ~checksum tables =
  let buffer = Buffer.create 8192 in
  Buffer.add_string buffer
    "(* This table is generated from the pinned TempleOS opcode database.\n";
  Buffer.add_string buffer
    "   Run the opcode table generator after an approved reference update. *)\n\n";
  Buffer.add_string buffer "[@@@ocamlformat \"disable\"]\n\n";
  Printf.bprintf buffer "let reference_commit = %S\n" commit;
  Printf.bprintf buffer "let source_path = %S\n" source_path;
  Printf.bprintf buffer "let source_sha256 = %S\n\n" checksum;
  Buffer.add_string buffer "type kind = Language | Assembly\n\n";
  Buffer.add_string buffer
    "type entry = {\n\
    \  kind : kind;\n\
    \  spelling : string;\n\
    \  templeos_id : int;\n\
    \  source_line : int;\n\
     }\n\n";
  add_table buffer "language" "Language" tables.Source.language;
  Buffer.add_char buffer '\n';
  add_table buffer "assembly" "Assembly" tables.Source.assembly;
  Buffer.contents buffer

let check_output path expected =
  let actual = read_file path in
  if not (String.equal actual expected) then
    fail
      "%s is stale; regenerate it with `dune exec tools/opcode_table_gen.exe \
       -- --source third_party/TempleOS/Compiler/OpCodes.DD --manifest \
       reference/manifest.json --output src/generated/opcode_keywords.ml`"
      path

let () =
  let config = parse_arguments () in
  let commit, checksum = manifest_metadata config.manifest in
  let source = read_file config.source in
  (match Source.verify_sha256 ~expected:checksum source with
  | Ok () -> ()
  | Error problem -> fail "%s" (Source.error_to_string problem));
  let tables =
    match Source.parse source with
    | Ok tables -> tables
    | Error problem -> fail "%s" (Source.error_to_string problem)
  in
  let generated = render ~commit ~checksum tables in
  match config.mode with
  | Write -> write_file config.output generated
  | Check -> check_output config.output generated
