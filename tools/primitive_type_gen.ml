module Source = Primitive_type_source
module Json_util = Yojson.Basic.Util

let expected_reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"
let kernel_source_path = "Kernel/KernelA.HH"
let cinit_source_path = "Compiler/CInit.HC"

type mode = Write | Check

type config = {
  kernel : string;
  cinit : string;
  manifest : string;
  output : string;
  mode : mode;
}

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline ("primitive type generator: " ^ message);
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
  let rec collect kernel cinit manifest output mode = function
    | [] -> (kernel, cinit, manifest, output, mode)
    | "--kernel" :: path :: rest ->
        collect (Some path) cinit manifest output mode rest
    | "--cinit" :: path :: rest ->
        collect kernel (Some path) manifest output mode rest
    | "--manifest" :: path :: rest ->
        collect kernel cinit (Some path) output mode rest
    | "--output" :: path :: rest ->
        collect kernel cinit manifest (Some path) mode rest
    | "--check" :: rest -> collect kernel cinit manifest output Check rest
    | option :: _ -> fail "unknown or incomplete option %S" option
  in
  let arguments = List.tl (Array.to_list Sys.argv) in
  let kernel, cinit, manifest, output, mode =
    collect None None None None Write arguments
  in
  let required name = function
    | Some value -> value
    | None -> fail "%s is required" name
  in
  {
    kernel = required "--kernel" kernel;
    cinit = required "--cinit" cinit;
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
      checksum
    in
    (commit, checksum kernel_source_path, checksum cinit_source_path)
  with
  | Sys_error message -> fail "cannot read %s: %s" path message
  | Yojson.Json_error message -> fail "cannot parse %s: %s" path message
  | Json_util.Type_error (message, _) ->
      fail "manifest %s has the wrong shape: %s" path message

let add_option buffer = function
  | None -> Buffer.add_string buffer "None"
  | Some value -> Printf.bprintf buffer "Some %S" value

let add_raw_type buffer (entry : Source.raw_type) =
  Printf.bprintf buffer
    "    { name = %S; templeos_id = %d; not_implemented = %b; fictitious = %b; \
     source_line = %d; source_comment = "
    entry.Source.name entry.templeos_id entry.not_implemented entry.fictitious
    entry.source_line;
  add_option buffer entry.source_comment;
  Buffer.add_string buffer " };\n"

let add_internal_type buffer (entry : Source.internal_type) =
  Printf.bprintf buffer
    "    { raw_name = %S; byte_size = %d; spelling = %S; source_line = %d };\n"
    entry.Source.raw_name entry.byte_size entry.spelling entry.source_line

let add_public_union buffer (entry : Source.public_union) =
  Printf.bprintf buffer
    "    { storage_spelling = %S; public_spelling = %S; source_line = %d };\n"
    entry.Source.storage_spelling entry.public_spelling entry.source_line

let render ~commit ~kernel_checksum ~cinit_checksum tables =
  let buffer = Buffer.create 8192 in
  Buffer.add_string buffer
    "(* This table is generated from the pinned TempleOS type definitions.\n";
  Buffer.add_string buffer
    "   Run the primitive type generator after an approved reference update. \
     *)\n\n";
  Buffer.add_string buffer "[@@@ocamlformat \"disable\"]\n\n";
  Printf.bprintf buffer "let reference_commit = %S\n" commit;
  Printf.bprintf buffer "let kernel_source_path = %S\n" kernel_source_path;
  Printf.bprintf buffer "let kernel_source_sha256 = %S\n" kernel_checksum;
  Printf.bprintf buffer "let cinit_source_path = %S\n" cinit_source_path;
  Printf.bprintf buffer "let cinit_source_sha256 = %S\n\n" cinit_checksum;
  Buffer.add_string buffer
    "type raw_type = {\n\
    \  name : string;\n\
    \  templeos_id : int;\n\
    \  not_implemented : bool;\n\
    \  fictitious : bool;\n\
    \  source_line : int;\n\
    \  source_comment : string option;\n\
     }\n\n";
  Buffer.add_string buffer
    "type raw_alias = {\n\
    \  name : string;\n\
    \  target_name : string;\n\
    \  templeos_id : int;\n\
    \  source_line : int;\n\
    \  source_comment : string option;\n\
     }\n\n";
  Buffer.add_string buffer
    "type public_union = {\n\
    \  storage_spelling : string;\n\
    \  public_spelling : string;\n\
    \  source_line : int;\n\
     }\n\n";
  Buffer.add_string buffer
    "type internal_type = {\n\
    \  raw_name : string;\n\
    \  byte_size : int;\n\
    \  spelling : string;\n\
    \  source_line : int;\n\
     }\n\n";
  Buffer.add_string buffer "let raw_types =\n  [\n";
  List.iter (add_raw_type buffer) tables.Source.raw_types;
  Buffer.add_string buffer "  ]\n\n";
  let pointer = tables.pointer_alias in
  Printf.bprintf buffer
    "let pointer_alias =\n\
    \  { name = %S; target_name = %S; templeos_id = %d; source_line = %d; \
     source_comment = "
    pointer.Source.name pointer.target_name pointer.templeos_id
    pointer.source_line;
  add_option buffer pointer.source_comment;
  Buffer.add_string buffer " }\n\n";
  Printf.bprintf buffer "let raw_types_count = %d\n" tables.raw_types_count;
  Printf.bprintf buffer "let unsigned_flag = %d\n" tables.unsigned_flag;
  Printf.bprintf buffer "let raw_group_mask = %d\n\n" tables.raw_group_mask;
  Buffer.add_string buffer "let public_unions =\n  [\n";
  List.iter (add_public_union buffer) tables.public_unions;
  Buffer.add_string buffer "  ]\n\n";
  Buffer.add_string buffer "let internal_types =\n  [\n";
  List.iter (add_internal_type buffer) tables.internal_types;
  Buffer.add_string buffer "  ]\n";
  Buffer.contents buffer

let check_output path expected =
  let actual = read_file path in
  if not (String.equal actual expected) then
    fail
      "%s is stale; regenerate it with `dune exec tools/primitive_type_gen.exe \
       -- --kernel third_party/TempleOS/Kernel/KernelA.HH --cinit \
       third_party/TempleOS/Compiler/CInit.HC --manifest \
       reference/manifest.json --output src/generated/primitive_raw_types.ml`"
      path

let verify_source path checksum source =
  match Source.verify_sha256 ~expected:checksum source with
  | Ok () -> ()
  | Error problem -> fail "%s: %s" path (Source.error_to_string problem)

let () =
  let config = parse_arguments () in
  let commit, kernel_checksum, cinit_checksum =
    manifest_metadata config.manifest
  in
  let kernel_source = read_file config.kernel in
  let cinit_source = read_file config.cinit in
  verify_source kernel_source_path kernel_checksum kernel_source;
  verify_source cinit_source_path cinit_checksum cinit_source;
  let tables =
    match Source.parse ~kernel_source ~cinit_source with
    | Ok tables -> tables
    | Error problem -> fail "%s" (Source.error_to_string problem)
  in
  let generated = render ~commit ~kernel_checksum ~cinit_checksum tables in
  match config.mode with
  | Write -> write_file config.output generated
  | Check -> check_output config.output generated
