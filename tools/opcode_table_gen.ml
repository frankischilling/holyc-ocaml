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

let add_entry buffer kind (entry : Source.entry) =
  Printf.bprintf buffer
    "    { kind = %s; spelling = %S; templeos_id = %d; source_line = %d };\n"
    kind entry.Source.spelling entry.templeos_id entry.source_line

let add_table buffer name kind entries =
  Printf.bprintf buffer "let %s =\n  [\n" name;
  List.iter (add_entry buffer kind) entries;
  Buffer.add_string buffer "  ]\n"

let register_kind_name = function
  | Source.R8 -> "R8"
  | Source.R16 -> "R16"
  | Source.R32 -> "R32"
  | Source.R64 -> "R64"
  | Source.Segment -> "Segment"
  | Source.Float_stack -> "Float_stack"
  | Source.Mm -> "Mm"
  | Source.Xmm -> "Xmm"

let add_register buffer (register : Source.register) =
  Printf.bprintf buffer
    "    { register_kind = %s; register_type = %d; spelling = %S; \
     register_number = %d; source_line = %d };\n"
    (register_kind_name register.register_kind)
    register.register_type register.spelling register.register_number
    register.source_line

let add_registers buffer registers =
  Buffer.add_string buffer "let registers =\n  [\n";
  List.iter (add_register buffer) registers;
  Buffer.add_string buffer "  ]\n"

let add_opcode_bytes buffer bytes =
  Buffer.add_char buffer '[';
  List.iteri
    (fun index byte ->
      if index > 0 then Buffer.add_string buffer "; ";
      Printf.bprintf buffer "0x%02X" byte)
    bytes;
  Buffer.add_char buffer ']'

let add_instruction buffer (instruction : Source.instruction) =
  Buffer.add_string buffer "        { entry_index = ";
  Printf.bprintf buffer "%d; opcode_bytes = " instruction.entry_index;
  add_opcode_bytes buffer instruction.opcode_bytes;
  Printf.bprintf buffer
    "; flags = 0x%03X; slash_value = %d; uasm_slash_value = %d; \
     opcode_modifier = %d; argument1 = %d; argument2 = %d; size1 = %d; size2 = \
     %d; source_line = %d };\n"
    instruction.flags instruction.slash_value instruction.uasm_slash_value
    instruction.opcode_modifier instruction.argument1 instruction.argument2
    instruction.size1 instruction.size2 instruction.source_line

let add_alias buffer (alias : Source.opcode_alias) =
  Printf.bprintf buffer "        { spelling = %S; source_line = %d };\n"
    alias.spelling alias.source_line

let add_opcode buffer (opcode : Source.opcode) =
  Printf.bprintf buffer "    { spelling = %S;\n" opcode.spelling;
  Buffer.add_string buffer "      instructions =\n        [\n";
  List.iter (add_instruction buffer) opcode.instructions;
  Buffer.add_string buffer "        ];\n";
  Buffer.add_string buffer "      aliases =\n        [\n";
  List.iter (add_alias buffer) opcode.aliases;
  Buffer.add_string buffer "        ];\n";
  Printf.bprintf buffer "      source_line = %d };\n" opcode.source_line

let add_opcodes buffer opcodes =
  Buffer.add_string buffer "let opcodes =\n  [\n";
  List.iter (add_opcode buffer) opcodes;
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
  Buffer.add_string buffer
    "type register_kind = R8 | R16 | R32 | R64 | Segment | Float_stack | Mm | \
     Xmm\n\n";
  Buffer.add_string buffer
    "type register = {\n\
    \  register_kind : register_kind;\n\
    \  register_type : int;\n\
    \  spelling : string;\n\
    \  register_number : int;\n\
    \  source_line : int;\n\
     }\n\n";
  Buffer.add_string buffer
    "type instruction = {\n\
    \  entry_index : int;\n\
    \  opcode_bytes : int list;\n\
    \  flags : int;\n\
    \  slash_value : int;\n\
    \  uasm_slash_value : int;\n\
    \  opcode_modifier : int;\n\
    \  argument1 : int;\n\
    \  argument2 : int;\n\
    \  size1 : int;\n\
    \  size2 : int;\n\
    \  source_line : int;\n\
     }\n\n";
  Buffer.add_string buffer
    "type opcode_alias = { spelling : string; source_line : int }\n\n";
  Buffer.add_string buffer
    "type opcode = {\n\
    \  spelling : string;\n\
    \  instructions : instruction list;\n\
    \  aliases : opcode_alias list;\n\
    \  source_line : int;\n\
     }\n\n";
  add_registers buffer tables.Source.registers;
  Buffer.add_char buffer '\n';
  add_table buffer "language" "Language" tables.Source.language;
  Buffer.add_char buffer '\n';
  add_table buffer "assembly" "Assembly" tables.Source.assembly;
  Buffer.add_char buffer '\n';
  add_opcodes buffer tables.Source.opcodes;
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
