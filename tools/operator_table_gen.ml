module Source = Operator_table_source
module Json_util = Yojson.Basic.Util

let expected_reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"
let kernel_source_path = "Kernel/KernelA.HH"
let compiler_source_path = "Compiler/CompilerA.HH"
let cinit_source_path = "Compiler/CInit.HC"
let lex_source_path = "Compiler/Lex.HC"

type mode = Write | Check

type config = {
  kernel : string;
  compiler : string;
  cinit : string;
  lex : string;
  manifest : string;
  output : string;
  mode : mode;
}

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline ("operator table generator: " ^ message);
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
  let rec collect kernel compiler cinit lex manifest output mode = function
    | [] -> (kernel, compiler, cinit, lex, manifest, output, mode)
    | "--kernel" :: path :: rest ->
        collect (Some path) compiler cinit lex manifest output mode rest
    | "--compiler" :: path :: rest ->
        collect kernel (Some path) cinit lex manifest output mode rest
    | "--cinit" :: path :: rest ->
        collect kernel compiler (Some path) lex manifest output mode rest
    | "--lex" :: path :: rest ->
        collect kernel compiler cinit (Some path) manifest output mode rest
    | "--manifest" :: path :: rest ->
        collect kernel compiler cinit lex (Some path) output mode rest
    | "--output" :: path :: rest ->
        collect kernel compiler cinit lex manifest (Some path) mode rest
    | "--check" :: rest ->
        collect kernel compiler cinit lex manifest output Check rest
    | option :: _ -> fail "unknown or incomplete option %S" option
  in
  let arguments = List.tl (Array.to_list Sys.argv) in
  let kernel, compiler, cinit, lex, manifest, output, mode =
    collect None None None None None None Write arguments
  in
  let required name = function
    | Some value -> value
    | None -> fail "%s is required" name
  in
  {
    kernel = required "--kernel" kernel;
    compiler = required "--compiler" compiler;
    cinit = required "--cinit" cinit;
    lex = required "--lex" lex;
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
    ( commit,
      checksum kernel_source_path,
      checksum compiler_source_path,
      checksum cinit_source_path,
      checksum lex_source_path )
  with
  | Sys_error message -> fail "cannot read %s: %s" path message
  | Yojson.Json_error message -> fail "cannot parse %s: %s" path message
  | Json_util.Type_error (message, _) ->
      fail "manifest %s has the wrong shape: %s" path message

let add_constant buffer (entry : Source.named_constant) =
  Printf.bprintf buffer "    { name = %S; value = 0x%X; source_line = %d };\n"
    entry.name entry.value entry.source_line

let add_constants buffer name entries =
  Printf.bprintf buffer "let %s =\n  [\n" name;
  List.iter (add_constant buffer) entries;
  Buffer.add_string buffer "  ]\n"

let add_dual_sequence buffer (entry : Source.dual_sequence) =
  let kind, token_name, token_id =
    match entry.kind with
    | Source.Token token ->
        ( "Token",
          Printf.sprintf "Some %S" token.name,
          Printf.sprintf "Some 0x%X" token.value )
    | Source.Block_comment -> ("Block_comment", "None", "None")
    | Source.Line_comment -> ("Line_comment", "None", "None")
  in
  Printf.bprintf buffer
    "    { group = %d; spelling = %S; kind = %s; token_name = %s; token_id = \
     %s; source_line = %d };\n"
    entry.group entry.spelling kind token_name token_id entry.source_line

let origin = function
  | Source.Dual_table group -> Printf.sprintf "Dual_table %d" group
  | Source.Shift_assignment -> "Shift_assignment"
  | Source.Dot_sequence -> "Dot_sequence"
  | Source.Current_position -> "Current_position"

let option_string = function
  | None -> "None"
  | Some value -> Printf.sprintf "Some %S" value

let option_int = function
  | None -> "None"
  | Some value -> Printf.sprintf "Some 0x%X" value

let add_operator buffer (entry : Source.operator) =
  Printf.bprintf buffer
    "    { spelling = %S; token_name = %s; token_id = %s; origin = %s; \
     source_line = %d };\n"
    entry.spelling
    (option_string entry.token_name)
    (option_int entry.token_id)
    (origin entry.origin) entry.source_line

let association = function
  | Source.Unspecified -> "Unspecified"
  | Source.Left -> "Left"
  | Source.Right -> "Right"

let add_binary buffer (entry : Source.binary_operator) =
  Printf.bprintf buffer
    "    { spelling = %S; token_name = %s; token_id = 0x%X; precedence_name = \
     %S; precedence_value = 0x%X; association = %s; ic_name = %S; ic_id = \
     0x%X; source_line = %d };\n"
    entry.spelling
    (option_string entry.token_name)
    entry.token_id entry.precedence_name entry.precedence_value
    (association entry.association)
    entry.ic_name entry.ic_id entry.source_line

let render ~commit ~kernel_checksum ~compiler_checksum ~cinit_checksum
    ~lex_checksum tables =
  let buffer = Buffer.create 16384 in
  Buffer.add_string buffer
    "(* This table is generated from the pinned TempleOS operator definitions.\n";
  Buffer.add_string buffer
    "   Run the operator table generator after an approved reference update. \
     *)\n\n";
  Buffer.add_string buffer "[@@@ocamlformat \"disable\"]\n\n";
  Printf.bprintf buffer "let reference_commit = %S\n" commit;
  Printf.bprintf buffer "let kernel_source_path = %S\n" kernel_source_path;
  Printf.bprintf buffer "let kernel_source_sha256 = %S\n" kernel_checksum;
  Printf.bprintf buffer "let compiler_source_path = %S\n" compiler_source_path;
  Printf.bprintf buffer "let compiler_source_sha256 = %S\n" compiler_checksum;
  Printf.bprintf buffer "let cinit_source_path = %S\n" cinit_source_path;
  Printf.bprintf buffer "let cinit_source_sha256 = %S\n" cinit_checksum;
  Printf.bprintf buffer "let lex_source_path = %S\n" lex_source_path;
  Printf.bprintf buffer "let lex_source_sha256 = %S\n\n" lex_checksum;
  Buffer.add_string buffer
    "type named_constant = { name : string; value : int; source_line : int }\n\n";
  Buffer.add_string buffer
    "type sequence_kind = Token | Block_comment | Line_comment\n\n";
  Buffer.add_string buffer
    "type dual_sequence = {\n\
    \  group : int;\n\
    \  spelling : string;\n\
    \  kind : sequence_kind;\n\
    \  token_name : string option;\n\
    \  token_id : int option;\n\
    \  source_line : int;\n\
     }\n\n";
  Buffer.add_string buffer
    "type operator_origin = Dual_table of int | Shift_assignment | \
     Dot_sequence | Current_position\n\n";
  Buffer.add_string buffer
    "type operator = {\n\
    \  spelling : string;\n\
    \  token_name : string option;\n\
    \  token_id : int option;\n\
    \  origin : operator_origin;\n\
    \  source_line : int;\n\
     }\n\n";
  Buffer.add_string buffer "type association = Unspecified | Left | Right\n\n";
  Buffer.add_string buffer
    "type binary_operator = {\n\
    \  spelling : string;\n\
    \  token_name : string option;\n\
    \  token_id : int;\n\
    \  precedence_name : string;\n\
    \  precedence_value : int;\n\
    \  association : association;\n\
    \  ic_name : string;\n\
    \  ic_id : int;\n\
    \  source_line : int;\n\
     }\n\n";
  add_constants buffer "tokens" tables.Source.tokens;
  Buffer.add_char buffer '\n';
  add_constants buffer "association_flags" tables.association_flags;
  Buffer.add_char buffer '\n';
  add_constants buffer "precedences" tables.precedences;
  Buffer.add_string buffer "\nlet dual_sequences =\n  [\n";
  List.iter (add_dual_sequence buffer) tables.dual_sequences;
  Buffer.add_string buffer "  ]\n\nlet operators =\n  [\n";
  List.iter (add_operator buffer) tables.operators;
  Buffer.add_string buffer "  ]\n\nlet binary_operators =\n  [\n";
  List.iter (add_binary buffer) tables.binary_operators;
  Buffer.add_string buffer "  ]\n";
  Buffer.contents buffer

let check_output path expected =
  let actual = read_file path in
  if not (String.equal actual expected) then
    fail
      "%s is stale; regenerate it with `dune exec tools/operator_table_gen.exe \
       -- --kernel third_party/TempleOS/Kernel/KernelA.HH --compiler \
       third_party/TempleOS/Compiler/CompilerA.HH --cinit \
       third_party/TempleOS/Compiler/CInit.HC --lex \
       third_party/TempleOS/Compiler/Lex.HC --manifest reference/manifest.json \
       --output src/generated/operator_tables.ml`"
      path

let verify_source path checksum source =
  match Source.verify_sha256 ~expected:checksum source with
  | Ok () -> ()
  | Error problem -> fail "%s: %s" path (Source.error_to_string problem)

let () =
  let config = parse_arguments () in
  let commit, kernel_checksum, compiler_checksum, cinit_checksum, lex_checksum =
    manifest_metadata config.manifest
  in
  let kernel_source = read_file config.kernel in
  let compiler_source = read_file config.compiler in
  let cinit_source = read_file config.cinit in
  let lex_source = read_file config.lex in
  verify_source kernel_source_path kernel_checksum kernel_source;
  verify_source compiler_source_path compiler_checksum compiler_source;
  verify_source cinit_source_path cinit_checksum cinit_source;
  verify_source lex_source_path lex_checksum lex_source;
  let tables =
    match
      Source.parse ~kernel_source ~compiler_source ~cinit_source ~lex_source
    with
    | Ok tables -> tables
    | Error problem -> fail "%s" (Source.error_to_string problem)
  in
  let generated =
    render ~commit ~kernel_checksum ~compiler_checksum ~cinit_checksum
      ~lex_checksum tables
  in
  match config.mode with
  | Write -> write_file config.output generated
  | Check -> check_output config.output generated
