module Source = Intermediate_code_source
module Json_util = Yojson.Basic.Util

let expected_reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"
let compiler_source_path = "Compiler/CompilerA.HH"
let cinit_source_path = "Compiler/CInit.HC"

type mode = Write | Check

type config = {
  compiler : string;
  cinit : string;
  manifest : string;
  output_ml : string;
  output_mli : string;
  mode : mode;
}

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline ("intermediate-code generator: " ^ message);
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
  let rec collect compiler cinit manifest output_ml output_mli mode = function
    | [] -> (compiler, cinit, manifest, output_ml, output_mli, mode)
    | "--compiler" :: path :: rest ->
        collect (Some path) cinit manifest output_ml output_mli mode rest
    | "--cinit" :: path :: rest ->
        collect compiler (Some path) manifest output_ml output_mli mode rest
    | "--manifest" :: path :: rest ->
        collect compiler cinit (Some path) output_ml output_mli mode rest
    | "--output-ml" :: path :: rest ->
        collect compiler cinit manifest (Some path) output_mli mode rest
    | "--output-mli" :: path :: rest ->
        collect compiler cinit manifest output_ml (Some path) mode rest
    | "--check" :: rest ->
        collect compiler cinit manifest output_ml output_mli Check rest
    | option :: _ -> fail "unknown or incomplete option %S" option
  in
  let compiler, cinit, manifest, output_ml, output_mli, mode =
    collect None None None None None Write (List.tl (Array.to_list Sys.argv))
  in
  let required name = function
    | Some value -> value
    | None -> fail "%s is required" name
  in
  {
    compiler = required "--compiler" compiler;
    cinit = required "--cinit" cinit;
    manifest = required "--manifest" manifest;
    output_ml = required "--output-ml" output_ml;
    output_mli = required "--output-mli" output_mli;
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
    (commit, checksum compiler_source_path, checksum cinit_source_path)
  with
  | Sys_error message -> fail "cannot read %s: %s" path message
  | Yojson.Json_error message -> fail "cannot parse %s: %s" path message
  | Json_util.Type_error (message, _) ->
      fail "manifest %s has the wrong shape: %s" path message

let verify_source path checksum source =
  match Source.verify_sha256 ~expected:checksum source with
  | Ok () -> ()
  | Error problem -> fail "%s: %s" path (Source.error_to_string problem)

let argument_name = function
  | Source.Zero -> "Zero"
  | Source.One -> "One"
  | Source.Two -> "Two"
  | Source.Variable -> "Variable"

let structural_name = function
  | Source.Null -> "Null"
  | Source.Dereference -> "Dereference"
  | Source.Assignment -> "Assignment"
  | Source.Comparison -> "Comparison"

let add_variant buffer (entry : Source.entry) =
  Printf.bprintf buffer "  | %s\n" entry.constructor_name

let add_constructor buffer (entry : Source.entry) =
  Printf.bprintf buffer "    %s;\n" entry.constructor_name

let add_match buffer project (entry : Source.entry) =
  Printf.bprintf buffer "  | %s -> %s\n" entry.constructor_name (project entry)

let add_reverse_match buffer project (entry : Source.entry) =
  Printf.bprintf buffer "  | %s -> Some %s\n" (project entry)
    entry.constructor_name

let add_info buffer (entry : Source.entry) =
  Printf.bprintf buffer
    "    { opcode = %s; source_name = %S; display_name = %S; code = 0x%02X; \
     argument_count = %s; result_count = %d; structural_type = %s; pops_float \
     = %b; prevents_constant_folding = %b; definition_line = %d; metadata_line \
     = %d };\n"
    entry.constructor_name entry.source_name entry.display_name entry.code
    (argument_name entry.argument_count)
    entry.result_count
    (structural_name entry.structural_type)
    entry.pops_float entry.prevents_constant_folding entry.definition_line
    entry.metadata_line

let add_common_types buffer entries =
  Buffer.add_string buffer "type t =\n";
  List.iter (add_variant buffer) entries;
  Buffer.add_string buffer
    "\ntype argument_count = Zero | One | Two | Variable\n\n";
  Buffer.add_string buffer
    "type structural_type =\n\
    \  | Null\n\
    \  | Dereference\n\
    \  | Assignment\n\
    \  | Comparison\n\n";
  Buffer.add_string buffer
    "type info = {\n\
    \  opcode : t;\n\
    \  source_name : string;\n\
    \  display_name : string;\n\
    \  code : int;\n\
    \  argument_count : argument_count;\n\
    \  result_count : int;\n\
    \  structural_type : structural_type;\n\
    \  pops_float : bool;\n\
    \  prevents_constant_folding : bool;\n\
    \  definition_line : int;\n\
    \  metadata_line : int;\n\
     }\n\n"

let render_ml ~commit ~compiler_checksum ~cinit_checksum tables =
  let entries = tables.Source.entries in
  let buffer = Buffer.create 65536 in
  Buffer.add_string buffer
    "(* Generated from the pinned TempleOS intermediate-code definitions and \
     metadata.\n";
  Buffer.add_string buffer
    "   Regenerate this file only as part of a reviewed reference or table \
     update. *)\n\n";
  Buffer.add_string buffer "[@@@ocamlformat \"disable\"]\n\n";
  Printf.bprintf buffer "let reference_commit = %S\n\n" commit;
  Buffer.add_string buffer
    "type source = { path : string; sha256 : string }\n\n";
  Printf.bprintf buffer
    "let sources =\n\
    \  [\n\
    \    { path = %S; sha256 = %S };\n\
    \    { path = %S; sha256 = %S };\n\
    \  ]\n\n"
    compiler_source_path compiler_checksum cinit_source_path cinit_checksum;
  add_common_types buffer entries;
  Printf.bprintf buffer "let count = 0x%X\n\n" tables.count;
  Buffer.add_string buffer "let all =\n  [\n";
  List.iter (add_constructor buffer) entries;
  Buffer.add_string buffer "  ]\n\n";
  Buffer.add_string buffer "let to_code = function\n";
  List.iter
    (add_match buffer (fun entry -> Printf.sprintf "0x%02X" entry.code))
    entries;
  Buffer.add_string buffer "\nlet of_code = function\n";
  List.iter
    (add_reverse_match buffer (fun entry -> Printf.sprintf "0x%02X" entry.code))
    entries;
  Buffer.add_string buffer "  | _ -> None\n\n";
  Buffer.add_string buffer "let of_source_name = function\n";
  List.iter
    (add_reverse_match buffer (fun entry ->
         Printf.sprintf "%S" entry.source_name))
    entries;
  Buffer.add_string buffer "  | _ -> None\n\n";
  Buffer.add_string buffer "let of_display_name = function\n";
  List.iter
    (add_reverse_match buffer (fun entry ->
         Printf.sprintf "%S" entry.display_name))
    entries;
  Buffer.add_string buffer "  | _ -> None\n\n";
  Buffer.add_string buffer "let information_array =\n  [|\n";
  List.iter (add_info buffer) entries;
  Buffer.add_string buffer "  |]\n\n";
  Buffer.add_string buffer
    "let information = Array.to_list information_array\n\n\
     let info opcode = Array.get information_array (to_code opcode)\n\n\
     let to_source_name opcode = (info opcode).source_name\n\
     let to_display_name opcode = (info opcode).display_name\n\n\
     let compare left right = Int.compare (to_code left) (to_code right)\n\
     let equal left right = compare left right = 0\n";
  Buffer.contents buffer

let render_mli entries =
  let buffer = Buffer.create 16384 in
  Buffer.add_string buffer
    "(* Generated interface for the pinned TempleOS intermediate-code \
     specification. *)\n\n";
  Buffer.add_string buffer "[@@@ocamlformat \"disable\"]\n\n";
  Buffer.add_string buffer "type t =\n";
  List.iter (add_variant buffer) entries;
  Buffer.add_string buffer
    "\ntype argument_count = Zero | One | Two | Variable\n\n";
  Buffer.add_string buffer
    "type structural_type =\n\
    \  | Null\n\
    \  | Dereference\n\
    \  | Assignment\n\
    \  | Comparison\n\n";
  Buffer.add_string buffer
    "type info = private {\n\
    \  opcode : t;\n\
    \  source_name : string;\n\
    \  display_name : string;\n\
    \  code : int;\n\
    \  argument_count : argument_count;\n\
    \  result_count : int;\n\
    \  structural_type : structural_type;\n\
    \  pops_float : bool;\n\
    \  prevents_constant_folding : bool;\n\
    \  definition_line : int;\n\
    \  metadata_line : int;\n\
     }\n\n";
  Buffer.add_string buffer
    "type source = { path : string; sha256 : string }\n\n\
     val reference_commit : string\n\
     val sources : source list\n\
     val count : int\n\
     val all : t list\n\
     val compare : t -> t -> int\n\
     val equal : t -> t -> bool\n\
     val to_code : t -> int\n\
     val of_code : int -> t option\n\
     val to_source_name : t -> string\n\
     val of_source_name : string -> t option\n\
     val to_display_name : t -> string\n\
     val of_display_name : string -> t option\n\
     val info : t -> info\n\
     val information : info list\n";
  Buffer.contents buffer

let check_output path expected =
  let actual = read_file path in
  if not (String.equal actual expected) then
    fail
      "%s is stale; regenerate both files with `dune exec \
       tools/intermediate_code_gen.exe -- --compiler \
       third_party/TempleOS/Compiler/CompilerA.HH --cinit \
       third_party/TempleOS/Compiler/CInit.HC --manifest \
       reference/manifest.json --output-ml src/generated/intermediate_codes.ml \
       --output-mli src/generated/intermediate_codes.mli`"
      path

let () =
  let config = parse_arguments () in
  let commit, compiler_checksum, cinit_checksum =
    manifest_metadata config.manifest
  in
  let compiler_source = read_file config.compiler in
  let cinit_source = read_file config.cinit in
  verify_source compiler_source_path compiler_checksum compiler_source;
  verify_source cinit_source_path cinit_checksum cinit_source;
  let tables =
    match Source.parse ~compiler_source ~cinit_source with
    | Ok tables -> tables
    | Error problem -> fail "%s" (Source.error_to_string problem)
  in
  let generated_ml =
    render_ml ~commit ~compiler_checksum ~cinit_checksum tables
  in
  let generated_mli = render_mli tables.entries in
  match config.mode with
  | Write ->
      write_file config.output_ml generated_ml;
      write_file config.output_mli generated_mli
  | Check ->
      check_output config.output_ml generated_ml;
      check_output config.output_mli generated_mli
