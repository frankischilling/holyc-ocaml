module Source = Global_record_flag_source
module Json_util = Yojson.Basic.Util

let expected_reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"

let source_paths =
  [
    "Kernel/KernelA.HH";
    "Compiler/PrsStmt.HC";
    "Compiler/PrsExp.HC";
    "Kernel/KHashB.HC";
    "Compiler/CHash.HC";
    "Compiler/AsmResolve.HC";
    "Doc/ScopingLinkage.DD";
  ]

type mode = Write | Check

type config = {
  reference_root : string;
  manifest : string;
  output_ml : string;
  output_mli : string;
  mode : mode;
}

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline ("global-record-flag generator: " ^ message);
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
  let rec collect reference_root manifest output_ml output_mli mode = function
    | [] -> (reference_root, manifest, output_ml, output_mli, mode)
    | "--reference-root" :: path :: rest ->
        collect (Some path) manifest output_ml output_mli mode rest
    | "--manifest" :: path :: rest ->
        collect reference_root (Some path) output_ml output_mli mode rest
    | "--output-ml" :: path :: rest ->
        collect reference_root manifest (Some path) output_mli mode rest
    | "--output-mli" :: path :: rest ->
        collect reference_root manifest output_ml (Some path) mode rest
    | "--check" :: rest ->
        collect reference_root manifest output_ml output_mli Check rest
    | option :: _ -> fail "unknown or incomplete option %S" option
  in
  let reference_root, manifest, output_ml, output_mli, mode =
    collect None None None None Write (List.tl (Array.to_list Sys.argv))
  in
  let required name = function
    | Some value -> value
    | None -> fail "%s is required" name
  in
  {
    reference_root = required "--reference-root" reference_root;
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
      (source_path, checksum)
    in
    (commit, List.map checksum source_paths)
  with
  | Sys_error message -> fail "cannot read %s: %s" path message
  | Yojson.Json_error message -> fail "cannot parse %s: %s" path message
  | Json_util.Type_error (message, _) ->
      fail "manifest %s has the wrong shape: %s" path message

let source_path config path = Filename.concat config.reference_root path

let verify_source path checksum source =
  match Source.verify_sha256 ~expected:checksum source with
  | Ok () -> ()
  | Error problem ->
      fail "%s" (Source.error_to_string { problem with path = Some path })

let source contents path =
  match List.assoc_opt path contents with
  | Some value -> value
  | None -> fail "internal error: %s was not loaded" path

let hash_constructor = function
  | "HTF_PRIVATE" -> "Private"
  | "HTF_PUBLIC" -> "Public"
  | "HTF_EXPORT" -> "Export"
  | "HTF_IMPORT" -> "Import"
  | "HTF_IMM" -> "Immediate"
  | "HTF_GOTO_LABEL" -> "Goto_label"
  | "HTF_RESOLVE" -> "Resolve"
  | "HTF_UNRESOLVED" -> "Unresolved"
  | "HTF_LOCAL" -> "Local"
  | name -> fail "no hash-flag constructor is defined for %s" name

let global_constructor = function
  | "GVF_FUN" -> "Function_pointer"
  | "GVF_IMPORT" -> "Import"
  | "GVF_EXTERN" -> "Extern"
  | "GVF_DATA_HEAP" -> "Data_heap"
  | "GVF_ALIAS" -> "Alias"
  | "GVF_ARRAY" -> "Array"
  | name -> fail "no global-flag constructor is defined for %s" name

let add_reference buffer (reference : Source.source_reference) =
  Printf.bprintf buffer "{ path = %S; line = %d }" reference.path reference.line

let add_reference_list buffer references =
  Buffer.add_string buffer "[ ";
  List.iteri
    (fun index reference ->
      if index > 0 then Buffer.add_string buffer "; ";
      add_reference buffer reference)
    references;
  Buffer.add_string buffer " ]"

let add_variant buffer constructor =
  Printf.bprintf buffer "    | %s\n" constructor

let add_flag_module buffer ~name ~constructor (entries : Source.flag_entry list)
    =
  Printf.bprintf buffer "module %s = struct\n  type t =\n" name;
  List.iter
    (fun (entry : Source.flag_entry) ->
      add_variant buffer (constructor entry.mask_name))
    entries;
  Buffer.add_string buffer "\n  let all =\n    [\n";
  List.iter
    (fun (entry : Source.flag_entry) ->
      Printf.bprintf buffer "      %s;\n" (constructor entry.mask_name))
    entries;
  Buffer.add_string buffer "    ]\n\n  let to_source_name = function\n";
  List.iter
    (fun (entry : Source.flag_entry) ->
      Printf.bprintf buffer "    | %s -> %S\n"
        (constructor entry.mask_name)
        entry.mask_name)
    entries;
  Buffer.add_string buffer "\n  let of_source_name = function\n";
  List.iter
    (fun (entry : Source.flag_entry) ->
      Printf.bprintf buffer "    | %S -> Some %s\n" entry.mask_name
        (constructor entry.mask_name))
    entries;
  Buffer.add_string buffer "    | _ -> None\n\n  let info = function\n";
  List.iter
    (fun (entry : Source.flag_entry) ->
      Printf.bprintf buffer
        "    | %s -> { bit_name = %s; mask_name = %S; bit_index = %d; mask = \
         0x%LxL; definition_line = %d; consumers = "
        (constructor entry.mask_name)
        (match entry.bit_name with
        | None -> "None"
        | Some name -> Printf.sprintf "Some %S" name)
        entry.mask_name entry.bit_index entry.mask entry.definition_line;
      add_reference_list buffer entry.consumers;
      Buffer.add_string buffer " }\n")
    entries;
  Buffer.add_string buffer
    "\n\
    \  let to_bit_index flag = (info flag).bit_index\n\
    \  let to_mask flag = (info flag).mask\n\
    \  let is_set ~mask flag = Int64.logand mask (to_mask flag) <> 0L\n\
    \  let set ~mask flag = Int64.logor mask (to_mask flag)\n\
    \  let clear ~mask flag = Int64.logand mask (Int64.lognot (to_mask flag))\n\
     end\n\n"

let add_flag_signature buffer ~name constructors =
  Printf.bprintf buffer "module %s : sig\n  type t =\n" name;
  List.iter (add_variant buffer) constructors;
  Buffer.add_string buffer
    "\n\
    \  val all : t list\n\
    \  val to_source_name : t -> string\n\
    \  val of_source_name : string -> t option\n\
    \  val to_bit_index : t -> int\n\
    \  val to_mask : t -> int64\n\
    \  val info : t -> flag_info\n\
    \  val is_set : mask:int64 -> t -> bool\n\
    \  val set : mask:int64 -> t -> int64\n\
    \  val clear : mask:int64 -> t -> int64\n\
     end\n\n"

let generated_ml ~commit ~checksums (tables : Source.tables) =
  let buffer = Buffer.create 16384 in
  Buffer.add_string buffer
    "(* Generated from the pinned TempleOS global-record flag specification. \
     *)\n\n\
     [@@@ocamlformat \"disable\"]\n\n\
     type source = { path : string; sha256 : string }\n\
     type source_reference = { path : string; line : int }\n\
     type global_type = { index_name : string; mask_name : string; type_index \
     : int; type_mask : int64; index_definition_line : int; \
     mask_definition_line : int }\n\
     type flag_info = { bit_name : string option; mask_name : string; \
     bit_index : int; mask : int64; definition_line : int; consumers : \
     source_reference list }\n\
     type behavior_info = { id : string; description : string; source : \
     source_reference }\n\n";
  Printf.bprintf buffer "let reference_commit = %S\n\nlet sources =\n  [\n"
    commit;
  List.iter
    (fun (path, sha256) ->
      Printf.bprintf buffer "    { path = %S; sha256 = %S };\n" path sha256)
    checksums;
  Buffer.add_string buffer "  ]\n\n";
  let global_type = tables.global_type in
  Printf.bprintf buffer
    "let global_type = { index_name = %S; mask_name = %S; type_index = %d; \
     type_mask = 0x%LxL; index_definition_line = %d; mask_definition_line = %d \
     }\n\n"
    global_type.index_name global_type.mask_name global_type.type_index
    global_type.type_mask global_type.index_definition_line
    global_type.mask_definition_line;
  add_flag_module buffer ~name:"Hash_flag" ~constructor:hash_constructor
    tables.hash_flags;
  add_flag_module buffer ~name:"Global_flag" ~constructor:global_constructor
    tables.global_flags;
  Buffer.add_string buffer "let behaviors =\n  [\n";
  List.iter
    (fun (behavior : Source.behavior_entry) ->
      Printf.bprintf buffer "    { id = %S; description = %S; source = "
        behavior.id behavior.description;
      add_reference buffer behavior.source;
      Buffer.add_string buffer " };\n")
    tables.behaviors;
  Buffer.add_string buffer
    "  ]\n\n\
     let behavior id = List.find_opt (fun item -> String.equal item.id id) \
     behaviors\n";
  Buffer.contents buffer

let generated_mli (tables : Source.tables) =
  let buffer = Buffer.create 8192 in
  Buffer.add_string buffer
    "(* Generated interface for the pinned TempleOS global-record flag \
     specification. *)\n\n\
     [@@@ocamlformat \"disable\"]\n\n\
     type source = { path : string; sha256 : string }\n\
     type source_reference = { path : string; line : int }\n\
     type global_type = private { index_name : string; mask_name : string; \
     type_index : int; type_mask : int64; index_definition_line : int; \
     mask_definition_line : int }\n\
     type flag_info = private { bit_name : string option; mask_name : string; \
     bit_index : int; mask : int64; definition_line : int; consumers : \
     source_reference list }\n\
     type behavior_info = private { id : string; description : string; source \
     : source_reference }\n\n\
     val reference_commit : string\n\
     val sources : source list\n\
     val global_type : global_type\n\n";
  add_flag_signature buffer ~name:"Hash_flag"
    (List.map
       (fun (entry : Source.flag_entry) -> hash_constructor entry.mask_name)
       tables.hash_flags);
  add_flag_signature buffer ~name:"Global_flag"
    (List.map
       (fun (entry : Source.flag_entry) -> global_constructor entry.mask_name)
       tables.global_flags);
  Buffer.add_string buffer
    "val behaviors : behavior_info list\n\
     val behavior : string -> behavior_info option\n";
  Buffer.contents buffer

let check_output path expected =
  let actual = read_file path in
  if not (String.equal actual expected) then
    fail
      "%s is stale; run dune exec tools/global_record_flag_gen.exe -- \
       --reference-root third_party/TempleOS --manifest \
       reference/manifest.json --output-ml \
       src/generated/global_record_flags.ml --output-mli \
       src/generated/global_record_flags.mli"
      path

let () =
  let config = parse_arguments () in
  let commit, checksums = manifest_metadata config.manifest in
  let contents =
    List.map
      (fun (path, checksum) ->
        let contents = read_file (source_path config path) in
        verify_source path checksum contents;
        (path, contents))
      checksums
  in
  let tables =
    match
      Source.parse
        ~kernel_source:(source contents "Kernel/KernelA.HH")
        ~prs_stmt_source:(source contents "Compiler/PrsStmt.HC")
        ~prs_exp_source:(source contents "Compiler/PrsExp.HC")
        ~khash_source:(source contents "Kernel/KHashB.HC")
        ~chash_source:(source contents "Compiler/CHash.HC")
        ~asm_resolve_source:(source contents "Compiler/AsmResolve.HC")
        ~scoping_source:(source contents "Doc/ScopingLinkage.DD")
    with
    | Ok tables -> tables
    | Error problem -> fail "%s" (Source.error_to_string problem)
  in
  let ml = generated_ml ~commit ~checksums tables in
  let mli = generated_mli tables in
  match config.mode with
  | Write ->
      write_file config.output_ml ml;
      write_file config.output_mli mli
  | Check ->
      check_output config.output_ml ml;
      check_output config.output_mli mli
