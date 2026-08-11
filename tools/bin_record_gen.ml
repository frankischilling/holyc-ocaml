module Source = Bin_record_source
module Json_util = Yojson.Basic.Util

let expected_reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"

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
      prerr_endline ("BIN-record generator: " ^ message);
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
    (commit, List.map checksum Source.required_source_paths)
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

let entry_constructor = function
  | "IET_END" -> "End"
  | "IET_REL_I0" -> "Rel_i0"
  | "IET_IMM_U0" -> "Imm_u0"
  | "IET_REL_I8" -> "Rel_i8"
  | "IET_IMM_U8" -> "Imm_u8"
  | "IET_REL_I16" -> "Rel_i16"
  | "IET_IMM_U16" -> "Imm_u16"
  | "IET_REL_I32" -> "Rel_i32"
  | "IET_IMM_U32" -> "Imm_u32"
  | "IET_REL_I64" -> "Rel_i64"
  | "IET_IMM_I64" -> "Imm_i64"
  | "IET_REL32_EXPORT" -> "Rel32_export"
  | "IET_IMM32_EXPORT" -> "Imm32_export"
  | "IET_REL64_EXPORT" -> "Rel64_export"
  | "IET_IMM64_EXPORT" -> "Imm64_export"
  | "IET_ABS_ADDR" -> "Abs_addr"
  | "IET_CODE_HEAP" -> "Code_heap"
  | "IET_ZEROED_CODE_HEAP" -> "Zeroed_code_heap"
  | "IET_DATA_HEAP" -> "Data_heap"
  | "IET_ZEROED_DATA_HEAP" -> "Zeroed_data_heap"
  | "IET_MAIN" -> "Main"
  | name -> fail "no entry constructor is defined for %s" name

let adjustment_constructor = function
  | "AAT_ADD_U8" -> "Add_u8"
  | "AAT_SUB_U8" -> "Sub_u8"
  | "AAT_ADD_U16" -> "Add_u16"
  | "AAT_SUB_U16" -> "Sub_u16"
  | "AAT_ADD_U32" -> "Add_u32"
  | "AAT_SUB_U32" -> "Sub_u32"
  | "AAT_ADD_U64" -> "Add_u64"
  | "AAT_SUB_U64" -> "Sub_u64"
  | name -> fail "no adjustment constructor is defined for %s" name

let width = function
  | Source.Width_0 -> "Width_0"
  | Source.Width_8 -> "Width_8"
  | Source.Width_16 -> "Width_16"
  | Source.Width_32 -> "Width_32"
  | Source.Width_64 -> "Width_64"

let source_status = function
  | Source.Source_active -> "Source_active"
  | Source.Source_fictitious -> "Source_fictitious"
  | Source.Source_not_implemented -> "Source_not_implemented"
  | Source.Source_not_really_used -> "Source_not_really_used"

let category = function
  | Source.Terminator -> "Terminator"
  | Source.Import -> "Import"
  | Source.Export -> "Export"
  | Source.Absolute_addresses -> "Absolute_addresses"
  | Source.Code_heap -> "Code_heap"
  | Source.Data_heap -> "Data_heap"
  | Source.Main -> "Main"

let leading_value = function
  | Source.No_leading_value -> "No_leading_value"
  | Source.Patch_offset -> "Patch_offset"
  | Source.Export_value -> "Export_value"
  | Source.Entry_count -> "Entry_count"
  | Source.Main_offset -> "Main_offset"

let name_mode = function
  | Source.No_name_field -> "No_name_field"
  | Source.Required_name -> "Required_name"
  | Source.First_name_then_inherited -> "First_name_then_inherited"
  | Source.Optional_export_name -> "Optional_export_name"
  | Source.Empty_name -> "Empty_name"

let payload = function
  | Source.No_payload -> "No_payload"
  | Source.U32_offsets -> "U32_offsets"
  | Source.I32_size_then_u32_offsets -> "I32_size_then_u32_offsets"
  | Source.I64_size_then_u32_offsets -> "I64_size_then_u32_offsets"

let relocation_kind = function
  | Source.Relative -> "Relative"
  | Source.Immediate -> "Immediate"

let pass1 = function
  | Source.Pass1_stop -> "Pass1_stop"
  | Source.Resolve_import -> "Resolve_import"
  | Source.Register_relative_export -> "Register_relative_export"
  | Source.Register_immediate_export -> "Register_immediate_export"
  | Source.Apply_module_base_u32 -> "Apply_module_base_u32"
  | Source.Allocate_code -> "Allocate_code"
  | Source.Allocate_zeroed_code -> "Allocate_zeroed_code"
  | Source.Allocate_data -> "Allocate_data"
  | Source.Allocate_zeroed_data -> "Allocate_zeroed_data"
  | Source.Pass1_ignore -> "Pass1_ignore"

let pass2 = function
  | Source.Pass2_stop -> "Pass2_stop"
  | Source.Execute_main -> "Execute_main"
  | Source.Skip_u32_offsets -> "Skip_u32_offsets"
  | Source.Skip_i32_size_and_u32_offsets -> "Skip_i32_size_and_u32_offsets"
  | Source.Skip_i64_size_and_u32_offsets -> "Skip_i64_size_and_u32_offsets"
  | Source.Pass2_ignore -> "Pass2_ignore"

let adjustment_operation = function
  | Source.Add -> "Add"
  | Source.Subtract -> "Subtract"

let add_reference buffer (reference : Source.source_reference) =
  Printf.bprintf buffer "{ path = %S; line = %d }" reference.path reference.line

let add_references buffer references =
  Buffer.add_string buffer "[";
  List.iteri
    (fun index reference ->
      if index > 0 then Buffer.add_string buffer "; ";
      add_reference buffer reference)
    references;
  Buffer.add_string buffer "]"

let add_sources buffer sources =
  Buffer.add_string buffer "let sources =\n  [\n";
  List.iter
    (fun (path, checksum) ->
      Printf.bprintf buffer "    { path = %S; sha256 = %S };\n" path checksum)
    sources;
  Buffer.add_string buffer "  ]\n\n"

let add_header_fields buffer fields =
  Buffer.add_string buffer "let header_fields =\n  [\n";
  List.iter
    (fun (field : Source.header_field) ->
      Printf.bprintf buffer
        "    { name = %S; source_type = %S; width_bytes = %d; offset = %d; \
         definition_line = %d };\n"
        field.name field.source_type field.width_bytes field.offset
        field.definition_line)
    fields;
  Buffer.add_string buffer "  ]\n\n"

let add_entry_type buffer entries =
  Buffer.add_string buffer "  type t =\n";
  List.iter
    (fun (entry : Source.entry) ->
      Printf.bprintf buffer "    | %s\n" (entry_constructor entry.name))
    entries;
  Buffer.add_char buffer '\n'

let add_entry_list buffer entries =
  Buffer.add_string buffer "  let all =\n    [\n";
  List.iter
    (fun (entry : Source.entry) ->
      Printf.bprintf buffer "      %s;\n" (entry_constructor entry.name))
    entries;
  Buffer.add_string buffer "    ]\n\n"

let add_entry_info buffer entries =
  Buffer.add_string buffer "  let info = function\n";
  List.iter
    (fun (entry : Source.entry) ->
      Printf.bprintf buffer
        "    | %s ->\n\
        \        { entry = %s; source_name = %S; code = %d; status = %s; \
         category = %s; leading_value = %s; name_mode = %s; payload = %s; \
         relocation = "
        (entry_constructor entry.name)
        (entry_constructor entry.name)
        entry.name entry.code
        (source_status entry.status)
        (category entry.category)
        (leading_value entry.leading_value)
        (name_mode entry.name_mode)
        (payload entry.payload);
      (match entry.relocation with
      | None -> Buffer.add_string buffer "None"
      | Some relocation ->
          Printf.bprintf buffer
            "Some { kind = %s; width = %s; displacement_bias = %d }"
            (relocation_kind relocation.kind)
            (width relocation.width) relocation.displacement_bias);
      Printf.bprintf buffer
        "; pass1 = %s; pass2 = %s; definition_line = %d; consumers = "
        (pass1 entry.pass1) (pass2 entry.pass2) entry.definition_line;
      add_references buffer entry.consumers;
      Buffer.add_string buffer "; }\n")
    entries;
  Buffer.add_char buffer '\n'

let add_entry_lookups buffer entries reserved_codes =
  Buffer.add_string buffer
    "  let to_source_name entry = (info entry).source_name\n";
  Buffer.add_string buffer "  let to_code entry = (info entry).code\n\n";
  Buffer.add_string buffer "  let of_code = function\n";
  List.iter
    (fun (entry : Source.entry) ->
      Printf.bprintf buffer "    | %d -> Some %s\n" entry.code
        (entry_constructor entry.name))
    entries;
  Buffer.add_string buffer "    | _ -> None\n\n";
  Buffer.add_string buffer "  let of_source_name = function\n";
  List.iter
    (fun (entry : Source.entry) ->
      Printf.bprintf buffer "    | %S -> Some %s\n" entry.name
        (entry_constructor entry.name))
    entries;
  Buffer.add_string buffer "    | _ -> None\n\n";
  Buffer.add_string buffer
    "  type decoded = Entry of t | Reserved of int | Unknown of int\n\n";
  Buffer.add_string buffer
    "  let decode code =\n\
    \    match of_code code with\n\
    \    | Some entry -> Entry entry\n\
    \    | None ->\n\
    \        if List.mem code reserved_codes then Reserved code else Unknown \
     code\n";
  ignore reserved_codes

let add_adjustment_type buffer adjustments =
  Buffer.add_string buffer "  type t =\n";
  List.iter
    (fun (adjustment : Source.adjustment) ->
      Printf.bprintf buffer "    | %s\n"
        (adjustment_constructor adjustment.name))
    adjustments;
  Buffer.add_char buffer '\n'

let add_adjustment_info buffer adjustments =
  Buffer.add_string buffer "  let all =\n    [\n";
  List.iter
    (fun (adjustment : Source.adjustment) ->
      Printf.bprintf buffer "      %s;\n"
        (adjustment_constructor adjustment.name))
    adjustments;
  Buffer.add_string buffer "    ]\n\n";
  Buffer.add_string buffer "  let info = function\n";
  List.iter
    (fun (adjustment : Source.adjustment) ->
      Printf.bprintf buffer
        "    | %s ->\n\
        \        { adjustment = %s; source_name = %S; code = %d; width = %s; \
         operation = %s; definition_line = %d; consumers = "
        (adjustment_constructor adjustment.name)
        (adjustment_constructor adjustment.name)
        adjustment.name adjustment.code (width adjustment.width)
        (adjustment_operation adjustment.operation)
        adjustment.definition_line;
      add_references buffer adjustment.consumers;
      Buffer.add_string buffer "; }\n")
    adjustments;
  Buffer.add_string buffer
    "\n\
    \  let to_source_name adjustment = (info adjustment).source_name\n\
    \  let to_code adjustment = (info adjustment).code\n\n\
    \  let of_code = function\n";
  List.iter
    (fun (adjustment : Source.adjustment) ->
      Printf.bprintf buffer "    | %d -> Some %s\n" adjustment.code
        (adjustment_constructor adjustment.name))
    adjustments;
  Buffer.add_string buffer
    "    | _ -> None\n\n  let of_source_name = function\n";
  List.iter
    (fun (adjustment : Source.adjustment) ->
      Printf.bprintf buffer "    | %S -> Some %s\n" adjustment.name
        (adjustment_constructor adjustment.name))
    adjustments;
  Buffer.add_string buffer "    | _ -> None\n"

let add_behavior buffer behavior =
  let field name reference =
    Printf.bprintf buffer "    %s = " name;
    add_reference buffer reference;
    Buffer.add_string buffer ";\n"
  in
  Buffer.add_string buffer "let behavior_sources =\n  {\n";
  field "header_layout" behavior.Source.header_layout;
  field "header_write" behavior.header_write;
  field "module_validation" behavior.module_validation;
  field "module_base" behavior.module_base;
  field "import_grouping" behavior.import_grouping;
  field "import_patches" behavior.import_patches;
  field "export_registration" behavior.export_registration;
  field "absolute_patch" behavior.absolute_patch;
  field "code_heap_patch" behavior.code_heap_patch;
  field "data_heap_patch" behavior.data_heap_patch;
  field "main_execution" behavior.main_execution;
  field "pass_order" behavior.pass_order;
  field "patch_termination" behavior.patch_termination;
  field "boot_patch_table" behavior.boot_patch_table;
  field "boot_absolute_patch" behavior.boot_absolute_patch;
  field "jit_adjustments" behavior.jit_adjustments;
  field "aot_adjustments" behavior.aot_adjustments;
  Buffer.add_string buffer "  }\n"

let render_ml ~commit ~sources tables =
  let buffer = Buffer.create 65536 in
  Buffer.add_string buffer
    "(* Generated from the pinned TempleOS BIN definitions and consumers. *)\n\n\
     [@@@ocamlformat \"disable\"]\n\n";
  Buffer.add_string buffer
    "type source = { path : string; sha256 : string }\n\
     type source_reference = { path : string; line : int }\n\n\
     type width = Width_0 | Width_8 | Width_16 | Width_32 | Width_64\n\n\
     let width_bytes = function\n\
    \  | Width_0 -> 0\n\
    \  | Width_8 -> 1\n\
    \  | Width_16 -> 2\n\
    \  | Width_32 -> 4\n\
    \  | Width_64 -> 8\n\n\
     type source_status = Source_active | Source_fictitious | \
     Source_not_implemented | Source_not_really_used\n\
     type category = Terminator | Import | Export | Absolute_addresses | \
     Code_heap | Data_heap | Main\n\
     type leading_value = No_leading_value | Patch_offset | Export_value | \
     Entry_count | Main_offset\n\
     type name_mode = No_name_field | Required_name | \
     First_name_then_inherited | Optional_export_name | Empty_name\n\
     type payload = No_payload | U32_offsets | I32_size_then_u32_offsets | \
     I64_size_then_u32_offsets\n\
     type relocation_kind = Relative | Immediate\n\
     type relocation = { kind : relocation_kind; width : width; \
     displacement_bias : int }\n\
     type pass1_action = Pass1_stop | Resolve_import | \
     Register_relative_export | Register_immediate_export | \
     Apply_module_base_u32 | Allocate_code | Allocate_zeroed_code | \
     Allocate_data | Allocate_zeroed_data | Pass1_ignore\n\
     type pass2_action = Pass2_stop | Execute_main | Skip_u32_offsets | \
     Skip_i32_size_and_u32_offsets | Skip_i64_size_and_u32_offsets | \
     Pass2_ignore\n\n\
     type header_field = { name : string; source_type : string; width_bytes : \
     int; offset : int; definition_line : int }\n\n\
     type adjustment_operation = Add | Subtract\n\n";
  Printf.bprintf buffer "let reference_commit = %S\n\n" commit;
  add_sources buffer sources;
  Printf.bprintf buffer "let signature_spelling = %S\n"
    tables.Source.signature_spelling;
  Printf.bprintf buffer "let signature_value = 0x%lxl\n" tables.signature_value;
  Printf.bprintf buffer "let signature_definition_line = %d\n"
    tables.signature_line;
  Printf.bprintf buffer "let header_size = %d\n" tables.header_size;
  Printf.bprintf buffer "let immediate_not_relative_mask = %d\n"
    tables.immediate_not_relative_mask;
  Printf.bprintf buffer "let immediate_not_relative_definition_line = %d\n\n"
    tables.immediate_not_relative_line;
  add_header_fields buffer tables.header_fields;
  Printf.bprintf buffer "let reserved_codes = [ %s ]\n\n"
    (String.concat "; " (List.map string_of_int tables.reserved_codes));
  Buffer.add_string buffer "module Entry = struct\n";
  add_entry_type buffer tables.entries;
  Buffer.add_string buffer
    "  type info = { entry : t; source_name : string; code : int; status : \
     source_status; category : category; leading_value : leading_value; \
     name_mode : name_mode; payload : payload; relocation : relocation option; \
     pass1 : pass1_action; pass2 : pass2_action; definition_line : int; \
     consumers : source_reference list }\n\n";
  add_entry_list buffer tables.entries;
  add_entry_info buffer tables.entries;
  add_entry_lookups buffer tables.entries tables.reserved_codes;
  Buffer.add_string buffer "end\n\nmodule Adjustment = struct\n";
  add_adjustment_type buffer tables.adjustments;
  Buffer.add_string buffer
    "  type info = { adjustment : t; source_name : string; code : int; width : \
     width; operation : adjustment_operation; definition_line : int; consumers \
     : source_reference list }\n\n";
  add_adjustment_info buffer tables.adjustments;
  Buffer.add_string buffer
    "end\n\n\
     type behavior_sources = { header_layout : source_reference; header_write \
     : source_reference; module_validation : source_reference; module_base : \
     source_reference; import_grouping : source_reference; import_patches : \
     source_reference; export_registration : source_reference; absolute_patch \
     : source_reference; code_heap_patch : source_reference; data_heap_patch : \
     source_reference; main_execution : source_reference; pass_order : \
     source_reference; patch_termination : source_reference; boot_patch_table \
     : source_reference; boot_absolute_patch : source_reference; \
     jit_adjustments : source_reference; aot_adjustments : source_reference }\n\n";
  add_behavior buffer tables.behavior;
  Buffer.contents buffer

let render_mli entries adjustments =
  let buffer = Buffer.create 32768 in
  Buffer.add_string buffer
    "(* Generated interface for the pinned TempleOS BIN specification. *)\n\n\
     [@@@ocamlformat \"disable\"]\n\n\
     type source = { path : string; sha256 : string }\n\
     type source_reference = { path : string; line : int }\n\
     type width = Width_0 | Width_8 | Width_16 | Width_32 | Width_64\n\
     val width_bytes : width -> int\n\
     type source_status = Source_active | Source_fictitious | \
     Source_not_implemented | Source_not_really_used\n\
     type category = Terminator | Import | Export | Absolute_addresses | \
     Code_heap | Data_heap | Main\n\
     type leading_value = No_leading_value | Patch_offset | Export_value | \
     Entry_count | Main_offset\n\
     type name_mode = No_name_field | Required_name | \
     First_name_then_inherited | Optional_export_name | Empty_name\n\
     type payload = No_payload | U32_offsets | I32_size_then_u32_offsets | \
     I64_size_then_u32_offsets\n\
     type relocation_kind = Relative | Immediate\n\
     type relocation = { kind : relocation_kind; width : width; \
     displacement_bias : int }\n\
     type pass1_action = Pass1_stop | Resolve_import | \
     Register_relative_export | Register_immediate_export | \
     Apply_module_base_u32 | Allocate_code | Allocate_zeroed_code | \
     Allocate_data | Allocate_zeroed_data | Pass1_ignore\n\
     type pass2_action = Pass2_stop | Execute_main | Skip_u32_offsets | \
     Skip_i32_size_and_u32_offsets | Skip_i64_size_and_u32_offsets | \
     Pass2_ignore\n\
     type header_field = private { name : string; source_type : string; \
     width_bytes : int; offset : int; definition_line : int }\n\
     type adjustment_operation = Add | Subtract\n\
     val reference_commit : string\n\
     val sources : source list\n\
     val signature_spelling : string\n\
     val signature_value : int32\n\
     val signature_definition_line : int\n\
     val header_size : int\n\
     val header_fields : header_field list\n\
     val immediate_not_relative_mask : int\n\
     val immediate_not_relative_definition_line : int\n\
     val reserved_codes : int list\n\n\
     module Entry : sig\n\
    \  type t =\n";
  List.iter
    (fun (entry : Source.entry) ->
      Printf.bprintf buffer "    | %s\n" (entry_constructor entry.name))
    entries;
  Buffer.add_string buffer
    "\n\
    \  type info = private { entry : t; source_name : string; code : int; \
     status : source_status; category : category; leading_value : \
     leading_value; name_mode : name_mode; payload : payload; relocation : \
     relocation option; pass1 : pass1_action; pass2 : pass2_action; \
     definition_line : int; consumers : source_reference list }\n\
    \  type decoded = Entry of t | Reserved of int | Unknown of int\n\
    \  val all : t list\n\
    \  val info : t -> info\n\
    \  val to_source_name : t -> string\n\
    \  val to_code : t -> int\n\
    \  val of_code : int -> t option\n\
    \  val of_source_name : string -> t option\n\
    \  val decode : int -> decoded\n\
     end\n\n\
     module Adjustment : sig\n\
    \  type t =\n";
  List.iter
    (fun (adjustment : Source.adjustment) ->
      Printf.bprintf buffer "    | %s\n"
        (adjustment_constructor adjustment.name))
    adjustments;
  Buffer.add_string buffer
    "\n\
    \  type info = private { adjustment : t; source_name : string; code : int; \
     width : width; operation : adjustment_operation; definition_line : int; \
     consumers : source_reference list }\n\
    \  val all : t list\n\
    \  val info : t -> info\n\
    \  val to_source_name : t -> string\n\
    \  val to_code : t -> int\n\
    \  val of_code : int -> t option\n\
    \  val of_source_name : string -> t option\n\
     end\n\n\
     type behavior_sources = private { header_layout : source_reference; \
     header_write : source_reference; module_validation : source_reference; \
     module_base : source_reference; import_grouping : source_reference; \
     import_patches : source_reference; export_registration : \
     source_reference; absolute_patch : source_reference; code_heap_patch : \
     source_reference; data_heap_patch : source_reference; main_execution : \
     source_reference; pass_order : source_reference; patch_termination : \
     source_reference; boot_patch_table : source_reference; \
     boot_absolute_patch : source_reference; jit_adjustments : \
     source_reference; aot_adjustments : source_reference }\n\
     val behavior_sources : behavior_sources\n";
  Buffer.contents buffer

let check_file path expected =
  let actual = read_file path in
  if not (String.equal actual expected) then
    fail "%s is stale; regenerate the BIN specification" path

let () =
  let config = parse_arguments () in
  let commit, checksums = manifest_metadata config.manifest in
  let sources =
    List.map
      (fun (path, checksum) ->
        let contents = read_file (source_path config path) in
        verify_source path checksum contents;
        (path, contents))
      checksums
  in
  let tables =
    match Source.parse ~sources with
    | Ok tables -> tables
    | Error problem -> fail "%s" (Source.error_to_string problem)
  in
  let implementation = render_ml ~commit ~sources:checksums tables in
  let interface = render_mli tables.entries tables.adjustments in
  match config.mode with
  | Write ->
      write_file config.output_ml implementation;
      write_file config.output_mli interface
  | Check ->
      check_file config.output_ml implementation;
      check_file config.output_mli interface
