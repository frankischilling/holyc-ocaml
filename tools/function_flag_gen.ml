module Source = Function_flag_source
module Json_util = Yojson.Basic.Util

let expected_reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"

let source_paths =
  [
    "Kernel/KernelA.HH";
    "Compiler/CompilerA.HH";
    "Compiler/PrsStmt.HC";
    "Compiler/PrsVar.HC";
    "Compiler/PrsExp.HC";
    "Compiler/OptPass3.HC";
    "Compiler/OptPass6.HC";
    "Compiler/OptPass789A.HC";
    "Kernel/FunSeg.HC";
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
      prerr_endline ("function-flag generator: " ^ message);
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
  | Error problem -> fail "%s" (Source.error_to_string { problem with path = Some path })

let source contents path =
  match List.assoc_opt path contents with
  | Some value -> value
  | None -> fail "internal error: %s was not loaded" path

let shared_constructor = function
  | "Cf_EXTERN" -> "Extern"
  | "Cf_INTERNAL_TYPE" -> "Internal_type"
  | name -> fail "no shared-flag constructor is defined for %s" name

let stored_constructor = function
  | "Ff_INTERRUPT" -> "Interrupt"
  | "Ff_HASERRCODE" -> "Has_error_code"
  | "Ff_ARGPOP" -> "Argument_pop"
  | "Ff_NOARGPOP" -> "No_argument_pop"
  | "Ff_INTERNAL" -> "Internal"
  | "Ff__EXTERN" -> "Underscore_extern"
  | "Ff_DOT_DOT_DOT" -> "Variadic"
  | "Ff_RET1" -> "Ret1"
  | name -> fail "no stored-flag constructor is defined for %s" name

let staging_constructor = function
  | "FSF_PUBLIC" -> "Public"
  | "FSF_ASM" -> "Assembly"
  | "FSF_STATIC" -> "Static"
  | "FSF__" -> "Underscore_name"
  | "FSF_INTERRUPT" -> "Interrupt"
  | "FSF_HASERRCODE" -> "Has_error_code"
  | "FSF_ARGPOP" -> "Argument_pop"
  | "FSF_NOARGPOP" -> "No_argument_pop"
  | name -> fail "no staging-flag constructor is defined for %s" name

let group_constructor = function
  | "FSG_FUN_FLAGS1" -> "Function_flags"
  | "FSG_FUN_FLAGS2" -> "Function_and_public_flags"
  | name -> fail "no flag-group constructor is defined for %s" name

let modifier_constructor = function
  | "Static" -> "Static"
  | "Interrupt" -> "Interrupt"
  | "Has_error_code" -> "Has_error_code"
  | "Argument_pop" -> "Argument_pop"
  | "No_argument_pop" -> "No_argument_pop"
  | "Public" -> "Public"
  | "Underscore_name" -> "Underscore_name"
  | name -> fail "no modifier constructor is defined for %s" name

let add_variant buffer constructor = Printf.bprintf buffer "    | %s\n" constructor

let add_source_reference buffer (reference : Source.source_reference) =
  Printf.bprintf buffer "{ path = %S; line = %d }" reference.path reference.line

let add_source_reference_list buffer references =
  Buffer.add_string buffer "[ ";
  List.iteri
    (fun index reference ->
      if index > 0 then Buffer.add_string buffer "; ";
      add_source_reference buffer reference)
    references;
  Buffer.add_string buffer " ]"

let add_flag_info buffer constructor (entry : Source.flag_entry) =
  Printf.bprintf buffer
    "    | %s -> { source_name = %S; bit_index = %d; mask = 0x%LxL; \
     definition_line = %d; consumers = "
    constructor entry.name entry.bit_index entry.mask entry.definition_line;
  add_source_reference_list buffer entry.consumers;
  Buffer.add_string buffer " }\n"

let add_flag_module buffer ~name ~constructor (entries : Source.flag_entry list) =
  Printf.bprintf buffer "module %s = struct\n" name;
  Buffer.add_string buffer "  type t =\n";
  List.iter
    (fun (entry : Source.flag_entry) ->
      add_variant buffer (constructor entry.name))
    entries;
  Buffer.add_string buffer "\n  let all =\n    [\n";
  List.iter
    (fun (entry : Source.flag_entry) ->
      Printf.bprintf buffer "      %s;\n" (constructor entry.name))
    entries;
  Buffer.add_string buffer "    ]\n\n  let to_source_name = function\n";
  List.iter
    (fun (entry : Source.flag_entry) ->
      Printf.bprintf buffer "    | %s -> %S\n" (constructor entry.name)
        entry.name)
    entries;
  Buffer.add_string buffer "\n  let of_source_name = function\n";
  List.iter
    (fun (entry : Source.flag_entry) ->
      Printf.bprintf buffer "    | %S -> Some %s\n" entry.name
        (constructor entry.name))
    entries;
  Buffer.add_string buffer "    | _ -> None\n\n  let info = function\n";
  List.iter
    (fun (entry : Source.flag_entry) ->
      add_flag_info buffer (constructor entry.name) entry)
    entries;
  Buffer.add_string buffer
    "\n  let to_bit_index flag = (info flag).bit_index\n\
     \  let to_mask flag = (info flag).mask\n\
     \  let is_set ~mask flag = Int64.logand mask (to_mask flag) <> 0L\n\
     \  let set ~mask flag = Int64.logor mask (to_mask flag)\n\
     \  let clear ~mask flag = Int64.logand mask (Int64.lognot (to_mask flag))\n\
     end\n\n"

let add_flag_module_signature buffer ~name ~constructors =
  Printf.bprintf buffer "module %s : sig\n  type t =\n" name;
  List.iter (add_variant buffer) constructors;
  Buffer.add_string buffer
    "\n  val all : t list\n\
     \  val to_source_name : t -> string\n\
     \  val of_source_name : string -> t option\n\
     \  val to_bit_index : t -> int\n\
     \  val to_mask : t -> int64\n\
     \  val info : t -> flag_info\n\
     \  val is_set : mask:int64 -> t -> bool\n\
     \  val set : mask:int64 -> t -> int64\n\
     \  val clear : mask:int64 -> t -> int64\n\
     end\n\n"

let expanded_group_members = function
  | "FSG_FUN_FLAGS1" ->
      [ "FSF_INTERRUPT"; "FSF_HASERRCODE"; "FSF_ARGPOP"; "FSF_NOARGPOP" ]
  | "FSG_FUN_FLAGS2" ->
      [
        "FSF_INTERRUPT";
        "FSF_HASERRCODE";
        "FSF_ARGPOP";
        "FSF_NOARGPOP";
        "FSF_PUBLIC";
      ]
  | name -> fail "cannot expand members for %s" name

let add_group_info buffer (entry : Source.group_entry) =
  let constructor = group_constructor entry.name in
  Printf.bprintf buffer
    "    | %s -> { group = %s; source_name = %S; mask = 0x%LxL; members = [ "
    constructor constructor entry.name entry.mask;
  List.iteri
    (fun index name ->
      if index > 0 then Buffer.add_string buffer "; ";
      Printf.bprintf buffer "Staging.%s" (staging_constructor name))
    (expanded_group_members entry.name);
  Buffer.add_string buffer " ]; source_terms = [ ";
  List.iteri
    (fun index name ->
      if index > 0 then Buffer.add_string buffer "; ";
      Printf.bprintf buffer "%S" name)
    entry.members;
  Printf.bprintf buffer "]; definition_line = %d; consumers = " entry.definition_line;
  add_source_reference_list buffer entry.consumers;
  Buffer.add_string buffer " }\n"

let add_operation buffer = function
  | Source.Add_bits bits -> Printf.bprintf buffer "Add_bits 0x%LxL" bits
  | Source.Replace_preserving { keep_mask; add_mask } ->
      Printf.bprintf buffer
        "Replace_preserving { keep_mask = 0x%LxL; add_mask = 0x%LxL }"
        keep_mask add_mask

let add_modifier_info buffer (entry : Source.transition_entry) =
  let constructor = modifier_constructor entry.name in
  Printf.bprintf buffer
    "    | %s -> { modifier = %s; spelling = %S; operation = " constructor
    constructor entry.spelling;
  add_operation buffer entry.operation;
  Buffer.add_string buffer "; sources = ";
  add_source_reference_list buffer entry.sources;
  Buffer.add_string buffer " }\n"

let add_behavior_field buffer name (reference : Source.source_reference) =
  Printf.bprintf buffer "    %s = { path = %S; line = %d };\n" name
    reference.path reference.line

let render_ml ~commit ~checksums (tables : Source.tables) =
  let buffer = Buffer.create 32768 in
  Buffer.add_string buffer
    "(* Generated from the pinned TempleOS function and parser flag definitions.\n\
     \   Regenerate this file only after reviewing the source behavior. *)\n\n";
  Buffer.add_string buffer "[@@@ocamlformat \"disable\"]\n\n";
  Printf.bprintf buffer "let reference_commit = %S\n\n" commit;
  Buffer.add_string buffer
    "type source = { path : string; sha256 : string }\n\n\
     type source_reference = { path : string; line : int }\n\n\
     let sources =\n  [\n";
  List.iter
    (fun (path, checksum) ->
      Printf.bprintf buffer "    { path = %S; sha256 = %S };\n" path checksum)
    checksums;
  Buffer.add_string buffer
    "  ]\n\n\
     type flag_info = {\n\
     \  source_name : string;\n\
     \  bit_index : int;\n\
     \  mask : int64;\n\
     \  definition_line : int;\n\
     \  consumers : source_reference list;\n\
     }\n\n";
  add_flag_module buffer ~name:"Shared" ~constructor:shared_constructor
    tables.shared_flags;
  add_flag_module buffer ~name:"Stored" ~constructor:stored_constructor
    tables.function_flags;
  add_flag_module buffer ~name:"Staging" ~constructor:staging_constructor
    tables.staging_flags;
  Buffer.add_string buffer "module Group = struct\n  type t =\n";
  List.iter
    (fun (entry : Source.group_entry) ->
      add_variant buffer (group_constructor entry.name))
    tables.groups;
  Buffer.add_string buffer
    "\n  type info = {\n\
     \    group : t;\n\
     \    source_name : string;\n\
     \    mask : int64;\n\
     \    members : Staging.t list;\n\
     \    source_terms : string list;\n\
     \    definition_line : int;\n\
     \    consumers : source_reference list;\n\
     \  }\n\n\
     \  let all =\n    [\n";
  List.iter
    (fun (entry : Source.group_entry) ->
      Printf.bprintf buffer "      %s;\n" (group_constructor entry.name))
    tables.groups;
  Buffer.add_string buffer "    ]\n\n  let info = function\n";
  List.iter (add_group_info buffer) tables.groups;
  Buffer.add_string buffer
    "\n  let to_source_name group = (info group).source_name\n\
     \  let of_source_name = function\n";
  List.iter
    (fun (entry : Source.group_entry) ->
      Printf.bprintf buffer "    | %S -> Some %s\n" entry.name
        (group_constructor entry.name))
    tables.groups;
  Buffer.add_string buffer
    "    | _ -> None\n\n\
     \  let to_mask group = (info group).mask\n\
     end\n\n";
  Buffer.add_string buffer
    "type transition_operation =\n\
     \  | Add_bits of int64\n\
     \  | Replace_preserving of { keep_mask : int64; add_mask : int64 }\n\n\
     module Modifier = struct\n\
     \  type t =\n";
  List.iter
    (fun (entry : Source.transition_entry) ->
      add_variant buffer (modifier_constructor entry.name))
    tables.transitions;
  Buffer.add_string buffer
    "\n  type info = {\n\
     \    modifier : t;\n\
     \    spelling : string;\n\
     \    operation : transition_operation;\n\
     \    sources : source_reference list;\n\
     \  }\n\n\
     \  let all =\n    [\n";
  List.iter
    (fun (entry : Source.transition_entry) ->
      Printf.bprintf buffer "      %s;\n" (modifier_constructor entry.name))
    tables.transitions;
  Buffer.add_string buffer "    ]\n\n  let info = function\n";
  List.iter (add_modifier_info buffer) tables.transitions;
  Buffer.add_string buffer
    "\n  let to_spelling modifier = (info modifier).spelling\n\
     end\n\n\
     let apply_modifier ~mask modifier =\n\
     \  match (Modifier.info modifier).operation with\n\
     \  | Add_bits bits -> Int64.logor mask bits\n\
     \  | Replace_preserving { keep_mask; add_mask } ->\n\
     \      Int64.logor (Int64.logand mask keep_mask) add_mask\n\n\
     let stored_of_staging = function\n\
     \  | Staging.Interrupt -> Some Stored.Interrupt\n\
     \  | Staging.Has_error_code -> Some Stored.Has_error_code\n\
     \  | Staging.Argument_pop -> Some Stored.Argument_pop\n\
     \  | Staging.No_argument_pop -> Some Stored.No_argument_pop\n\
     \  | Staging.Public | Staging.Assembly | Staging.Static | Staging.Underscore_name -> None\n\n\
     let stored_mask_of_staging mask =\n\
     \  Int64.logand mask (Group.to_mask Group.Function_flags)\n\n\
     let public_requested mask = Staging.is_set ~mask Staging.Public\n\
     let assembly_mode mask = Staging.is_set ~mask Staging.Assembly\n\n\
     let derives_ret1 ~argument_count ~variadic =\n\
     \  (not variadic)\n\
     \  && Int64.compare argument_count 0L > 0\n\
     \  && Int64.compare argument_count 4095L <= 0\n\n\
     let caller_expects_callee_pop ~stored_mask =\n\
     \  (Stored.is_set ~mask:stored_mask Stored.Ret1\n\
     \  || Stored.is_set ~mask:stored_mask Stored.Argument_pop)\n\
     \  && not (Stored.is_set ~mask:stored_mask Stored.No_argument_pop)\n\n\
     let interrupt_discards_error_code ~stored_mask =\n\
     \  Stored.is_set ~mask:stored_mask Stored.Interrupt\n\
     \  && Stored.is_set ~mask:stored_mask Stored.Has_error_code\n\n\
     let is_internal ~stored_mask = Stored.is_set ~mask:stored_mask Stored.Internal\n\n";
  Buffer.add_string buffer
    "type behavior_sources = {\n\
     \  symbol_flag_transfer : source_reference;\n\
     \  public_type_transfer : source_reference;\n\
     \  automatic_ret1 : source_reference;\n\
     \  variadic_declaration : source_reference;\n\
     \  variadic_optimizer : source_reference;\n\
     \  caller_cleanup : source_reference;\n\
     \  try_cleanup : source_reference;\n\
     \  internal_dispatch : source_reference;\n\
     \  internal_clobber : source_reference;\n\
     \  symbol_lookup_exclusion : source_reference;\n\
     \  interrupt_restore : source_reference;\n\
     \  interrupt_return : source_reference;\n\
     \  interrupt_error_code : source_reference;\n\
     \  callee_cleanup : source_reference;\n\
     \  interrupt_save : source_reference;\n\
     }\n\n\
     let behavior_sources =\n  {\n";
  let behavior = tables.behavior in
  add_behavior_field buffer "symbol_flag_transfer" behavior.symbol_flag_transfer;
  add_behavior_field buffer "public_type_transfer" behavior.public_type_transfer;
  add_behavior_field buffer "automatic_ret1" behavior.automatic_ret1;
  add_behavior_field buffer "variadic_declaration" behavior.variadic_declaration;
  add_behavior_field buffer "variadic_optimizer" behavior.variadic_optimizer;
  add_behavior_field buffer "caller_cleanup" behavior.caller_cleanup;
  add_behavior_field buffer "try_cleanup" behavior.try_cleanup;
  add_behavior_field buffer "internal_dispatch" behavior.internal_dispatch;
  add_behavior_field buffer "internal_clobber" behavior.internal_clobber;
  add_behavior_field buffer "symbol_lookup_exclusion" behavior.symbol_lookup_exclusion;
  add_behavior_field buffer "interrupt_restore" behavior.interrupt_restore;
  add_behavior_field buffer "interrupt_return" behavior.interrupt_return;
  add_behavior_field buffer "interrupt_error_code" behavior.interrupt_error_code;
  add_behavior_field buffer "callee_cleanup" behavior.callee_cleanup;
  add_behavior_field buffer "interrupt_save" behavior.interrupt_save;
  Buffer.add_string buffer "  }\n";
  Buffer.contents buffer

let render_mli (tables : Source.tables) =
  let buffer = Buffer.create 16384 in
  Buffer.add_string buffer
    "(* Generated interface for the pinned TempleOS function-flag specification. *)\n\n\
     [@@@ocamlformat \"disable\"]\n\n\
     type source = { path : string; sha256 : string }\n\n\
     type source_reference = { path : string; line : int }\n\n\
     type flag_info = private {\n\
     \  source_name : string;\n\
     \  bit_index : int;\n\
     \  mask : int64;\n\
     \  definition_line : int;\n\
     \  consumers : source_reference list;\n\
     }\n\n\
     val reference_commit : string\n\
     val sources : source list\n\n";
  add_flag_module_signature buffer ~name:"Shared"
    ~constructors:
      (List.map
         (fun (entry : Source.flag_entry) -> shared_constructor entry.name)
         tables.shared_flags);
  add_flag_module_signature buffer ~name:"Stored"
    ~constructors:
      (List.map
         (fun (entry : Source.flag_entry) -> stored_constructor entry.name)
         tables.function_flags);
  add_flag_module_signature buffer ~name:"Staging"
    ~constructors:
      (List.map
         (fun (entry : Source.flag_entry) -> staging_constructor entry.name)
         tables.staging_flags);
  Buffer.add_string buffer "module Group : sig\n  type t =\n";
  List.iter
    (fun (entry : Source.group_entry) ->
      add_variant buffer (group_constructor entry.name))
    tables.groups;
  Buffer.add_string buffer
    "\n  type info = private {\n\
     \    group : t;\n\
     \    source_name : string;\n\
     \    mask : int64;\n\
     \    members : Staging.t list;\n\
     \    source_terms : string list;\n\
     \    definition_line : int;\n\
     \    consumers : source_reference list;\n\
     \  }\n\n\
     \  val all : t list\n\
     \  val to_source_name : t -> string\n\
     \  val of_source_name : string -> t option\n\
     \  val to_mask : t -> int64\n\
     \  val info : t -> info\n\
     end\n\n\
     type transition_operation =\n\
     \  | Add_bits of int64\n\
     \  | Replace_preserving of { keep_mask : int64; add_mask : int64 }\n\n\
     module Modifier : sig\n\
     \  type t =\n";
  List.iter
    (fun (entry : Source.transition_entry) ->
      add_variant buffer (modifier_constructor entry.name))
    tables.transitions;
  Buffer.add_string buffer
    "\n  type info = private {\n\
     \    modifier : t;\n\
     \    spelling : string;\n\
     \    operation : transition_operation;\n\
     \    sources : source_reference list;\n\
     \  }\n\n\
     \  val all : t list\n\
     \  val to_spelling : t -> string\n\
     \  val info : t -> info\n\
     end\n\n\
     val apply_modifier : mask:int64 -> Modifier.t -> int64\n\
     val stored_of_staging : Staging.t -> Stored.t option\n\
     val stored_mask_of_staging : int64 -> int64\n\
     val public_requested : int64 -> bool\n\
     val assembly_mode : int64 -> bool\n\
     val derives_ret1 : argument_count:int64 -> variadic:bool -> bool\n\
     val caller_expects_callee_pop : stored_mask:int64 -> bool\n\
     val interrupt_discards_error_code : stored_mask:int64 -> bool\n\
     val is_internal : stored_mask:int64 -> bool\n\n\
     type behavior_sources = private {\n\
     \  symbol_flag_transfer : source_reference;\n\
     \  public_type_transfer : source_reference;\n\
     \  automatic_ret1 : source_reference;\n\
     \  variadic_declaration : source_reference;\n\
     \  variadic_optimizer : source_reference;\n\
     \  caller_cleanup : source_reference;\n\
     \  try_cleanup : source_reference;\n\
     \  internal_dispatch : source_reference;\n\
     \  internal_clobber : source_reference;\n\
     \  symbol_lookup_exclusion : source_reference;\n\
     \  interrupt_restore : source_reference;\n\
     \  interrupt_return : source_reference;\n\
     \  interrupt_error_code : source_reference;\n\
     \  callee_cleanup : source_reference;\n\
     \  interrupt_save : source_reference;\n\
     }\n\n\
     val behavior_sources : behavior_sources\n";
  Buffer.contents buffer

let check_output path expected =
  let actual = read_file path in
  if not (String.equal actual expected) then
    fail
      "%s is stale; regenerate both files with `dune exec \
       tools/function_flag_gen.exe -- --reference-root third_party/TempleOS \
       --manifest reference/manifest.json --output-ml \
       src/generated/function_flags.ml --output-mli \
       src/generated/function_flags.mli`"
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
        ~compiler_source:(source contents "Compiler/CompilerA.HH")
        ~prs_stmt_source:(source contents "Compiler/PrsStmt.HC")
        ~prs_var_source:(source contents "Compiler/PrsVar.HC")
        ~prs_exp_source:(source contents "Compiler/PrsExp.HC")
        ~opt_pass3_source:(source contents "Compiler/OptPass3.HC")
        ~opt_pass6_source:(source contents "Compiler/OptPass6.HC")
        ~opt_pass789a_source:(source contents "Compiler/OptPass789A.HC")
        ~fun_seg_source:(source contents "Kernel/FunSeg.HC")
    with
    | Ok tables -> tables
    | Error problem -> fail "%s" (Source.error_to_string problem)
  in
  let generated_ml = render_ml ~commit ~checksums tables in
  let generated_mli = render_mli tables in
  match config.mode with
  | Write ->
      write_file config.output_ml generated_ml;
      write_file config.output_mli generated_mli
  | Check ->
      check_output config.output_ml generated_ml;
      check_output config.output_mli generated_mli
