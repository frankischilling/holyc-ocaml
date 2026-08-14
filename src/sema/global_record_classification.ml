module Hash_flag = Global_record_flag.Hash_flag
module Global_flag = Global_record_flag.Global_flag

type value_access =
  | Jit_direct_address
  | Jit_extern_address_slot
  | Aot_code_heap_reference
  | Aot_data_heap_reference
  | Aot_import_reference
  | Aot_extern_unimplemented

type cleanup = Free_data_address | Preserve_aliased_data_address

type map_visibility =
  | Map_visible
  | Map_omitted_import
  | Map_omitted_private
  | Map_omitted_import_and_private

type aot_publication =
  | No_aot_publication
  | Aot_export_record
  | Aot_import_record

type record_state = { staging_mask : int64; compiler_option_mask : int64 }

type classified_record = {
  source : Global_resolution.global_record;
  state : record_state;
  hash_flag_mask : int64;
  global_flag_mask : int64;
  import_name : string option;
  value_access : value_access;
  cleanup : cleanup;
  map_visibility : map_visibility;
  aot_publication : aot_publication;
}

type t = {
  compilation_mode : Global_resolution.compilation_mode;
  records : classified_record list;
}

let make_record_state ~staging_mask ~compiler_option_mask =
  { staging_mask; compiler_option_mask }

let compilation_mode classification = classification.compilation_mode
let records classification = classification.records
let record_state_staging_mask state = state.staging_mask
let record_state_compiler_option_mask state = state.compiler_option_mask
let classified_record_source record = record.source
let classified_record_state record = record.state
let hash_type_mask _ = Global_record_flag.global_type.type_mask
let hash_flag_mask record = record.hash_flag_mask

let combined_hash_mask record =
  Int64.logor (hash_type_mask record) record.hash_flag_mask

let global_flag_mask record = record.global_flag_mask
let import_name record = record.import_name
let value_access record = record.value_access
let cleanup record = record.cleanup
let map_visibility record = record.map_visibility
let aot_publication record = record.aot_publication

let is_public record =
  Hash_flag.is_set ~mask:record.hash_flag_mask Hash_flag.Public

let is_private record =
  Hash_flag.is_set ~mask:record.hash_flag_mask Hash_flag.Private

let value_access_name = function
  | Jit_direct_address -> "jit-direct-address"
  | Jit_extern_address_slot -> "jit-extern-address-slot"
  | Aot_code_heap_reference -> "aot-code-heap-reference"
  | Aot_data_heap_reference -> "aot-data-heap-reference"
  | Aot_import_reference -> "aot-import-reference"
  | Aot_extern_unimplemented -> "aot-extern-unimplemented"

let cleanup_name = function
  | Free_data_address -> "free-data-address"
  | Preserve_aliased_data_address -> "preserve-aliased-data-address"

let map_visibility_name = function
  | Map_visible -> "visible"
  | Map_omitted_import -> "omitted-import"
  | Map_omitted_private -> "omitted-private"
  | Map_omitted_import_and_private -> "omitted-import-and-private"

let aot_publication_name = function
  | No_aot_publication -> "none"
  | Aot_export_record -> "export"
  | Aot_import_record -> "import"

let is_definition = function
  | Global_resolution.Definition | Global_resolution.Intern -> true
  | Global_resolution.Extern
  | Global_resolution.Alternate_extern
  | Global_resolution.Import
  | Global_resolution.Alternate_import -> false

let is_import = function
  | Global_resolution.Import | Global_resolution.Alternate_import -> true
  | Global_resolution.Definition
  | Global_resolution.Extern
  | Global_resolution.Alternate_extern
  | Global_resolution.Intern -> false

let add_hash_flag condition flag mask =
  if condition then Hash_flag.set ~mask flag else mask

let add_global_flag condition flag mask =
  if condition then Global_flag.set ~mask flag else mask

let public_requested state = Function_flag.public_requested state.staging_mask

let private_requested state =
  Compiler_option.is_enabled ~mask:state.compiler_option_mask
    Compiler_option.Keep_private

let hash_flags compilation_mode record state =
  let kind = Global_resolution.global_record_kind record in
  let storage = Global_resolution.global_record_storage record in
  0L
  |> add_hash_flag (private_requested state) Hash_flag.Private
  |> add_hash_flag (public_requested state) Hash_flag.Public
  |> add_hash_flag
       (compilation_mode = Global_resolution.Aot
       &&
       match kind with
       | Global_resolution.Alternate_extern -> true
       | Global_resolution.Definition | Global_resolution.Intern ->
           storage = Global_resolution.Code_heap
       | Global_resolution.Extern
       | Global_resolution.Import
       | Global_resolution.Alternate_import -> false)
       Hash_flag.Export
  |> add_hash_flag (is_import kind) Hash_flag.Import
  |> add_hash_flag
       (compilation_mode = Global_resolution.Jit
       && kind = Global_resolution.Extern)
       Hash_flag.Unresolved

let global_flags record =
  let kind = Global_resolution.global_record_kind record in
  let global = Global_resolution.global_record_global record in
  let function_pointer =
    match Global_type_resolution.global_declarator_kind global with
    | Global_type_resolution.Function_pointer _ -> true
    | Global_type_resolution.Object -> false
  in
  let array = Global_type_resolution.global_array_dimensions global <> [] in
  let data_heap =
    is_definition kind
    && Global_resolution.global_record_storage record
       = Global_resolution.Data_heap
  in
  let alias =
    kind = Global_resolution.Alternate_extern
    || Option.is_some (Global_resolution.global_record_alias_target record)
  in
  0L
  |> add_global_flag function_pointer Global_flag.Function_pointer
  |> add_global_flag (is_import kind) Global_flag.Import
  |> add_global_flag (kind = Global_resolution.Extern) Global_flag.Extern
  |> add_global_flag data_heap Global_flag.Data_heap
  |> add_global_flag alias Global_flag.Alias
  |> add_global_flag array Global_flag.Array

let record_import_name record =
  let declaration = Global_resolution.global_record_declaration record in
  match Global_resolution.declaration_binding declaration with
  | Some binding when is_import (Global_resolution.global_record_kind record) ->
      let target = Global_resolution.source_binding_target binding in
      Some
        (match Global_resolution.binding_target_name target with
        | Some target -> target
        | None ->
            record |> Global_resolution.global_record_symbol |> Symbol.name)
  | None | Some _ -> None

let value_access_for compilation_mode global_flags =
  match compilation_mode with
  | Global_resolution.Jit ->
      if Global_flag.is_set ~mask:global_flags Global_flag.Extern then
        Jit_extern_address_slot
      else Jit_direct_address
  | Global_resolution.Aot ->
      if Global_flag.is_set ~mask:global_flags Global_flag.Extern then
        Aot_extern_unimplemented
      else if Global_flag.is_set ~mask:global_flags Global_flag.Import then
        Aot_import_reference
      else if Global_flag.is_set ~mask:global_flags Global_flag.Data_heap then
        Aot_data_heap_reference
      else Aot_code_heap_reference

let cleanup_for global_flags =
  if Global_flag.is_set ~mask:global_flags Global_flag.Alias then
    Preserve_aliased_data_address
  else Free_data_address

let map_visibility_for hash_flags =
  match
    ( Hash_flag.is_set ~mask:hash_flags Hash_flag.Import,
      Hash_flag.is_set ~mask:hash_flags Hash_flag.Private )
  with
  | false, false -> Map_visible
  | true, false -> Map_omitted_import
  | false, true -> Map_omitted_private
  | true, true -> Map_omitted_import_and_private

let aot_publication_for compilation_mode hash_flags =
  match compilation_mode with
  | Global_resolution.Jit -> No_aot_publication
  | Global_resolution.Aot ->
      if Hash_flag.is_set ~mask:hash_flags Hash_flag.Import then
        Aot_import_record
      else if Hash_flag.is_set ~mask:hash_flags Hash_flag.Export then
        Aot_export_record
      else No_aot_publication

let classify_record compilation_mode source state =
  let hash_flag_mask = hash_flags compilation_mode source state in
  let global_flag_mask = global_flags source in
  {
    source;
    state;
    hash_flag_mask;
    global_flag_mask;
    import_name = record_import_name source;
    value_access = value_access_for compilation_mode global_flag_mask;
    cleanup = cleanup_for global_flag_mask;
    map_visibility = map_visibility_for hash_flag_mask;
    aot_publication = aot_publication_for compilation_mode hash_flag_mask;
  }

let validate_state source state =
  let declaration = Global_resolution.global_record_declaration source in
  let resolution_externs_to_imports =
    Compiler_option.is_enabled
      ~mask:(Global_resolution.declaration_compiler_option_mask declaration)
      Compiler_option.Externs_to_imports
  in
  let state_externs_to_imports =
    Compiler_option.is_enabled ~mask:state.compiler_option_mask
      Compiler_option.Externs_to_imports
  in
  if resolution_externs_to_imports = state_externs_to_imports then Ok ()
  else
    Error
      "global record classification has a different extern-to-imports state \
       than global resolution"

let classify resolution states =
  let sources = Global_resolution.records resolution in
  if List.length sources <> List.length states then
    Error
      "global record classification requires one source state per resolved \
       record"
  else
    let compilation_mode = Global_resolution.compilation_mode resolution in
    let rec classify_all records_rev sources states =
      match (sources, states) with
      | [], [] -> Ok { compilation_mode; records = List.rev records_rev }
      | source :: source_rest, state :: state_rest -> (
          match validate_state source state with
          | Error _ as error -> error
          | Ok () ->
              classify_all
                (classify_record compilation_mode source state :: records_rev)
                source_rest state_rest)
      | [], _ :: _ | _ :: _, [] -> assert false
    in
    classify_all [] sources states
