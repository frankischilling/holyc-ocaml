type call_access =
  | Internal_operation
  | Direct_executable_call
  | Jit_extern_address_slot_call
  | Aot_import_call
  | Aot_extern_call

type hash_value_access =
  | Hash_returns_function_record
  | Hash_returns_executable_address

type runtime_lookup =
  | Runtime_lookup_visible
  | Runtime_lookup_omits_extern
  | Runtime_lookup_omits_internal
  | Runtime_lookup_omits_extern_and_internal

type map_visibility =
  | Map_visible
  | Map_omitted_import
  | Map_omitted_private
  | Map_omitted_import_and_private

type aot_resolution =
  | No_aot_resolution
  | Aot_resolve_references
  | Aot_resolution_shadowed_by_import

type aot_publication =
  | No_aot_publication
  | Aot_import_record
  | Aot_export_record

type declaration_state
type record
type classified_declaration
type classified_identity
type t

val make_declaration_state :
  staging_mask:int64 ->
  compiler_option_mask:int64 ->
  ?import_name:string ->
  unit ->
  declaration_state
(** Capture the parser and compiler-option state at one declaration. Import
    declarations also carry the exact local or alternate loader spelling. *)

val classify :
  Function_resolution.t -> declaration_state list -> (t, string) result
(** Replay [PrsFunJoin] and the binding-specific mutations in source order. The
    state list must correspond one-for-one with the resolved declarations. *)

val compilation_mode : t -> Function_resolution.compilation_mode
val declarations : t -> classified_declaration list
val identities : t -> classified_identity list
val declaration_state_staging_mask : declaration_state -> int64
val declaration_state_compiler_option_mask : declaration_state -> int64
val declaration_state_import_name : declaration_state -> string option

val classified_declaration_source :
  classified_declaration -> Function_resolution.resolved_declaration

val classified_declaration_state : classified_declaration -> declaration_state
val classified_declaration_record : classified_declaration -> record

val classified_identity_source :
  classified_identity -> Function_resolution.identity

val classified_identity_record : classified_identity -> record
val shared_flag_mask : record -> int64
val stored_flag_mask : record -> int64
val function_flag_mask : record -> int64
val hash_type_mask : record -> int64
val hash_flag_mask : record -> int64
val combined_hash_mask : record -> int64
val import_name : record -> string option
val call_access : record -> call_access
val hash_value_access : record -> hash_value_access
val runtime_lookup : record -> runtime_lookup
val map_visibility : record -> map_visibility
val aot_resolution : record -> aot_resolution
val aot_publication : record -> aot_publication
val is_extern : record -> bool
val is_internal : record -> bool
val is_public : record -> bool
val is_private : record -> bool
val call_access_name : call_access -> string
val hash_value_access_name : hash_value_access -> string
val runtime_lookup_name : runtime_lookup -> string
val map_visibility_name : map_visibility -> string
val aot_resolution_name : aot_resolution -> string
val aot_publication_name : aot_publication -> string

module Shared_flag = Function_flag.Shared
module Stored_flag = Function_flag.Stored
module Hash_flag = Global_record_flag.Hash_flag
