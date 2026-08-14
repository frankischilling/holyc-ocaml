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

type record_state
type classified_record
type t

val make_record_state :
  staging_mask:int64 -> compiler_option_mask:int64 -> record_state
(** Capture the parser staging and compiler-option state at one declaration. The
    classifier is pure; compile-time option execution can supply a distinct
    state for each source-ordered record. Its extern-to-imports and
    globals-on-data-heap bits must agree with the state used by global
    resolution. *)

val classify : Global_resolution.t -> record_state list -> (t, string) result
(** Derive the pinned [CHashGlblVar] masks and their consumer-facing policy. The
    states must correspond one-for-one with the resolved records. *)

val compilation_mode : t -> Global_resolution.compilation_mode
val records : t -> classified_record list
val record_state_staging_mask : record_state -> int64
val record_state_compiler_option_mask : record_state -> int64

val classified_record_source :
  classified_record -> Global_resolution.global_record

val classified_record_state : classified_record -> record_state
val hash_type_mask : classified_record -> int64
val hash_flag_mask : classified_record -> int64
val combined_hash_mask : classified_record -> int64
val global_flag_mask : classified_record -> int64
val import_name : classified_record -> string option
val value_access : classified_record -> value_access
val cleanup : classified_record -> cleanup
val map_visibility : classified_record -> map_visibility
val aot_publication : classified_record -> aot_publication
val is_public : classified_record -> bool
val is_private : classified_record -> bool
val value_access_name : value_access -> string
val cleanup_name : cleanup -> string
val map_visibility_name : map_visibility -> string
val aot_publication_name : aot_publication -> string
