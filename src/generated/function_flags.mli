(* Generated interface for the pinned TempleOS function-flag specification. *)

[@@@ocamlformat "disable"]

type source = { path : string; sha256 : string }

type source_reference = { path : string; line : int }

type function_type = private {
  index_name : string;
  mask_name : string;
  type_index : int;
  type_mask : int64;
  index_definition_line : int;
  mask_definition_line : int;
}

type flag_info = private {
  source_name : string;
  bit_index : int;
  mask : int64;
  definition_line : int;
  consumers : source_reference list;
}

val reference_commit : string
val sources : source list
val function_type : function_type

module Shared : sig
  type t =
    | Extern
    | Internal_type

  val all : t list
  val to_source_name : t -> string
  val of_source_name : string -> t option
  val to_bit_index : t -> int
  val to_mask : t -> int64
  val info : t -> flag_info
  val is_set : mask:int64 -> t -> bool
  val set : mask:int64 -> t -> int64
  val clear : mask:int64 -> t -> int64
end

module Stored : sig
  type t =
    | Interrupt
    | Has_error_code
    | Argument_pop
    | No_argument_pop
    | Internal
    | Underscore_extern
    | Variadic
    | Ret1

  val all : t list
  val to_source_name : t -> string
  val of_source_name : string -> t option
  val to_bit_index : t -> int
  val to_mask : t -> int64
  val info : t -> flag_info
  val is_set : mask:int64 -> t -> bool
  val set : mask:int64 -> t -> int64
  val clear : mask:int64 -> t -> int64
end

module Staging : sig
  type t =
    | Public
    | Assembly
    | Static
    | Underscore_name
    | Interrupt
    | Has_error_code
    | Argument_pop
    | No_argument_pop

  val all : t list
  val to_source_name : t -> string
  val of_source_name : string -> t option
  val to_bit_index : t -> int
  val to_mask : t -> int64
  val info : t -> flag_info
  val is_set : mask:int64 -> t -> bool
  val set : mask:int64 -> t -> int64
  val clear : mask:int64 -> t -> int64
end

module Group : sig
  type t =
    | Function_flags
    | Function_and_public_flags

  type info = private {
    group : t;
    source_name : string;
    mask : int64;
    members : Staging.t list;
    source_terms : string list;
    definition_line : int;
    consumers : source_reference list;
  }

  val all : t list
  val to_source_name : t -> string
  val of_source_name : string -> t option
  val to_mask : t -> int64
  val info : t -> info
end

type transition_operation =
  | Add_bits of int64
  | Replace_preserving of { keep_mask : int64; add_mask : int64 }

module Modifier : sig
  type t =
    | Static
    | Interrupt
    | Has_error_code
    | Argument_pop
    | No_argument_pop
    | Public
    | Underscore_name

  type info = private {
    modifier : t;
    spelling : string;
    operation : transition_operation;
    sources : source_reference list;
  }

  val all : t list
  val to_spelling : t -> string
  val info : t -> info
end

val apply_modifier : mask:int64 -> Modifier.t -> int64
val stored_of_staging : Staging.t -> Stored.t option
val stored_mask_of_staging : int64 -> int64
val public_requested : int64 -> bool
val assembly_mode : int64 -> bool
val derives_ret1 : argument_count:int64 -> variadic:bool -> bool
val caller_expects_callee_pop : stored_mask:int64 -> bool
val interrupt_discards_error_code : stored_mask:int64 -> bool
val is_internal : stored_mask:int64 -> bool

type behavior_sources = private {
  record_creation_extern : source_reference;
  symbol_flag_transfer : source_reference;
  public_type_transfer : source_reference;
  private_type_transfer : source_reference;
  automatic_ret1 : source_reference;
  variadic_declaration : source_reference;
  intern_transition : source_reference;
  bound_extern_transition : source_reference;
  bound_extern_aot_resolve : source_reference;
  import_transition : source_reference;
  definition_aot_publication : source_reference;
  definition_resolves : source_reference;
  external_call_dispatch : source_reference;
  hash_value_dispatch : source_reference;
  map_visibility : source_reference;
  aot_import_precedence : source_reference;
  aot_export_resolution : source_reference;
  variadic_optimizer : source_reference;
  caller_cleanup : source_reference;
  try_cleanup : source_reference;
  internal_dispatch : source_reference;
  internal_clobber : source_reference;
  symbol_lookup_exclusion : source_reference;
  interrupt_restore : source_reference;
  interrupt_return : source_reference;
  interrupt_error_code : source_reference;
  callee_cleanup : source_reference;
  interrupt_save : source_reference;
}

val behavior_sources : behavior_sources
