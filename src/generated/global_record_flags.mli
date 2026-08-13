(* Generated interface for the pinned TempleOS global-record flag specification. *)

[@@@ocamlformat "disable"]

type source = { path : string; sha256 : string }
type source_reference = { path : string; line : int }
type global_type = private { index_name : string; mask_name : string; type_index : int; type_mask : int64; index_definition_line : int; mask_definition_line : int }
type flag_info = private { bit_name : string option; mask_name : string; bit_index : int; mask : int64; definition_line : int; consumers : source_reference list }
type behavior_info = private { id : string; description : string; source : source_reference }

val reference_commit : string
val sources : source list
val global_type : global_type

module Hash_flag : sig
  type t =
    | Private
    | Public
    | Export
    | Import
    | Immediate
    | Goto_label
    | Resolve
    | Unresolved
    | Local

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

module Global_flag : sig
  type t =
    | Function_pointer
    | Import
    | Extern
    | Data_heap
    | Alias
    | Array

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

val behaviors : behavior_info list
val behavior : string -> behavior_info option
