(* Generated interface for the pinned TempleOS member-list flag specification. *)

[@@@ocamlformat "disable"]

type source = { path : string; sha256 : string }
type source_reference = { path : string; line : int }
type flag_info = private { source_name : string; bit_index : int; mask : int64; definition_line : int; consumers : source_reference list }
type behavior_info = private { id : string; description : string; source : source_reference }

val reference_commit : string
val sources : source list

type t =
  | Default_available
  | Lastclass
  | String_default_available
  | Function_pointer
  | Variadic
  | No_unused_warning
  | Static

val all : t list
val to_source_name : t -> string
val of_source_name : string -> t option
val to_bit_index : t -> int
val to_mask : t -> int64
val info : t -> flag_info
val is_set : mask:int64 -> t -> bool
val set : mask:int64 -> t -> int64
val clear : mask:int64 -> t -> int64

val behaviors : behavior_info list
val behavior : string -> behavior_info option
