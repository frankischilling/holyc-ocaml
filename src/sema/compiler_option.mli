type t =
  | Echo
  | Trace
  | Warn_unused_var
  | Warn_paren
  | Warn_dup_types
  | Warn_header_mismatch
  | Externs_to_imports
  | Keep_private
  | No_reg_var
  | Globals_on_data_heap
  | No_builtin_const
  | Use_imm64

type phase =
  | Lexing
  | Parsing
  | Function_diagnostics
  | Symbol_registration
  | Linkage
  | Allocation
  | Optimization
  | Code_emission

type source_status = Defined | Source_marked_incomplete
type source = { path : string; sha256 : string }
type source_reference = { path : string; line : int }

type info = {
  option : t;
  source_name : string;
  bit_index : int;
  mask : int64;
  initially_enabled : bool;
  phases : phase list;
  source_status : source_status;
  definition_line : int;
  source_comment : string option;
  consumers : source_reference list;
}

type api = {
  state_expression : string;
  option_source : source_reference;
  get_option_source : source_reference;
  bit_set_source : source_reference;
  set_returns_previous : bool;
}

val reference_commit : string
val sources : source list
val all : t list
val compare : t -> t -> int
val equal : t -> t -> bool
val to_string : t -> string
val of_string : string -> t option
val of_bit_index : int -> t option
val info : t -> info
val information : info list
val mask : t -> int64

val known_mask : int64
(** Union of every source-backed compiler option bit. *)

val initial_mask : int64
val is_enabled : mask:int64 -> t -> bool
val set : mask:int64 -> t -> bool -> int64 * bool
val intentional_gaps : (int * int) list
val api : api
