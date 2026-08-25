type error = {
  code : string;
  message : string;
  instruction_id : int option;
  span : Common.Span.t option;
}

module Instruction_id : sig
  type t

  val of_int : int -> (t, error) result
  val to_int : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Value_id : sig
  type t

  val of_int : int -> (t, error) result
  val to_int : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Block_id : sig
  type t

  val of_int : int -> (t, error) result
  val to_int : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

type payload =
  | Integer of int64
  | Float_bits of int64
  | Bytes of string
  | Symbol of Sema.Symbol.t
  | Block of Block_id.t
  | Block_targets of Block_id.t list

type value_definition = { value_id : Value_id.t }

type description = {
  instruction_id : Instruction_id.t;
  opcode : Opcode.t;
  operands : Value_id.t list;
  result : value_definition option;
  target_type : Sema.Type.t option;
  payload : payload option;
  flags : int64;
  span : Common.Span.t option;
}

type instruction
type t

val reference_commit : string
val known_flag_mask : int64

val create : description list -> (t, error list) result
(** Check instruction shapes, source order, IDs, spans, and flag bits before
    constructing an immutable sequence. *)

val instructions : t -> instruction list
val description : instruction -> description
val length : t -> int

val human : t -> string
(** Render the versioned, deterministic text form used by tests and tools. *)

val human_body : t -> string
(** Render instructions without the schema header. This is used by enclosing IR
    containers that provide their own versioned header. *)
