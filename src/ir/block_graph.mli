module Block_id = Instruction_sequence.Block_id
module Instruction_id = Instruction_sequence.Instruction_id

type error = {
  code : string;
  message : string;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

type block_description = {
  block_id : Block_id.t;
  instructions : Instruction_sequence.description list;
}

type block
type t

val create :
  entry:Block_id.t -> block_description list -> (t, error list) result
(** Build an immutable graph in source order. The constructor validates each
    child sequence, global identities, terminators, targets, fallthrough, and
    the unique stream-end marker. A value used outside its defining block must
    have a unique definition that dominates the use. Disconnected regions are
    seeded in source order and checked as additional entries. *)

val entry : t -> block
val blocks : t -> block list

val definition_order : t -> block list
(** Return blocks in dominator-tree order. Every external value producer
    precedes its consumers; instructions within each block retain source order.
*)

val find_block : t -> Block_id.t -> block option
val block_id : block -> Block_id.t
val instructions : block -> Instruction_sequence.t
val successors : block -> Block_id.t list

val human : t -> string
(** Render the versioned, deterministic graph form used by tests and tools. *)

val human_body : t -> string
(** Render the entry and blocks without the graph schema header. *)
