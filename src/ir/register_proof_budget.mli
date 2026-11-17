type t
(** Hosted work accounting for allocation proofs, separate from reached guest
    work and native graph/frame/image quotas. Exhaustion revokes the complete
    proof, including conservative fallbacks; it never selects a stack home. *)

exception Exhausted

val max_depth : int
(** Recursive proof visitors retain a 256-level host-stack safeguard. *)

val capacity : source_bytes:int -> retained_nodes:int -> int
(** Checked sizing policy, exposed for exact/overflow boundary tests. Negative
    sizes and overflowing additions or scaling raise [Exhausted]. *)

val create : ?max_work:int -> Function_body.t -> t
(** Default work is 64 times one plus the retained source byte extent, typed
    result count, original member count, complete block count and instruction
    count. All arithmetic and source extents are checked. [max_work] can only
    reduce that allowance, for callers testing or imposing stricter policy.
    Source size supplies fuel, never source authenticity. *)

val charge : t -> int -> unit
val enter : t -> int -> unit
val remaining : t -> int
val protect : (unit -> 'a option) -> 'a option

val users :
  t ->
  Instruction_sequence.description list ->
  Instruction_sequence.Value_id.t ->
  Instruction_sequence.description list
(** Index complete instruction uses once, retaining instruction order and one
    entry per consuming instruction even if an operand is repeated. *)

(** Physical equality is mandatory: structurally equal foreign owners never
    authenticate. Tables are per proof attempt, never retained authority. *)
module Identity (Key : sig
  type t
end) : Hashtbl.S with type key = Key.t
