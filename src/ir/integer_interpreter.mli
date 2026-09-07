type word_type = I64 | U64
type word = private { type_ : word_type; bits : int64 }
type termination = Stream_end | Returned of word option
type error_stage = Configuration | Preflight | Execution

type error = private {
  stage : error_stage;
  code : string;
  message : string;
  executed_steps : int;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

type t

val reference_commit : string

val execute : max_steps:int -> X87_stack.t -> (t, error list) result
(** Preflight and execute the source-audited integer subset. Every executed
    instruction consumes one step, and all unsupported instructions are rejected
    before execution, including instructions in unreachable blocks. *)

val termination : t -> termination
val executed_steps : t -> int

val human : t -> string
(** Render the versioned, deterministic execution result. *)
