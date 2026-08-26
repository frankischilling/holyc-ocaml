type error = {
  code : string;
  message : string;
  stream_id : int option;
  item_position : int option;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

module Stream_id : sig
  type t

  val of_int : int -> (t, error) result
  val to_int : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

type description = {
  stream_id : Stream_id.t;
  item_position : int;
  compiler_options : int64;
  span : Common.Span.t option;
  body : Block_graph.t;
}

type t

val reference_commit : string

val create : description -> (t, error list) result
(** Validate one executable top-level statement stream and its x87 stack before
    constructing an immutable body. *)

val stream_id : t -> Stream_id.t
val item_position : t -> int
val compiler_options : t -> int64
val span : t -> Common.Span.t option
val body : t -> Block_graph.t
val x87 : t -> X87_stack.t

val human : t -> string
(** Render the versioned top-level stream form used by tests and later tools. *)
