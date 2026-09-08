type error = {
  code : string;
  message : string;
  function_id : int option;
  symbol_id : int option;
  position : int option;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

module Function_id : sig
  type t

  val of_int : int -> (t, error) result
  val to_int : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

type member_description = {
  position : int;
  symbol : Sema.Symbol.t;
  type_ : Sema.Type.t;
  span : Common.Span.t option;
}

type description = {
  function_id : Function_id.t;
  symbol : Sema.Symbol.t;
  function_scope : Sema.Symbol.Scope_id.t;
  return_type : Sema.Type.t;
  parameters : member_description list;
  locals : member_description list;
  stored_flags : int64;
  compiler_options : int64;
  span : Common.Span.t option;
  body : Block_graph.t;
}

type member
type t

val reference_commit : string
val known_stored_flag_mask : int64
val known_compiler_option_mask : int64

val create : description -> (t, error list) result
(** Validate the function boundary and its checked graph, including x87 stack
    discipline, before constructing an immutable body. *)

val with_definition :
  records:Sema.Function_record_classification.t ->
  sources:Sema.Function_call_expression_result.t ->
  frames:Sema.Function_frame_layout.t ->
  definition:Sema.Function_record_classification.classified_declaration ->
  frame:Sema.Function_frame_layout.function_layout ->
  t ->
  (t, error list) result
(** Bind a body's definition owner to its checked callable identity. The exact
    record, typed source, frame, members, type, flags and options must agree.
    Typed sources retain the exact resolved declaration, and the frame retains
    its exact input header; reconstructed metadata cannot replace either. The
    callable identity comes from the selected Definition snapshot; [symbol] and
    frame ownership remain attached to that definition's own header. *)

val callable_symbol : t -> Sema.Symbol.t
(** The checked module call identity, or [symbol] for a raw body. *)

val definition_declaration :
  t -> Sema.Function_resolution.resolved_declaration option

val definition_matches_frame :
  t -> Sema.Function_frame_layout.function_layout -> bool
(** A bound body requires its exact associated frame. A raw body retains the
    existing structural frame validation contract. *)

val function_id : t -> Function_id.t
val symbol : t -> Sema.Symbol.t
val function_scope : t -> Sema.Symbol.Scope_id.t
val return_type : t -> Sema.Type.t
val parameters : t -> member list
val locals : t -> member list
val stored_flags : t -> int64
val compiler_options : t -> int64
val span : t -> Common.Span.t option
val body : t -> Block_graph.t
val x87 : t -> X87_stack.t
val member_position : member -> int
val member_symbol : member -> Sema.Symbol.t
val member_type : member -> Sema.Type.t
val member_span : member -> Common.Span.t option

val human : t -> string
(** Render the versioned named-function form used by tests and later tools. *)
