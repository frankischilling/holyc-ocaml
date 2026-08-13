module Id : sig
  type t

  val of_int : int -> t
  val to_int : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Scope_id : sig
  type t

  val of_int : int -> t
  val to_int : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

type kind =
  | Internal_type
  | Aggregate_type
  | Function
  | Global_variable
  | Parameter
  | Local_variable
  | Member
  | Label
  | Assembler_symbol
  | Module
  | Generated

type source_origin = {
  span : Common.Span.t;
  source_segments : Common.Span.t list;
  generated_from : Common.Span.t option;
  defined_at : Common.Span.t option;
}

type origin =
  | Pinned_source of { path : string; line : int }
  | Source_location of source_origin
  | Synthesized of string

type t

val create :
  id:Id.t ->
  scope_id:Scope_id.t ->
  name:string ->
  kind:kind ->
  origin:origin ->
  t

val id : t -> Id.t
val scope_id : t -> Scope_id.t
val name : t -> string
val kind : t -> kind
val origin : t -> origin
val kind_name : kind -> string
val compare_kind : kind -> kind -> int
val equal_kind : kind -> kind -> bool
val reference_commit : string
