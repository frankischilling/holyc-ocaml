type segment = {
  generated_start : int;
  generated_stop : int;
  source_span : Common.Span.t;
}

type t

val id : t -> int
val name : t -> string
val replacement : t -> string
val name_span : t -> Common.Span.t
val definition_span : t -> Common.Span.t
val replacement_span : t -> Common.Span.t
val segments : t -> segment list

module Environment : sig
  type definition = t
  type t

  val create : unit -> t

  val copy : t -> t
  (** Copy the current definitions and identity counter. Later definitions in
      either environment do not affect the other. *)

  val define :
    t ->
    name:string ->
    replacement:string ->
    name_span:Common.Span.t ->
    definition_span:Common.Span.t ->
    replacement_span:Common.Span.t ->
    segments:segment list ->
    definition

  val find : t -> string -> definition option
  val all : t -> definition list
  val dump : Common.Source_manager.t -> t -> string
end
