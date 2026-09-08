type kind =
  | Export_system_symbol
  | Import_system_symbol
  | Definition
  | Global_variable
  | Class
  | Internal_type
  | Function
  | Word
  | Dictionary_word
  | Keyword
  | Assembly_keyword
  | Opcode
  | Register
  | File
  | Module
  | Help_file
  | Frame_pointer

type source_origin = {
  span : Common.Span.t;
  source_segments : Common.Span.t list;
  generated_from : Common.Span.t option;
  defined_at : Common.Span.t option;
}

type origin =
  | Pinned_source of { path : string; line : int }
  | Source_span of Common.Span.t
  | Source_location of source_origin
  | Session_registration

type parameter_call_shape = {
  parameter_name : string option;
  has_default : bool;
}

type function_call_shape = {
  parameters : parameter_call_shape list;
  variadic : bool;
}

type entry

val id : entry -> int
val name : entry -> string
val kind : entry -> kind
val origin : entry -> origin
val function_call_shape : entry -> function_call_shape option
val kind_name : kind -> string
val kind_bit : kind -> int

type lookup = Absent | Present of entry | Shadowed_by_local

module Environment : sig
  type t
  type local_context

  val create : unit -> t

  val task_view : t -> t
  (** A persistent owner in the same publication store. Lookups see unowned
      baseline entries and this view's entries. The root environment inspects
      all publications; other task views cannot see or complete this owner's
      entries. Local contexts belong only to the view. *)

  val copy : t -> t
  (** Copy the visible entries and active local contexts. Later registrations in
      either environment do not affect the other. *)

  val add :
    ?origin:origin ->
    ?function_call_shape:function_call_shape ->
    t ->
    name:string ->
    kind:kind ->
    unit ->
    entry

  val find_preprocessor : t -> string -> lookup

  val complete_function_header :
    t ->
    entry:entry ->
    function_call_shape:function_call_shape ->
    (entry, string) result
  (** Replace this exact provisional entry in its original publication position.
      Prior entry snapshots and copied environments remain unchanged;
      intervening shadow declarations remain newer. Completion is allowed only
      once by its writer owner. *)

  val all : t -> entry list

  val find_function : t -> string -> entry option
  (** Function-kind-filtered table lookup used at function publication. *)

  val begin_local_context : t -> local_context

  val without_locals : t -> (unit -> 'a) -> 'a
  (** Suspend the current function's local visibility while running task
      commands. Published task entries persist; the saved local context stack is
      restored on success, reported failure or an exception. *)

  val add_local : t -> local_context -> name:string -> (unit, string) result
  val end_local_context : t -> local_context -> (unit, string) result

  val to_yojson :
    ?source_only:bool -> Common.Source_manager.t -> t -> Yojson.Safe.t

  val human : ?source_only:bool -> Common.Source_manager.t -> t -> string
  val json : ?source_only:bool -> Common.Source_manager.t -> t -> string
  val dump : Common.Source_manager.t -> t -> string
end
