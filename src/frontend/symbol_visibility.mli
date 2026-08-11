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

type origin =
  | Pinned_source of { path : string; line : int }
  | Source_span of Common.Span.t
  | Session_registration

type entry

val id : entry -> int
val name : entry -> string
val kind : entry -> kind
val origin : entry -> origin
val kind_name : kind -> string
val kind_bit : kind -> int

type lookup = Absent | Present of entry | Shadowed_by_local

module Environment : sig
  type t
  type local_context

  val create : unit -> t

  val add :
    ?origin:origin -> t -> name:string -> kind:kind -> unit -> entry

  val find_preprocessor : t -> string -> lookup
  val all : t -> entry list
  val begin_local_context : t -> local_context

  val add_local :
    t -> local_context -> name:string -> (unit, string) result

  val end_local_context : t -> local_context -> (unit, string) result
  val dump : Common.Source_manager.t -> t -> string
end
