type t = Date | Time | Line | Command_line | File | Directory

val all : t list
val spelling : t -> string
val find : string -> t option
val matches_standard_body : t -> string -> bool

module Settings : sig
  type t

  val create :
    ?date:string ->
    ?time:string ->
    ?command_line:bool ->
    unit ->
    (t, string) result

  val date : t -> string
  val time : t -> string
  val command_line : t -> bool
end

val expand :
  Settings.t ->
  t ->
  source:Common.Source_file.t ->
  line:int ->
  source_depth:int ->
  string
