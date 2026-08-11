type compilation_mode = Jit | Aot

val compilation_mode_name : compilation_mode -> string

module Config : sig
  type t

  val create :
    ?working_directory:string ->
    ?include_roots:string list ->
    ?templeos_root:string ->
    ?compilation_mode:compilation_mode ->
    ?max_conditional_depth:int ->
    ?max_include_depth:int ->
    ?max_source_bytes:int ->
    ?max_definition_depth:int ->
    ?max_generated_bytes:int ->
    unit ->
    (t, string) result

  val resolver : t -> Include_resolver.t
  val compilation_mode : t -> compilation_mode
  val max_conditional_depth : t -> int
  val max_include_depth : t -> int
  val max_source_bytes : t -> int
  val max_definition_depth : t -> int
  val max_generated_bytes : t -> int
end

type t

val create :
  sources:Common.Source_manager.t ->
  definitions:Definition.Environment.t ->
  config:Config.t ->
  Common.Source_file.t ->
  t

val next : t -> Lexer.item
val definitions : t -> Definition.t list
val definition_dump : t -> string

val lex_all :
  sources:Common.Source_manager.t ->
  definitions:Definition.Environment.t ->
  config:Config.t ->
  Common.Source_file.t ->
  (Token.t list, Common.Diagnostic.t list) result
