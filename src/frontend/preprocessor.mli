module Config : sig
  type t

  val create :
    ?working_directory:string ->
    ?include_roots:string list ->
    ?templeos_root:string ->
    ?max_include_depth:int ->
    ?max_source_bytes:int ->
    unit ->
    (t, string) result

  val resolver : t -> Include_resolver.t
  val max_include_depth : t -> int
  val max_source_bytes : t -> int
end

type t

val create :
  sources:Common.Source_manager.t ->
  config:Config.t ->
  Common.Source_file.t ->
  t

val next : t -> Lexer.item

val lex_all :
  sources:Common.Source_manager.t ->
  config:Config.t ->
  Common.Source_file.t ->
  (Token.t list, Common.Diagnostic.t list) result
