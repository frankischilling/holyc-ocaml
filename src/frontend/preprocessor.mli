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
    ?max_expression_nodes:int ->
    ?predefined_date:string ->
    ?predefined_time:string ->
    ?command_line_source:bool ->
    unit ->
    (t, string) result

  val resolver : t -> Include_resolver.t
  val compilation_mode : t -> compilation_mode
  val max_conditional_depth : t -> int
  val max_include_depth : t -> int
  val max_source_bytes : t -> int
  val max_definition_depth : t -> int
  val max_generated_bytes : t -> int
  val max_expression_nodes : t -> int
  val predefined : t -> Predefined.Settings.t
end

type t

type output = {
  tokens : Token.t list;
  diagnostics : Common.Diagnostic.t list;
  help_metadata : Help_metadata.t;
}
(** Tokens and diagnostics collected from one complete stream. The token list
    includes EOF, diagnostics retain their source order, and help metadata is
    scoped to this stream. *)

val create :
  sources:Common.Source_manager.t ->
  definitions:Definition.Environment.t ->
  symbols:Symbol_visibility.Environment.t ->
  config:Config.t ->
  Common.Source_file.t ->
  t

val next : t -> Lexer.item
val definitions : t -> Definition.t list
val definition_dump : t -> string
val help_metadata : t -> Help_metadata.t

val collect_all :
  sources:Common.Source_manager.t ->
  definitions:Definition.Environment.t ->
  symbols:Symbol_visibility.Environment.t ->
  config:Config.t ->
  Common.Source_file.t ->
  output

val has_errors : output -> bool
(** [has_errors output] is true when at least one diagnostic has error severity.
    Warnings and notes do not make the output fatal. *)

val lex_all :
  sources:Common.Source_manager.t ->
  definitions:Definition.Environment.t ->
  symbols:Symbol_visibility.Environment.t ->
  config:Config.t ->
  Common.Source_file.t ->
  (Token.t list, Common.Diagnostic.t list) result
