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
    ?physical_nul_terminates:bool ->
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

  val physical_nul_terminates : t -> bool
  (** Whether a NUL byte ends root and included physical sources. Generated
      definition and predefined frames remain strict. This is disabled by
      default and enabled by the pinned compatibility corpus. *)

  val predefined : t -> Predefined.Settings.t
end

type t

type diagnostic_context = private {
  include_stack : Common.Diagnostic.related list;
  definition_trace : Common.Diagnostic.related list;
}
(** Source context for the most recently returned token. A streaming consumer
    should capture this value before requesting another item. *)

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
val diagnostic_context : t -> diagnostic_context
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
