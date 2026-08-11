type compilation_mode = Jit | Aot

val compilation_mode_name : compilation_mode -> string

(** [Hosted_strict] reports unmatched boundaries. [Templeos_permissive] follows
    the pinned lexer's silent scan and EOF behavior. Hosted limits remain active
    under both policies. *)
type conditional_recovery = Hosted_strict | Templeos_permissive

val conditional_recovery_name : conditional_recovery -> string

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
    ?conditional_recovery:conditional_recovery ->
    unit ->
    (t, string) result

  val resolver : t -> Include_resolver.t
  val compilation_mode : t -> compilation_mode
  val conditional_recovery : t -> conditional_recovery
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
  conditional_recovery : conditional_recovery;
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

val report_human : reference_commit:string -> output -> string
val report_to_yojson : reference_commit:string -> output -> Yojson.Safe.t

val report_json : reference_commit:string -> output -> string
(** Versioned preprocessing reports. Callers must pass the exact TempleOS
    reference commit used for the compatibility result. *)

val lex_all :
  sources:Common.Source_manager.t ->
  definitions:Definition.Environment.t ->
  symbols:Symbol_visibility.Environment.t ->
  config:Config.t ->
  Common.Source_file.t ->
  (Token.t list, Common.Diagnostic.t list) result
