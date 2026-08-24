type t
type item = Token of Token.t | Diagnostic of Common.Diagnostic.t
type definition_terminator = End_of_line | End_of_file | Nul

(** Records whether a lexer configured with [nul_terminates] reached a physical
    EOF or a source terminator. Ordinary lexers still diagnose an embedded NUL.
*)
type termination =
  | Physical_eof
  | Nul_terminated of { terminator_offset : int; trailing_bytes : int }

type definition_replacement = {
  replacement : string;
  replacement_span : Common.Span.t;
  segments : Definition.segment list;
  terminator : definition_terminator;
}

val create :
  ?mode:Token.mode ->
  ?generated_from:Common.Span.t ->
  ?defined_at:Common.Span.t ->
  ?caller:t ->
  ?nul_terminates:bool ->
  ?recover_normalized_doldoc:bool ->
  Common.Source_file.t ->
  t

val offset : t -> int
val termination : t -> termination option
val local_at_end : t -> bool

val consume_continuation_marker : t -> Common.Span.t option
(** Consume an immediate backslash used by source constructs that explicitly
    request continued lexical input. No trivia is skipped. *)

val capture_definition_replacement : t -> definition_replacement
val scan_to_directive_marker : t -> (Token.t option, Common.Diagnostic.t) result
val next : t -> item

val lex_all :
  ?mode:Token.mode ->
  ?nul_terminates:bool ->
  ?recover_normalized_doldoc:bool ->
  Common.Source_file.t ->
  (Token.t list, Common.Diagnostic.t list) result
