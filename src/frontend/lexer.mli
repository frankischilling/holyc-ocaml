type t
type item = Token of Token.t | Diagnostic of Common.Diagnostic.t

val create : ?mode:Token.mode -> Common.Source_file.t -> t
val next : t -> item

val lex_all :
  ?mode:Token.mode ->
  Common.Source_file.t ->
  (Token.t list, Common.Diagnostic.t list) result
