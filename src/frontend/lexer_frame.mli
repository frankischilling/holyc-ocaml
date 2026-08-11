type kind = Root | Included
type t

val root : mode:Token.mode -> Common.Source_file.t -> t

val push_include :
  caller:t ->
  source:Common.Source_file.t ->
  include_origin:Common.Span.t ->
  include_spelling:string ->
  t

val kind : t -> kind
val source : t -> Common.Source_file.t
val source_id : t -> Common.Source_id.t
val canonical_path : t -> string
val display_path : t -> string
val lexer : t -> Lexer.t
val caller : t -> t option
val include_origin : t -> Common.Span.t option
val include_spelling : t -> string option
val source_depth : t -> int
val current_offset : t -> int
val current_position : t -> (Common.Source_file.position, string) result
val include_stack : t -> Common.Diagnostic.related list
val find_active_path : t -> string -> t option
