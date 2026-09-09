type leaf
type tree = Scalar of leaf | Braced of tree list | Unbraced of tree list
type t
type pending

val create : Frontend.Ast.initial_value -> t
(** Legacy AST-only entry: retain the complete immutable parser tree and mint
    its ordered leaf identities at declaration type collection. No expressions
    are synthesized for strings or missing elements. *)

val begin_parser :
  Frontend.Parser.global_initializer_start -> (pending, string) result

val observe_parser_leaf :
  pending -> Frontend.Parser.completed_initializer_leaf -> (leaf, string) result

val parser_leaf :
  pending -> Frontend.Parser.completed_initializer_leaf -> (leaf, string) result

val complete_parser :
  pending -> Frontend.Parser.declaration_event -> (t, string) result
(** Observe each original synchronous parser boundary once. Completion requires
    the exact owner, equals location, scalar nodes, paths and complete ordered
    transcript. The final immutable tree reuses the already observed leaves.
    This is source evidence, independent of typing, layout and runtime effects.
    Failed validation leaves the pending transcript unchanged. *)

val source_ast : t -> Frontend.Ast.initial_value
val tree : t -> tree
val leaves : t -> leaf list
val origin : t -> Symbol.origin
val matches_ast : t -> Frontend.Ast.initial_value -> bool
val owns_leaf : t -> leaf -> bool
val leaf_index : leaf -> int
val leaf_path : leaf -> int list
val leaf_origin : leaf -> Symbol.origin
val leaf_expression_ast : leaf -> Frontend.Ast.expression

val leaf_parser_receipt :
  leaf -> Frontend.Parser.completed_initializer_leaf option

val leaf_identifiers : leaf -> (string * Symbol.origin) list
val origin_of_location : Frontend.Ast.location -> Symbol.origin
