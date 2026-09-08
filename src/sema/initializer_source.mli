type leaf
type tree = Scalar of leaf | Braced of tree list | Unbraced of tree list
type t

val create : Frontend.Ast.initial_value -> t
(** Retain the complete immutable parser tree and mint its ordered leaf
    identities once, at declaration type collection. No expressions are
    synthesized for strings or missing elements. *)

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
val leaf_identifiers : leaf -> (string * Symbol.origin) list
val origin_of_location : Frontend.Ast.location -> Symbol.origin
