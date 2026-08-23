type error_kind = Explicit_return
type error

val validate : Frontend.Ast.module_ -> (unit, error) result
(** Reject source constructs that require an active function when they occur
    under executable top-level statements. Function definitions are outside this
    traversal. *)

val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Sema.Symbol.origin
val error_message : error -> string
val error_to_string : error -> string
