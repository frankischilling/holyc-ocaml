val pointer_depth : Frontend.Ast.pointer_layer list -> (int, string) result
(** Validate consecutive original pointer children and the native depth limit.
*)

val builtin :
  Frontend.Ast.type_specifier ->
  Frontend.Ast.pointer_layer list ->
  (Type_reference.t, string) result
(** Check the original primitive/internal type and pointer children without
    looking up a name, constructing replacement syntax or admitting storage.
    Named aggregate types require separate selected type evidence. *)
