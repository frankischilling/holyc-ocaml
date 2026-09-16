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

type selected_aggregate

type selected_source =
  | Function_return of Frontend.Parser.function_publication
  | Function_parameter of Frontend.Parser.function_parameter_publication

val select_aggregate :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  source:selected_source ->
  Declaration_collection.publication ->
  (selected_aggregate, string) result
(** Mint one proof only during the exact parser owner's original declaration or
    parameter callback. The function/parameter receipt supplies its own frozen
    named selection, type occurrence and pointer children; callers cannot
    provide replacement children. The selected frontend entry must map to the
    exact aggregate semantic publication in this table/namespace. The proof
    stores that publication's canonical aggregate identity, so an exact
    forward/definition completion keeps one physical semantic symbol while a
    later shadow does not. The proof remains immutable after the callback and
    grants no layout or runtime authority. *)

val selected :
  selected_aggregate ->
  Frontend.Ast.type_specifier ->
  Frontend.Ast.pointer_layer list ->
  (Type_reference.t, string) result
(** Resolve only the exact retained named type occurrence and its original
    pointer-layer children. Direct aggregate pointers are admitted; aggregate
    values still require separate layout/ABI authority and are rejected here. *)

val validate_selected_aggregate :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  selected_aggregate ->
  (unit, string) result
(** Check that an already minted proof's canonical aggregate identity belongs to
    the consuming function's exact semantic table and module namespace. This is
    required again at consumers because the same physical parser AST can be
    analyzed under a distinct semantic namespace. *)
