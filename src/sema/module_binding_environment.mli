type resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type point
type t
type cursor

val create :
  table:Symbol_table.t ->
  environment:Outer_environment.t ->
  expressions:Module_expression_binding.t ->
  (t, string) result
(** Validate the module publication stream and the complete mode-specific outer
    table chain used by expression binders. *)

val table : t -> Symbol_table.t
val environment : t -> Outer_environment.t
val expressions : t -> Module_expression_binding.t
val owns_table : t -> Symbol_table.t -> bool
val find_point : t -> Symbol.t -> point option
val point_publication : point -> Module_expression_binding.publication

val initial_cursor : t -> cursor
(** Cursors advance monotonically. A point from another environment or one the
    cursor has already passed is rejected. *)

val publish_before : cursor -> point -> (cursor, string) result
(** Publish every module record strictly before the selected point. *)

val publish_through : cursor -> point -> (cursor, string) result
(** Publish every module record through the selected point. *)

val resolve : cursor -> string -> resolution option
(** Resolve a spelling against the visible module prefix, then the complete
    outer table chain. *)
