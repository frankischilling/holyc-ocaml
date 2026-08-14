type resolution = Module_binding_environment.resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type global
type t
type cursor

val create :
  table:Symbol_table.t ->
  environment:Outer_environment.t ->
  expressions:Module_expression_binding.t ->
  globals:Global_resolution.t ->
  (t, string) result
(** Validate the common module and outer lookup state used by source-positioned
    global declarator expressions. *)

val table : t -> Symbol_table.t
val environment : t -> Outer_environment.t
val expressions : t -> Module_expression_binding.t
val source_globals : t -> Global_resolution.t
val globals : t -> global list
val owns_table : t -> Symbol_table.t -> bool
val global_record : global -> Global_resolution.global_record
val global_publication : global -> Module_expression_binding.publication
val initial_cursor : t -> cursor

val publish_before : cursor -> global -> (cursor, string) result
(** Publish every module record strictly before the selected global. *)

val publish_through : cursor -> global -> (cursor, string) result
(** Publish every module record through the selected global. *)

val resolve : cursor -> string -> resolution option
(** Resolve a spelling against the visible module prefix, then the complete
    outer table chain. *)
