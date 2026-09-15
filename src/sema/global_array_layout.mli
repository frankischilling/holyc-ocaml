type dimension_input = {
  dimension : Global_dimension_binding.resolved_dimension;
  expression_origin : Symbol.origin option;
  expression : Frontend.Ast.expression option;
}

type global_input = {
  global : Global_dimension_binding.resolved_global;
  dimensions : dimension_input list;
}

type layout
type t

val layout :
  table:Symbol_table.t ->
  bindings:Global_dimension_binding.t ->
  global_input list ->
  (t, string) result
(** Evaluate source-ordered fixed dimensions from the exact retained binding
    batch. Empty, nonpositive and dependent extents remain explicit boundaries.
    Only dimension-sized metadata is allocated; no element image is created. *)

val owns_table : t -> Symbol_table.t -> bool
val owns_bindings : t -> Global_dimension_binding.t -> bool
val bindings : t -> Global_dimension_binding.t
val layouts : t -> layout list

val find : t -> Global_resolution.global_record -> layout option
(** Lookup requires the original physical declaration record. *)

val record : layout -> Global_resolution.global_record
val source : layout -> Global_dimension_binding.resolved_global
val dimension_inputs : layout -> dimension_input list
val dimensions : layout -> int64 list
val extent : layout -> Compiler_record.global_extent

val element_count : layout -> int64
(** Scalars retain an empty dimension list and an element count of one. *)
