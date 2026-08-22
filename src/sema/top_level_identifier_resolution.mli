type module_value =
  | Global_value of {
      global : Global_type_resolution.global;
      value : Function_call_resolution.identifier_value;
    }
  | Direct_function_value of {
      declaration : Function_resolution.resolved_declaration;
      value : Function_call_resolution.identifier_value;
    }
  | Aggregate_offset_base of Module_expression_binding.publication

type resolution =
  | Module_value of module_value
  | Outer_type_required of Outer_environment.binding

type leaf
type t
type error_kind = Invalid_input of string
type error

val make_leaf :
  node:Top_level_expression_tree.expression_node ->
  occurrence:Top_level_outer_expression_binding.occurrence ->
  resolution:resolution ->
  (leaf, error) result

val create :
  table:Symbol_table.t ->
  source:Top_level_expression_tree.t ->
  leaf list ->
  (t, error) result
(** Freeze one checked classification for every bound identifier leaf in an
    executable top-level expression tree. Module globals and functions retain
    source-derived value facts. Aggregate records remain offset bases. Outer
    records stay explicitly unavailable until their snapshots carry types. *)

val owns_table : t -> Symbol_table.t -> bool
val source : t -> Top_level_expression_tree.t
val leaves : t -> leaf list

val find_leaf :
  t -> Top_level_outer_expression_binding.occurrence -> leaf option

val leaf_node : leaf -> Top_level_expression_tree.expression_node
val leaf_occurrence : leaf -> Top_level_outer_expression_binding.occurrence
val leaf_resolution : leaf -> resolution
val module_value_type : module_value -> Type.t option

val module_value_shape :
  module_value -> Function_call_resolution.identifier_value_shape option

val module_value_array_rank : module_value -> int option
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
