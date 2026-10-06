type t

val create :
  ?callback:
    Frontend.Parser.completed_callback_signature
    * Function_type_resolution.function_pointer ->
  ?selected_aggregate:Function_type_resolution.selected_aggregate_resolver ->
  table:Symbol_table.t ->
  header:Function_collection.collected_function ->
  allocation:Compiler_record.static_allocation ->
  selection:Frontend.Parser.reference_selection ->
  unit ->
  (t, string) result

val allocation : t -> Compiler_record.static_allocation
val symbol : t -> Symbol.t
val type_ : t -> Type.t
val storage_type : t -> Type.t
val return_reference : t -> Type_reference.t option
val callback_pointer : t -> Function_type_resolution.function_pointer option
val dimensions : t -> int64 list

val owns_identifier : t -> Frontend.Ast.identifier -> bool
(** The exact identifier whose original token selected this private allocation.
    Another occurrence selecting the same local retains its own receipt. *)
