type t

val create :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  receipt:Frontend.Parser.completed_call ->
  selected:Function_resolution.resolved_declaration ->
  arguments:Function_type_resolution.resolved_function ->
  emission_snapshot:Function_record_phase.snapshot ->
  emission:Function_record_classification.classified_declaration ->
  (t, string) result
(** Check separate original call metadata. Runtime authority additionally
    requires registration by the owning task at the original call events. *)

val source : t -> Frontend.Ast.call_expression option
val receipt : t -> Frontend.Parser.completed_call option
val selected : t -> Function_resolution.resolved_declaration
val arguments : t -> Function_type_resolution.resolved_function
val emission : t -> Function_record_classification.classified_declaration
val emission_header : t -> Function_type_resolution.resolved_function
val emission_fixed_count : t -> int
val owns_table : t -> Symbol_table.t -> bool
val owns_namespace : t -> Declaration_collection.namespace -> bool

val create_implicit :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  receipt:Frontend.Parser.implicit_output_selection ->
  selected:Function_resolution.resolved_declaration ->
  arguments:Function_type_resolution.resolved_function ->
  emission_snapshot:Function_record_phase.snapshot ->
  emission:Function_record_classification.classified_declaration ->
  (t, string) result
(** Check separate original call metadata. Runtime authority additionally
    requires registration by the owning task at the original call events. *)

val implicit_receipt : t -> Frontend.Parser.implicit_output_selection option
val implicit_source : t -> Frontend.Ast.implicit_output_statement option
val command_start : t -> Frontend.Parser.command_start
