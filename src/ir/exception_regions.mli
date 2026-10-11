type scope = Sema.Exception_region.t * Sema.Exception_region.arm

type block_context = {
  block : Instruction_sequence.Block_id.t;
  scopes : scope list;
}

type description = {
  source : Sema.Exception_region.t;
  enter : Instruction_sequence.description;
  leave : Instruction_sequence.description;
  early_leaves : Instruction_sequence.description list;
  handler_return : Instruction_sequence.description;
  catch_target : Instruction_sequence.Block_id.t;
  untry_target : Instruction_sequence.Block_id.t;
}

type region
type operation = Enter of region | Leave of region | Catch_finish of region
type t

val create :
  span:Common.Span.t ->
  graph:Block_graph.t ->
  blocks:block_context list ->
  description list ->
  (t, Common.Diagnostic.t list) result
(** Seal original zero-input NOP1 entry/leave markers and the catch RET against
    one immutable graph. Every block retains its source handler path. All
    ordinary edges must preserve that path after the original entry/leave
    transitions; a return cannot escape a catch or a still-active try. *)

val source : region -> Sema.Exception_region.t
val catch_target : region -> Instruction_sequence.Block_id.t
val untry_target : region -> Instruction_sequence.Block_id.t
val regions : t -> region list
val is_empty : t -> bool
val matches_graph : t -> Block_graph.t -> bool
val matches_records : t -> Sema.Function_record_classification.t -> bool

val matches_function : t -> Function_body.t option -> bool
(** Require each region's original checked definition; [None] is entry source. A
    raw or foreign function body cannot acquire a containing source owner. *)

val find : t -> Instruction_sequence.description -> operation option
(** A copied marker, another graph, or a matching instruction number grants no
    handler operation. The runtime must also retain this exact context as the
    active entry/function owner. *)
