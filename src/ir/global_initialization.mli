type phase = Compile_initializer | Load_initializer
type region
type t

type region_description = {
  root : Sema.Function_call_expression_result.top_level_root_result;
  first : Instruction_sequence.Instruction_id.t;
  last : Instruction_sequence.Instruction_id.t;
}

val create :
  span:Common.Span.t ->
  globals:Integer_globals.t ->
  entry:X87_stack.t ->
  region_description list ->
  (t, Common.Diagnostic.t list) result
(** Check complete, ordered, nonoverlapping declaration regions in the exact
    entry graph. Each region starts with its destination address and ends with
    its canonical initializer store and expression boundary. Entry instruction
    IDs must increase in physical order. Operands and call scopes are closed
    within each region; ordinary expressions cannot supply initializer values.
*)

val matches : t -> globals:Integer_globals.t -> entry:X87_stack.t -> bool
val regions : t -> region list
val prepared_steps : t -> int
val find : t -> Instruction_sequence.Instruction_id.t -> region option
val root : region -> Sema.Function_call_expression_result.top_level_root_result
val describe : region -> region_description
val symbol : region -> Sema.Symbol.t
val phase : region -> phase
val phase_name : phase -> string
val human : t -> string
