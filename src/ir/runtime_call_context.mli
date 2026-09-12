type source =
  | Function_call of Sema.Function_call_target_classification.t
  | Top_level_call of Sema.Top_level_function_call_target_classification.t
  | Function_output of Sema.Implicit_output_argument_binding.bound_output
  | Top_level_output of
      Sema.Top_level_implicit_output_argument_binding.bound_output

type description = {
  source : source;
  first : Instruction_sequence.Instruction_id.t;
  last : Instruction_sequence.Instruction_id.t;
  discard : Instruction_sequence.Instruction_id.t option;
}

val original_phase : source -> Sema.Function_call_phase.t option

val cleanup_slot_count :
  source -> fixed_count:int -> variadic_count:int64 -> variadic:bool -> int64

type provider = Print | Put_chars | Stream_print
type owner = Entry | Function of Function_body.t
type argument_role = Fixed of int | Variadic_count | Variadic of int
type argument
type call
type t

val original_phases : t -> Sema.Function_call_phase.t list

val create :
  records:Sema.Function_record_classification.t ->
  function_sources:Sema.Function_call_expression_result.t ->
  top_level:Sema.Function_call_expression_result.top_level_t ->
  initialization:Global_initialization.t ->
  entry:X87_stack.t ->
  entry_calls:description list ->
  functions:(Function_body.t * description list) list ->
  (t, Common.Diagnostic.t list) result
(** Seal checked semantic call sources against the exact completed graphs.
    Declaration snapshots, complete call scopes, pushed argument producers,
    hidden counts and implicit statement discards are checked before
    publication. Scheduled initializer calls must belong to that exact checked
    initializer expression, including nested argument expressions; implicit
    output statements cannot belong to initializer regions. Matching spellings
    or table-local symbol IDs do not establish ownership. *)

val matches :
  t ->
  entry:X87_stack.t ->
  initialization:Global_initialization.t option ->
  functions:Function_body.t list ->
  bool

val find_start :
  t -> owner:owner -> Instruction_sequence.Instruction_id.t -> call option

val is_implicit_discard :
  t -> owner:owner -> Instruction_sequence.Instruction_id.t -> bool

val is_prepared_default :
  t -> owner:owner -> Instruction_sequence.Instruction_id.t -> bool

val provider : call -> provider option
val symbol : call -> Sema.Symbol.t
val return_type : call -> Sema.Type.t
val call_opcode : call -> Opcode.t
val cleanup_opcode : call -> Opcode.t
val cleanup_bytes : call -> int64
val first : call -> Instruction_sequence.Instruction_id.t
val call_instruction : call -> Instruction_sequence.Instruction_id.t
val cleanup_instruction : call -> Instruction_sequence.Instruction_id.t
val last : call -> Instruction_sequence.Instruction_id.t
val result_value : call -> Instruction_sequence.Value_id.t

val arguments : call -> argument list
(** Argument producers in their final physical push order. Fixed and variadic
    positions retain source order; the hidden count occupies its own ABI slot.
*)

val argument_role : argument -> argument_role
val argument_producer : argument -> Instruction_sequence.Instruction_id.t
val argument_value : argument -> Instruction_sequence.Value_id.t
val argument_source_type : argument -> Sema.Type.t
val argument_target_type : argument -> Sema.Type.t
val variadic_count : call -> int64 option
val declaration : call -> Sema.Function_resolution.resolved_declaration
val header : call -> Sema.Function_type_resolution.resolved_function
val retained_function : call -> Retained_function.t option
val compilation_mode : t -> Sema.Function_resolution.compilation_mode

val entry_item_index : t -> call -> int option
(** Original containing item for an entry call, including checked static
    initializer regions. Function-body calls inherit the invoking entry's
    publication boundary. Foreign calls have no item in this context. *)

val dimension_dependencies :
  t -> Sema.Compiler_record.runtime_dimension_proposal list

val owns_top_level :
  t -> Sema.Function_call_expression_result.top_level_t -> bool

val offset_dependencies : t -> Sema.Compiler_record.aggregate_offset list
