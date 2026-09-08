type t
type lowering_result = Lowered of t | Unsupported_call

val reference_commit : string

val lower :
  ?frame:Sema.Function_frame_layout.function_layout ->
  ?globals:Integer_globals.t ->
  ?lower_call:Expression_lowering.call_lowerer ->
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  target:Sema.Function_call_target_classification.t ->
  Sema.Function_call_expression_result.expression_result ->
  (lowering_result, Instruction_sequence.error list) result
(** Lower one checked direct executable, extern-slot, import, or extern call
    whose provided fixed and variadic arguments are supported by
    [Expression_lowering]. [lower_call] also composes nested call arguments
    through that planner while preserving right-to-left argument execution. *)

val lower_top_level :
  ?frame:Sema.Function_frame_layout.function_layout ->
  ?globals:Integer_globals.t ->
  ?lower_call:Expression_lowering.call_lowerer ->
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  target:Sema.Top_level_function_call_target_classification.t ->
  Sema.Function_call_expression_result.expression_result ->
  (lowering_result, Instruction_sequence.error list) result
(** Lower one checked executable top-level direct call through the same call
    composer used for function-scope calls. *)

val sequence : t -> Instruction_sequence.t

val lower_implicit_output :
  ?frame:Sema.Function_frame_layout.function_layout ->
  ?globals:Integer_globals.t ->
  ?lower_call:Expression_lowering.call_lowerer ->
  records:Sema.Function_record_classification.t ->
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  Sema.Implicit_output_argument_binding.bound_output ->
  (lowering_result, Instruction_sequence.error list) result

val lower_top_level_implicit_output :
  ?frame:Sema.Function_frame_layout.function_layout ->
  ?globals:Integer_globals.t ->
  ?lower_call:Expression_lowering.call_lowerer ->
  records:Sema.Function_record_classification.t ->
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  Sema.Top_level_implicit_output_argument_binding.bound_output ->
  (lowering_result, Instruction_sequence.error list) result
(** Compose checked provided output arguments through the same canonical call
    builder. Selected declaration records remain exact; deferred outer targets,
    defaults and unsupported conversions expose no partial call. *)

val runtime_call : t -> Runtime_call_context.description
(** Unsealed source and instruction identities, checked against the complete
    graph by [Runtime_call_context.create]. Statement lowering supplies the
    implicit discard identity after appending its expression boundary. *)

val result_value : t -> Instruction_sequence.Value_id.t
val result_type : t -> Sema.Type.t
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_value_id : t -> Instruction_sequence.Value_id.t
val human : t -> string
