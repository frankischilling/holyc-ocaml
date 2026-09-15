type prepared_address

val prepare_fragment_initializer :
  Initializer_fragment_destination.t ->
  (prepared_address, Instruction_sequence.error list) result

val strides : prepared_address -> int64 list
(** Retain the complete declared stride sequence for ordinary reached addresses.
*)

val prepare_initializer :
  globals:Integer_globals.t ->
  Sema.Function_call_expression_result.top_level_root_result ->
  (prepared_address, Instruction_sequence.error list) result
(** Accept an exact scalar initializer root or an original scalar-store array
    leaf. Array destinations come from their checked layout; string-copy leaves
    cannot be emitted as scheduled stores. *)

type t

val prepare_static_initializer :
  globals:Integer_globals.t ->
  Integer_globals.static_slot ->
  Sema.Function_call_expression_result.initializer_result ->
  (prepared_address, Instruction_sequence.error list) result

val prepare :
  ?frame:Sema.Function_frame_layout.function_layout ->
  globals:Integer_globals.t ->
  Sema.Function_call_expression_result.expression_result ->
  (prepared_address option, Instruction_sequence.error list) result
(** Check a source-visible module global against the exact program object and
    its retained type, rank, publication and source occurrence. Static locals
    additionally require their exact declaring [frame] and retained binding. *)

val lower_prepared :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  prepared_address ->
  (t, Instruction_sequence.error list) result
(** Emit the canonical storage producer followed, for each initializer array
    rank, by a pointer-typed stride immediate, an internal-I64 coordinate,
    pointer-typed multiplication and pointer-typed addition. All dimensions are
    consumed, including zero coordinates. Scalar and ordinary reached addresses
    retain their single-producer prefix. *)

val sequence : t -> Instruction_sequence.t
val result_value : t -> Instruction_sequence.Value_id.t
val result_type : t -> Sema.Type.t
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_value_id : t -> Instruction_sequence.Value_id.t
