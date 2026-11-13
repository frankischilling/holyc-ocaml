(** Source-backed admission for scalar F64 and authenticated pure linked
    regions. Unproved normalization and extended-precision links reject. *)
type load = Bits | Zero | One | Signed_integer

type conversion = To_float | To_integer

type conversion_entry = private {
  conversion_instruction : Ir.Instruction_sequence.Instruction_id.t;
  conversion_result : Ir.Instruction_sequence.Value_id.t;
  conversion : conversion;
  deferred : bool;
  elided : bool;
      (** The source zero-to-I64 rewrite removed this exact integer-zero
          conversion; its input word already equals binary64 positive zero. *)
}

val conversion_type : conversion -> Sema.Type.t

type intrinsic_entry = private {
  intrinsic_instruction : Ir.Instruction_sequence.Instruction_id.t;
  intrinsic_input : Ir.Instruction_sequence.Value_id.t;
  intrinsic_result : Ir.Instruction_sequence.Value_id.t;
  intrinsic_conversion : Ir.Numeric_intrinsic.conversion;
}

type operation =
  | Normalized of Ir.Numeric_normalization.t
  | Convert_to_float
  | Literal_bits of int64
      (** The source-authenticated unary minus of an immediate F64 positive zero
          in a comparison region. Other constant folds remain closed. *)
  | Negate
  | Add
  | Subtract
  | Multiply
  | Divide
  | Copy_left
  | Copy_right
  | Alias_left
  | Alias_right

type orientation = Left_first | Right_first | Bit_copy

type entry = private {
  instruction_id : Ir.Instruction_sequence.Instruction_id.t;
  result_id : Ir.Instruction_sequence.Value_id.t;
  operands : Ir.Instruction_sequence.Value_id.t list;
  loads : load list;
  operation : operation;
  live_before : Ir.Instruction_sequence.Value_id.t list;
  live_after : Ir.Instruction_sequence.Value_id.t list;
  peak_depth : int;
  orientation : orientation;
  live_steps : Ir.Instruction_sequence.Value_id.t list list;
  result_conversion : conversion option;
}

type comparison_kind =
  | Bitwise
  | Signed_word
  | Ordered
      (** [Bitwise] is pinned F64 equality. [Signed_word] is the exact
          public-F64 direct-read versus positive-zero rewrite for original < or
          >=. [Ordered] consumes two x87 operands with the source-selected
          unordered predicate. *)

type comparison_link = Unlinked | Linked_left | Linked_right

type comparison_context =
  | Comparison_value
  | Comparison_branch of {
      role : Sema.Function_call_resolution.condition_role;
      negated : bool;
      consumer : Ir.Instruction_sequence.Instruction_id.t;
    }

type comparison_entry = private {
  comparison_instruction : Ir.Instruction_sequence.Instruction_id.t;
  comparison_result : Ir.Instruction_sequence.Value_id.t;
  comparison_operands : Ir.Instruction_sequence.Value_id.t list;
  comparison_loads : load list;
  comparison_kind : comparison_kind;
  comparison_context : comparison_context;
  comparison_opcode : Ir.Opcode.t;
  comparison_predicate : X86_64_encoder.condition;
  comparison_negate_result : bool;
  comparison_orientation : orientation;
  comparison_link : comparison_link;
  comparison_conversion_input : Ir.Instruction_sequence.Value_id.t option;
      (** Same-instruction CN_A1/CN_A2 FILD input, distinct from an extended
          owner transferred by an earlier instruction. *)
  comparison_live_before : Ir.Instruction_sequence.Value_id.t list;
  comparison_live_steps : Ir.Instruction_sequence.Value_id.t list list;
  comparison_peak_depth : int;
}

type emission = {
  arithmetic_entries : entry list;
  conversion_entries : conversion_entry list;
  intrinsic_entries : intrinsic_entry list;
  comparison_entries : comparison_entry list;
}

type error = { message : string; span : Common.Span.t option }
type t

val is_f64 : Sema.Type.t -> bool

type source = {
  runtime : Ir.Runtime_call_context.t;
  body : Ir.Function_body.t;
  frame : Sema.Function_frame_layout.function_layout;
}

val prepare :
  ?source:source ->
  ?runtime:Ir.Runtime_call_context.t ->
  ?owner:Ir.Runtime_call_context.owner ->
  Ir.Block_graph.t ->
  (t, error list) result

val find :
  t ->
  graph:Ir.Block_graph.t ->
  Ir.Instruction_sequence.description ->
  entry option

val peak_depth : t -> int

val validated_entries : t -> graph:Ir.Block_graph.t -> entry list option
(** Validate the complete frozen graph and original source region evidence once,
    then return the entries for immediate synchronous emission. *)

val validated_conversions :
  t -> graph:Ir.Block_graph.t -> conversion_entry list option

val validated_intrinsics :
  t -> graph:Ir.Block_graph.t -> intrinsic_entry list option

val validated_comparisons :
  t -> graph:Ir.Block_graph.t -> comparison_entry list option

val validated_emission : t -> graph:Ir.Block_graph.t -> emission option
(** Validate all arithmetic, conversion, intrinsic and terminal comparison
    receipts once for immediate synchronous emission. Comparison orientation
    denotes physical load order; [comparison_live_steps] lists ST0 first.
    Ordered comparisons end with an empty x87 stack and an I64 discriminator. *)
