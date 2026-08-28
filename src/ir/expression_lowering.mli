type t
type lowering_result = Lowered of t | Unsupported_expression

val reference_commit : string

val lower_typed_result :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  Sema.Function_call_expression_result.expression_result ->
  (lowering_result, Instruction_sequence.error list) result
(** Lower an integer, character, [F64], ordinary current-position, completed
    function-body or executable top-level aggregate offset, or source-local
    [defined] semantic expression tree. A known [defined] result emits an
    internal [I64] [IC_IMM_I64] with the complete expression span; unresolved
    nonlocal names remain unsupported. [$$] emits a checked zero-operand
    [IC_RIP] address producer, while concrete address selection remains backend
    work. Integer trees accept the checked integer binary operations; pure [F64]
    trees and mixed integer/[F64] trees accept shifts, multiplication, division,
    modulo, bitwise operations, addition, subtraction, the six comparisons, and
    the three logical binary operations. A mixed edge marks the retained integer
    producer with [ICF_RES_TO_F64] without folding its payload. A retained root
    conversion applies [ICF_RES_TO_F64] to an integer producer or
    [ICF_RES_TO_INT] to an [F64] producer; grouping and unary plus forward that
    request to the final retained instruction. Floating comparisons also carry
    [ICF_USE_F64], while floating logical operations remain unflagged; both
    retain the checked internal [I64] result. A completed aggregate offset emits
    an internal [I64] [IC_IMM_I64] from its retained final cumulative byte
    offset without repeating member lookup or layout. HolyC power accepts every
    checked integer/[F64] operand pair, marks each integer producer with
    [ICF_RES_TO_F64], and emits an unflagged [IC_POWER] with an internal [F64]
    result. Numeric prefixes and primitive postfix casts compose within their
    checked domain; address and dereference remain confined to integer and
    pointer trees. A postfix cast emits [IC_HOLYC_TYPECAST] with the full cast
    span and pinned [was_paren] payload. The module owns source-order traversal,
    TempleOS's immediate address/dereference cancellation, and consecutive
    identity allocation. Expressions outside the implemented tree shapes return
    [Unsupported_expression] without returning a partial sequence. *)

val sequence : t -> Instruction_sequence.t
val result_value : t -> Instruction_sequence.Value_id.t
val result_type : t -> Sema.Type.t
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_value_id : t -> Instruction_sequence.Value_id.t

val human : t -> string
(** Render the versioned deterministic expression-lowering form. *)
