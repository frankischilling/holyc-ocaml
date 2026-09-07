type t
type lowering_result = Lowered of t | Unsupported_expression

type call_lowerer =
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  Sema.Function_call_expression_result.expression_result ->
  (Instruction_sequence.t option, Instruction_sequence.error list) result
(** A provider for exact typed calls. It must validate target ownership and
    return a complete canonical call sequence at the supplied identities, or
    [None] for unsupported calls. The expression emitter checks consecutive
    identities and the final call-end type, symbol and source span. *)

val reference_commit : string

val lower_typed_result :
  ?frame:Sema.Function_frame_layout.function_layout ->
  ?globals:Integer_globals.t ->
  ?lower_call:call_lowerer ->
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  Sema.Function_call_expression_result.expression_result ->
  (lowering_result, Instruction_sequence.error list) result
(** Lower an integer, character, [F64], ordinary current-position, completed
    direct or standalone function-body or executable top-level aggregate offset,
    or source-local [defined] semantic expression tree. A known [defined] result
    emits an internal [I64] [IC_IMM_I64] with the complete expression span;
    unresolved nonlocal names remain unsupported. [$$] emits a checked
    zero-operand [IC_RIP] address producer, while concrete address selection
    remains backend work. Integer trees accept the checked integer binary
    operations. Integer comparison chains share each middle operand and combine
    adjacent comparisons with eager [IC_AND_AND]. The cumulative unsigned
    comparison class is carried by an internal [U64] view where needed. Grouping
    and tighter right operands retain their source precedence; multiple pending
    comparison reductions and floating chains are unsupported. Pure [F64] trees
    and mixed integer/[F64] trees accept shifts, multiplication, division,
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
    pointer trees. An exact checked direct [address-of Function] emits a
    canonical-symbol [IC_IMM_I64] in resolved JIT mode, a canonical-symbol
    [IC_ABS_ADDR] in resolved AOT mode, or an [IC_IMM_I64] address-slot producer
    followed by [IC_DEREF] for an unresolved JIT extern. The same atomic node is
    available in function-body and executable top-level trees, and direct-call
    composition marks only its final producer for pushing. A postfix cast emits
    [IC_HOLYC_TYPECAST] with the full cast span and pinned [was_paren] payload.
    The module owns source-order traversal, TempleOS's immediate
    address/dereference cancellation, and consecutive identity allocation. With
    [frame], scalar I64/U64 bound identifiers load their exact checked slots,
    and simple assignments store through the checked destination address without
    reading its old contents. [globals] enables the same scalar loads/stores for
    exact module-bound globals in function or top-level expressions, preserving
    their JIT/AOT symbol-backed address intent. With [frame], other pointer
    operations remain unsupported. Without [frame], pointer-tree lowering keeps
    its existing domain; pointer execution remains outside the bounded program
    VM. [lower_call] composes calls as expression nodes and applies retained
    result conversion only to the final call-end producer. Expressions outside
    the implemented tree shapes return [Unsupported_expression] without
    returning a partial sequence. *)

val lower_initializer :
  frame:Sema.Function_frame_layout.function_layout ->
  ?globals:Integer_globals.t ->
  ?lower_call:call_lowerer ->
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  Sema.Function_call_expression_result.initializer_result ->
  (lowering_result, Instruction_sequence.error list) result
(** Store a checked scalar I64/U64 initializer into its exact automatic frame
    slot, preserving the source value and destination class. *)

val lower_global_initializer :
  globals:Integer_globals.t ->
  ?lower_call:call_lowerer ->
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  Sema.Function_call_expression_result.top_level_root_result ->
  (lowering_result, Instruction_sequence.error list) result
(** Store an exact declaration-owned initializer through its checked global
    destination. This emits the store; scheduling and constant preparation
    remain the program initialization context's responsibility. *)

val sequence : t -> Instruction_sequence.t
val result_value : t -> Instruction_sequence.Value_id.t
val result_type : t -> Sema.Type.t
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_value_id : t -> Instruction_sequence.Value_id.t

val human : t -> string
(** Render the versioned deterministic expression-lowering form. *)
