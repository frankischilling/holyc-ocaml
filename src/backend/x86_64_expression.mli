type word_type = I64 | U64
type error = { code : string; message : string; span : Common.Span.t option }
type t

val hard_max_stack_bytes : int
(** Maximum hosted spill frame size: 4088 bytes. *)

val validate_limits :
  max_ir_instructions:int -> max_code_bytes:int -> (unit, error list) result
(** Limits must be positive and at most 100,000 IR instructions and 16 MiB of
    encoded code. No graph traversal or code allocation is performed. *)

val validate_stack_limit : max_stack_bytes:int -> (unit, error list) result
(** Stack spill limits may range from zero through [hard_max_stack_bytes]. Zero
    disables spills. Invalid configuration is reported as HCBACK0001 without a
    source span. *)

val compile :
  ?max_stack_bytes:int ->
  max_ir_instructions:int ->
  max_code_bytes:int ->
  Ir.X87_stack.t ->
  (t, error list) result
(** Preflight one entry block without edges, pointer-free internal I64/U64
    values, zero instruction flags, and an exact return-value/return suffix.
    Supported producers are integer immediates, unary minus, complement,
    add/subtract/multiply/and/or/xor, left/right shifts, the six comparisons,
    logical NOT, and eager logical AND/OR/XOR values. Comparisons return
    internal I64 zero or one; ordered conditions consume both operand
    computation classes. Logical NOT returns zero or one in its operand's
    forwarded computation class, including U64. Shifts use the promoted
    computation class of both operands; right shift is arithmetic for I64 and
    logical for U64, while x86-64 masks the CL count to six bits. Binary logical
    operations independently normalize both complete operands and return I64.
    Internal I64/U64 word views with [IC_HOLYC_TYPECAST], integer payload zero
    and no flags preserve the bits and select the target computation class.
    These views support the cumulative unsigned class of ordinary lowered
    comparison chains and the internal [I64i]/[U64i] source spellings. Casts
    whose immediate source operand is parenthesized carry payload one and remain
    unsupported, as do broader conversions. All uses must follow unique
    definitions. Register allocation uses only [X86_64_encoder.registers],
    reserves RCX only while emitting a variable-count shift, reuses registers
    and eight-byte stack slots after final use, and spills only after the
    complete IR has passed preflight. A nonzero spill frame is 8 modulo 16, at
    most 4088 bytes, and is allocated by a fixed seven-byte prologue and
    released immediately before RET. [max_stack_bytes] defaults to 4088; zero
    retains a register-only gate. Code size includes all spill traffic and frame
    instructions and is checked before encoded bytes are allocated.

    Error codes are HCBACK0001 (configuration or IR instruction limit),
    HCBACK0002 (unsupported domain), HCBACK0003 (malformed IR or type
    relationship), HCBACK0004 (spill-frame/register pressure), and HCBACK0005
    (encoded code budget). *)

val code : t -> string
(** Return a fresh copy; mutation through [Bytes.unsafe_of_string] cannot change
    this compiled value or any other result of [code]. *)

val value_type : t -> word_type
val ir_instructions : t -> int
val machine_instructions : t -> int
val register_peak : t -> int

val frame_bytes : t -> int
(** Actual fixed spill frame size, or zero for a register-only image. *)

val windows_unwind_info : t -> string
(** Return a fresh copy of conventional Windows x64 UNWIND_INFO for the fixed
    stack-allocation prologue. Register-only images return the empty string. *)
