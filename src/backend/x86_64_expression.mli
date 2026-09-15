type word_type = I64 | U64
type error = { code : string; message : string; span : Common.Span.t option }
type t

val validate_limits :
  max_ir_instructions:int -> max_code_bytes:int -> (unit, error list) result
(** Limits must be positive and at most 100,000 IR instructions and 16 MiB of
    encoded code. No graph traversal or code allocation is performed. *)

val compile :
  max_ir_instructions:int ->
  max_code_bytes:int ->
  Ir.X87_stack.t ->
  (t, error list) result
(** Preflight one entry block without edges, pointer-free internal I64/U64
    values, zero instruction flags, and an exact return-value/return suffix.
    Supported producers are integer immediates, unary minus, complement, and
    add/subtract/multiply/and/or/xor. All uses must follow unique definitions.
    Register allocation uses only [X86_64_encoder.registers], with reuse after
    the final use and rejection when a spill would be needed. Code is emitted
    only after graph, type, register-pressure and byte-budget validation.

    Error codes are HCBACK0001 (configuration or IR instruction limit),
    HCBACK0002 (unsupported domain), HCBACK0003 (malformed IR or type
    relationship), HCBACK0004 (register pressure), and HCBACK0005 (encoded code
    budget). *)

val code : t -> string
(** Return a fresh copy; mutation through [Bytes.unsafe_of_string] cannot change
    this compiled value or any other result of [code]. *)

val value_type : t -> word_type
val ir_instructions : t -> int
val machine_instructions : t -> int
val register_peak : t -> int
