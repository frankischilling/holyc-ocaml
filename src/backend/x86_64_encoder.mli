type register = Rax | Rcx | Rdx | R8 | R9 | R10 | R11
type unary = Neg | Not
type binary = Add | Sub | Imul | And | Or | Xor
type shift = Shl | Shr | Sar
type stack_slot
type stack_frame

type condition =
  | E
  | NE
  | L
  | GE
  | G
  | LE
  | B
  | AE
  | A
  | BE
      (** [E]/[NE] test equality/inequality. [L]/[GE]/[G]/[LE] use signed order;
          [B]/[AE]/[A]/[BE] use unsigned order. Conditions read the current
          processor flags; the encoder does not infer operand types. *)

type instruction =
  | Mov_imm64 of register * int64
  | Mov of register * register
  | Load_stack of register * stack_slot
  | Store_stack of stack_slot * register
  | Alloc_stack of stack_frame
  | Free_stack of stack_frame
  | Unary of unary * register
  | Binary of binary * register * register
  | Shift_cl of shift * register
      (** Shift the full-width destination by CL. [Shl] is left shift, [Shr]
          logical right shift, and [Sar] arithmetic right shift. The count is
          read from RCX implicitly; the destination may be any public register.
      *)
  | Cmp of register * register
      (** [Cmp (left, right)] sets flags for the full-width subtraction
          [left - right] without changing either register. *)
  | Test of register
      (** [Test register] sets flags from the full-width value using
          [TEST register,register], without changing the register. *)
  | Setcc of condition * register
      (** Write zero or one to the destination's low byte, preserving the higher
          bits and flags. Only R8b through R11b need a REX prefix. *)
  | Movzx8 of register * register
      (** Destination then source: zero-extend the source's low byte into the
          full 64-bit destination, preserving flags. The registers may alias. *)
  | Ret
      (** Apart from [Cmp]'s left/right operands, register pairs are destination
          then source. Arithmetic and bitwise operations use the full 64-bit
          registers; [Imul] retains the low 64 product bits. Spill instructions
          address their validated fixed RSP frame explicitly; the only implicit
          stack access is the return address read by [Ret]. *)

val registers : register list
(** Deterministic allocation order: RAX, RCX, RDX, R8, R9, R10, R11. *)

val stack_slot : offset:int -> (stack_slot, string) result
(** Construct an eight-byte spill slot addressed from RSP. Offsets are aligned
    multiples of eight from zero through 4080. *)

val stack_frame : bytes:int -> (stack_frame, string) result
(** Construct a fixed spill frame. Sizes are 8..4088 bytes and 8 modulo 16,
    keeping the generated body aligned after the host call pushes its return
    address. *)

val max_code_bytes : int
(** Hard limit of 16 MiB for one encoded instruction sequence, further bounded
    by [Sys.max_string_length] on the compiler host. *)

val size : instruction -> int
(** Exact encoded byte count, including any REX prefix and immediate. [Shift_cl]
    is always three bytes (REX.W with REX.B when needed, D3, ModR/M). [Setcc]
    uses three bytes for AL/CL/DL and four for R8b through R11b. Stack
    loads/stores always use an eight-byte fixed-disp32 SIB form; stack
    allocation/free always use seven-byte imm32 forms. *)

val encode : instruction -> string
(** Encode one instruction into a fresh string using the pinned opcode facts. *)

val encode_all :
  max_code_bytes:int -> instruction list -> (string, string) result
(** Validate the positive limit and exact total size before allocating a code
    buffer. A limit above [max_code_bytes] is rejected. *)
