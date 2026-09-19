type register = Rax | Rcx | Rdx | R8 | R9 | R10 | R11
type unary = Neg | Not
type binary = Add | Sub | Imul | And | Or | Xor
type shift = Shl | Shr | Sar
type status_abi = Windows_x64 | System_v_x64
type narrow_frame_width = Frame8 | Frame16 | Frame32
type frame_extension = Sign_extend | Zero_extend
type stack_slot
type stack_frame
type frame_slot
type arena_slot
type call_frame

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
  | Push_rbp
      (** Save the caller's RBP. RBP is intentionally not allocator-visible. *)
  | Mov_rbp_rsp
      (** Establish RBP from the current RSP with the fixed [MOV RBP,RSP] form.
      *)
  | Load_frame of register * frame_slot
      (** Load one qword from a validated fixed RBP-relative frame slot. *)
  | Store_frame of frame_slot * register
      (** Store one qword to a validated fixed RBP-relative frame slot. *)
  | Load_frame_narrow of
      register * frame_slot * narrow_frame_width * frame_extension
      (** Load an 8/16/32-bit RBP-relative scalar and sign- or zero-extend it to
          the complete 64-bit destination. *)
  | Store_frame_narrow of frame_slot * narrow_frame_width * register
      (** Store only the selected low 8/16/32 bits to an RBP-relative scalar.
          Adjacent bytes are not modified. *)
  | Load_arena of register * arena_slot
      (** Load one qword from the sealed R9-relative private data arena. *)
  | Store_arena of arena_slot * register
      (** Store one qword to the sealed R9-relative private data arena. *)
  | Load_arena_narrow of
      register * arena_slot * narrow_frame_width * frame_extension
      (** Load an 8/16/32-bit arena scalar and sign- or zero-extend it exactly
          as the corresponding RBP-relative scalar load. *)
  | Store_arena_narrow of arena_slot * narrow_frame_width * register
      (** Store only the selected low 8/16/32 bits to the private data arena. *)
  | Alloc_call_frame of call_frame
  | Free_call_frame of call_frame
      (** Allocate/free a fixed 16-byte-aligned callable frame while keeping RSP
          constant throughout the function body. *)
  | Call of int64
      (** Direct CALL with a signed rel32 displacement from the instruction end.
      *)
  | Pop_rbp  (** Restore the caller's RBP immediately before returning. *)
  | Unary of unary * register
  | Binary of binary * register * register
  | Shift_cl of shift * register
      (** Shift the full-width destination by CL. [Shl] is left shift, [Shr]
          logical right shift, and [Sar] arithmetic right shift. The count is
          read from RCX implicitly; the destination may be any public register.
      *)
  | Capture_status of status_abi
      (** Copy the ABI's first pointer argument into private R11. Windows x64
          reads RCX; System V x86-64 reads RDI. RDI is not a general allocator
          register. *)
  | Zero_edx
      (** Clear EDX with XOR EDX,EDX, which also clears the full RDX value. *)
  | Cqo  (** Sign-extend RAX into RDX:RAX before signed division. *)
  | Div_rcx
      (** Unsigned divide RDX:RAX by RCX. Quotient is RAX, remainder RDX. *)
  | Idiv_rcx
      (** Signed divide RDX:RAX by RCX. Quotient is RAX, remainder RDX. *)
  | Cmp_imm8 of register * int
      (** Compare a full-width register with a sign-extended signed imm8. *)
  | Jump of int64
  | Jump_equal of int64
  | Jump_not_equal of int64
      (** Signed rel32 displacement from the end of this instruction. *)
  | Store_status_kind of int
  | Store_status_site of int
      (** Private stores through R11 only. Kinds are 1..2 and sites 1..100000;
          both use the qword C7 imm32 form at displacements zero/eight. *)
  | Load_context of register * int
      (** Load one qword from the private R11 context at an aligned byte offset
          from zero through 72. Offset 72 is the immutable arena pointer; the
          generated instruction API cannot write that word. *)
  | Store_context of int * register
      (** Store one qword to the private R11 context at an aligned byte offset
          from zero through 64. *)
  | Store_context_imm of int * int
      (** Store a sign-extended imm32 qword to the private R11 context at an
          aligned byte offset from zero through 64. *)
  | Dec of register
      (** Decrement one full-width register with the qword FF /1 form. *)
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
          address their validated fixed RSP frame explicitly. [Call]/[Ret] write
          and read the return address; [Push_rbp]/[Pop_rbp] save and restore the
          frame pointer. Other storage operations use their explicit frame
          slots. *)

val registers : register list
(** Deterministic allocation order: RAX, RCX, RDX, R8, R9, R10, R11. *)

val stack_slot : offset:int -> (stack_slot, string) result
(** Construct an eight-byte spill slot addressed from RSP. Offsets are aligned
    multiples of eight from zero through 4080. *)

val stack_frame : bytes:int -> (stack_frame, string) result
(** Construct a fixed spill frame. Sizes are 8..4088 bytes and 8 modulo 16,
    keeping the generated body aligned after the host call pushes its return
    address. *)

val frame_slot : offset:int -> (frame_slot, string) result
(** Construct an aligned qword slot addressed from RBP with a signed disp32.
    This dedicated address form does not expose RBP as a general register. *)

val scalar_frame_slot : offset:int -> (frame_slot, string) result
(** Construct an RBP-relative scalar address with any signed disp32. Width and
    extension remain explicit in [Load_frame_narrow]/[Store_frame_narrow]. *)

val arena_slot : offset:int -> (arena_slot, string) result
(** Construct an R9-relative private-arena address. Offsets are nonnegative
    signed disp32 values and need not be aligned, so adjacent narrow scalar
    objects and initialization bytes remain representable. The constructor does
    not expose or synthesize an arena base address. *)

val call_frame : bytes:int -> (call_frame, string) result
(** Construct a fixed callable frame. Sizes are positive 16-byte multiples up to
    4080 bytes, so PUSH RBP plus one frame allocation never skips a 4 KiB stack
    guard page. Outgoing ABI home space, when required, is part of this fixed
    allocation. *)

val max_code_bytes : int
(** Hard limit of 16 MiB for one encoded instruction sequence, further bounded
    by [Sys.max_string_length] on the compiler host. *)

val size : instruction -> int
(** Exact encoded byte count, including any REX prefix and immediate. [Shift_cl]
    is always three bytes (REX.W with REX.B when needed, D3, ModR/M). [Setcc]
    uses three bytes for AL/CL/DL and four for R8b through R11b. Stack
    loads/stores always use an eight-byte fixed-disp32 SIB form; stack
    allocation/free always use seven-byte imm32 forms. Qword frame loads/stores
    use a seven-byte RBP+disp32 form. Narrow frame loads/stores are seven or
    eight bytes depending on width/prefix requirements. Callable allocation/free
    uses a seven-byte imm32 RSP form. Direct CALL and branches use fixed rel32
    forms; status/context immediate stores are always eight bytes. Private
    context register loads/stores use fixed disp8 forms. Context loads admit the
    immutable arena-pointer word at offset 72; stores remain restricted through
    offset 64. Arena qword/narrow accesses use fixed R9+disp32 forms. Invalid
    immediate, branch or private-context operands raise [Invalid_argument]. *)

val encode : instruction -> string
(** Encode one instruction into a fresh string using the pinned opcode facts. *)

val encode_all :
  max_code_bytes:int -> instruction list -> (string, string) result
(** Validate the positive limit and exact total size before allocating a code
    buffer. A limit above [max_code_bytes] is rejected. *)
