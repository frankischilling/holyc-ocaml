type register = Rax | Rcx | Rdx | R8 | R9 | R10 | R11
type unary = Neg | Not
type binary = Add | Sub | Imul | And | Or | Xor

type instruction =
  | Mov_imm64 of register * int64
  | Mov of register * register
  | Unary of unary * register
  | Binary of binary * register * register
  | Ret
      (** Register operands are destination then source. Every data operation
          uses the full 64-bit registers. [Imul] retains the low 64 product
          bits. The only implicit stack access is the return address read by
          [Ret]. *)

val registers : register list
(** Deterministic allocation order: RAX, RCX, RDX, R8, R9, R10, R11. *)

val max_code_bytes : int
(** Hard limit of 16 MiB for one encoded instruction sequence, further bounded
    by [Sys.max_string_length] on the compiler host. *)

val size : instruction -> int
(** Exact encoded byte count, including the REX prefix and immediate. *)

val encode : instruction -> string
(** Encode one instruction into a fresh string using the pinned opcode facts. *)

val encode_all :
  max_code_bytes:int -> instruction list -> (string, string) result
(** Validate the positive limit and exact total size before allocating a code
    buffer. A limit above [max_code_bytes] is rejected. *)
