module Facts = Generated.Opcode_keywords

type register = Rax | Rcx | Rdx | R8 | R9 | R10 | R11
type unary = Neg | Not
type binary = Add | Sub | Imul | And | Or | Xor
type shift = Shl | Shr | Sar
type status_abi = Windows_x64 | System_v_x64
type condition = E | NE | L | GE | G | LE | B | AE | A | BE
type stack_slot = { offset : int }
type stack_frame = { bytes : int }

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
  | Capture_status of status_abi
  | Zero_edx
  | Cqo
  | Div_rcx
  | Idiv_rcx
  | Cmp_imm8 of register * int
  | Jump of int64
  | Jump_equal of int64
  | Jump_not_equal of int64
  | Store_status_kind of int
  | Store_status_site of int
  | Load_context of register * int
  | Store_context of int * register
  | Store_context_imm of int * int
  | Dec of register
  | Cmp of register * register
  | Test of register
  | Setcc of condition * register
  | Movzx8 of register * register
  | Ret

let registers = [ Rax; Rcx; Rdx; R8; R9; R10; R11 ]
let max_code_bytes = min (16 * 1024 * 1024) Sys.max_string_length

let stack_slot ~offset =
  if offset < 0 || offset > 4080 || offset mod 8 <> 0 then
    Error "stack slot offset must be an aligned value between 0 and 4080"
  else Ok { offset }

let stack_frame ~bytes =
  if bytes < 8 || bytes > 4088 || bytes mod 16 <> 8 then
    Error
      "stack frame size must be between 8 and 4088 and congruent to 8 mod 16"
  else Ok { bytes }

let register_spelling = function
  | Rax -> "RAX"
  | Rcx -> "RCX"
  | Rdx -> "RDX"
  | R8 -> "R8"
  | R9 -> "R9"
  | R10 -> "R10"
  | R11 -> "R11"

let register_numbers =
  List.map
    (fun register ->
      let spelling = register_spelling register in
      let fact =
        List.find
          (fun (fact : Facts.register) ->
            fact.register_kind = Facts.R64 && fact.spelling = spelling)
          Facts.registers
      in
      (register, fact.register_number))
    registers

let register_number register = List.assoc register register_numbers

(* Select existing generated facts by their pinned source locations. The
   encoder owns REX/ModR/M construction, not a second opcode database. *)
let source_form spelling source_line =
  let opcode =
    List.find
      (fun (opcode : Facts.opcode) -> opcode.spelling = spelling)
      Facts.opcodes
  in
  List.find
    (fun (instruction : Facts.instruction) ->
      instruction.source_line = source_line)
    opcode.instructions

let mov_immediate = source_form "MOV" 276
let mov_register = source_form "MOV" 265
let mov_load = source_form "MOV" 261
let mov_store = source_form "MOV" 265
let add_immediate = source_form "ADD" 322
let subtract_immediate = source_form "SUB" 437
let negate = source_form "NEG" 680
let complement = source_form "NOT" 675
let add = source_form "ADD" 330
let subtract = source_form "SUB" 445
let multiply = source_form "IMUL2" 694
let bitwise_and = source_form "AND" 353
let bitwise_or = source_form "OR" 399
let bitwise_xor = source_form "XOR" 501
let shift_left_cl = source_form "SHL" 1107
let shift_right_cl = source_form "SHR" 1125
let shift_arithmetic_right_cl = source_form "SAR" 1143
let compare_immediate_byte = source_form "CMP" 365
let zero_register32 = source_form "XOR" 496
let divide = source_form "DIV" 708
let signed_divide = source_form "IDIV" 713
let sign_extend_rax = source_form "CQO" 780
let jump = source_form "JMP" 584
let jump_equal = source_form "JE" 608
let jump_not_equal = source_form "JNE" 612
let store_immediate = source_form "MOV" 284
let decrement = source_form "DEC" 670
let compare = source_form "CMP" 376
let test = source_form "TEST" 461
let movzx_byte = source_form "MOVZX" 893
let set_equal = source_form "SETE" 983
let set_not_equal = source_form "SETNE" 984
let set_less = source_form "SETL" 991
let set_greater_equal = source_form "SETGE" 992
let set_greater = source_form "SETG" 994
let set_less_equal = source_form "SETLE" 993
let set_below = source_form "SETB" 981
let set_above_equal = source_form "SETAE" 982
let set_above = source_form "SETA" 986
let set_below_equal = source_form "SETBE" 985
let return = source_form "RET" 961

let condition_form = function
  | E -> set_equal
  | NE -> set_not_equal
  | L -> set_less
  | GE -> set_greater_equal
  | G -> set_greater
  | LE -> set_less_equal
  | B -> set_below
  | AE -> set_above_equal
  | A -> set_above
  | BE -> set_below_equal

let shift_form = function
  | Shl -> shift_left_cl
  | Shr -> shift_right_cl
  | Sar -> shift_arithmetic_right_cl

let form = function
  | Mov_imm64 _ -> mov_immediate
  | Mov _ -> mov_register
  | Load_stack _ -> mov_load
  | Store_stack _ -> mov_store
  | Alloc_stack _ -> subtract_immediate
  | Free_stack _ -> add_immediate
  | Unary (Neg, _) -> negate
  | Unary (Not, _) -> complement
  | Binary (Add, _, _) -> add
  | Binary (Sub, _, _) -> subtract
  | Binary (Imul, _, _) -> multiply
  | Binary (And, _, _) -> bitwise_and
  | Binary (Or, _, _) -> bitwise_or
  | Binary (Xor, _, _) -> bitwise_xor
  | Shift_cl (shift, _) -> shift_form shift
  | Capture_status _ -> mov_register
  | Zero_edx -> zero_register32
  | Cqo -> sign_extend_rax
  | Div_rcx -> divide
  | Idiv_rcx -> signed_divide
  | Cmp_imm8 _ -> compare_immediate_byte
  | Jump _ -> jump
  | Jump_equal _ -> jump_equal
  | Jump_not_equal _ -> jump_not_equal
  | Store_status_kind _ | Store_status_site _ -> store_immediate
  | Load_context _ -> mov_load
  | Store_context _ -> mov_store
  | Store_context_imm _ -> store_immediate
  | Dec _ -> decrement
  | Cmp _ -> compare
  | Test _ -> test
  | Setcc (condition, _) -> condition_form condition
  | Movzx8 _ -> movzx_byte
  | Ret -> return

let signed_rel32 value =
  Int64.compare value (-0x80000000L) >= 0
  && Int64.compare value 0x7fffffffL <= 0

let signed_int32 value =
  let value = Int64.of_int value in
  Int64.compare value (-0x80000000L) >= 0
  && Int64.compare value 0x7fffffffL <= 0

let valid_context_offset offset =
  offset >= 0 && offset <= 40 && offset mod 8 = 0

let validate = function
  | Cmp_imm8 (_, immediate) when immediate < -128 || immediate > 127 ->
      invalid_arg "Cmp_imm8 immediate must fit signed eight bits"
  | (Jump displacement | Jump_equal displacement | Jump_not_equal displacement)
    when not (signed_rel32 displacement) ->
      invalid_arg "relative branch displacement must fit signed 32 bits"
  | Store_status_kind kind when kind < 1 || kind > 2 ->
      invalid_arg "status kind must be 1 or 2"
  | Store_status_site site when site < 1 || site > 100_000 ->
      invalid_arg "status site must be between 1 and 100000"
  | (Load_context (_, offset) | Store_context (offset, _))
    when not (valid_context_offset offset) ->
      invalid_arg "private context offset must be aligned from 0 through 40"
  | Store_context_imm (offset, _) when not (valid_context_offset offset) ->
      invalid_arg "private context offset must be aligned from 0 through 40"
  | Store_context_imm (_, immediate) when not (signed_int32 immediate) ->
      invalid_arg "private context immediate must fit signed 32 bits"
  | _ -> ()

let size instruction =
  validate instruction;
  let opcode_bytes = List.length (form instruction).opcode_bytes in
  match instruction with
  | Mov_imm64 _ -> opcode_bytes + 1 + 8
  | Load_stack _ | Store_stack _ -> 8
  | Alloc_stack _ | Free_stack _ -> 7
  | Capture_status _ | Div_rcx | Idiv_rcx -> 3
  | Zero_edx | Cqo -> 2
  | Cmp_imm8 _ -> 4
  | Jump _ -> 5
  | Jump_equal _ | Jump_not_equal _ -> 6
  | Store_status_kind _ | Store_status_site _ -> 8
  | Load_context _ | Store_context _ -> 4
  | Store_context_imm _ -> 8
  | Dec _ -> 3
  | Mov _ | Unary _ | Binary _ | Shift_cl _ | Cmp _ | Test _ | Movzx8 _ ->
      opcode_bytes + 1 + 1
  | Setcc (_, destination) ->
      opcode_bytes + 1 + if register_number destination < 8 then 0 else 1
  | Ret -> opcode_bytes

let write buffer position instruction =
  validate instruction;
  let byte value =
    Bytes.set buffer !position (Char.chr value);
    incr position
  in
  let selected = form instruction in
  let opcodes () = List.iter byte selected.opcode_bytes in
  let imm32 value =
    for index = 0 to 3 do
      byte ((value lsr (index * 8)) land 0xff)
    done
  in
  let imm32_int64 value =
    for index = 0 to 3 do
      byte
        (Int64.to_int
           (Int64.logand (Int64.shift_right_logical value (index * 8)) 0xffL))
    done
  in
  (* Asm.HC:580-605,615-638 places the high register bits in REX.R/B.
     Mod=11 selects registers, so no SIB or displacement is emitted. *)
  let modrm ~reg ~rm =
    byte (0x48 lor ((reg land 8) lsr 1) lor ((rm land 8) lsr 3));
    opcodes ();
    byte (0xc0 lor ((reg land 7) lsl 3) lor (rm land 7))
  in
  match instruction with
  | Mov_imm64 (destination, immediate) ->
      let destination = register_number destination in
      byte (0x48 lor ((destination land 8) lsr 3));
      (* The selected MOV form has one opcode byte and an eight-byte
         immediate. Never shorten this to a sign/zero-extending form. *)
      List.iter
        (fun opcode -> byte (opcode lor (destination land 7)))
        selected.opcode_bytes;
      for index = 0 to 7 do
        byte
          (Int64.to_int
             (Int64.logand
                (Int64.shift_right_logical immediate (index * 8))
                0xffL))
      done
  | Mov (destination, source) ->
      modrm ~reg:(register_number source) ~rm:(register_number destination)
  | Load_stack (destination, slot) ->
      let destination = register_number destination in
      (* Fixed disp32 SIB form: RSP cannot be the ModR/M base without a SIB.
         REX.R carries the high destination bit; REX.B/X remain clear. *)
      byte (0x48 lor ((destination land 8) lsr 1));
      opcodes ();
      byte (0x84 lor ((destination land 7) lsl 3));
      byte 0x24;
      imm32 slot.offset
  | Store_stack (slot, source) ->
      let source = register_number source in
      byte (0x48 lor ((source land 8) lsr 1));
      opcodes ();
      byte (0x84 lor ((source land 7) lsl 3));
      byte 0x24;
      imm32 slot.offset
  | Alloc_stack frame | Free_stack frame ->
      byte 0x48;
      opcodes ();
      byte (0xc0 lor (selected.slash_value lsl 3) lor 4);
      imm32 frame.bytes
  | Unary (_, destination) ->
      modrm ~reg:selected.slash_value ~rm:(register_number destination)
  | Binary (Imul, destination, source) ->
      (* IMUL2 is R64,RM64; the other binary forms are RM64,R64. *)
      modrm ~reg:(register_number destination) ~rm:(register_number source)
  | Binary (_, destination, source) ->
      modrm ~reg:(register_number source) ~rm:(register_number destination)
  | Shift_cl (_, destination) ->
      (* OpCodes.DD:1107/1125/1143 are the 64-bit RM64,CL forms. The slash
         value selects SHL/SHR/SAR; RCX is implicit and REX.B extends RM64. *)
      modrm ~reg:selected.slash_value ~rm:(register_number destination)
  | Capture_status abi ->
      (* MOV R11,RCX on Windows x64; MOV R11,RDI on System V x86-64. R11 is
         fixed private state for fault-capable images and RDI is intentionally
         absent from the public allocator register set. *)
      byte 0x49;
      opcodes ();
      byte
        (match abi with
        | Windows_x64 -> 0xcb
        | System_v_x64 -> 0xfb)
  | Zero_edx ->
      (* BackLib.HC:404-411 uses XOR r32,r32 to zero the complete register. *)
      opcodes ();
      byte 0xd2
  | Cqo ->
      byte 0x48;
      opcodes ()
  | Div_rcx | Idiv_rcx ->
      byte 0x48;
      opcodes ();
      byte (0xc0 lor (selected.slash_value lsl 3) lor register_number Rcx)
  | Cmp_imm8 (register, immediate) ->
      let register = register_number register in
      byte (0x48 lor ((register land 8) lsr 3));
      opcodes ();
      byte (0xc0 lor (selected.slash_value lsl 3) lor (register land 7));
      byte (immediate land 0xff)
  | Jump displacement | Jump_equal displacement | Jump_not_equal displacement ->
      opcodes ();
      imm32_int64 displacement
  | Store_status_kind _ | Store_status_site _ ->
      let displacement, immediate =
        match instruction with
        | Store_status_kind value -> (0, value)
        | Store_status_site value -> (8, value)
        | _ -> assert false
      in
      (* MOV qword ptr [R11+disp8],imm32. Only the two private status fields are
         expressible by this API; the positive immediate is sign-extended to the
         same 64-bit value. *)
      byte 0x49;
      opcodes ();
      byte 0x43;
      byte displacement;
      imm32 immediate
  | Load_context (destination, displacement) ->
      let destination = register_number destination in
      (* MOV r64,[R11+disp8]. R11 requires REX.B; REX.R carries the high
         destination bit. The bounded six-word context always fits disp8. *)
      byte (0x49 lor ((destination land 8) lsr 1));
      opcodes ();
      byte (0x43 lor ((destination land 7) lsl 3));
      byte displacement
  | Store_context (displacement, source) ->
      let source = register_number source in
      (* MOV [R11+disp8],r64. R11 requires REX.B; REX.R carries the high source
         bit. *)
      byte (0x49 lor ((source land 8) lsr 1));
      opcodes ();
      byte (0x43 lor ((source land 7) lsl 3));
      byte displacement
  | Store_context_imm (displacement, immediate) ->
      (* MOV qword ptr [R11+disp8],imm32. *)
      byte 0x49;
      opcodes ();
      byte 0x43;
      byte displacement;
      imm32 immediate
  | Dec register ->
      let register = register_number register in
      byte (0x48 lor ((register land 8) lsr 3));
      opcodes ();
      byte (0xc0 lor (selected.slash_value lsl 3) lor (register land 7))
  | Cmp (left, right) ->
      (* CMP RM64,R64 sets flags for left-right without changing either input. *)
      modrm ~reg:(register_number right) ~rm:(register_number left)
  | Test register ->
      let register = register_number register in
      modrm ~reg:register ~rm:register
  | Setcc (_, destination) ->
      let destination = register_number destination in
      (* SETcc takes RM8, not a slash-group extension. Its generated slash
         value is SV_NONE; the ModR/M reg field is zero. Asm.HC:484-502
         supplies REX.B for extended byte registers. AL/CL/DL need no prefix. *)
      if destination >= 8 then byte 0x41;
      opcodes ();
      byte (0xc0 lor (destination land 7))
  | Movzx8 (destination, source) ->
      (* MOVZX R64,RM8 reads only the source byte and clears all higher bits. *)
      modrm ~reg:(register_number destination) ~rm:(register_number source)
  | Ret -> opcodes ()

let encode instruction =
  let buffer = Bytes.create (size instruction) in
  let position = ref 0 in
  write buffer position instruction;
  Bytes.to_string buffer

let encode_all ~max_code_bytes:limit instructions =
  if limit <= 0 || limit > max_code_bytes then
    Error
      (Printf.sprintf "max_code_bytes must be between 1 and %d" max_code_bytes)
  else
    try
      let rec measure total = function
        | [] -> Ok total
        | instruction :: remaining ->
            let length = size instruction in
            if length > limit - total then
              Error "encoded instructions exceed max_code_bytes"
            else measure (total + length) remaining
      in
      match measure 0 instructions with
      | Error message -> Error message
      | Ok length ->
          let buffer = Bytes.create length in
          let position = ref 0 in
          List.iter (write buffer position) instructions;
          Bytes.to_string buffer |> Result.ok
    with Invalid_argument message -> Error message
