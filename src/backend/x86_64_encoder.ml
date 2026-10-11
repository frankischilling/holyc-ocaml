module Facts = Generated.Opcode_keywords

type register = Rax | Rcx | Rdx | R8 | R9 | R10 | R11
type unary = Neg | Not
type binary = Add | Sub | Imul | And | Or | Xor
type shift = Shl | Shr | Sar
type bit = Bt | Bts | Btr | Btc
type status_abi = Windows_x64 | System_v_x64
type condition = E | NE | L | GE | G | LE | B | AE | A | BE
type narrow_frame_width = Frame8 | Frame16 | Frame32
type frame_extension = Sign_extend | Zero_extend
type atomic_width = Atomic8 | Atomic16 | Atomic32 | Atomic64
type stack_slot = { offset : int }
type stack_frame = { bytes : int }
type frame_slot = { frame_offset : int }
type arena_slot = { arena_offset : int }
type call_frame = { call_bytes : int }

type instruction =
  | Mov_imm64 of register * int64
  | Mov of register * register
  | Load_stack of register * stack_slot
  | Store_stack of stack_slot * register
  | Address_stack of register * stack_slot
  | Address_code_relative of register * int64
  | Alloc_stack of stack_frame
  | Free_stack of stack_frame
  | Push_rbp
  | Mov_rbp_rsp
  | Load_frame of register * frame_slot
  | Store_frame of frame_slot * register
  | Load_frame_narrow of
      register * frame_slot * narrow_frame_width * frame_extension
  | Store_frame_narrow of frame_slot * narrow_frame_width * register
  | Load_arena of register * arena_slot
  | Store_arena of arena_slot * register
  | Load_arena_narrow of
      register * arena_slot * narrow_frame_width * frame_extension
  | Store_arena_narrow of arena_slot * narrow_frame_width * register
  | Address_frame of register * frame_slot
  | Address_arena of register * arena_slot
  | Load_indirect of register * register * int
  | Store_indirect of register * register
  | Store_indirect_offset of register * int * register
  | Load_indirect_narrow of
      register * register * narrow_frame_width * frame_extension
  | Store_indirect_narrow of register * narrow_frame_width * register
  | Lock_binary_indirect of binary * register * atomic_width * register
  | Lock_cmpxchg_indirect of register * atomic_width * register
  | Alloc_call_frame of call_frame
  | Free_call_frame of call_frame
  | Call of int64
  | Call_stack of stack_slot
  | Pop_rbp
  | Unary of unary * register
  | Binary of binary * register * register
  | Bsf of register * register
  | Bsr of register * register
  | Bit of bit * register * register
  | Shift_cl of shift * register
  | Shift_immediate of shift * register * int64
  | Capture_status of status_abi
  | Source_arguments of status_abi
  | Compiler_option_arguments of status_abi
  | Zero_edx
  | Cqo
  | Div_rcx
  | Idiv_rcx
  | Cmp_imm8 of register * int
  | Jump of int64
  | Jump_equal of int64
  | Jump_not_equal of int64
  | Jump_below of int64
  | Jump_less of int64
  | Jump_overflow of int64
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

let frame_slot ~offset =
  let value = Int64.of_int offset in
  if
    offset mod 8 <> 0
    || Int64.compare value (-0x80000000L) < 0
    || Int64.compare value 0x7ffffff8L > 0
  then Error "frame slot offset must be an aligned signed 32-bit displacement"
  else Ok { frame_offset = offset }

let scalar_frame_slot ~offset =
  let value = Int64.of_int offset in
  if
    Int64.compare value (-0x80000000L) < 0
    || Int64.compare value 0x7fffffffL > 0
  then Error "scalar frame slot offset must be a signed 32-bit displacement"
  else Ok { frame_offset = offset }

let arena_slot ~offset =
  let value = Int64.of_int offset in
  if offset < 0 || Int64.compare value 0x7fffffffL > 0 then
    Error "arena slot offset must be a nonnegative signed 32-bit displacement"
  else Ok { arena_offset = offset }

let call_frame ~bytes =
  if bytes < 16 || bytes > 4080 || bytes mod 16 <> 0 then
    Error "call frame size must be a 16-byte multiple between 16 and 4080 bytes"
  else Ok { call_bytes = bytes }

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

(* Select generated forms by their pinned source locations. Native recipes
   retain fixed layouts and use Asm.Encoding for validation and byte emission. *)
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

let load_address = source_form "LEA" 833
let mov_immediate = source_form "MOV" 276
let mov_register = source_form "MOV" 265
let mov_load = source_form "MOV" 261
let mov_store = source_form "MOV" 265
let mov_load32 = source_form "MOV" 260
let mov_store8 = source_form "MOV" 262
let mov_store16 = source_form "MOV" 263
let mov_store32 = source_form "MOV" 264
let movsx_load8 = source_form "MOVSX" 885
let movsx_load16 = source_form "MOVSX" 887
let movsxd_load32 = source_form "MOVSXD" 889
let movzx_load8 = source_form "MOVZX" 893
let movzx_load16 = source_form "MOVZX" 895
let push_register = source_form "PUSH" 227
let pop_register = source_form "POP" 243
let call_relative = source_form "CALL" 571
let call_indirect = source_form "CALL" 573
let add_immediate = source_form "ADD" 322
let subtract_immediate = source_form "SUB" 437
let negate = source_form "NEG" 680
let complement = source_form "NOT" 675
let add = source_form "ADD" 330
let subtract = source_form "SUB" 445
let multiply = source_form "IMUL2" 694
let scan_forward = source_form "BSF" 727
let scan_reverse = source_form "BSR" 731
let bit_test = source_form "BT" 738
let bit_complement = source_form "BTC" 748
let bit_reset = source_form "BTR" 758
let bit_set = source_form "BTS" 768
let bitwise_and = source_form "AND" 353
let bitwise_or = source_form "OR" 399
let bitwise_xor = source_form "XOR" 501
let lock_prefix = source_form "LOCK" 864

let atomic_binary_forms =
  List.map
    (fun (operation, spelling, first_line) ->
      ( operation,
        Array.init 4 (fun width -> source_form spelling (first_line + width)) ))
    [
      (Add, "ADD", 327);
      (Sub, "SUB", 442);
      (And, "AND", 350);
      (Or, "OR", 396);
      (Xor, "XOR", 498);
    ]

let atomic_cmpxchg_forms =
  Array.init 4 (fun width -> source_form "CMPXCHG" (791 + width))

let atomic_width_index = function
  | Atomic8 -> 0
  | Atomic16 -> 1
  | Atomic32 -> 2
  | Atomic64 -> 3

let atomic_binary_form operation width =
  match List.assoc_opt operation atomic_binary_forms with
  | Some forms -> forms.(atomic_width_index width)
  | None -> invalid_arg "LOCK requires memory ADD, SUB, AND, OR or XOR"

let shift_left_cl = source_form "SHL" 1107
let shift_right_cl = source_form "SHR" 1125
let shift_arithmetic_right_cl = source_form "SAR" 1143
let shift_left_immediate = source_form "SHL" 1115
let shift_right_immediate = source_form "SHR" 1133
let shift_arithmetic_right_immediate = source_form "SAR" 1151
let shift_left_one = source_form "SHL1" 1120
let shift_right_one = source_form "SHR1" 1138
let shift_arithmetic_right_one = source_form "SAR1" 1156
let compare_immediate_byte = source_form "CMP" 365
let zero_register32 = source_form "XOR" 496
let divide = source_form "DIV" 708
let signed_divide = source_form "IDIV" 713
let sign_extend_rax = source_form "CQO" 780
let jump = source_form "JMP" 584
let jump_equal = source_form "JE" 608
let jump_not_equal = source_form "JNE" 612
let jump_below = source_form "JB" 600
let jump_less = source_form "JL" 640
let jump_overflow = source_form "JO" 592
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

let immediate_shift_form shift count =
  match (shift, count = 1L) with
  | Shl, true -> shift_left_one
  | Shr, true -> shift_right_one
  | Sar, true -> shift_arithmetic_right_one
  | Shl, false -> shift_left_immediate
  | Shr, false -> shift_right_immediate
  | Sar, false -> shift_arithmetic_right_immediate

let narrow_load_form width extension =
  match (width, extension) with
  | Frame8, Sign_extend -> movsx_load8
  | Frame8, Zero_extend -> movzx_load8
  | Frame16, Sign_extend -> movsx_load16
  | Frame16, Zero_extend -> movzx_load16
  | Frame32, Sign_extend -> movsxd_load32
  | Frame32, Zero_extend -> mov_load32

let narrow_store_form = function
  | Frame8 -> mov_store8
  | Frame16 -> mov_store16
  | Frame32 -> mov_store32

let form = function
  | Mov_imm64 _ -> mov_immediate
  | Mov _ -> mov_register
  | Load_stack _ -> mov_load
  | Store_stack _ -> mov_store
  | Address_stack _ | Address_code_relative _ -> load_address
  | Alloc_stack _ -> subtract_immediate
  | Free_stack _ -> add_immediate
  | Push_rbp -> push_register
  | Mov_rbp_rsp -> mov_register
  | Load_frame _ -> mov_load
  | Store_frame _ -> mov_store
  | Load_frame_narrow (_, _, width, extension) ->
      narrow_load_form width extension
  | Store_frame_narrow (_, width, _) -> narrow_store_form width
  | Load_arena _ -> mov_load
  | Store_arena _ -> mov_store
  | Load_arena_narrow (_, _, width, extension) ->
      narrow_load_form width extension
  | Store_arena_narrow (_, width, _) -> narrow_store_form width
  | Address_frame _ | Address_arena _ -> load_address
  | Load_indirect _ -> mov_load
  | Store_indirect _ | Store_indirect_offset _ -> mov_store
  | Load_indirect_narrow (_, _, width, extension) ->
      narrow_load_form width extension
  | Store_indirect_narrow (_, width, _) -> narrow_store_form width
  | Lock_binary_indirect (operation, _, width, _) ->
      atomic_binary_form operation width
  | Lock_cmpxchg_indirect (_, width, _) ->
      atomic_cmpxchg_forms.(atomic_width_index width)
  | Alloc_call_frame _ -> subtract_immediate
  | Free_call_frame _ -> add_immediate
  | Call _ -> call_relative
  | Call_stack _ -> call_indirect
  | Pop_rbp -> pop_register
  | Unary (Neg, _) -> negate
  | Unary (Not, _) -> complement
  | Binary (Add, _, _) -> add
  | Binary (Sub, _, _) -> subtract
  | Binary (Imul, _, _) -> multiply
  | Bsf _ -> scan_forward
  | Bsr _ -> scan_reverse
  | Bit (Bt, _, _) -> bit_test
  | Bit (Bts, _, _) -> bit_set
  | Bit (Btr, _, _) -> bit_reset
  | Bit (Btc, _, _) -> bit_complement
  | Binary (And, _, _) -> bitwise_and
  | Binary (Or, _, _) -> bitwise_or
  | Binary (Xor, _, _) -> bitwise_xor
  | Shift_cl (shift, _) -> shift_form shift
  | Shift_immediate (shift, _, count) -> immediate_shift_form shift count
  | Capture_status _ | Source_arguments _ | Compiler_option_arguments _ ->
      mov_register
  | Zero_edx -> zero_register32
  | Cqo -> sign_extend_rax
  | Div_rcx -> divide
  | Idiv_rcx -> signed_divide
  | Cmp_imm8 _ -> compare_immediate_byte
  | Jump _ -> jump
  | Jump_equal _ -> jump_equal
  | Jump_not_equal _ -> jump_not_equal
  | Jump_below _ -> jump_below
  | Jump_less _ -> jump_less
  | Jump_overflow _ -> jump_overflow
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

let valid_context_read_offset offset =
  offset >= 0 && offset <= 184 && offset mod 8 = 0

let valid_context_write_offset offset =
  (offset >= 0 && offset <= 64 && offset mod 8 = 0)
  || offset = 88 || offset = 96 || offset = 104 || offset = 120 || offset = 128
  || offset = 160

let valid_reference_offset offset =
  offset = 0 || offset = 8 || offset = 16 || offset = 24

let validate = function
  | Lock_binary_indirect (Imul, _, _, _) ->
      invalid_arg "LOCK requires memory ADD, SUB, AND, OR or XOR"
  | Load_indirect (_, _, offset) when not (valid_reference_offset offset) ->
      invalid_arg
        "reference descriptor offset must be zero, eight, sixteen or \
         twenty-four"
  | Store_indirect_offset (_, offset, _)
    when not (valid_reference_offset offset) ->
      invalid_arg
        "reference descriptor offset must be zero, eight, sixteen or \
         twenty-four"
  | (Load_frame (_, slot) | Store_frame (slot, _))
    when slot.frame_offset mod 8 <> 0 ->
      invalid_arg "qword frame access requires an aligned frame slot"
  | Cmp_imm8 (_, immediate) when immediate < -128 || immediate > 127 ->
      invalid_arg "Cmp_imm8 immediate must fit signed eight bits"
  | Jump displacement
  | Jump_equal displacement
  | Jump_not_equal displacement
  | Jump_below displacement
  | Jump_less displacement
  | Jump_overflow displacement
  | Call displacement
  | Address_code_relative (_, displacement)
    when not (signed_rel32 displacement) ->
      invalid_arg "relative branch displacement must fit signed 32 bits"
  | Store_status_kind kind when kind < 1 || kind > 2 ->
      invalid_arg "status kind must be 1 or 2"
  | Store_status_site site when site < 1 || site > 100_000 ->
      invalid_arg "status site must be between 1 and 100000"
  | Load_context (_, offset) when not (valid_context_read_offset offset) ->
      invalid_arg
        "private context read offset must be aligned from 0 through 184"
  | Store_context (offset, _) when not (valid_context_write_offset offset) ->
      invalid_arg
        "private context write offset must be aligned from 0 through 64, or \
         88, 96, 104, 120, 128 or 160"
  | Store_context_imm (offset, _) when not (valid_context_write_offset offset)
    ->
      invalid_arg
        "private context write offset must be aligned from 0 through 64, or \
         88, 96, 104, 120, 128 or 160"
  | Store_context_imm (_, immediate) when not (signed_int32 immediate) ->
      invalid_arg "private context immediate must fit signed 32 bits"
  | _ -> ()

module Encoding = Asm.Encoding

let checked = function
  | Ok value -> value
  | Error message -> invalid_arg message

let signed bytes value =
  Encoding.field ~bytes ~range:Encoding.Signed value |> checked

let plans instruction =
  validate instruction;
  let selected = form instruction in
  let one ?prefixes ?opcode_patch ?modrm ?sib ?displacement ?immediate () =
    [
      Encoding.plan ~mode:Asm.Operand.Use64 ~form:selected ?prefixes
        ?opcode_patch ?modrm ?sib ?displacement ?immediate ()
      |> checked;
    ]
  in
  (* BackA.HC:44-51 and Asm.HC:580-638 assign register extensions to REX.R/B.
     The fixed native layout keeps disp32 even when a shorter form would fit. *)
  let register ?immediate ~reg ~rm () =
    one
      ~prefixes:[ 0x48 lor ((reg land 8) lsr 1) lor ((rm land 8) lsr 3) ]
      ~modrm:(0xc0 lor ((reg land 7) lsl 3) lor (rm land 7))
      ?immediate ()
  in
  let memory ?(prefixes = []) ?(wide = true) ~reg ~base displacement =
    let rex =
      (if wide then 0x48 else 0x40)
      lor ((reg land 8) lsr 1)
      lor ((base land 8) lsr 3)
    in
    one ~prefixes:(prefixes @ [ rex ])
      ~modrm:(0x80 lor ((reg land 7) lsl 3) lor (base land 7))
      ?sib:(if base land 7 = 4 then Some 0x24 else None)
      ~displacement:(signed 4 (Int64.of_int displacement))
      ()
  in
  let wide_load width extension =
    width <> Frame32 || extension <> Zero_extend
  in
  let store_prefixes width = if width = Frame16 then [ 0x66 ] else [] in
  let context ~reg ?immediate displacement =
    one
      ~prefixes:[ 0x49 lor ((reg land 8) lsr 1) ]
      ~modrm:
        ((if displacement <= 127 then 0x43 else 0x83) lor ((reg land 7) lsl 3))
      ~displacement:
        (signed
           (if displacement <= 127 then 1 else 4)
           (Int64.of_int displacement))
      ?immediate ()
  in
  match instruction with
  | Mov_imm64 (destination, immediate) ->
      let destination = register_number destination in
      one
        ~prefixes:[ 0x48 lor ((destination land 8) lsr 3) ]
        ~opcode_patch:(Encoding.Or_low3 (destination land 7))
        ~immediate:(signed 8 immediate) ()
  | Mov (destination, source) ->
      register ~reg:(register_number source)
        ~rm:(register_number destination)
        ()
  | Address_code_relative (destination, displacement) ->
      let destination = register_number destination in
      one
        ~prefixes:[ 0x48 lor ((destination land 8) lsr 1) ]
        ~modrm:(0x05 lor ((destination land 7) lsl 3))
        ~displacement:(signed 4 displacement) ()
  | Load_stack (destination, slot) | Address_stack (destination, slot) ->
      memory ~reg:(register_number destination) ~base:4 slot.offset
  | Store_stack (slot, source) ->
      memory ~reg:(register_number source) ~base:4 slot.offset
  | Alloc_stack frame | Free_stack frame ->
      register ~reg:selected.slash_value ~rm:4
        ~immediate:(signed 4 (Int64.of_int frame.bytes))
        ()
  | Push_rbp | Pop_rbp -> one ~opcode_patch:(Encoding.Or_low3 5) ()
  | Mov_rbp_rsp ->
      (* RSP and RBP remain outside the allocator-visible register type. *)
      register ~reg:4 ~rm:5 ()
  | Address_frame (destination, slot) | Load_frame (destination, slot) ->
      memory ~reg:(register_number destination) ~base:5 slot.frame_offset
  | Store_frame (slot, source) ->
      memory ~reg:(register_number source) ~base:5 slot.frame_offset
  | Load_frame_narrow (destination, slot, width, extension) ->
      memory
        ~wide:(wide_load width extension)
        ~reg:(register_number destination)
        ~base:5 slot.frame_offset
  | Store_frame_narrow (slot, width, source) ->
      memory ~prefixes:(store_prefixes width) ~wide:false
        ~reg:(register_number source) ~base:5 slot.frame_offset
  | Address_arena (destination, slot) | Load_arena (destination, slot) ->
      memory ~reg:(register_number destination) ~base:9 slot.arena_offset
  | Store_arena (slot, source) ->
      memory ~reg:(register_number source) ~base:9 slot.arena_offset
  | Load_arena_narrow (destination, slot, width, extension) ->
      memory
        ~wide:(wide_load width extension)
        ~reg:(register_number destination)
        ~base:9 slot.arena_offset
  | Store_arena_narrow (slot, width, source) ->
      memory ~prefixes:(store_prefixes width) ~wide:false
        ~reg:(register_number source) ~base:9 slot.arena_offset
  | Load_indirect (destination, base, offset) ->
      memory
        ~reg:(register_number destination)
        ~base:(register_number base) offset
  | Store_indirect (base, source) ->
      memory ~reg:(register_number source) ~base:(register_number base) 0
  | Store_indirect_offset (base, offset, source) ->
      memory ~reg:(register_number source) ~base:(register_number base) offset
  | Load_indirect_narrow (destination, base, width, extension) ->
      memory
        ~wide:(wide_load width extension)
        ~reg:(register_number destination)
        ~base:(register_number base) 0
  | Store_indirect_narrow (base, width, source) ->
      memory ~prefixes:(store_prefixes width) ~wide:false
        ~reg:(register_number source) ~base:(register_number base) 0
  | Lock_binary_indirect (_, base, width, source)
  | Lock_cmpxchg_indirect (base, width, source) ->
      memory
        ~prefixes:
          (lock_prefix.opcode_bytes @ if width = Atomic16 then [ 0x66 ] else [])
        ~wide:(width = Atomic64) ~reg:(register_number source)
        ~base:(register_number base) 0
  | Alloc_call_frame frame | Free_call_frame frame ->
      register ~reg:selected.slash_value ~rm:4
        ~immediate:(signed 4 (Int64.of_int frame.call_bytes))
        ()
  | Call displacement -> one ~immediate:(signed 4 displacement) ()
  | Call_stack slot ->
      (* CALL RM64 uses its implicit qword width without a REX prefix. *)
      one
        ~modrm:(0x84 lor (selected.slash_value lsl 3))
        ~sib:0x24
        ~displacement:(signed 4 (Int64.of_int slot.offset))
        ()
  | Unary (_, destination) ->
      register ~reg:selected.slash_value ~rm:(register_number destination) ()
  | Bsf (destination, source) | Bsr (destination, source) ->
      register
        ~reg:(register_number destination)
        ~rm:(register_number source) ()
  | Bit (_, field, index) ->
      register ~reg:(register_number index) ~rm:(register_number field) ()
  | Binary (Imul, destination, source) ->
      register
        ~reg:(register_number destination)
        ~rm:(register_number source) ()
  | Binary (_, destination, source) ->
      register ~reg:(register_number source)
        ~rm:(register_number destination)
        ()
  | Shift_cl (_, destination) ->
      register ~reg:selected.slash_value ~rm:(register_number destination) ()
  | Shift_immediate (_, destination, count) ->
      (* Native shift counts intentionally use their low byte, matching ICU8 in
         BackA.HC. The checked field receives that explicit byte value. *)
      let immediate =
        if count = 1L then None
        else
          Some
            (Encoding.field ~bytes:1 ~range:Encoding.Unsigned
               (Int64.logand count 0xffL)
            |> checked)
      in
      register ~reg:selected.slash_value
        ~rm:(register_number destination)
        ?immediate ()
  | Capture_status abi ->
      register
        ~reg:
          (match abi with
          | Windows_x64 -> 1
          | System_v_x64 -> 7)
        ~rm:11 ()
  | Source_arguments abi ->
      register ~reg:11
        ~rm:
          (match abi with
          | Windows_x64 -> 1
          | System_v_x64 -> 7)
        ()
  | Compiler_option_arguments abi ->
      (* System V stages the index and operation in RSI/RDX before passing the
         private R11 context in RDI. Every MOV uses its own shared plan. *)
      let arguments =
        match abi with
        | Windows_x64 -> []
        | System_v_x64 -> register ~reg:2 ~rm:6 () @ register ~reg:8 ~rm:2 ()
      in
      arguments
      @ register ~reg:11
          ~rm:
            (match abi with
            | Windows_x64 -> 1
            | System_v_x64 -> 7)
          ()
  | Zero_edx -> one ~modrm:0xd2 ()
  | Cqo -> one ~prefixes:[ 0x48 ] ()
  | Div_rcx | Idiv_rcx ->
      register ~reg:selected.slash_value ~rm:(register_number Rcx) ()
  | Cmp_imm8 (destination, immediate) ->
      register ~reg:selected.slash_value
        ~rm:(register_number destination)
        ~immediate:(signed 1 (Int64.of_int immediate))
        ()
  | Jump displacement
  | Jump_equal displacement
  | Jump_not_equal displacement
  | Jump_below displacement
  | Jump_less displacement
  | Jump_overflow displacement -> one ~immediate:(signed 4 displacement) ()
  | Store_status_kind immediate ->
      context ~reg:0 ~immediate:(signed 4 (Int64.of_int immediate)) 0
  | Store_status_site immediate ->
      context ~reg:0 ~immediate:(signed 4 (Int64.of_int immediate)) 8
  | Load_context (destination, displacement) ->
      context ~reg:(register_number destination) displacement
  | Store_context (displacement, source) ->
      context ~reg:(register_number source) displacement
  | Store_context_imm (displacement, immediate) ->
      context ~reg:0 ~immediate:(signed 4 (Int64.of_int immediate)) displacement
  | Dec destination ->
      register ~reg:selected.slash_value ~rm:(register_number destination) ()
  | Cmp (left, right) ->
      register ~reg:(register_number right) ~rm:(register_number left) ()
  | Test destination ->
      let destination = register_number destination in
      register ~reg:destination ~rm:destination ()
  | Setcc (_, destination) ->
      let destination = register_number destination in
      one
        ~prefixes:(if destination >= 8 then [ 0x41 ] else [])
        ~modrm:(0xc0 lor (destination land 7))
        ()
  | Movzx8 (destination, source) ->
      register
        ~reg:(register_number destination)
        ~rm:(register_number source) ()
  | Ret -> one ()

let size instruction =
  List.fold_left
    (fun total plan -> total + Encoding.size plan)
    0 (plans instruction)

let encode instruction =
  Encoding.encode_all ~max_bytes:max_code_bytes (plans instruction) |> checked

let encode_all ~max_code_bytes:limit instructions =
  if limit <= 0 || limit > max_code_bytes then
    Error
      (Printf.sprintf "max_code_bytes must be between 1 and %d" max_code_bytes)
  else
    try
      let rec prepare total reversed = function
        | [] -> Encoding.encode_all ~max_bytes:limit (List.rev reversed)
        | instruction :: remaining ->
            let current = plans instruction in
            let length =
              List.fold_left
                (fun total plan -> total + Encoding.size plan)
                0 current
            in
            if length > limit - total then
              Error "encoded instructions exceed max_code_bytes"
            else
              prepare (total + length)
                (List.rev_append current reversed)
                remaining
      in
      prepare 0 [] instructions
    with Invalid_argument message -> Error message
