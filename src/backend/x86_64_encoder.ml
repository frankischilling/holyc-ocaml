module Facts = Generated.Opcode_keywords

type register = Rax | Rcx | Rdx | R8 | R9 | R10 | R11
type unary = Neg | Not
type binary = Add | Sub | Imul | And | Or | Xor
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
  | Cmp _ -> compare
  | Test _ -> test
  | Setcc (condition, _) -> condition_form condition
  | Movzx8 _ -> movzx_byte
  | Ret -> return

let size instruction =
  let opcode_bytes = List.length (form instruction).opcode_bytes in
  match instruction with
  | Mov_imm64 _ -> opcode_bytes + 1 + 8
  | Load_stack _ | Store_stack _ -> 8
  | Alloc_stack _ | Free_stack _ -> 7
  | Mov _ | Unary _ | Binary _ | Cmp _ | Test _ | Movzx8 _ ->
      opcode_bytes + 1 + 1
  | Setcc (_, destination) ->
      opcode_bytes + 1 + if register_number destination < 8 then 0 else 1
  | Ret -> opcode_bytes

let write buffer position instruction =
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
