module Facts = Generated.Opcode_keywords

type register = Rax | Rcx | Rdx | R8 | R9 | R10 | R11
type unary = Neg | Not
type binary = Add | Sub | Imul | And | Or | Xor

type instruction =
  | Mov_imm64 of register * int64
  | Mov of register * register
  | Unary of unary * register
  | Binary of binary * register * register
  | Ret

let registers = [ Rax; Rcx; Rdx; R8; R9; R10; R11 ]
let max_code_bytes = min (16 * 1024 * 1024) Sys.max_string_length

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
let negate = source_form "NEG" 680
let complement = source_form "NOT" 675
let add = source_form "ADD" 330
let subtract = source_form "SUB" 445
let multiply = source_form "IMUL2" 694
let bitwise_and = source_form "AND" 353
let bitwise_or = source_form "OR" 399
let bitwise_xor = source_form "XOR" 501
let return = source_form "RET" 961

let form = function
  | Mov_imm64 _ -> mov_immediate
  | Mov _ -> mov_register
  | Unary (Neg, _) -> negate
  | Unary (Not, _) -> complement
  | Binary (Add, _, _) -> add
  | Binary (Sub, _, _) -> subtract
  | Binary (Imul, _, _) -> multiply
  | Binary (And, _, _) -> bitwise_and
  | Binary (Or, _, _) -> bitwise_or
  | Binary (Xor, _, _) -> bitwise_xor
  | Ret -> return

let size instruction =
  let opcode_bytes = List.length (form instruction).opcode_bytes in
  match instruction with
  | Mov_imm64 _ -> opcode_bytes + 1 + 8
  | Mov _ | Unary _ | Binary _ -> opcode_bytes + 1 + 1
  | Ret -> opcode_bytes

let write buffer position instruction =
  let byte value =
    Bytes.set buffer !position (Char.chr value);
    incr position
  in
  let selected = form instruction in
  let opcodes () = List.iter byte selected.opcode_bytes in
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
  | Unary (_, destination) ->
      modrm ~reg:selected.slash_value ~rm:(register_number destination)
  | Binary (Imul, destination, source) ->
      (* IMUL2 is R64,RM64; the other binary forms are RM64,R64. *)
      modrm ~reg:(register_number destination) ~rm:(register_number source)
  | Binary (_, destination, source) ->
      modrm ~reg:(register_number source) ~rm:(register_number destination)
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
