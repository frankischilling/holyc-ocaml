type kind = R8 | R16 | R32 | R64 | Segment | Float_stack | Mm | Xmm
type t = Generated.Opcode_keywords.register

let all = Generated.Opcode_keywords.registers

let by_spelling =
  let table = Hashtbl.create (List.length all) in
  List.iter
    (fun (register : t) -> Hashtbl.add table register.spelling register)
    all;
  table

let find source_spelling = Hashtbl.find_opt by_spelling source_spelling
let spelling (register : t) = register.spelling

let kind (register : t) =
  match register.register_kind with
  | Generated.Opcode_keywords.R8 -> R8
  | Generated.Opcode_keywords.R16 -> R16
  | Generated.Opcode_keywords.R32 -> R32
  | Generated.Opcode_keywords.R64 -> R64
  | Generated.Opcode_keywords.Segment -> Segment
  | Generated.Opcode_keywords.Float_stack -> Float_stack
  | Generated.Opcode_keywords.Mm -> Mm
  | Generated.Opcode_keywords.Xmm -> Xmm

let number (register : t) = register.register_number
let source_line (register : t) = register.source_line
