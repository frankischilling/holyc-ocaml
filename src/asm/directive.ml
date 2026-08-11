type t = Generated.Opcode_keywords.entry

let all = Generated.Opcode_keywords.assembly

let find text =
  List.find_opt
    (fun entry -> String.equal entry.Generated.Opcode_keywords.spelling text)
    all

let spelling entry = entry.Generated.Opcode_keywords.spelling
let templeos_id entry = entry.Generated.Opcode_keywords.templeos_id
let source_line entry = entry.Generated.Opcode_keywords.source_line
let compare left right = Int.compare (templeos_id left) (templeos_id right)
