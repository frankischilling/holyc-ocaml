type t = Generated.Opcode_keywords.opcode
type resolved = { opcode : t; source_spelling : string; is_alias : bool }

let all = Generated.Opcode_keywords.opcodes
let spelling (opcode : t) = opcode.spelling
let source_line (opcode : t) = opcode.source_line

let aliases (opcode : t) =
  List.map
    (fun (alias : Generated.Opcode_keywords.opcode_alias) -> alias.spelling)
    opcode.aliases

let entries =
  List.concat_map
    (fun (opcode : t) ->
      { opcode; source_spelling = opcode.spelling; is_alias = false }
      :: List.map
           (fun (alias : Generated.Opcode_keywords.opcode_alias) ->
             { opcode; source_spelling = alias.spelling; is_alias = true })
           opcode.aliases)
    all

let by_spelling =
  let table = Hashtbl.create (List.length entries) in
  List.iter
    (fun resolved -> Hashtbl.add table resolved.source_spelling resolved)
    entries;
  table

let resolve source_spelling = Hashtbl.find_opt by_spelling source_spelling
let resolved_opcode resolved = resolved.opcode
let resolved_source_spelling resolved = resolved.source_spelling
let resolved_is_alias resolved = resolved.is_alias
