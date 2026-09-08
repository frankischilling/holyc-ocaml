let canonical_u64_registers =
  List.init 16 (fun register_number ->
      match
        List.find_opt
          (fun (register : Generated.Opcode_keywords.register) ->
            register.register_kind = Generated.Opcode_keywords.R64
            && register.register_number = register_number)
          Generated.Opcode_keywords.registers
      with
      | Some register -> (register.spelling, register_number)
      | None ->
          invalid_arg
            (Printf.sprintf
               "checked opcode table lacks canonical U64 register %d"
               register_number))

let canonical_u64_register_number spelling =
  canonical_u64_registers
  |> List.find_map (fun (candidate, number) ->
      if String.equal candidate spelling then Some number else None)

let is_canonical_u64_register spelling =
  Option.is_some (canonical_u64_register_number spelling)
