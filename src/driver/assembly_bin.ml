module Bin = Asm.Bin_module
module Entry = Bin.Entry

exception Rejected of Common.Diagnostic.t

let emit ?max_bytes ?max_work ?(main = []) ?(records = [])
    (image : Assembly_encoding.t) =
  let root = image.assembly.assembly_unit_location in
  let error message =
    Common.Diagnostic.make ~code:"HCBIN0001" ~severity:Common.Diagnostic.Error
      ~message ~primary:root.span ()
  in
  try
    let fail message = raise (Rejected (error message)) in
    if image.position <> 0L then
      fail "BIN assembly must start at raw position zero";
    let globals = Hashtbl.create 16 in
    List.iter
      (fun (s : Assembly_encoding.symbol) ->
        if s.scope = None && not (Hashtbl.mem globals s.name) then
          Hashtbl.add globals s.name s)
      image.symbols;
    let absolute =
      List.filter_map
        (fun (r : Assembly_encoding.relocation) ->
          if r.symbol = None && r.type_id land 1 = 0 then Some r.offset
          else None)
        image.relocations
      |> List.rev
    in
    let generated =
      (if absolute = [] then [] else [ Bin.Absolute_addresses absolute ])
      @ List.filter_map
          (fun (s : Assembly_encoding.symbol) ->
            if s.exported then
              Some
                (Bin.Export
                   {
                     kind = Entry.Rel32_export;
                     value = s.position;
                     name = s.name;
                   })
            else None)
          image.symbols
    in
    let mains =
      List.map
        (fun name ->
          match Hashtbl.find_opt globals name with
          | Some s -> Bin.Main (Int64.to_int s.position)
          | None -> fail ("unknown MAIN label: " ^ name))
        main
    in
    let fields =
      List.filter
        (fun (r : Assembly_encoding.relocation) -> Option.is_some r.symbol)
        image.relocations
    in
    let groups = Hashtbl.create 16 and names = ref [] in
    List.iter
      (fun (r : Assembly_encoding.relocation) ->
        let name = Option.get r.symbol in
        let previous =
          match Hashtbl.find_opt groups name with
          | None ->
              names := name :: !names;
              []
          | Some fields -> fields
        in
        Hashtbl.replace groups name (r :: previous))
      fields;
    let imports =
      List.concat_map
        (fun name ->
          List.rev (Hashtbl.find groups name)
          |> List.mapi (fun index (r : Assembly_encoding.relocation) ->
              let kind =
                match Entry.of_code r.type_id with
                | Some kind -> kind
                | None -> fail "invalid assembly import identity"
              in
              Bin.Import
                {
                  kind;
                  offset = r.offset;
                  name = (if index = 0 then name else "");
                }))
        (List.rev !names)
    in
    match
      Bin.create ?max_bytes ?max_work ~alignment_bits:image.max_align_bits
        ~origin:image.origin ~body:image.bytes
        (generated @ mains @ records @ imports)
    with
    | Ok module_ -> Ok module_
    | Error e ->
        let prefix =
          match e.record_index with
          | None -> ""
          | Some index -> Printf.sprintf "record %d: " index
        in
        Error (error (prefix ^ e.message))
  with Rejected diagnostic -> Error diagnostic
