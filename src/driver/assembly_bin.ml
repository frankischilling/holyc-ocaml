module Bin = Asm.Bin_module

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
    let object_ =
      match
        Assembly_encoding.native_sections ~mode:Asm.Native_sections.Aot image
      with
      | Ok object_ -> object_
      | Error diagnostic -> raise (Rejected diagnostic)
    in
    let main =
      List.map
        (fun name ->
          match Hashtbl.find_opt globals name with
          | Some s -> s.id
          | None -> fail ("unknown MAIN label: " ^ name))
        main
    in
    match
      Bin.of_sections ?max_bytes ?max_work ~origin:image.origin ~main ~records
        object_
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
