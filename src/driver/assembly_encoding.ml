include Assembly_directives

let schema = "holyc-assembly-encoding-v1"

let hex bytes =
  let digits = "0123456789abcdef" in
  String.init
    (String.length bytes * 2)
    (fun index ->
      let byte = Char.code bytes.[index / 2] in
      digits.[if index mod 2 = 0 then byte lsr 4 else byte land 15])

let address position = Printf.sprintf "0x%016Lx" position

let compression_name = function
  | Frontend.Assembly_binary.Raw -> "raw"
  | Frontend.Assembly_binary.Ct_none -> "CT_NONE"
  | Frontend.Assembly_binary.Ct_7_bit -> "CT_7_BIT"
  | Frontend.Assembly_binary.Ct_8_bit -> "CT_8_BIT"

let to_yojson sources encoded =
  let location (location : Ast.location) =
    let span = location.span in
    let fields =
      [
        ("source_id", `Int (Common.Source_id.to_int span.source));
        ("start", `Int span.start);
        ("stop", `Int span.stop);
      ]
    in
    match Common.Source_manager.find sources span.source with
    | None -> `Assoc fields
    | Some source ->
        let position =
          match Common.Source_file.position source span.start with
          | Error _ -> []
          | Ok position ->
              [ ("line", `Int position.line); ("column", `Int position.column) ]
        in
        `Assoc
          ((("path", `String (Common.Source_file.display_path source)) :: fields)
          @ position)
  in
  let instruction (entry : instruction) =
    let selection = entry.selection in
    `Assoc
      [
        ("item_index", `Int entry.item_index);
        ("position", `String (address entry.position));
        ("offset", `Int entry.offset);
        ("size", `Int entry.size);
        ( "bytes",
          `String (hex (String.sub encoded.bytes entry.offset entry.size)) );
        ( "opcode",
          `String (Asm.Opcode.resolved_source_spelling selection.opcode) );
        ("mode", `Int (Asm.Operand.mode_bits selection.mode));
        ("form_entry_index", `Int selection.form.entry_index);
        ("form_source_line", `Int selection.form.source_line);
      ]
  in
  let entry (entry : entry) =
    `Assoc
      [
        ("item_index", `Int entry.item_index);
        ("kind", `String entry.kind);
        ("position", `String (address entry.position));
        ("offset", `Int entry.offset);
        ("size", `Int entry.size);
        ("listing", `Bool entry.listing);
        ("location", location entry.location);
        ( "bytes",
          `String (hex (String.sub encoded.bytes entry.offset entry.size)) );
      ]
  in
  let symbol (symbol : symbol) =
    `Assoc
      [
        ("name", `String symbol.name);
        ( "scope",
          Option.fold ~none:`Null
            ~some:(fun scope -> `String scope)
            symbol.scope );
        ("exported", `Bool symbol.exported);
        ("position", `String (address symbol.position));
        ("item_index", `Int symbol.item_index);
      ]
  in
  let relocation (relocation : relocation) =
    `Assoc
      [
        ("offset", `Int relocation.offset);
        ("width", `Int relocation.width);
        ("kind", `String relocation.kind);
        ( "symbol",
          Option.fold ~none:`Null
            ~some:(fun name -> `String name)
            relocation.symbol );
      ]
  in
  let binary_input (input : binary_input) =
    let value = input.provenance in
    `Assoc
      [
        ("item_index", `Int input.item_index);
        ("spelling", `String value.spelling);
        ("resolved_path", `String value.resolved_path);
        ("display_path", `String value.display_path);
        ("raw_size", `Int value.raw_size);
        ("decoded_size", `Int value.decoded_size);
        ("raw_sha256", `String value.raw_sha256);
        ("decoded_sha256", `String value.decoded_sha256);
        ("compression", `String (compression_name value.compression));
      ]
  in
  `Assoc
    [
      ("schema", `String schema);
      ("reference_commit", `String Generated.Opcode_keywords.reference_commit);
      ("opcode_source_sha256", `String Generated.Opcode_keywords.source_sha256);
      ("position", `String (address encoded.position));
      ("end_position", `String (address encoded.end_position));
      ("code_size", `Int (String.length encoded.bytes));
      ("bytes", `String (hex encoded.bytes));
      ("instructions", `List (List.map instruction encoded.instructions));
      ("entries", `List (List.map entry encoded.entries));
      ( "origin",
        Option.fold ~none:`Null
          ~some:(fun origin -> `String (address origin))
          encoded.origin );
      ("max_align_bits", `Int encoded.max_align_bits);
      ("imports", `List (List.map (fun name -> `String name) encoded.imports));
      ("symbols", `List (List.map symbol encoded.symbols));
      ("relocations", `List (List.map relocation encoded.relocations));
      ("binary_inputs", `List (List.map binary_input encoded.binary_inputs));
      ("work", `Int encoded.work);
      ("input_bytes", `Int encoded.input_bytes);
      ("assembly", Frontend.Ast_dump.assembly_to_yojson sources encoded.assembly);
    ]

let human encoded =
  let buffer = Buffer.create 256 in
  Printf.bprintf buffer
    "%s\nreference %s\nposition %s\nend-position %s\ncode-bytes %d\n" schema
    Generated.Opcode_keywords.reference_commit (address encoded.position)
    (address encoded.end_position)
    (String.length encoded.bytes);
  let instructions = ref encoded.instructions in
  List.iter
    (fun (entry : entry) ->
      match !instructions with
      | (instruction : instruction) :: rest
        when instruction.item_index = entry.item_index ->
          instructions := rest;
          let selection = instruction.selection in
          Printf.bprintf buffer "%s  %-30s  %s USE%d OpCodes.DD:%d listing=%b\n"
            (address entry.position)
            (hex (String.sub encoded.bytes entry.offset entry.size))
            (Asm.Opcode.resolved_source_spelling selection.opcode)
            (Asm.Operand.mode_bits selection.mode)
            selection.form.source_line entry.listing
      | _ ->
          Printf.bprintf buffer "%s  %s bytes=%d offset=%d listing=%b\n"
            (address entry.position) entry.kind entry.size entry.offset
            entry.listing)
    encoded.entries;
  Option.iter
    (fun origin ->
      Printf.bprintf buffer "origin %s (unrelocated image)\n" (address origin))
    encoded.origin;
  Printf.bprintf buffer
    "directive-work %d\nbinary-input-bytes %d\nrelocations %d\n" encoded.work
    encoded.input_bytes
    (List.length encoded.relocations);
  Buffer.contents buffer
