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

type linked = {
  image : t;
  bytes : string;
  base : int64;
  position : int64;
  end_position : int64;
  symbols : symbol list;
}

let link ?(symbols = []) ?max_work ~base (image : t) =
  let root = image.assembly.assembly_unit_location in
  try
    if base < 0L || base > Int64.sub Int64.max_int image.end_position then
      reject ~code:"HCASM0009" root
        "assembly load base and image end exceed the nonnegative signed \
         address range";
    let records =
      List.map
        (fun (relocation : relocation) ->
          let kind =
            match relocation.symbol with
            | Some name ->
                if Option.is_some relocation.relative_base then
                  Asm.Relocation.Relative_import name
                else Asm.Relocation.Absolute_import name
            | None ->
                if relocation.type_id land 1 = 0 then Asm.Relocation.Add_base
                else Asm.Relocation.Subtract_base
          in
          match
            Asm.Relocation.make ~offset:relocation.offset
              ~width:relocation.width ~kind
          with
          | Ok record -> record
          | Error message ->
              reject ~code:"HCASM0009" relocation.location message)
        image.relocations
    in
    match
      Asm.Relocation.apply ?max_work ~position:image.position ~base ~symbols
        ~bytes:image.bytes records
    with
    | Error error ->
        let location =
          Option.bind error.relocation_index (List.nth_opt image.relocations)
          |> Option.fold ~none:root ~some:(fun (relocation : relocation) ->
              relocation.location)
        in
        Error (diagnostic ~code:"HCASM0009" location error.message)
    | Ok bytes ->
        Ok
          {
            image;
            bytes;
            base;
            position = Int64.add base image.position;
            end_position = Int64.add base image.end_position;
            symbols =
              List.map
                (fun (symbol : symbol) ->
                  { symbol with position = Int64.add base symbol.position })
                image.symbols;
          }
  with Rejected diagnostic -> Error diagnostic

let compression_name = function
  | Frontend.Assembly_binary.Raw -> "raw"
  | Frontend.Assembly_binary.Ct_none -> "CT_NONE"
  | Frontend.Assembly_binary.Ct_7_bit -> "CT_7_BIT"
  | Frontend.Assembly_binary.Ct_8_bit -> "CT_8_BIT"

let to_yojson sources (encoded : t) =
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
        ( "initial_bytes",
          `String
            (hex (String.sub encoded.initial_bytes entry.offset entry.size)) );
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
        ("id", `Int symbol.id);
        ("name", `String symbol.name);
        ( "scope",
          Option.fold ~none:`Null
            ~some:(fun scope -> `String scope)
            symbol.scope );
        ("exported", `Bool symbol.exported);
        ("position", `String (address symbol.position));
        ("item_index", `Int symbol.item_index);
        ( "definition_item_indices",
          `List
            (List.map (fun index -> `Int index) symbol.definition_item_indices)
        );
      ]
  in
  let relocation (relocation : relocation) =
    `Assoc
      [
        ("offset", `Int relocation.offset);
        ("width", `Int relocation.width);
        ("kind", `String relocation.kind);
        ("type_id", `Int relocation.type_id);
        ("position", `String (address relocation.position));
        ( "relative_base",
          Option.fold ~none:`Null
            ~some:(fun value -> `String (address value))
            relocation.relative_base );
        ("value", `String (address relocation.value));
        ("location", location relocation.location);
        ( "symbol",
          Option.fold ~none:`Null
            ~some:(fun name -> `String name)
            relocation.symbol );
      ]
  in
  let fixup (fixup : fixup) =
    `Assoc
      [
        ("item_index", `Int fixup.item_index);
        ("offset", `Int fixup.offset);
        ("position", `String (address fixup.position));
        ("width", `Int fixup.width);
        ( "relative_base",
          Option.fold ~none:`Null
            ~some:(fun value -> `String (address value))
            fixup.relative_base );
        ("value", `String (address fixup.value));
        ("symbols", `List (List.map (fun name -> `String name) fixup.symbols));
        ("symbol_ids", `List (List.map (fun id -> `Int id) fixup.symbol_ids));
        ( "imported",
          Option.fold ~none:`Null
            ~some:(fun name -> `String name)
            fixup.imported );
        ("location", location fixup.location);
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
      ( "native_sections",
        match native_sections encoded with
        | Ok object_ -> Asm.Native_sections.to_yojson object_
        | Error _ -> `Null );
      ("reference_commit", `String Generated.Opcode_keywords.reference_commit);
      ("opcode_source_sha256", `String Generated.Opcode_keywords.source_sha256);
      ("position", `String (address encoded.position));
      ("end_position", `String (address encoded.end_position));
      ("code_size", `Int (String.length encoded.bytes));
      ("bytes", `String (hex encoded.bytes));
      ("initial_bytes", `String (hex encoded.initial_bytes));
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
      ("fixups", `List (List.map fixup encoded.fixups));
      ( "external_symbols",
        `List
          (List.map
             (fun (name, value) ->
               `Assoc
                 [ ("name", `String name); ("value", `String (address value)) ])
             encoded.external_symbols) );
      ("binary_inputs", `List (List.map binary_input encoded.binary_inputs));
      ("work", `Int encoded.work);
      ("input_bytes", `Int encoded.input_bytes);
      ("assembly", Frontend.Ast_dump.assembly_to_yojson sources encoded.assembly);
    ]

let human (encoded : t) =
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

let map (encoded : t) =
  let buffer = Buffer.create 256 in
  Printf.bprintf buffer "reference %s\nposition %s\nend-position %s\n"
    Generated.Opcode_keywords.reference_commit (address encoded.position)
    (address encoded.end_position);
  List.iter
    (fun (symbol : symbol) ->
      let kind =
        if symbol.exported then "export"
        else if
          Option.is_some symbol.scope
          || String.starts_with ~prefix:"@@" symbol.name
        then "local"
        else "global"
      in
      Printf.bprintf buffer "symbol %d %s %s %s scope=%s definitions=%s\n"
        symbol.id (address symbol.position) kind symbol.name
        (Option.value ~default:"<root>" symbol.scope)
        (String.concat ","
           (List.map string_of_int symbol.definition_item_indices)))
    encoded.symbols;
  List.iteri
    (fun index name -> Printf.bprintf buffer "import %d %s\n" index name)
    encoded.imports;
  List.iter
    (fun (name, value) ->
      Printf.bprintf buffer "external %s %s\n" (address value) name)
    encoded.external_symbols;
  List.iter
    (fun (relocation : relocation) ->
      Printf.bprintf buffer "relocation %s offset=%d width=%d %s symbol=%s\n"
        (address relocation.position)
        relocation.offset relocation.width relocation.kind
        (Option.value ~default:"<image>" relocation.symbol))
    encoded.relocations;
  Buffer.contents buffer

let listing sources (encoded : t) =
  let buffer = Buffer.create 256 in
  Buffer.add_string buffer "Final assembly bytes at raw coordinates\n";
  List.iter
    (fun (entry : entry) ->
      if entry.listing then
        let span = entry.location.span in
        let source =
          match Common.Source_manager.find sources span.source with
          | None ->
              Printf.sprintf "source:%d" (Common.Source_id.to_int span.source)
          | Some file -> (
              let path = Common.Source_file.display_path file in
              match Common.Source_file.position file span.start with
              | Error _ -> path
              | Ok position ->
                  Printf.sprintf "%s:%d:%d" path position.line position.column)
        in
        if entry.size = 0 then
          Printf.bprintf buffer "%s  %-16s  %s %s\n" (address entry.position) ""
            source entry.kind
        else
          let cursor = ref 0 in
          while !cursor < entry.size do
            let count = min 8 (entry.size - !cursor) in
            Printf.bprintf buffer "%s  %-16s  %s %s\n"
              (address (Int64.add entry.position (Int64.of_int !cursor)))
              (hex (String.sub encoded.bytes (entry.offset + !cursor) count))
              source entry.kind;
            cursor := !cursor + count
          done)
    encoded.entries;
  Buffer.contents buffer

let linked_to_yojson sources (linked : linked) =
  `Assoc
    [
      ("schema", `String "holyc-linked-assembly-v1");
      ("reference_commit", `String Generated.Opcode_keywords.reference_commit);
      ("base", `String (address linked.base));
      ("position", `String (address linked.position));
      ("end_position", `String (address linked.end_position));
      ("code_size", `Int (String.length linked.bytes));
      ("bytes", `String (hex linked.bytes));
      ( "symbols",
        `List
          (List.map
             (fun (symbol : symbol) ->
               `Assoc
                 [
                   ("id", `Int symbol.id);
                   ("name", `String symbol.name);
                   ("position", `String (address symbol.position));
                   ("exported", `Bool symbol.exported);
                 ])
             linked.symbols) );
      ("image", to_yojson sources linked.image);
    ]

let linked_human (linked : linked) =
  Printf.sprintf
    "%sload-base %s\n\
     loaded-position %s\n\
     loaded-end-position %s\n\
     loaded-bytes %s\n"
    (human linked.image) (address linked.base) (address linked.position)
    (address linked.end_position)
    (hex linked.bytes)
