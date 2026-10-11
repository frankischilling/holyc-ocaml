module Ast = Frontend.Ast
module Encoder = Asm.Encoder

type instruction = {
  item_index : int;
  operation : Ast.inline_assembly_operation;
  position : int64;
  offset : int;
  size : int;
}

type t = {
  assembly : Ast.assembly_unit;
  bytes : string;
  position : int64;
  end_position : int64;
  instructions : instruction list;
}

let diagnostic ~code (location : Ast.location) message =
  let related message span = Common.Diagnostic.{ span; message } in
  let secondary =
    Option.to_list
      (Option.map (related "expanded from here") location.generated_from)
    @ Option.to_list (Option.map (related "defined here") location.defined_at)
  in
  Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error ~message
    ~primary:location.span ~secondary ()

let operation_matches_selection (operation : Ast.inline_assembly_operation) =
  let selection = operation.inline_assembly_selection in
  let opcode = operation.inline_assembly_opcode in
  operation.inline_assembly_mode = selection.mode
  && opcode.assembly_source_token.raw
     = Asm.Opcode.resolved_source_spelling selection.opcode
  && (match opcode.assembly_token_kind with
    | Ast.Assembly_opcode_token { canonical_spelling; source_is_alias } ->
        canonical_spelling
        = Asm.Opcode.spelling (Asm.Opcode.resolved_opcode selection.opcode)
        && source_is_alias = Asm.Opcode.resolved_is_alias selection.opcode
    | _ -> false)
  && List.length operation.inline_assembly_operands
     = List.length selection.operands
  && List.for_all2
       (fun (operand : Ast.inline_assembly_operand) selected ->
         operand.inline_assembly_operand == selected)
       operation.inline_assembly_operands selection.operands

let use_directive_matches (directive : Ast.inline_assembly_directive) bits =
  let token = directive.inline_assembly_directive_token in
  let spelling = Printf.sprintf "USE%d" bits in
  token.assembly_source_token.raw = spelling
  && directive.inline_assembly_directive_arguments = []
  && directive.inline_assembly_directive_type_definitions = []
  &&
  match (token.assembly_token_kind, Asm.Directive.find spelling) with
  | Ast.Assembly_directive_token { templeos_id }, Some original ->
      templeos_id = Asm.Directive.templeos_id original
  | _ -> false

let encode ?(position = 0L) ~max_code_bytes (assembly : Ast.assembly_unit) =
  let reject ?(code = "HCASM0001") location message =
    Error (diagnostic ~code location message)
  in
  let root = assembly.assembly_unit_location in
  if max_code_bytes <= 0 || max_code_bytes > Encoder.max_code_bytes then
    reject ~code:"HCASM0002" root
      (Printf.sprintf "assembly code byte limit must be between 1 and %d"
         Encoder.max_code_bytes)
  else if position < 0L then
    reject ~code:"HCASM0003" root
      "assembly output position must be a nonnegative signed 64-bit value"
  else
    let rec prepare item_index offset current mode reversed_plans
        reversed_entries = function
      | [] -> (
          let plans = List.rev reversed_plans in
          if mode <> assembly.assembly_unit_final_mode then
            reject ~code:"HCASM0005" root
              "assembly final mode does not match its ordered operations"
          else
            match Encoder.encode_all ~max_bytes:max_code_bytes plans with
            | Error error -> reject root error.message
            | Ok bytes ->
                Ok
                  {
                    assembly;
                    bytes;
                    position;
                    end_position = current;
                    instructions = List.rev reversed_entries;
                  })
      | Ast.Inline_assembly_empty _ :: rest ->
          prepare (item_index + 1) offset current mode reversed_plans
            reversed_entries rest
      | Ast.Inline_assembly_directive directive :: rest -> (
          match directive.inline_assembly_directive_kind with
          | Ast.Inline_assembly_use_directive { segment_width_bits } -> (
              let mode =
                match segment_width_bits with
                | 16 -> Some Asm.Operand.Use16
                | 32 -> Some Asm.Operand.Use32
                | 64 -> Some Asm.Operand.Use64
                | _ -> None
              in
              match mode with
              | Some _
                when not (use_directive_matches directive segment_width_bits) ->
                  reject ~code:"HCASM0005"
                    directive.inline_assembly_directive_location
                    "assembly USE directive does not own its mode token"
              | Some mode ->
                  prepare (item_index + 1) offset current mode reversed_plans
                    reversed_entries rest
              | None ->
                  reject ~code:"HCASM0005"
                    directive.inline_assembly_directive_location
                    "assembly USE directive has an invalid mode")
          | _ ->
              reject ~code:"HCASM0004"
                directive.inline_assembly_directive_location
                "this assembly directive requires directive execution or layout"
          )
      | Ast.Inline_assembly_label label :: _ ->
          reject ~code:"HCASM0004" label.assembly_label_location
            "assembly labels require label layout and fixup resolution"
      | Ast.Inline_assembly_instruction operation :: rest -> (
          let location = operation.inline_assembly_operation_location in
          if
            operation.inline_assembly_mode <> mode
            || not (operation_matches_selection operation)
          then
            reject ~code:"HCASM0005" location
              "assembly operation does not own its selected opcode and operands"
          else
            let selection = operation.inline_assembly_selection in
            if selection.encoded_bytes > max_code_bytes - offset then
              reject ~code:"HCASM0002" location
                "assembly instruction sequence exceeds its code byte limit"
            else if
              current
              > Int64.sub Int64.max_int (Int64.of_int selection.encoded_bytes)
            then
              reject ~code:"HCASM0003" location
                "assembly output position exceeds the signed 64-bit range"
            else
              match Encoder.plan ~position:current selection with
              | Error error ->
                  let location =
                    match error.operand_index with
                    | Some index -> (
                        match
                          List.nth_opt operation.inline_assembly_operands index
                        with
                        | Some operand ->
                            operand.inline_assembly_operand_location
                        | None -> location)
                    | None -> location
                  in
                  reject location error.message
              | Ok plan ->
                  let size = Encoder.size plan in
                  let entry =
                    { item_index; operation; position = current; offset; size }
                  in
                  prepare (item_index + 1) (offset + size)
                    (Int64.add current (Int64.of_int size))
                    mode (plan :: reversed_plans)
                    (entry :: reversed_entries)
                    rest)
    in
    prepare 0 0 position assembly.assembly_unit_initial_mode [] []
      assembly.assembly_unit_items

let schema = "holyc-assembly-encoding-v1"

let hex bytes =
  let digits = "0123456789abcdef" in
  String.init
    (String.length bytes * 2)
    (fun index ->
      let byte = Char.code bytes.[index / 2] in
      digits.[if index mod 2 = 0 then byte lsr 4 else byte land 15])

let address position = Printf.sprintf "0x%016Lx" position

let to_yojson sources encoded =
  let instruction (entry : instruction) =
    let selection = entry.operation.inline_assembly_selection in
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
      ("assembly", Frontend.Ast_dump.assembly_to_yojson sources encoded.assembly);
    ]

let human encoded =
  let buffer = Buffer.create 256 in
  Printf.bprintf buffer
    "%s\nreference %s\nposition %s\nend-position %s\ncode-bytes %d\n" schema
    Generated.Opcode_keywords.reference_commit (address encoded.position)
    (address encoded.end_position)
    (String.length encoded.bytes);
  List.iter
    (fun (entry : instruction) ->
      let selection = entry.operation.inline_assembly_selection in
      Printf.bprintf buffer "%s  %-30s  %s USE%d OpCodes.DD:%d\n"
        (address entry.position)
        (hex (String.sub encoded.bytes entry.offset entry.size))
        (Asm.Opcode.resolved_source_spelling selection.opcode)
        (Asm.Operand.mode_bits selection.mode)
        selection.form.source_line)
    encoded.instructions;
  Buffer.contents buffer
