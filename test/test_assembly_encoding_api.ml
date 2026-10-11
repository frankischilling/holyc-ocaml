open Holyc_lib
module Operand = Asm_operand
module Encoding = Assembly_encoding

let parse ?mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"assembly-encoding-api.ASM" ~contents
  in
  let config =
    match Preprocessor.Config.create ~working_directory:(Sys.getcwd ()) () with
    | Ok config -> config
    | Error message -> Alcotest.fail message
  in
  let output =
    Holyc_lib.parse_assembly_detailed ?mode session ~config ~source
  in
  match output.Parser.assembly with
  | Some assembly -> (session, assembly)
  | None ->
      Alcotest.failf "assembly fixture did not parse: %s"
        (String.concat "; "
           (List.map
              (fun (diagnostic : Diagnostic.t) ->
                diagnostic.code ^ ": " ^ diagnostic.message)
              output.diagnostics))

let operations (assembly : Ast.assembly_unit) =
  List.filter_map
    (function
      | Ast.Inline_assembly_instruction operation -> Some operation
      | _ -> None)
    assembly.assembly_unit_items

let remake_unit ?items ?initial_mode ?final_mode (assembly : Ast.assembly_unit)
    =
  Ast.make_assembly_unit
    ~items:(Option.value ~default:assembly.assembly_unit_items items)
    ~lines:assembly.assembly_unit_lines
    ~initial_mode:
      (Option.value ~default:assembly.assembly_unit_initial_mode initial_mode)
    ~final_mode:
      (Option.value ~default:assembly.assembly_unit_final_mode final_mode)
    ~location:assembly.assembly_unit_location

let remake_operation ?opcode ?mode ?selection ?operands
    (operation : Ast.inline_assembly_operation) =
  Ast.make_inline_assembly_operation
    ~opcode:(Option.value ~default:operation.inline_assembly_opcode opcode)
    ~mode:(Option.value ~default:operation.inline_assembly_mode mode)
    ~selection:
      (Option.value ~default:operation.inline_assembly_selection selection)
    ~operands:
      (Option.value ~default:operation.inline_assembly_operands operands)
    ~separator:operation.inline_assembly_separator
    ~semicolon:operation.inline_assembly_semicolon
    ~location:operation.inline_assembly_operation_location

let remake_directive ?kind ?token ?arguments
    (directive : Ast.inline_assembly_directive) =
  Ast.make_inline_assembly_directive
    ~kind:(Option.value ~default:directive.inline_assembly_directive_kind kind)
    ~directive:
      (Option.value ~default:directive.inline_assembly_directive_token token)
    ~arguments:
      (Option.value ~default:directive.inline_assembly_directive_arguments
         arguments)
    ~type_definitions:directive.inline_assembly_directive_type_definitions
    ~separators:directive.inline_assembly_directive_separators
    ~semicolon:directive.inline_assembly_directive_semicolon
    ~location:directive.inline_assembly_directive_location

let remake_token ~kind (token : Ast.assembly_token) =
  Ast.make_assembly_token ~kind ~source_token:token.assembly_source_token
    ~location:token.assembly_token_location

let contains text fragment =
  let rec at offset =
    offset + String.length fragment <= String.length text
    && (String.sub text offset (String.length fragment) = fragment
       || at (offset + 1))
  in
  at 0

let encode ?position ?(max_code_bytes = 32) assembly =
  match Encoding.encode ?position ~max_code_bytes assembly with
  | Ok encoded -> encoded
  | Error diagnostic ->
      Alcotest.failf "assembly encoding failed: %s: %s" diagnostic.code
        diagnostic.message

let reject ?position ?(max_code_bytes = 32) label ~code ~message assembly =
  match Encoding.encode ?position ~max_code_bytes assembly with
  | Ok encoded ->
      Alcotest.failf "%s unexpectedly emitted %d bytes" label
        (String.length encoded.bytes)
  | Error diagnostic ->
      Alcotest.(check string) (label ^ " diagnostic code") code diagnostic.code;
      Alcotest.(check bool)
        (label ^ ": " ^ diagnostic.message)
        true
        (contains diagnostic.message message);
      Alcotest.(check bool)
        (label ^ " error severity")
        true
        (diagnostic.severity = Diagnostic.Error);
      diagnostic

let limits_and_positions () =
  let _, assembly = parse "MOV RAX,RBX; RET;" in
  let expected = "\x48\x8b\xc3\xc3" in
  let encoded = encode ~max_code_bytes:4 assembly in
  Alcotest.(check string) "exact aggregate limit" expected encoded.bytes;
  let last = List.nth (operations assembly) 1 in
  let over_limit =
    reject ~max_code_bytes:3 "aggregate limit one byte short" ~code:"HCASM0002"
      ~message:"sequence exceeds" assembly
  in
  Alcotest.(check bool)
    "aggregate limit identifies the first instruction that does not fit" true
    (over_limit.primary = last.inline_assembly_operation_location.span);
  List.iter
    (fun max_code_bytes ->
      ignore
        (reject ~max_code_bytes "invalid public output limit" ~code:"HCASM0002"
           ~message:"limit must be between" assembly))
    [ 0; Asm_encoder.max_code_bytes + 1 ];
  ignore
    (reject ~position:(-1L) "negative output position" ~code:"HCASM0003"
       ~message:"nonnegative" assembly);
  let final =
    encode ~position:(Int64.sub Int64.max_int 4L) ~max_code_bytes:4 assembly
  in
  Alcotest.(check int64)
    "instruction sequence ending at the last signed address" Int64.max_int
    final.end_position;
  let overflow =
    reject
      ~position:(Int64.sub Int64.max_int 3L)
      "later instruction overflows the output position" ~code:"HCASM0003"
      ~message:"position exceeds" assembly
  in
  Alcotest.(check bool)
    "position overflow identifies the instruction crossing the boundary" true
    (overflow.primary = last.inline_assembly_operation_location.span);
  let _, nop = parse "NOP;" in
  ignore
    (reject ~position:Int64.max_int "first instruction end overflows"
       ~code:"HCASM0003" ~message:"position exceeds" nop)

let operation_ownership () =
  let _, assembly = parse "MOV RAX,RBX; RET;" in
  let _, repeated = parse "MOV RAX,RBX; RET;" in
  let operation = List.hd (operations assembly) in
  let repeated_operation = List.hd (operations repeated) in
  let first_operand =
    (List.hd operation.inline_assembly_operands).inline_assembly_operand
  in
  let repeated_operand =
    (List.hd repeated_operation.inline_assembly_operands)
      .inline_assembly_operand
  in
  Alcotest.(check bool)
    "independent parses retain equal operand tokens" true
    (Operand.tokens first_operand = Operand.tokens repeated_operand);
  Alcotest.(check bool)
    "independent parses have distinct operand ownership" false
    (first_operand == repeated_operand);
  let one operation =
    remake_unit ~items:[ Ast.Inline_assembly_instruction operation ] assembly
  in
  ignore (encode (one (remake_operation operation)));
  let opcode = operation.inline_assembly_opcode in
  let return_opcode =
    (List.nth (operations assembly) 1).inline_assembly_opcode
  in
  List.iter
    (fun (label, forged) ->
      ignore
        (reject label ~code:"HCASM0005" ~message:"does not own" (one forged)))
    [
      ( "selection from an independent parse",
        remake_operation ~selection:repeated_operation.inline_assembly_selection
          operation );
      ( "equal operands from an independent parse",
        remake_operation ~operands:repeated_operation.inline_assembly_operands
          operation );
      ( "reordered owned operands",
        remake_operation
          ~operands:(List.rev operation.inline_assembly_operands)
          operation );
      ( "missing selected operand",
        remake_operation
          ~operands:(List.tl operation.inline_assembly_operands)
          operation );
      ("another opcode token", remake_operation ~opcode:return_opcode operation);
      ( "opcode token with a false canonical spelling",
        remake_operation
          ~opcode:
            (remake_token opcode
               ~kind:
                 (Ast.Assembly_opcode_token
                    { canonical_spelling = "RET"; source_is_alias = false }))
          operation );
      ( "opcode token with a false alias flag",
        remake_operation
          ~opcode:
            (remake_token opcode
               ~kind:
                 (Ast.Assembly_opcode_token
                    { canonical_spelling = "MOV"; source_is_alias = true }))
          operation );
      ( "non-opcode token with opcode spelling",
        remake_operation
          ~opcode:(remake_token opcode ~kind:Ast.Assembly_identifier_token)
          operation );
    ];
  ignore
    (reject "operation mode differs from its selection" ~code:"HCASM0005"
       ~message:"does not own"
       (one (remake_operation ~mode:Operand.Use32 operation)
       |> remake_unit ~initial_mode:Operand.Use32 ~final_mode:Operand.Use32));
  ignore
    (reject "initial mode differs from the first operation" ~code:"HCASM0005"
       ~message:"does not own"
       (remake_unit ~initial_mode:Operand.Use32 assembly));
  ignore
    (reject "final mode differs from the ordered operations" ~code:"HCASM0005"
       ~message:"final mode"
       (remake_unit ~final_mode:Operand.Use32 assembly))

let use_directive_ownership () =
  let _, modes = parse "USE16; NOP; USE32; NOP; USE64; RET;" in
  let encoded = encode ~max_code_bytes:3 modes in
  Alcotest.(check string)
    "ordered USE directives and separators emit only instructions"
    "\x90\x90\xc3" encoded.bytes;
  Alcotest.(check (list int))
    "operations retain each active mode" [ 16; 32; 64 ]
    (List.map
       (fun (entry : Encoding.instruction) ->
         Operand.mode_bits entry.operation.inline_assembly_mode)
       encoded.instructions);
  let _, assembly = parse "USE16;" in
  let directive =
    List.find_map
      (function
        | Ast.Inline_assembly_directive value -> Some value
        | _ -> None)
      assembly.assembly_unit_items
    |> Option.get
  in
  let one ?final_mode directive =
    remake_unit ?final_mode
      ~items:[ Ast.Inline_assembly_directive directive ]
      assembly
  in
  ignore (encode (one (remake_directive directive)));
  ignore
    (reject "invalid USE width" ~code:"HCASM0005" ~message:"invalid mode"
       (one
          (remake_directive
             ~kind:
               (Ast.Inline_assembly_use_directive { segment_width_bits = 8 })
             directive)));
  ignore
    (reject "USE width contradicts its source token" ~code:"HCASM0005"
       ~message:"mode token"
       (one ~final_mode:Operand.Use32
          (remake_directive
             ~kind:
               (Ast.Inline_assembly_use_directive { segment_width_bits = 32 })
             directive)));
  let token = directive.inline_assembly_directive_token in
  let use32_id =
    Asm_directive.find "USE32" |> Option.get |> Asm_directive.templeos_id
  in
  List.iter
    (fun (label, forged) ->
      ignore (reject label ~code:"HCASM0005" ~message:"mode token" (one forged)))
    [
      ("USE with arguments", remake_directive ~arguments:[ token ] directive);
      ( "USE token with another original directive identifier",
        remake_directive
          ~token:
            (remake_token token
               ~kind:(Ast.Assembly_directive_token { templeos_id = use32_id }))
          directive );
      ( "USE spelling in a non-directive token",
        remake_directive
          ~token:(remake_token token ~kind:Ast.Assembly_identifier_token)
          directive );
    ]

let layout_refusals () =
  List.iter
    (fun (label, contents, message) ->
      let _, assembly = parse contents in
      ignore (reject label ~code:"HCASM0004" ~message assembly))
    [
      ("label after an instruction", "NOP; entry_label: RET;", "label layout");
      ("data after an instruction", "NOP; DU8 1; RET;", "directive execution");
      ("origin after an instruction", "NOP; ORG 16; RET;", "directive execution");
      ("listing directive", "NOP; LIST; RET;", "directive execution");
    ]

let retained_source_metadata () =
  let session, assembly = parse "MOV RAX,RBX; RET;" in
  let encoded = encode ~position:0x100L ~max_code_bytes:4 assembly in
  Alcotest.(check bool)
    "encoding retains its exact assembly" true
    (encoded.assembly == assembly);
  let original_operations = operations assembly in
  Alcotest.(check int)
    "one receipt per original instruction"
    (List.length original_operations)
    (List.length encoded.instructions);
  List.iter2
    (fun (entry : Encoding.instruction) original ->
      Alcotest.(check bool)
        "instruction receipt retains the original operation" true
        (entry.operation == original))
    encoded.instructions original_operations;
  Alcotest.(check (list int))
    "instruction offsets" [ 0; 3 ]
    (List.map
       (fun (entry : Encoding.instruction) -> entry.offset)
       encoded.instructions);
  Alcotest.(check (list int64))
    "instruction positions" [ 0x100L; 0x103L ]
    (List.map
       (fun (entry : Encoding.instruction) -> entry.position)
       encoded.instructions);
  let item_indices =
    assembly.assembly_unit_items
    |> List.mapi (fun index -> function
      | Ast.Inline_assembly_instruction _ -> Some index
      | _ -> None)
    |> List.filter_map Fun.id
  in
  Alcotest.(check (list int))
    "instruction receipts refer to original item indices" item_indices
    (List.map
       (fun (entry : Encoding.instruction) -> entry.item_index)
       encoded.instructions);
  let sources = Session.sources session in
  let json = Encoding.to_yojson sources encoded in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "report retains the complete original AST"
    (Ast_dump.assembly_to_yojson sources assembly |> Yojson.Safe.to_string)
    (json |> member "assembly" |> Yojson.Safe.to_string);
  Alcotest.(check string)
    "reported bytes" "488bc3c3"
    (json |> member "bytes" |> to_string);
  Alcotest.(check int)
    "reported byte count" 4
    (json |> member "code_size" |> to_int);
  Alcotest.(check string)
    "reported initial position" "0x0000000000000100"
    (json |> member "position" |> to_string);
  Alcotest.(check string)
    "reported end position" "0x0000000000000104"
    (json |> member "end_position" |> to_string);
  Alcotest.(check (list string))
    "reported instruction byte ranges" [ "488bc3"; "c3" ]
    (json |> member "instructions" |> to_list
    |> List.map (fun item -> item |> member "bytes" |> to_string))

let tests =
  [
    Alcotest.test_case "public aggregate limits and positions" `Quick
      limits_and_positions;
    Alcotest.test_case
      "operation selections retain opcode and operand ownership" `Quick
      operation_ownership;
    Alcotest.test_case "ordered USE directives retain their mode tokens" `Quick
      use_directive_ownership;
    Alcotest.test_case "labels and directives require layout" `Quick
      layout_refusals;
    Alcotest.test_case "encoding reports retain their exact parsed source"
      `Quick retained_source_metadata;
  ]
