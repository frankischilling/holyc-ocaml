module Lowering = Holyc_lib.Ir_goto_label_lowering
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Labels = Holyc_lib.Semantic_label_resolution
module Symbol = Holyc_lib.Semantic_symbol
module Symbol_table = Holyc_lib.Semantic_symbol_table
module Preprocessor = Holyc_lib.Preprocessor

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temp_directory run =
  let path = Filename.temp_dir "holyc-ir-labels-" "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> run path)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let show_sequence_error (error : Sequence.error) =
  error.code ^ ": " ^ error.message

let show_sequence_errors errors =
  String.concat "; " (List.map show_sequence_error errors)

let check_code description expected actual =
  Alcotest.(check string) description expected actual

let expect_ast = function
  | Ok ast -> ast
  | Error diagnostics ->
      Alcotest.failf "expected an AST, got %d diagnostics"
        (List.length diagnostics)

let config ?working_directory mode =
  checked
    (Preprocessor.Config.create ?working_directory ~compilation_mode:mode ())

let resolve_function ?working_directory ~mode ~path source name =
  let session = Holyc_lib.Session.create () in
  let file = Holyc_lib.Session.add_source session ~path ~contents:source in
  let ast =
    let config = config ?working_directory mode in
    Holyc_lib.parse_with_config session ~config ~source:file |> expect_ast
  in
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let functions =
    checked (Holyc_lib.collect_functions session ~declarations ast)
  in
  let resolution = checked (Holyc_lib.resolve_labels session ~functions ast) in
  Labels.functions resolution
  |> List.find (fun function_ ->
      function_ |> Labels.function_symbol |> Symbol.name |> String.equal name)

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok show_sequence_error

let block_id value =
  Sequence.Block_id.of_int value |> require_ok show_sequence_error

let lower ?(instruction = 40) ?(block = 30) function_ =
  let instruction_id = instruction_id instruction in
  let block_id = block_id block in
  Lowering.lower_function_labels ~instruction_id ~block_id function_

let descriptions lowered =
  lowered |> Lowering.sequence |> Sequence.instructions
  |> List.map Sequence.description

let block_number = Sequence.Block_id.to_int

let payload_block (description : Sequence.description) =
  match description.payload with
  | Some (Sequence.Block block) -> block_number block
  | _ -> Alcotest.fail "goto or label has no block payload"

let opcode_name (description : Sequence.description) =
  Opcode.to_source_name description.opcode

let origin_span occurrence =
  match Labels.occurrence_origin occurrence with
  | Symbol.Source_location location -> Some location.span
  | Symbol.Pinned_source _ | Symbol.Synthesized _ -> None

let forward_and_backward_gotos () =
  let source = "U0 Flow(){goto later;back:goto back;later:goto later;}" in
  List.iter
    (fun mode ->
      let function_ =
        resolve_function ~mode ~path:"goto-flow.HC" source "Flow"
      in
      let lowered = lower function_ |> require_ok show_sequence_errors in
      let items = descriptions lowered in
      Alcotest.(check (list string))
        "source-ordered label and goto opcodes"
        [ "IC_JMP"; "IC_LABEL"; "IC_JMP"; "IC_LABEL"; "IC_JMP" ]
        (List.map opcode_name items);
      Alcotest.(check (list int))
        "every occurrence uses its assigned block" [ 30; 31; 31; 30; 30 ]
        (List.map payload_block items);
      let assignments =
        Lowering.label_blocks lowered
        |> List.map (fun (symbol, block) ->
            let name = Symbol.name symbol in
            (name, block_number block))
      in
      let expected_assignments = [ ("later", 30); ("back", 31) ] in
      Alcotest.(check (list (pair string int)))
        "first occurrence controls block assignment" expected_assignments
        assignments;
      let occurrences = Labels.function_occurrences function_ in
      Alcotest.(check bool)
        "instructions keep exact occurrence spans" true
        (List.map (fun item -> item.Sequence.span) items
        = List.map origin_span occurrences);
      Alcotest.(check (list int64))
        "goto and label flags are clear" [ 0L; 0L; 0L; 0L; 0L ]
        (List.map (fun item -> item.Sequence.flags) items);
      let next_instruction =
        Lowering.next_instruction_id lowered |> Sequence.Instruction_id.to_int
      in
      Alcotest.(check int)
        "instruction identities advance through every occurrence" 45
        next_instruction;
      Alcotest.(check int)
        "block identities advance through every label" 32
        (Lowering.next_block_id lowered |> block_number))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deterministic_dump () =
  let jump_definition = "#define JUMP goto done" in
  let destination_definition = "#define DEST done:" in
  let replay_function = "U0 Replay(){JUMP;DEST}" in
  let source =
    String.concat "\n"
      [ jump_definition; destination_definition; replay_function ]
  in
  let function_ =
    resolve_function ~mode:Preprocessor.Jit ~path:"goto-replay.HC" source
      "Replay"
  in
  let first = lower ~instruction:7 ~block:9 function_ in
  let second = lower ~instruction:7 ~block:9 function_ in
  let first = first |> require_ok show_sequence_errors |> Lowering.human in
  let second = second |> require_ok show_sequence_errors |> Lowering.human in
  Alcotest.(check string) "goto lowering replays deterministically" first second;
  Alcotest.(check bool)
    "dump records the assigned label block" true
    (String.contains first '^');
  let occurrences = Labels.function_occurrences function_ in
  Alcotest.(check bool)
    "definition-backed occurrences keep complete provenance" true
    (List.for_all
       (fun occurrence ->
         match Labels.occurrence_origin occurrence with
         | Symbol.Source_location location -> Option.is_some location.defined_at
         | Symbol.Pinned_source _ | Symbol.Synthesized _ -> false)
       occurrences);
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      let included_file = Filename.concat root "labels.HC" in
      let root_source = "#include \"labels\"" in
      write_file root_file root_source;
      write_file included_file "U0 Included(){goto done;done:}";
      let included =
        resolve_function ~working_directory:root ~mode:Preprocessor.Jit
          ~path:root_file root_source "Included"
      in
      let lowered = lower included |> require_ok show_sequence_errors in
      Alcotest.(check (list string))
        "included labels lower through the same checked path"
        [ "IC_JMP"; "IC_LABEL" ]
        (lowered |> descriptions |> List.map opcode_name))

let make_manual_function ?(definitions = 1) definition_kind =
  let table = Symbol_table.create () in
  let root = Symbol_table.root table in
  let module_scope =
    Symbol_table.create_scope table ~parent:root ~kind:Symbol_table.Module
      ~name:"manual.HC" ()
    |> checked
  in
  let function_symbol =
    Symbol_table.add table ~scope:module_scope ~name:"Manual"
      ~kind:Symbol.Function ~origin:(Symbol.Synthesized "manual function")
    |> checked
  in
  let function_scope =
    Symbol_table.create_scope table ~parent:module_scope
      ~kind:Symbol_table.Function ~name:"Manual" ()
    |> checked
  in
  let occurrences =
    List.init definitions (fun occurrence_index ->
        Labels.make_definition ~name:"target" ~definition_kind
          ~origin:(Symbol.Synthesized "manual definition") ~occurrence_index
        |> checked)
  in
  let facts =
    Labels.make_function ~symbol:function_symbol ~scope:function_scope
      ~item_index:0 occurrences
    |> checked
  in
  let resolution = Labels.resolve ~table [ facts ] |> checked in
  Labels.functions resolution |> List.hd

let unsupported_and_incomplete_metadata () =
  let expect_unsupported description function_ =
    match lower function_ with
    | Error [ (error : Sequence.error) ] ->
        Alcotest.(check string) description "HCIRL0006" error.code
    | Error errors ->
        Alcotest.failf "expected one assembly error, got %d"
          (List.length errors)
    | Ok _ -> Alcotest.fail "assembly label lowering was accepted"
  in
  make_manual_function Labels.Assembly_local_label
  |> expect_unsupported "assembly form code";
  make_manual_function ~definitions:2 Labels.Assembly_global_label
  |> expect_unsupported "repeated assembly definition code";
  let language = make_manual_function Labels.Language_label in
  match lower language with
  | Error [ (error : Sequence.error) ] ->
      Alcotest.(check string) "metadata code" "HCIRL0004" error.code
  | Error errors ->
      Alcotest.failf "expected one metadata error, got %d" (List.length errors)
  | Ok _ -> Alcotest.fail "incomplete label metadata was accepted"

let identity_exhaustion () =
  let source = "U0 Limits(){first:goto second;second:}" in
  let function_ =
    resolve_function ~mode:Preprocessor.Jit ~path:"goto-limits.HC" source
      "Limits"
  in
  List.iter
    (fun result ->
      match result with
      | Error [ (error : Sequence.error) ] ->
          check_code "identity exhaustion code" "HCIRL0005" error.code
      | Error errors ->
          Alcotest.failf "expected one exhaustion error, got %d"
            (List.length errors)
      | Ok _ -> Alcotest.fail "goto or label identity exhaustion was accepted")
    [
      lower ~instruction:Int.max_int function_;
      lower ~block:(Int.max_int - 1) function_;
    ]

let tests =
  [
    Alcotest.test_case "forward and backward gotos" `Quick
      forward_and_backward_gotos;
    Alcotest.test_case "deterministic dump" `Quick deterministic_dump;
    Alcotest.test_case "unsupported forms and metadata" `Quick
      unsupported_and_incomplete_metadata;
    Alcotest.test_case "identity exhaustion" `Quick identity_exhaustion;
  ]
