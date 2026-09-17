open Holyc_lib
module Source = Holyc_lib__Driver.Integer_source
module Indexed = Holyc_lib__Driver.Label_resolution
module Lower = Holyc_lib__Ir.Integer_program_lowering
module Labels = Semantic_label_resolution
module Frame = Semantic_function_frame_layout
module Symbol = Semantic_symbol
module Sequence = Ir_instruction_sequence
module Graph = Ir_block_graph
module Fragment = Ir_goto_label_lowering

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let sequence_errors errors =
  errors
  |> List.map (fun (error : Sequence.error) ->
      error.code ^ ": " ^ error.message)
  |> String.concat "; "

type fixture = {
  index : Indexed.indexed;
  frames : Semantic_function_frame_layout.t;
  ast : Ast.module_;
}

let prepare mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"goto-composition.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  let ast =
    parse_with_config session ~config ~source |> require_ok diagnostics_text
  in
  let prepared =
    Source.prepare_unit session ~config ~span:ast.span ast
    |> require_ok diagnostics_text
  in
  { index = Source.labels prepared; frames = Source.frames prepared; ast }

let definition fixture name =
  fixture.ast.items
  |> List.find_map (function
    | Ast.Function_definition definition when definition.name.spelling = name ->
        Some definition
    | _ -> None)
  |> Option.get

let frame fixture name =
  Frame.functions fixture.frames
  |> List.find (fun frame -> Symbol.name (Frame.function_symbol frame) = name)

let labels fixture name =
  Indexed.function_for_symbol fixture.index
    (Frame.function_symbol (frame fixture name))
  |> Option.get

let source_statements fixture name =
  match (definition fixture name).body with
  | Some (Ast.Block_statement block) -> block.block_statements
  | _ -> Alcotest.fail "goto composition fixture needs one block body"

let resolved fixture name statement =
  Indexed.occurrence_for_statement fixture.index
    ~function_symbol:(Frame.function_symbol (frame fixture name))
    statement
  |> require_ok Fun.id

let statement fixture name source =
  let occurrence = resolved fixture name source in
  let span = (Ast.statement_location source).span in
  match source with
  | Ast.Goto_statement _ -> Lower.Goto (span, occurrence)
  | Ast.Label_statement _ -> Lower.Label (span, occurrence)
  | _ -> Alcotest.fail "low-level fixture contains a nonlabel statement"

let statements fixture name =
  source_statements fixture name |> List.map (statement fixture name)

let lower fixture name statements =
  Lower.lower_complete ~frame:(frame fixture name) ~labels:(labels fixture name)
    ~span:(definition fixture name).location.span statements

let reject label result =
  match result with
  | Error ((_ : Diagnostic.t) :: _) -> ()
  | Error [] -> Alcotest.failf "%s returned no diagnostic" label
  | Ok _ -> Alcotest.failf "%s was accepted" label

let graph result =
  result |> require_ok diagnostics_text |> Lower.graph |> Ir_x87_stack.graph

let descriptions graph =
  Graph.blocks graph
  |> List.concat_map (fun block ->
      Graph.instructions block |> Sequence.instructions
      |> List.map Sequence.description)

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let original_statement_and_owner () =
  let contents = "U0 F(){goto done;done:} U0 G(){goto done;done:}" in
  List.iter
    (fun mode ->
      let fixture = prepare mode contents in
      let foreign = prepare mode contents in
      let owner = Frame.function_symbol (frame fixture "F") in
      let first = List.hd (source_statements fixture "F") in
      let expected = resolved fixture "F" first in
      Alcotest.(check bool)
        "lookup returns the original opaque occurrence" true
        (expected == List.hd (Labels.function_occurrences (labels fixture "F")));
      Alcotest.(check bool)
        "a foreign equal-source owner has no indexed function" true
        (Indexed.function_for_symbol fixture.index
           (Frame.function_symbol (frame foreign "F"))
        = None);
      let reject_lookup label owner source =
        Alcotest.(check bool)
          label true
          (Indexed.occurrence_for_statement fixture.index ~function_symbol:owner
             source
          |> Result.is_error)
      in
      reject_lookup "equal-source statements from another parse are foreign"
        owner
        (List.hd (source_statements foreign "F"));
      reject_lookup "same-name targets in another function remain foreign" owner
        (List.hd (source_statements fixture "G"));
      reject_lookup "a real statement cannot be borrowed by another owner"
        (Frame.function_symbol (frame fixture "G"))
        first;
      let reconstructed =
        match first with
        | Ast.Goto_statement source -> Ast.Goto_statement source
        | _ -> Alcotest.fail "first source occurrence is not a goto"
      in
      Alcotest.(check bool)
        "test really reconstructed the outer statement" true
        (reconstructed != first);
      reject_lookup "equal source payload is not the original statement object"
        owner reconstructed;
      let instruction_id =
        Sequence.Instruction_id.of_int 20
        |> require_ok (fun e -> e.Sequence.message)
      in
      let block_id =
        Sequence.Block_id.of_int 8 |> require_ok (fun e -> e.Sequence.message)
      in
      let lowered =
        Fragment.lower_function_labels ~instruction_id ~block_id
          (labels fixture "F")
        |> require_ok sequence_errors
      in
      Alcotest.(check bool)
        "fragment retrieves its exact occurrence" true
        (Option.is_some (Fragment.description_for_occurrence lowered expected));
      Alcotest.(check bool)
        "fragment rejects a foreign occurrence with equal IDs" true
        (Fragment.description_for_occurrence lowered
           (resolved foreign "F" (List.hd (source_statements foreign "F")))
        = None))
    modes

let complete_original_occurrence_set () =
  let contents = "U0 F(){goto done;done:} U0 G(){goto done;done:}" in
  List.iter
    (fun mode ->
      let fixture = prepare mode contents in
      let foreign = prepare mode contents in
      let body = statements fixture "F" in
      ignore (graph (lower fixture "F" body));
      let goto = List.nth body 0 and label = List.nth body 1 in
      reject "missing goto" (lower fixture "F" [ label ]);
      reject "missing label" (lower fixture "F" [ goto ]);
      reject "all label evidence removed" (lower fixture "F" []);
      reject "duplicate goto" (lower fixture "F" [ goto; goto; label ]);
      reject "duplicate label" (lower fixture "F" [ goto; label; label ]);
      reject "foreign equal-source occurrence set"
        (lower fixture "F" (statements foreign "F"));
      reject "same spelling does not confer cross-function ownership"
        (lower fixture "F" (statements fixture "G"));
      let span = (definition fixture "F").location.span in
      reject "a label context requires its named frame"
        (Lower.lower_complete ~labels:(labels fixture "F") ~span body);
      reject "a frame alone does not confer label authority"
        (Lower.lower_complete ~frame:(frame fixture "F") ~span body);
      reject "equal-source foreign label context cannot borrow a frame"
        (Lower.lower_complete ~frame:(frame fixture "F")
           ~labels:(labels foreign "F") ~span body);
      reject "another local function cannot lend its label context"
        (Lower.lower_complete ~frame:(frame fixture "F")
           ~labels:(labels fixture "G") ~span body);
      (match goto with
      | Lower.Goto (at, occurrence) -> (
          reject "goto occurrence cannot become a label"
            (lower fixture "F" [ Lower.Label (at, occurrence); label ]);
          reject
            "an enclosing module span cannot replace the original jump span"
            (lower fixture "F"
               [ Lower.Goto (fixture.ast.span, occurrence); label ]);
          reject "a nearby enlarged jump span is not the original source span"
            (lower fixture "F"
               [
                 Lower.Goto ({ at with stop = at.stop + 1 }, occurrence); label;
               ]);
          match Labels.occurrence_origin occurrence with
          | Symbol.Source_location identifier ->
              reject
                "target identifier span cannot replace the complete goto span"
                (lower fixture "F"
                   [ Lower.Goto (identifier.span, occurrence); label ])
          | _ -> Alcotest.fail "source goto lost its identifier span")
      | _ -> assert false);
      match label with
      | Lower.Label (at, occurrence) ->
          reject "label occurrence cannot become a goto"
            (lower fixture "F" [ goto; Lower.Goto (at, occurrence) ])
      | _ -> assert false)
    modes

let labels_are_structural () =
  List.iter
    (fun mode ->
      let source = "U0 F(){goto done;first:second:done:}" in
      let fixture = prepare mode source in
      let body = statements fixture "F" in
      let first = graph (lower fixture "F" body) in
      let repeated = graph (lower fixture "F" body) in
      let fresh = prepare mode source in
      let replayed = graph (lower fresh "F" (statements fresh "F")) in
      Alcotest.(check string)
        "repeated lowering has stable graph identities" (Graph.human first)
        (Graph.human repeated);
      Alcotest.(check string)
        "fresh equal source has deterministic block layout" (Graph.human first)
        (Graph.human replayed);
      let items = descriptions first in
      Alcotest.(check bool)
        "no executable label instruction is introduced" true
        (List.for_all
           (fun item -> item.Sequence.opcode <> Ir_opcode.Ic_label)
           items);
      let simple = prepare mode "U0 F(){goto done;done:}" in
      let simple_graph = graph (lower simple "F" (statements simple "F")) in
      Alcotest.(check int)
        "consecutive labels add no executable work"
        (List.length (descriptions simple_graph))
        (List.length items);
      Alcotest.(check bool)
        "consecutive labels create empty fallthrough blocks" true
        (Graph.blocks first
        |> List.exists (fun block ->
            Sequence.instructions (Graph.instructions block) = []
            && List.length (Graph.successors block) = 1));
      let source_goto = List.hd (source_statements fixture "F") in
      let expected_span = (Ast.statement_location source_goto).span in
      let jump = List.hd items in
      Alcotest.(check bool)
        "emitted goto retains its full source statement span" true
        (jump.span = Some expected_span);
      let origin =
        Labels.occurrence_origin (resolved fixture "F" source_goto)
      in
      (match origin with
      | Symbol.Source_location location ->
          Alcotest.(check bool)
            "identifier and statement spans stay distinct" true
            (location.span <> expected_span)
      | _ -> Alcotest.fail "source goto lost identifier provenance");
      let ids =
        List.map
          (fun item ->
            Sequence.Instruction_id.to_int item.Sequence.instruction_id)
          items
      in
      Alcotest.(check int)
        "every emitted instruction has its own ID" (List.length ids)
        (List.length (List.sort_uniq Int.compare ids)))
    modes

let split_frame_statement_origins () =
  List.iter
    (fun mode ->
      List.iter
        (fun contents ->
          let fixture = prepare mode contents in
          let original = List.hd (source_statements fixture "F") in
          let occurrence = resolved fixture "F" original in
          let statement_span = (Ast.statement_location original).span in
          (match Labels.occurrence_origin occurrence with
          | Symbol.Source_location identifier ->
              Alcotest.(check bool)
                "split definition actually crosses primary source frames" true
                (identifier.span.source <> statement_span.source)
          | _ -> Alcotest.fail "split definition lost its target origin");
          (match Labels.occurrence_statement_origin occurrence with
          | Some (Symbol.Source_location statement) ->
              Alcotest.(check bool)
                "resolved proof retains the exact original full statement" true
                (statement.span = statement_span)
          | _ -> Alcotest.fail "split definition lost its full statement origin");
          let lowered = graph (lower fixture "F" (statements fixture "F")) in
          Alcotest.(check bool)
            "split-frame jump emits its retained full span" true
            ((List.hd (descriptions lowered)).span = Some statement_span))
        [
          "#define DEST done\nU0 F(){goto DEST;DEST:}";
          "#define GO goto\nU0 F(){GO done;done:}";
        ])
    modes

let native_label_block_bounds () =
  let compile ?max_blocks ?max_ir_instructions ?max_code_bytes mode contents =
    let session = Session.create () in
    let source =
      Session.add_source session ~path:"goto-block-limits.hc" ~contents
    in
    let config =
      Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
    in
    Native_program.compile ?max_blocks ?max_ir_instructions ?max_code_bytes
      session ~config ~source
  in
  List.iter
    (fun mode ->
      let simple = "U0 V(){goto tail;tail:}V();" in
      let source = "U0 V(){goto tail;one:two:three:four:tail:}V();" in
      let small = (compile mode simple |> require_ok diagnostics_text).value in
      let baseline =
        (compile mode source |> require_ok diagnostics_text).value
      in
      let blocks = X86_64_program.block_count baseline in
      let instructions = X86_64_program.ir_instructions baseline in
      let bytes = String.length (X86_64_program.code baseline) in
      Alcotest.(check int)
        "every added empty label counts toward graph capacity"
        (X86_64_program.block_count small + 4)
        blocks;
      Alcotest.(check int)
        "extra labels do not consume fake IR instructions"
        (X86_64_program.ir_instructions small)
        instructions;
      ignore
        (compile ~max_blocks:blocks ~max_ir_instructions:instructions
           ~max_code_bytes:bytes mode source
        |> require_ok diagnostics_text);
      reject "one-below native block quota includes empty label blocks"
        (compile ~max_blocks:(blocks - 1) mode source);
      reject "one-below native instruction quota includes actual gotos"
        (compile ~max_ir_instructions:(instructions - 1) mode source);
      reject "one-below native code quota includes emitted control flow"
        (compile ~max_code_bytes:(bytes - 1) mode source))
    modes

let tests =
  [
    Alcotest.test_case
      "original AST statements and owners bind exact occurrences" `Quick
      original_statement_and_owner;
    Alcotest.test_case
      "composition requires every original occurrence exactly once" `Quick
      complete_original_occurrence_set;
    Alcotest.test_case
      "label blocks preserve deterministic zero-work fallthrough" `Quick
      labels_are_structural;
    Alcotest.test_case "split macro frames keep distinct full statement origins"
      `Quick split_frame_statement_origins;
    Alcotest.test_case "empty label blocks remain bounded without fake IR work"
      `Quick native_label_block_bounds;
  ]
