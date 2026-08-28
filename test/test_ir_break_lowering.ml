module Lowering = Holyc_lib.Ir_break_lowering
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Breaks = Holyc_lib.Semantic_break_resolution
module Symbol = Holyc_lib.Semantic_symbol
module Symbol_table = Holyc_lib.Semantic_symbol_table
module Preprocessor = Holyc_lib.Preprocessor
module Session = Holyc_lib.Session
module Driver = Holyc_lib
module Block_id = Sequence.Block_id

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temp_directory run =
  let path = Filename.temp_dir "holyc-ir-breaks-" "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> run path)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let format_error code message = code ^ ": " ^ message
let show_error error = format_error error.Sequence.code error.message
let show_errors errors = String.concat "; " (List.map show_error errors)

let expect_ast = function
  | Ok ast -> ast
  | Error diagnostics ->
      Alcotest.failf "expected an AST, got %d diagnostics"
        (List.length diagnostics)

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search index =
    if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else search (index + 1)
  in
  fragment_length = 0 || search 0

let config ?working_directory mode =
  Preprocessor.Config.create ?working_directory ~compilation_mode:mode ()
  |> checked

let resolve_function ?working_directory ~mode ~path source name =
  let session = Session.create () in
  let file = Session.add_source session ~path ~contents:source in
  let config = config ?working_directory mode in
  let parsed = Driver.parse_with_config session ~config ~source:file in
  let ast = expect_ast parsed in
  let result = Driver.collect_declarations session ast in
  let declarations = checked result in
  let functions =
    Driver.collect_functions session ~declarations ast |> checked
  in
  let result = Driver.resolve_breaks session ~functions ast in
  let resolution = checked result in
  let has_name function_ =
    let symbol = Breaks.function_symbol function_ in
    String.equal (Symbol.name symbol) name
  in
  List.find has_name (Breaks.functions resolution)

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok show_error

let block_id value = Block_id.of_int value |> require_ok show_error

let lower ?(instruction = 40) ?(block = 30) function_ =
  let instruction_id = instruction_id instruction in
  let block_id = block_id block in
  Lowering.lower_function_breaks ~instruction_id ~block_id function_

let descriptions lowered =
  lowered |> Lowering.sequence |> Sequence.instructions
  |> List.map Sequence.description

let block_number = Sequence.Block_id.to_int

let payload_block (description : Sequence.description) =
  match description.payload with
  | Some (Sequence.Block block) -> block_number block
  | _ -> Alcotest.fail "break jump has no block payload"

let origin_span occurrence =
  match Breaks.break_origin occurrence with
  | Symbol.Source_location location -> Some location.span
  | Symbol.Pinned_source _ | Symbol.Synthesized _ -> None

let nested_break_jumps () =
  let source =
    "U0 Nested(I64 active){\n\
     while(active){break;for(;active;){break;while(active)break;}}\n\
     do break;while(active);\n\
     switch(active){case 0:break;start:case 1:break;\n\
     while(active)break;end:break;}\n\
     }"
  in
  List.iter
    (fun mode ->
      let function_ =
        resolve_function ~mode ~path:"nested-break-ir.HC" source "Nested"
      in
      let lowered = lower function_ |> require_ok show_errors in
      let items = descriptions lowered in
      Alcotest.(check (list string))
        "every break emits a jump"
        (List.init 8 (Fun.const "IC_JMP"))
        (List.map
           (fun item -> Opcode.to_source_name item.Sequence.opcode)
           items);
      Alcotest.(check (list int))
        "nearest regions map to exact blocks"
        [ 30; 31; 32; 33; 34; 35; 36; 34 ]
        (List.map payload_block items);
      Alcotest.(check (list int))
        "every region receives a consecutive block"
        [ 30; 31; 32; 33; 34; 35; 36 ]
        (lowered |> Lowering.region_blocks
        |> List.map (fun (_, block) -> block_number block));
      let occurrences = Breaks.function_breaks function_ in
      Alcotest.(check bool)
        "jumps keep exact break spans" true
        (List.map (fun item -> item.Sequence.span) items
        = List.map origin_span occurrences);
      let clear_flags = List.init 8 (Fun.const 0L) in
      let flags = List.map (fun item -> item.Sequence.flags) items in
      Alcotest.(check (list int64))
        "break jump flags are clear" clear_flags flags;
      let next_instruction = Lowering.next_instruction_id lowered in
      Alcotest.(check int)
        "instruction identities advance through breaks" 48
        (Sequence.Instruction_id.to_int next_instruction);
      Alcotest.(check int)
        "block identities advance through regions" 37
        (Lowering.next_block_id lowered |> block_number))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deterministic_provenance_and_empty_region () =
  let source =
    "#define EXIT break\nU0 Replay(I64 active){while(active)EXIT;}"
  in
  let path = "break-replay.HC" in
  let mode = Preprocessor.Jit in
  let function_ = resolve_function ~mode ~path source "Replay" in
  let first = lower ~instruction:7 ~block:9 function_ in
  let second = lower ~instruction:7 ~block:9 function_ in
  let first = first |> require_ok show_errors |> Lowering.human in
  let second = second |> require_ok show_errors |> Lowering.human in
  Alcotest.(check string) "deterministic replay" first second;
  let has_version = contains first "holyc-ir-break-v1" in
  let has_region = contains first "region=0:^b9" in
  Alcotest.(check bool)
    "dump records its version and assigned block" true
    (has_version && has_region);
  let empty =
    resolve_function ~mode:Preprocessor.Jit ~path:"empty-break.HC"
      "U0 Empty(I64 active){while(active);}" "Empty"
  in
  let lowered = lower empty |> require_ok show_errors in
  Alcotest.(check int)
    "a region without breaks still receives a block" 1
    (List.length (Lowering.region_blocks lowered));
  Alcotest.(check int)
    "an empty break stream consumes no instruction" 0
    (lowered |> descriptions |> List.length);
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      let include_file = Filename.concat root "included.HC" in
      let root_source = "#include \"included\"" in
      write_file root_file root_source;
      write_file include_file "U0 Included(I64 x){while(x)break;}";
      let function_ =
        resolve_function ~working_directory:root ~mode:Preprocessor.Aot
          ~path:root_file root_source "Included"
      in
      let lowered = lower function_ |> require_ok show_errors in
      Alcotest.(check (list string))
        "included break uses checked path" [ "IC_JMP" ]
        (lowered |> descriptions
        |> List.map (fun item -> item.Sequence.opcode)
        |> List.map Opcode.to_source_name))

let manual_function () =
  let table = Symbol_table.create () in
  let root = Symbol_table.root table in
  let module_kind = Symbol_table.Module in
  let module_scope =
    Symbol_table.create_scope table ~parent:root ~kind:module_kind
      ~name:"manual.HC" ()
    |> checked
  in
  let symbol =
    let origin = Symbol.Synthesized "manual function" in
    Symbol_table.add table ~scope:module_scope ~name:"Manual"
      ~kind:Symbol.Function ~origin
    |> checked
  in
  let scope =
    Symbol_table.create_scope table ~parent:module_scope
      ~kind:Symbol_table.Function ~name:"Manual" ()
    |> checked
  in
  let origin = Symbol.Synthesized "manual break" in
  let region =
    Breaks.make_region ~region_index:0 ~kind:Breaks.While_region ~origin
    |> checked
  in
  let occurrence =
    Breaks.make_break ~occurrence_index:0 ~origin ~target_region_index:0
    |> checked
  in
  let facts =
    Breaks.make_function ~symbol ~scope ~item_index:0 ~regions:[ region ]
      ~breaks:[ occurrence ]
    |> checked
  in
  let resolution = Breaks.resolve ~table [ facts ] |> checked in
  Breaks.functions resolution |> List.hd

let incomplete_metadata () =
  match lower (manual_function ()) with
  | Error [ (error : Sequence.error) ] ->
      Alcotest.(check string) "metadata code" "HCIRL0004" error.code
  | Error errors ->
      let count = List.length errors in
      Alcotest.failf "expected one metadata error, got %d" count
  | Ok _ -> Alcotest.fail "incomplete break metadata was accepted"

let identity_exhaustion () =
  let source =
    "U0 Limits(I64 active){while(active)break;do break;while(active);}"
  in
  let path = "break-limits.HC" in
  let mode = Preprocessor.Jit in
  let function_ = resolve_function ~mode ~path source "Limits" in
  List.iter
    (fun result ->
      match result with
      | Error [ (error : Sequence.error) ] ->
          Alcotest.(check string)
            "identity exhaustion code" "HCIRL0005" error.code
      | Error errors ->
          Alcotest.failf "expected one exhaustion error, got %d"
            (List.length errors)
      | Ok _ -> Alcotest.fail "break identity exhaustion was accepted")
    [
      lower ~instruction:Int.max_int function_;
      lower ~block:(Int.max_int - 1) function_;
    ]

let tests =
  [
    Alcotest.test_case "nested break jumps" `Quick nested_break_jumps;
    Alcotest.test_case "provenance and empty region" `Quick
      deterministic_provenance_and_empty_region;
    Alcotest.test_case "incomplete metadata" `Quick incomplete_metadata;
    Alcotest.test_case "identity exhaustion" `Quick identity_exhaustion;
  ]
