module Graph = Holyc_lib.Ir_block_graph
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Type = Holyc_lib.Semantic_type
module Primitive = Holyc_lib.Primitive_type

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let show_sequence_error (error : Sequence.error) =
  error.code ^ ": " ^ error.message

let show_graph_error (error : Graph.error) = error.code ^ ": " ^ error.message

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok show_sequence_error

let value_id value =
  Sequence.Value_id.of_int value |> require_ok show_sequence_error

let block_id value =
  Graph.Block_id.of_int value |> require_ok show_sequence_error

let i64 =
  Type.make_primitive ~form:Type.Internal_storage ~primitive:Primitive.I64
    ~pointer_depth:0
  |> require_ok Fun.id

let result value : Sequence.value_definition = { value_id = value_id value }

let description ?(operands = []) ?result ?target_type ?payload ?(flags = 0L)
    ?span id opcode : Sequence.description =
  {
    instruction_id = instruction_id id;
    opcode;
    operands;
    result;
    target_type;
    payload;
    flags;
    span;
  }

let block id instructions : Graph.block_description =
  { block_id = block_id id; instructions }

let require_graph ~entry descriptions =
  match Graph.create ~entry:(block_id entry) descriptions with
  | Ok graph -> graph
  | Error errors ->
      Alcotest.fail (String.concat "\n" (List.map show_graph_error errors))

let graph_errors ~entry descriptions =
  match Graph.create ~entry:(block_id entry) descriptions with
  | Ok _ -> Alcotest.fail "expected block graph validation to fail"
  | Error errors -> errors

let error_codes errors =
  List.map (fun (error : Graph.error) -> error.code) errors

let has_code code errors =
  Alcotest.(check bool)
    ("contains " ^ code) true
    (List.mem code (error_codes errors))

let block_numbers graph =
  Graph.blocks graph
  |> List.map (fun block -> Graph.block_id block |> Graph.Block_id.to_int)

let successor_numbers block =
  Graph.successors block |> List.map Graph.Block_id.to_int

let find graph id =
  match Graph.find_block graph (block_id id) with
  | Some value -> value
  | None -> Alcotest.failf "block ^b%d was not found" id

let checked_control_flow_graph () =
  let graph =
    require_graph ~entry:10
      [
        block 10
          [
            description ~result:(result 0) ~target_type:i64
              ~payload:(Sequence.Integer 1L) 0 Opcode.Ic_imm_i64;
            description
              ~operands:[ value_id 0 ]
              ~payload:(Sequence.Block (block_id 8))
              1 Opcode.Ic_br_zero;
          ];
        block 3
          [
            description ~payload:(Sequence.Block (block_id 20)) 2 Opcode.Ic_jmp;
          ];
        block 8
          [
            description ~result:(result 3) ~target_type:i64
              ~payload:(Sequence.Integer 0L) 3 Opcode.Ic_imm_i64;
            description ~result:(result 4) ~target_type:i64
              ~payload:(Sequence.Integer 2L) 4 Opcode.Ic_imm_i64;
            description
              ~operands:[ value_id 3; value_id 4 ]
              ~payload:
                (Sequence.Block_targets [ block_id 20; block_id 3; block_id 20 ])
              5 Opcode.Ic_switch;
          ];
        block 20 [ description 6 Opcode.Ic_ret ];
      ]
  in
  Alcotest.(check (list int))
    "source block order" [ 10; 3; 8; 20 ] (block_numbers graph);
  Alcotest.(check int)
    "entry block" 10
    (Graph.entry graph |> Graph.block_id |> Graph.Block_id.to_int);
  Alcotest.(check (list int))
    "conditional target then fallthrough" [ 8; 3 ]
    (successor_numbers (find graph 10));
  Alcotest.(check (list int))
    "jump target" [ 20 ]
    (successor_numbers (find graph 3));
  Alcotest.(check (list int))
    "ordered unique switch successors" [ 20; 3 ]
    (successor_numbers (find graph 8));
  Alcotest.(check (list int))
    "return has no successor" []
    (successor_numbers (find graph 20));
  let dump_lines = String.split_on_char '\n' (Graph.human graph) in
  Alcotest.(check bool)
    "switch payload remains ordered" true
    (List.mem "!i5 IC_SWITCH %v3 %v4 blocks:[^b20,^b3,^b20] flags=0x000000000"
       dump_lines)

let ordinary_fallthrough_and_dump () =
  let descriptions =
    [
      block 9 [ description 0 Opcode.Ic_label ];
      block 2 [ description 1 Opcode.Ic_end ];
    ]
  in
  let graph = require_graph ~entry:9 descriptions in
  let repeated = require_graph ~entry:9 descriptions in
  let expected =
    "holyc-ir-graph-v1 reference=c26482bb6ad3f80106d28504ec5db3c6a360732c\n\
     entry=^b9\n\
     block ^b9\n\
     !i0 IC_LABEL flags=0x000000000\n\
     successors=[^b2]\n\
     block ^b2\n\
     !i1 IC_END flags=0x000000000\n\
     successors=[]\n"
  in
  Alcotest.(check string) "versioned graph dump" expected (Graph.human graph);
  Alcotest.(check string)
    "repeat construction" (Graph.human graph) (Graph.human repeated)

let payload_target_is_not_always_an_edge () =
  let graph =
    require_graph ~entry:4
      [
        block 4
          [
            description
              ~payload:(Sequence.Block (block_id 99))
              0 Opcode.Ic_label;
          ];
        block 7 [ description 1 Opcode.Ic_ret ];
      ]
  in
  Alcotest.(check (list int))
    "label payload does not create an edge" [ 7 ]
    (successor_numbers (find graph 4))

let duplicate_blocks_are_rejected () =
  graph_errors ~entry:1
    [
      block 1 [ description 0 Opcode.Ic_ret ];
      block 1 [ description 1 Opcode.Ic_ret ];
    ]
  |> has_code "HCIR0011"

let duplicate_global_identities_are_rejected () =
  let duplicate_instruction =
    graph_errors ~entry:1
      [
        block 1 [ description 0 Opcode.Ic_ret ];
        block 2 [ description 0 Opcode.Ic_ret ];
      ]
  in
  has_code "HCIR0012" duplicate_instruction;
  let duplicate_value =
    graph_errors ~entry:1
      [
        block 1
          [
            description ~result:(result 0) ~target_type:i64 0 Opcode.Ic_imm_i64;
            description ~payload:(Sequence.Block (block_id 2)) 1 Opcode.Ic_jmp;
          ];
        block 2
          [
            description ~result:(result 0) ~target_type:i64 2 Opcode.Ic_imm_i64;
            description 3 Opcode.Ic_ret;
          ];
      ]
  in
  has_code "HCIR0013" duplicate_value

let child_sequence_errors_retain_block_context () =
  let errors =
    graph_errors ~entry:6 [ block 6 [ description 0 Opcode.Ic_add ] ]
  in
  has_code "HCIR0004" errors;
  match
    List.find_opt (fun (error : Graph.error) -> error.code = "HCIR0004") errors
  with
  | None -> Alcotest.fail "missing child error"
  | Some error ->
      Alcotest.(check (option int)) "child block" (Some 6) error.block_id;
      Alcotest.(check (option int))
        "child instruction" (Some 0) error.instruction_id

let transfer_targets_are_checked () =
  let unknown =
    graph_errors ~entry:1
      [
        block 1
          [
            description ~payload:(Sequence.Block (block_id 99)) 0 Opcode.Ic_jmp;
          ];
      ]
  in
  has_code "HCIR0017" unknown;
  let wrong_single =
    graph_errors ~entry:1
      [
        block 1
          [
            description
              ~payload:(Sequence.Block_targets [ block_id 1 ])
              0 Opcode.Ic_jmp;
          ];
      ]
  in
  has_code "HCIR0016" wrong_single;
  let wrong_switch =
    graph_errors ~entry:1
      [
        block 1
          [
            description ~result:(result 0) ~target_type:i64 0 Opcode.Ic_imm_i64;
            description ~result:(result 1) ~target_type:i64 1 Opcode.Ic_imm_i64;
            description
              ~operands:[ value_id 0; value_id 1 ]
              ~payload:(Sequence.Block (block_id 1))
              2 Opcode.Ic_switch;
          ];
      ]
  in
  has_code "HCIR0016" wrong_switch

let terminators_and_stream_end_are_checked () =
  let source = Holyc_lib.Source_id.of_int 5 |> require_ok Fun.id in
  let span = Holyc_lib.Span.unsafe_make ~source ~start:10 ~stop:14 in
  let after_terminator =
    graph_errors ~entry:1
      [
        block 1
          [
            description ~payload:(Sequence.Block (block_id 2)) 0 Opcode.Ic_jmp;
            description ~span 1 Opcode.Ic_label;
          ];
        block 2 [ description 2 Opcode.Ic_ret ];
      ]
  in
  has_code "HCIR0015" after_terminator;
  (match
     List.find_opt
       (fun (error : Graph.error) -> error.code = "HCIR0015")
       after_terminator
   with
  | None -> Alcotest.fail "missing terminator error"
  | Some error ->
      Alcotest.(check (option int)) "block context" (Some 1) error.block_id;
      Alcotest.(check (option int))
        "instruction context" (Some 1) error.instruction_id;
      Alcotest.(check bool) "source context" true (error.span = Some span));
  let repeated_end =
    graph_errors ~entry:1
      [
        block 1 [ description 0 Opcode.Ic_end ];
        block 2 [ description 1 Opcode.Ic_end ];
      ]
  in
  has_code "HCIR0019" repeated_end;
  has_code "HCIR0020" repeated_end;
  let block_after_end =
    graph_errors ~entry:1
      [
        block 1 [ description 0 Opcode.Ic_end ];
        block 2 [ description 1 Opcode.Ic_ret ];
      ]
  in
  has_code "HCIR0020" block_after_end

let entry_and_final_fallthrough_are_checked () =
  graph_errors ~entry:8 [ block 1 [ description 0 Opcode.Ic_ret ] ]
  |> has_code "HCIR0014";
  graph_errors ~entry:1 [ block 1 [ description 0 Opcode.Ic_label ] ]
  |> has_code "HCIR0018";
  graph_errors ~entry:1 [ block 1 [] ] |> has_code "HCIR0018"

let ordered_graph raw_ids =
  let ids =
    List.mapi (fun index raw -> index + (abs (raw mod 50) * 1_000)) raw_ids
  in
  match ids with
  | [] -> true
  | entry :: _ -> (
      let descriptions =
        List.mapi
          (fun index id -> block id [ description index Opcode.Ic_ret ])
          ids
      in
      match Graph.create ~entry:(block_id entry) descriptions with
      | Error _ -> false
      | Ok graph ->
          block_numbers graph = ids
          && List.for_all
               (fun id -> Option.is_some (Graph.find_block graph (block_id id)))
               ids
          && String.equal (Graph.human graph) (Graph.human graph))

let order_and_lookup_property =
  QCheck.Test.make ~count:500
    ~name:"block order, lookup, and dumps are deterministic"
    QCheck.(list_small nat_small)
    ordered_graph

let error_signature errors =
  List.map
    (fun (error : Graph.error) ->
      ( error.code,
        error.message,
        error.block_id,
        error.instruction_id,
        error.span ))
    errors

let deterministic_error_property =
  QCheck.Test.make ~count:500 ~name:"block graph errors are deterministic"
    QCheck.(int_bound 10_000)
    (fun raw_id ->
      let id = raw_id + 1 in
      let invalid =
        [
          block id [ description 0 Opcode.Ic_label ];
          block id [ description 0 Opcode.Ic_end ];
        ]
      in
      match
        ( Graph.create ~entry:(block_id id) invalid,
          Graph.create ~entry:(block_id id) invalid )
      with
      | Error first, Error second ->
          error_signature first = error_signature second
      | Ok _, _ | _, Ok _ -> false)

let tests =
  [
    Alcotest.test_case "checked control-flow graph" `Quick
      checked_control_flow_graph;
    Alcotest.test_case "fallthrough and deterministic dump" `Quick
      ordinary_fallthrough_and_dump;
    Alcotest.test_case "non-transfer block payload" `Quick
      payload_target_is_not_always_an_edge;
    Alcotest.test_case "duplicate blocks" `Quick duplicate_blocks_are_rejected;
    Alcotest.test_case "global instruction and value IDs" `Quick
      duplicate_global_identities_are_rejected;
    Alcotest.test_case "child sequence errors" `Quick
      child_sequence_errors_retain_block_context;
    Alcotest.test_case "transfer target checks" `Quick
      transfer_targets_are_checked;
    Alcotest.test_case "terminator and end checks" `Quick
      terminators_and_stream_end_are_checked;
    Alcotest.test_case "entry and final fallthrough" `Quick
      entry_and_final_fallthrough_are_checked;
    QCheck_alcotest.to_alcotest order_and_lookup_property;
    QCheck_alcotest.to_alcotest deterministic_error_property;
  ]
