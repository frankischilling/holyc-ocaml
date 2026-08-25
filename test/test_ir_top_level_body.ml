module Top_level = Holyc_lib.Ir_top_level_body
module Graph = Holyc_lib.Ir_block_graph
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Option_ = Holyc_lib.Compiler_option
module Type = Holyc_lib.Semantic_type
module Primitive = Holyc_lib.Primitive_type
module Source_id = Holyc_lib.Source_id
module Span = Holyc_lib.Span

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let show_sequence_error (error : Sequence.error) =
  error.code ^ ": " ^ error.message

let show_graph_error (error : Graph.error) = error.code ^ ": " ^ error.message

let show_top_level_error (error : Top_level.error) =
  error.code ^ ": " ^ error.message

let stream_id value =
  Top_level.Stream_id.of_int value |> require_ok show_top_level_error

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok show_sequence_error

let value_id value =
  Sequence.Value_id.of_int value |> require_ok show_sequence_error

let block_id value =
  Sequence.Block_id.of_int value |> require_ok show_sequence_error

let f64 =
  Type.make_primitive ~form:Type.Internal_storage ~primitive:Primitive.F64
    ~pointer_depth:0
  |> require_ok Fun.id

let instruction ?(operands = []) ?result ?target_type ?(flags = 0L) id opcode :
    Sequence.description =
  {
    instruction_id = instruction_id id;
    opcode;
    operands;
    result =
      Option.map (fun value -> Sequence.{ value_id = value_id value }) result;
    target_type;
    payload = None;
    flags;
    span = None;
  }

let graph ~entry blocks =
  Graph.create ~entry:(block_id entry) blocks
  |> require_ok (fun errors ->
      String.concat "; " (List.map show_graph_error errors))

let valid_graph () =
  let id = block_id 6 in
  graph ~entry:6
    [
      Graph.
        {
          block_id = id;
          instructions =
            [ instruction 0 Opcode.Ic_enter; instruction 1 Opcode.Ic_end ];
        };
    ]

let description ?(item_position = 11) ?(compiler_options = 0L) ?span ?body () :
    Top_level.description =
  {
    stream_id = stream_id 3;
    item_position;
    compiler_options;
    span;
    body = Option.value body ~default:(valid_graph ());
  }

let create description =
  Top_level.create description
  |> require_ok (fun errors ->
      String.concat "; " (List.map show_top_level_error errors))

let errors description =
  match Top_level.create description with
  | Ok _ -> Alcotest.fail "expected top-level IR validation to fail"
  | Error errors -> errors

let has_code code errors =
  Alcotest.(check bool)
    ("contains " ^ code) true
    (List.exists (fun (error : Top_level.error) -> error.code = code) errors)

let stream_ids_are_checked () =
  match Top_level.Stream_id.of_int (-1) with
  | Ok _ -> Alcotest.fail "negative top-level stream ID was accepted"
  | Error error -> Alcotest.(check string) "stable code" "HCIR0034" error.code

let top_level_metadata_is_retained () =
  let span =
    Span.{ source = Source_id.of_int 2 |> Result.get_ok; start = 7; stop = 19 }
  in
  let options = Option_.mask Option_.Trace in
  let checked = description ~compiler_options:options ~span () |> create in
  Alcotest.(check int)
    "stream ID" 3
    (Top_level.stream_id checked |> Top_level.Stream_id.to_int);
  Alcotest.(check int) "item position" 11 (Top_level.item_position checked);
  Alcotest.(check int64)
    "compiler options" options
    (Top_level.compiler_options checked);
  Alcotest.(check bool)
    "checked graph retained" true
    (Top_level.body checked
    == Holyc_lib.Ir_x87_stack.graph (Top_level.x87 checked));
  let expected =
    "holyc-ir-top-level-v1 reference=c26482bb6ad3f80106d28504ec5db3c6a360732c\n\
     stream ^t3 item-position=11 options=0x2 [OPTf_TRACE] @source=2:7..19\n\
     body\n\
     entry=^b6\n\
     block ^b6\n\
     !i0 IC_ENTER flags=0x000000000\n\
     !i1 IC_END flags=0x000000000\n\
     successors=[]\n\
     x87=verified\n"
  in
  Alcotest.(check string) "versioned dump" expected (Top_level.human checked)

let metadata_failures_are_collected () =
  let invalid_span =
    Span.{ source = Source_id.of_int 0 |> Result.get_ok; start = 8; stop = 2 }
  in
  let failures =
    description ~item_position:(-4) ~compiler_options:(Int64.shift_left 1L 63)
      ~span:invalid_span ()
    |> errors
  in
  failures |> has_code "HCIR0035";
  failures |> has_code "HCIR0036";
  failures |> has_code "HCIR0037"

let x87_failures_keep_stream_context () =
  let dont_push = Int64.shift_left 1L 21 in
  let id = block_id 0 in
  let body =
    graph ~entry:0
      [
        Graph.
          {
            block_id = id;
            instructions =
              [
                instruction ~result:0 ~target_type:f64 0 Opcode.Ic_imm_f64;
                instruction
                  ~operands:[ value_id 0 ]
                  ~result:1 ~target_type:f64 ~flags:dont_push 1 Opcode.Ic_sqr;
                instruction 2 Opcode.Ic_ret;
              ];
          };
      ]
  in
  let failures = description ~body () |> errors in
  failures |> has_code "HCIR0022";
  let failure =
    List.find
      (fun (error : Top_level.error) -> error.code = "HCIR0022")
      failures
  in
  Alcotest.(check (option int)) "stream context" (Some 3) failure.stream_id;
  Alcotest.(check (option int)) "item context" (Some 11) failure.item_position;
  Alcotest.(check (option int)) "block context" (Some 0) failure.block_id;
  Alcotest.(check (option int))
    "instruction context" (Some 1) failure.instruction_id

let construction_is_deterministic () =
  let valid () = description () |> create |> Top_level.human in
  Alcotest.(check string) "same dump" (valid ()) (valid ());
  let invalid () =
    description ~item_position:(-1) ~compiler_options:(Int64.shift_left 1L 60)
      ()
    |> errors
    |> List.map (fun (error : Top_level.error) -> (error.code, error.message))
  in
  Alcotest.(check bool) "same failures" true (invalid () = invalid ())

let tests =
  [
    Alcotest.test_case "stream IDs" `Quick stream_ids_are_checked;
    Alcotest.test_case "retained metadata" `Quick top_level_metadata_is_retained;
    Alcotest.test_case "metadata failures" `Quick
      metadata_failures_are_collected;
    Alcotest.test_case "x87 failure context" `Quick
      x87_failures_keep_stream_context;
    Alcotest.test_case "deterministic construction" `Quick
      construction_is_deterministic;
  ]
