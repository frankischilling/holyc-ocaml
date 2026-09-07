open Holyc_lib
module H = Test_ir_integer_interpreter
module A = Test_ir_frame_address_lowering
module Frame = Semantic_function_frame_layout
module Typed = Semantic_function_call_expression_result
module Body = Ir_function_body
module Expr = Ir_expression_lowering
module VM = Ir_integer_interpreter
module Seq = Ir_instruction_sequence
module Op = Ir_opcode
module Graph = Ir_block_graph
module Type = Semantic_type
module Return = Ir_return_lowering

let source = "I64 Add(I64 a,I64 b){I64 c; c=a+b; return c;}"

let analyze ?(mode = Preprocessor.Jit) text =
  let frames, results = A.analyze ~compilation_mode:mode text in
  let function_ = A.function_named results "Add" in
  (A.frame_for frames function_, function_)

let body frame return_type blocks =
  let members kind =
    Frame.function_locations frame
    |> List.filter (fun location -> Frame.location_kind location = kind)
    |> List.mapi (fun position location ->
        Body.
          {
            position;
            symbol = Frame.location_symbol location;
            type_ = Frame.location_checked_type location;
            span = None;
          })
  in
  let body =
    Graph.create ~entry:(H.block_id 0) blocks
    |> H.require_ok (fun errors ->
        String.concat "; " (List.map H.show_graph_error errors))
  in
  Body.create
    {
      function_id = Test_ir_function_body.function_id 0;
      symbol = Frame.function_symbol frame;
      function_scope =
        Semantic_symbol_table.scope_id (Frame.function_scope frame);
      return_type;
      parameters = members Frame.Named_parameter;
      locals = members Frame.Automatic_local;
      stored_flags = 0L;
      compiler_options = 0L;
      span = None;
      body;
    }
  |> H.require_ok (fun errors ->
      String.concat "; "
        (List.map Test_ir_function_body.show_function_error errors))

let address ~type_ start displacement =
  let pointer = Type.pointer_to type_ |> H.require_ok Fun.id in
  [
    H.description ~result:(H.result start) ~target_type:pointer start Op.Ic_rbp;
    H.imm ~id:(start + 1) ~value:(start + 1) ~type_:pointer displacement;
    H.binary ~id:(start + 2) ~left:start ~right:(start + 1) ~value:(start + 2)
      ~type_:pointer Op.Ic_add;
  ]

let load ~type_ start displacement =
  address ~type_ start displacement
  @ [
      H.unary ~id:(start + 3) ~operand:(start + 2) ~value:(start + 3) ~type_
        Op.Ic_deref;
    ]

let block id instructions : Graph.block_description =
  { block_id = H.block_id id; instructions }

let add_body frame =
  let type_ = H.public_i64 in
  body frame type_
    [
      block 0
        (load ~type_ 0 16L @ load ~type_ 4 24L
        @ [ H.binary ~id:8 ~left:3 ~right:7 ~value:8 ~type_:H.i64 Op.Ic_add ]
        @ address ~type_ 9 (-8L)
        @ [ H.binary ~id:12 ~left:11 ~right:8 ~value:12 ~type_ Op.Ic_assign ]
        @ load ~type_ 13 (-8L)
        @ [
            H.return_value ~id:17 ~operand:16 ~type_; H.description 18 Op.Ic_ret;
          ]);
    ]

let execute ?(max_steps = 100) ?(max_frame_bytes = 24) frame arguments function_
    =
  VM.execute_function ~max_steps ~max_frame_bytes ~frame ~arguments function_

let expect_word expected result =
  let execution = result |> H.require_ok H.show_vm_errors in
  (match VM.termination execution with
  | VM.Returned (Some word) ->
      Alcotest.(check int64) "returned word" expected word.bits
  | _ -> Alcotest.fail "expected a returned word");
  execution

let lower frame ~instruction ~value root =
  Expr.lower_typed_result ~frame
    ~instruction_id:(H.instruction_id instruction)
    ~value_id:(H.value_id value) root
  |> H.require_ok (fun errors ->
      String.concat "; " (List.map H.show_sequence_error errors))
  |> Test_ir_expression_lowering.require_lowered

let descriptions expression =
  Expr.sequence expression |> Seq.instructions |> List.map Seq.description

let composed_body frame function_ =
  let next_instruction = ref 0 and next_value = ref 0 in
  let statements =
    A.expression_values function_
    |> List.concat_map (fun root ->
        let expression =
          lower frame ~instruction:!next_instruction ~value:!next_value root
        in
        let discard =
          Seq.Instruction_id.to_int (Expr.next_instruction_id expression)
        in
        next_instruction := discard + 1;
        next_value := Seq.Value_id.to_int (Expr.next_value_id expression);
        descriptions expression
        @ [
            H.end_expression ~id:discard
              ~operand:(Seq.Value_id.to_int (Expr.result_value expression))
              ();
          ])
  in
  let returned =
    Return.lower_function_return ~frame
      ~instruction_id:(H.instruction_id !next_instruction)
      ~value_id:(H.value_id !next_value) ~leave:(H.block_id 1)
      (Typed.function_returns function_ |> List.hd)
    |> H.require_ok (fun errors ->
        String.concat "; " (List.map H.show_sequence_error errors))
    |> function
    | Return.Lowered result -> result
    | Return.Unsupported_expression -> Alcotest.fail "unsupported typed return"
  in
  body frame
    (Return.return_type returned)
    [
      block 0
        (statements
        @ (Return.sequence returned |> Seq.instructions
         |> List.map Seq.description));
      block 1
        [
          H.description
            (Seq.Instruction_id.to_int (Return.next_instruction_id returned))
            Op.Ic_ret;
        ];
    ]

let joined_lowering_execution () =
  List.iter
    (fun (source, arguments, expected) ->
      let frame, function_ = analyze source in
      let function_ = composed_body frame function_ in
      ignore
        (execute ~max_frame_bytes:64 frame arguments function_
        |> expect_word expected))
    [
      (source, [ 20L; 22L ], 42L);
      ("I64 Add(I64 a,I64 b){I64 c; (c)=a+b; return c;}", [ 20L; 22L ], 42L);
      ("I64 Add(I64 a,I64 b){I64 c; ((c))=a+b; return c;}", [ 20L; 22L ], 42L);
      ("I64 Add(I64 a,I64 b){I64 c; c=a+b; return c;}", [ -7L; 2L ], -5L);
      ( "U64 Add(U64 a,U64 b){U64 c; c=a+b; return c;}",
        [ Int64.min_int; 1L ],
        Int64.succ Int64.min_int );
      ("I64 Add(I64 a,I64 b){I64 c,d; c=d=a+b; return c+d;}", [ 20L; 22L ], 84L);
      ("I64 Add(I64 a,I64 b){I64 c; c=a; c=b; return c;}", [ 20L; 22L ], 22L);
      ("I64 Add(I64 a,I64 b){a=a+b; return a;}", [ 20L; 22L ], 42L);
    ]

let expect_error code result =
  match result with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error errors -> (
      match
        List.find_opt (fun (error : VM.error) -> error.code = code) errors
      with
      | Some error -> error
      | None -> Alcotest.fail (H.show_vm_errors errors))

let limits_and_uninitialized () =
  let frame, _ = analyze source in
  let function_ = add_body frame in
  let exhausted =
    execute ~max_steps:18 frame [ 20L; 22L ] function_
    |> expect_error "HCIRVM0007"
  in
  Alcotest.(check int) "exact step bound" 18 exhausted.executed_steps;
  Alcotest.(check (option int))
    "attempted return" (Some 18) exhausted.instruction_id;
  List.iter
    (fun max_frame_bytes ->
      ignore
        (execute ~max_frame_bytes frame [ 20L; 22L ] function_
        |> expect_error "HCIRVM0011"))
    [ 1; 16; 23 ];
  ignore
    (execute ~max_frame_bytes:0 frame [ 20L; 22L ] function_
    |> expect_error "HCIRVM0001");
  ignore (execute frame [ 20L ] function_ |> expect_error "HCIRVM0011");
  let frame, function_ = analyze "I64 Add(I64 a,I64 b){I64 c; return c;}" in
  let function_ = composed_body frame function_ in
  let error =
    execute frame [ 20L; 22L ] function_ |> expect_error "HCIRVM0012"
  in
  Alcotest.(check int) "load consumes its step" 4 error.executed_steps;
  Alcotest.(check (option int)) "load instruction" (Some 3) error.instruction_id;
  Alcotest.(check bool) "source span retained" true (Option.is_some error.span)

let return_domains () =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, expected_type) ->
          let frame, function_ = analyze ~mode text in
          let function_ = composed_body frame function_ in
          let execution =
            execute frame [ Int64.min_int; 0L ] function_
            |> expect_word Int64.min_int
          in
          match VM.termination execution with
          | Returned (Some word) ->
              Alcotest.(check bool)
                "declared return signedness" true
                (word.type_ = expected_type)
          | _ -> assert false)
        [
          ("I64 Add(U64 a,U64 b){return a;}", VM.I64);
          ("U64 Add(I64 a,I64 b){return a;}", VM.U64);
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ];
  let frame, _ = analyze source in
  let missing =
    body frame H.public_i64 [ block 0 [ H.description 0 Op.Ic_ret ] ]
  in
  let error = execute frame [ 0L; 0L ] missing |> expect_error "HCIRVM0013" in
  Alcotest.(check int)
    "missing return value fails at return" 1 error.executed_steps

let public_unsigned_negation () =
  let frame, function_ = analyze "U64 Add(U64 a,U64 b){return -a;}" in
  let function_ = composed_body frame function_ in
  let execution = execute frame [ 5L; 0L ] function_ |> expect_word (-5L) in
  (match VM.termination execution with
  | Returned (Some word) ->
      Alcotest.(check bool)
        "public unsigned negation retains U64" true (word.type_ = VM.U64)
  | _ -> assert false);
  let invalid =
    body frame H.public_u64
      [
        block 0
          [
            H.imm ~id:0 ~value:0 ~type_:H.u64 5L;
            H.unary ~id:1 ~operand:0 ~value:1 ~type_:H.public_u64
              Op.Ic_unary_minus;
            H.return_value ~id:2 ~operand:1 ~type_:H.public_u64;
            H.description 3 Op.Ic_ret;
          ];
      ]
  in
  ignore (execute frame [ 0L; 0L ] invalid |> expect_error "HCIRVM0006")

let storage_across_transfers () =
  let frame, _ = analyze source in
  let type_ = H.public_i64 in
  let function_ =
    body frame type_
      [
        block 0
          (load ~type_ 0 16L @ address ~type_ 4 (-8L)
          @ [
              H.binary ~id:7 ~left:6 ~right:3 ~value:7 ~type_ Op.Ic_assign;
              H.jump ~id:8 ~target:1;
            ]);
        block 1
          (load ~type_ 9 (-8L)
          @ [ H.branch ~id:13 ~operand:12 ~target:3 Op.Ic_br_zero ]);
        block 2
          (load ~type_ 14 (-8L)
          @ [
              H.imm ~id:18 ~value:18 ~type_:H.i64 1L;
              H.binary ~id:19 ~left:17 ~right:18 ~value:19 ~type_:H.i64
                Op.Ic_sub;
            ]
          @ address ~type_ 20 (-8L)
          @ [
              H.binary ~id:23 ~left:22 ~right:19 ~value:23 ~type_ Op.Ic_assign;
              H.jump ~id:24 ~target:1;
            ]);
        block 3
          (load ~type_ 25 (-8L)
          @ [
              H.return_value ~id:29 ~operand:28 ~type_;
              H.description 30 Op.Ic_ret;
            ]);
      ]
  in
  List.iter
    (fun input ->
      ignore
        (execute ~max_steps:200 frame [ input; 99L ] function_ |> expect_word 0L))
    [ 3L; 0L; 2L ];
  ignore
    (execute ~max_steps:20 frame [ 100L; 0L ] function_
    |> expect_error "HCIRVM0007")

let invocation_isolation () =
  let frame, _ = analyze source in
  let type_ = H.public_i64 in
  let function_ =
    body frame type_
      [
        block 0
          (load ~type_ 0 16L
          @ [ H.branch ~id:4 ~operand:3 ~target:2 Op.Ic_br_zero ]);
        block 1
          (address ~type_ 5 (-8L)
          @ [
              H.imm ~id:8 ~value:8 ~type_:H.i64 42L;
              H.binary ~id:9 ~left:7 ~right:8 ~value:9 ~type_ Op.Ic_assign;
              H.jump ~id:10 ~target:2;
            ]);
        block 2
          (load ~type_ 11 (-8L)
          @ [
              H.return_value ~id:15 ~operand:14 ~type_;
              H.description 16 Op.Ic_ret;
            ]);
      ]
  in
  ignore (execute frame [ 1L; 0L ] function_ |> expect_word 42L);
  let error = execute frame [ 0L; 0L ] function_ |> expect_error "HCIRVM0012" in
  Alcotest.(check (option int))
    "second invocation reads its own local" (Some 14) error.instruction_id;
  ignore (execute frame [ 1L; 0L ] function_ |> expect_word 42L)

let assignment_results_and_types () =
  let frame, _ = analyze source in
  let type_ = H.public_i64 in
  let function_ =
    body frame type_
      [
        block 0
          (address ~type_ 0 (-8L)
          @ [
              H.imm ~id:3 ~value:3 ~type_:H.u64 Int64.min_int;
              H.binary ~id:4 ~left:2 ~right:3 ~value:4 ~type_ Op.Ic_assign;
              H.return_value ~id:5 ~operand:4 ~type_;
              H.description 6 Op.Ic_ret;
            ]);
      ]
  in
  let execution =
    execute frame [ 0L; 0L ] function_ |> expect_word Int64.min_int
  in
  (match VM.termination execution with
  | Returned (Some word) ->
      Alcotest.(check bool)
        "assignment yields destination signedness" true (word.type_ = VM.I64)
  | _ -> assert false);
  let wrong_type =
    body frame type_
      [
        block 0
          (address ~type_ 0 (-8L)
          @ [
              H.unary ~id:3 ~operand:2 ~value:3 ~type_:H.public_u64 Op.Ic_deref;
              H.return_value ~id:4 ~operand:3 ~type_;
              H.description 5 Op.Ic_ret;
            ]);
      ]
  in
  ignore (execute frame [ 0L; 0L ] wrong_type |> expect_error "HCIRVM0006");
  let public_cast =
    body frame type_
      [
        block 0
          [
            H.imm ~id:0 ~value:0 ~type_:H.i64 42L;
            H.description
              ~operands:[ H.value_id 0 ]
              ~result:(H.result 1) ~target_type:type_ ~payload:(Seq.Integer 0L)
              1 Op.Ic_holyc_typecast;
            H.return_value ~id:2 ~operand:1 ~type_;
            H.description 3 Op.Ic_ret;
          ];
      ]
  in
  ignore (execute frame [ 0L; 0L ] public_cast |> expect_error "HCIRVM0005")

let lowering_boundaries () =
  let frame, function_ = analyze source in
  let root = A.return_value function_ in
  let foreign, _ = analyze source in
  (match
     Expr.lower_typed_result ~frame:foreign ~instruction_id:(H.instruction_id 0)
       ~value_id:(H.value_id 0) root
   with
  | Error ({ Seq.code = "HCIRL0004"; _ } :: _) -> ()
  | _ -> Alcotest.fail "a foreign frame must fail exact binding validation");
  List.iter
    (fun (instruction, value) ->
      match
        Expr.lower_typed_result ~frame
          ~instruction_id:(H.instruction_id instruction)
          ~value_id:(H.value_id value) root
      with
      | Error ({ Seq.code = "HCIRL0005"; _ } :: _) -> ()
      | _ ->
          Alcotest.fail
            "allocation exhaustion must not publish a partial frame load")
    [ (Int.max_int - 3, 0); (0, Int.max_int - 3) ];
  List.iter
    (fun text ->
      let frame, function_ = analyze text in
      let root = A.expression_values function_ |> List.hd in
      match
        Expr.lower_typed_result ~frame ~instruction_id:(H.instruction_id 0)
          ~value_id:(H.value_id 0) root
      with
      | Ok Expr.Unsupported_expression -> ()
      | _ -> Alcotest.fail "unsupported storage expression was accepted")
    [
      "I64 Add(){I32 c;c=1;return 0;}";
      "I64 Add(){I32 c[1];c;return 0;}";
      "I64 Add(){I64 *c[2];c;return 0;}";
      "I64 Add(){F64 c;c=1.0;return 0;}";
      "I64 Add(){I32 c;&c;return 0;}";
      "I64 global; I64 Add(){global=1;return 0;}";
    ];
  List.iter
    (fun mode ->
      List.iter
        (fun (text, bytes) ->
          let frame, function_ = analyze ~mode text in
          let function_ = composed_body frame function_ in
          ignore
            (execute ~max_frame_bytes:bytes frame [] function_
            |> expect_word 42L);
          ignore
            (execute ~max_frame_bytes:(bytes - 1) frame [] function_
            |> expect_error "HCIRVM0011"))
        [
          ("I64 Add(){I64 c[1];c;return 42;}", 8);
          ("I64 Add(){I64 c[2];c;return 42;}", 16);
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let frame_boundaries () =
  let frame, _ = analyze source in
  let function_ = add_body frame in
  let foreign, _ = analyze source in
  ignore (execute foreign [ 20L; 22L ] function_ |> expect_error "HCIRVM0011");
  ignore
    (VM.execute ~max_steps:100 (Body.x87 function_) |> expect_error "HCIRVM0002");
  List.iter
    (fun source ->
      let unsupported, _ = analyze source in
      let function_ =
        body unsupported H.public_i64 [ block 0 [ H.description 0 Op.Ic_ret ] ]
      in
      ignore
        (execute ~max_frame_bytes:64 unsupported [] function_
        |> expect_error "HCIRVM0011"))
    [
      "I64 Add(){I32 c;}";
      "I64 Add(){F64 c;}";
      "I64 Add(){I64 *c[2];}";
      "I64 Add(){I64 c[0];}";
      "I64 Add(){static I64 c;}";
      "I64 Add(){I32 *c;}";
    ];
  let malformed =
    body frame H.public_i64
      [
        block 0
          (load ~type_:H.public_i64 0 (-16L)
          @ [
              H.return_value ~id:4 ~operand:3 ~type_:H.public_i64;
              H.description 5 Op.Ic_ret;
            ]);
      ]
  in
  let error =
    execute frame [ 20L; 22L ] malformed |> expect_error "HCIRVM0004"
  in
  Alcotest.(check int)
    "unallocated address rejected in preflight" 0 error.executed_steps;
  let unreachable =
    body frame H.public_i64
      [
        block 0
          [
            H.imm ~id:0 ~value:0 ~type_:H.i64 42L;
            H.return_value ~id:1 ~operand:0 ~type_:H.public_i64;
            H.description 2 Op.Ic_ret;
          ];
        block 1 [ H.description 3 Op.Ic_nop1; H.description 4 Op.Ic_ret ];
      ]
  in
  ignore (execute frame [ 20L; 22L ] unreachable |> expect_error "HCIRVM0002")

let typed_frame_values () =
  let frame, function_ = analyze source in
  let lower value =
    Expr.lower_typed_result ~frame ~instruction_id:(H.instruction_id 0)
      ~value_id:(H.value_id 0) value
    |> H.require_ok (fun errors ->
        String.concat "; " (List.map H.show_sequence_error errors))
    |> Test_ir_expression_lowering.require_lowered
  in
  let assignment = A.expression_values function_ |> List.hd |> lower in
  let instructions =
    Expr.sequence assignment |> Seq.instructions |> List.map Seq.description
  in
  Alcotest.(check int)
    "two parameter loads, no destination load" 2
    (List.length
       (List.filter (fun item -> item.Seq.opcode = Op.Ic_deref) instructions));
  Alcotest.(check bool)
    "assignment is last" true
    ((List.hd (List.rev instructions)).Seq.opcode = Op.Ic_assign);
  ignore (A.return_value function_ |> lower)

let real_frame_storage () =
  let frame, _ = analyze source in
  let function_ = add_body frame in
  List.iter
    (fun (a, b, expected) ->
      let execution =
        execute ~max_steps:19 frame [ a; b ] function_ |> expect_word expected
      in
      Alcotest.(check int)
        "all instructions charged" 19
        (VM.executed_steps execution))
    [
      (20L, 22L, 42L);
      (-7L, 2L, -5L);
      (0L, 0L, 0L);
      (Int64.max_int, 1L, Int64.min_int);
    ]

let tests =
  [
    Alcotest.test_case "typed frame reads and assignment" `Quick
      typed_frame_values;
    Alcotest.test_case "parameter and local storage execute" `Quick
      real_frame_storage;
    Alcotest.test_case "joined checked lowering and execution" `Quick
      joined_lowering_execution;
    Alcotest.test_case "exact limits and uninitialized reads" `Quick
      limits_and_uninitialized;
    Alcotest.test_case "declared integer return domains" `Quick return_domains;
    Alcotest.test_case "public and internal unsigned negation" `Quick
      public_unsigned_negation;
    Alcotest.test_case "storage survives branches and loop visits" `Quick
      storage_across_transfers;
    Alcotest.test_case "invocations never share local initialization" `Quick
      invocation_isolation;
    Alcotest.test_case "assignment values and exact storage types" `Quick
      assignment_results_and_types;
    Alcotest.test_case "lowering ownership and allocation boundaries" `Quick
      lowering_boundaries;
    Alcotest.test_case "frame and unsupported boundaries" `Quick
      frame_boundaries;
  ]
