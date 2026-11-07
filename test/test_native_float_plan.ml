module Plan = Holyc_lib__Backend.X86_64_float_plan
module G = Holyc_lib.Ir_block_graph
module S = Holyc_lib.Ir_instruction_sequence
module O = Holyc_lib.Ir_opcode
module T = Holyc_lib.Semantic_type
module P = Holyc_lib.Primitive_type

let get = function
  | Ok value -> value
  | Error _ -> Alcotest.fail "invalid fixture"

let iid n = S.Instruction_id.of_int n |> get
let vid n = S.Value_id.of_int n |> get
let bid n = S.Block_id.of_int n |> get

let primitive p =
  T.make_primitive ~form:Internal_storage ~primitive:p ~pointer_depth:0 |> get

let f64 = primitive P.F64
let i64 = primitive P.I64

let instruction ?(operands = []) ?result ?target_type ?payload ?(flags = 0L) id
    opcode : S.description =
  {
    instruction_id = iid id;
    opcode;
    operands = List.map vid operands;
    result =
      Option.map (fun n -> ({ value_id = vid n } : S.value_definition)) result;
    target_type;
    payload;
    flags;
    span = None;
  }

let literal id bits =
  instruction ~result:id ~target_type:f64 ~payload:(S.Float_bits bits) id
    O.Ic_imm_f64

let binary ?(flags = 0L) id opcode left right =
  instruction ~operands:[ left; right ] ~result:id ~target_type:f64 ~flags id
    opcode

let graph descriptions =
  let end_ = instruction 100 O.Ic_end in
  G.create ~entry:(bid 0)
    [ { G.block_id = bid 0; instructions = descriptions @ [ end_ ] } ]
  |> get

let prepare graph =
  match Plan.prepare graph with
  | Ok plan -> plan
  | Error errors ->
      Alcotest.fail
        (String.concat "; "
           (List.map (fun (e : Plan.error) -> e.message) errors))

let entry graph plan description =
  Plan.find plan ~graph description |> Option.get

let rejects label descriptions =
  Alcotest.(check bool)
    label true
    (Result.is_error (Plan.prepare (graph descriptions)))

let isolated () =
  let op = binary 3 O.Ic_sub 1 2 in
  let g =
    graph [ literal 1 0x4000000000000000L; literal 2 0x4014000000000000L; op ]
  in
  let plan = prepare g in
  let e = entry g plan op in
  Alcotest.(check int)
    "original instruction" 3
    (S.Instruction_id.to_int e.instruction_id);
  Alcotest.(check int) "original result" 3 (S.Value_id.to_int e.result_id);
  Alcotest.(check (list int))
    "ordered operands" [ 1; 2 ]
    (List.map S.Value_id.to_int e.operands);
  Alcotest.(check int) "memory arithmetic peak" 1 (Plan.peak_depth plan);
  Alcotest.(check (list (list int)))
    "extended identities" [ []; [ 1 ]; [ 3 ]; [] ]
    (List.map (List.map S.Value_id.to_int) e.live_steps);
  let foreign =
    graph [ literal 1 0x4000000000000000L; literal 2 0x4014000000000000L; op ]
  in
  Alcotest.(check bool)
    "foreign graph rejected" true
    (Option.is_none (Plan.find plan ~graph:foreign op))

let constants () =
  let op = binary 3 O.Ic_sub 1 2 in
  let g =
    graph [ literal 1 0x4000000000000000L; literal 2 0x3ff0000000000000L; op ]
  in
  let e = entry g (prepare g) op in
  Alcotest.(check bool)
    "builtin first, reverse memory" true
    (e.orientation = Plan.Right_first);
  let g =
    graph [ literal 1 0x3ff0000000000000L; literal 2 0x3ff0000000000000L; op ]
  in
  Alcotest.(check int) "two constants peak" 2 (Plan.peak_depth (prepare g));
  let neg =
    instruction ~operands:[ 1 ] ~result:2 ~target_type:f64 2 O.Ic_unary_minus
  in
  let g = graph [ literal 1 Int64.min_int; neg ] in
  let e = entry g (prepare g) neg in
  Alcotest.(check bool)
    "negative zero arithmetic loads FLDZ" true (e.loads = [ Plan.Zero ]);
  rejects "option-dependent PI load rejects"
    [ literal 1 0x400921fb54442d18L; literal 2 0x4000000000000000L; op ]

let identities () =
  List.iter
    (fun (opcode, l, r, expected) ->
      let op = binary 3 opcode 1 2 in
      let g = graph [ literal 1 l; literal 2 r; op ] in
      let e = entry g (prepare g) op in
      Alcotest.(check bool) "source identity copy" true (e.operation = expected);
      Alcotest.(check int) "copy does not use x87" 0 e.peak_depth)
    [
      (O.Ic_add, 0L, 0x7ff0000000001234L, Plan.Copy_right);
      (O.Ic_sub, Int64.min_int, 0L, Plan.Copy_left);
      (O.Ic_mul, 0x3ff0000000000000L, 0x7ff0000000001234L, Plan.Copy_right);
      (O.Ic_div, 0x7ff0000000001234L, 0x3ff0000000000000L, Plan.Copy_left);
    ]

let boundaries () =
  let a = literal 1 0x4340000000000000L and b = literal 2 0x3ff0000000000000L in
  rejects "linked precision cannot round silently"
    [ a; b; binary 3 O.Ic_add 1 2; binary 4 O.Ic_sub 3 1 ];
  rejects "forged depth-balanced suppression"
    [ a; b; binary ~flags:0x1200000L 3 O.Ic_add 1 2 ];
  let integer =
    instruction ~result:2 ~target_type:i64 ~payload:(S.Integer 1L) 2
      O.Ic_imm_i64
  in
  rejects "implicit conversion rejected" [ a; integer; binary 3 O.Ic_add 1 2 ];
  rejects "float remainder rejected" [ a; b; binary 3 O.Ic_mod 1 2 ];
  let literal_graph = graph [ literal 1 0x7ff0000000001234L ] in
  Alcotest.(check int)
    "literal bits alone need no x87" 0
    (Plan.peak_depth (prepare literal_graph))

let () =
  Alcotest.run "native scalar F64 plan"
    [
      ( "source plans",
        [
          Alcotest.test_case "identities and ownership" `Quick isolated;
          Alcotest.test_case "constant load orientation" `Quick constants;
          Alcotest.test_case "BackFB identity copies" `Quick identities;
          Alcotest.test_case "fail-closed boundaries" `Quick boundaries;
        ] );
    ]
