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
    "negative zero arithmetic retains raw m64 bits" true
    (e.loads = [ Plan.Bits ]);
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
  List.iter
    (fun opcode ->
      rejects "raw graphs cannot authorize floating comparison"
        [
          a;
          b;
          instruction ~operands:[ 1; 2 ] ~result:3 ~target_type:i64 ~flags:0x40L
            3 opcode;
        ])
    [
      O.Ic_equ_equ; Ic_not_equ; Ic_less; Ic_greater_equ; Ic_greater; Ic_less_equ;
    ];
  let literal_graph = graph [ literal 1 0x7ff0000000001234L ] in
  Alcotest.(check int)
    "literal bits alone need no x87" 0
    (Plan.peak_depth (prepare literal_graph))

module E = Holyc_lib.X86_64_encoder
module F = Holyc_lib.Ir_function_body
module X = Holyc_lib__Ir.X87_stack

let source_plan ?(parameters = "F64 a,F64 b,F64 c") body =
  let open Holyc_lib in
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"comparison-plan.hc"
      ~contents:("I64 F(" ^ parameters ^ "){" ^ body ^ "}42;")
  in
  let program =
    match
      compile_integer_program session ~source
        ~config:
          (Preprocessor.Config.create ~compilation_mode:Preprocessor.Aot ()
          |> Result.get_ok)
    with
    | Ok result -> result.value
    | Error errors ->
        Alcotest.fail
          (String.concat "; "
             (List.map
                (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
                errors))
  in
  let fn = List.hd (integer_program_functions program) in
  let source : Plan.source =
    {
      runtime = integer_program_runtime_calls program;
      body = fn.body;
      frame = fn.frame;
    }
  in
  let graph = X.graph (F.x87 fn.body) in
  let plan =
    match Plan.prepare ~source graph with
    | Ok plan -> plan
    | Error errors ->
        Alcotest.fail
          (body ^ ": "
          ^ String.concat "; "
              (List.map (fun (e : Plan.error) -> e.message) errors))
  in
  let emission = Plan.validated_emission plan ~graph |> Option.get in
  (plan, graph, emission)

let only_comparison emission =
  match emission.Plan.comparison_entries with
  | [ entry ] -> entry
  | _ -> Alcotest.fail "expected one exact comparison receipt"

(* Literal tables read from Templates.HC:39-85,280-322,456-498.
   Orientation is physical LOAD order, whereas live_steps lists ST0 first. *)
let comparison_templates () =
  List.iter
    (fun (left, right, link, orientations, predicates) ->
      List.iter2
        (fun (operator, orientation) predicate ->
          let _, _, emission =
            source_plan ("return " ^ left ^ operator ^ right ^ ";")
          in
          let e = only_comparison emission in
          Alcotest.(check bool)
            "ordered terminal" true
            (e.comparison_kind = Plan.Ordered);
          Alcotest.(check bool)
            "source link side" true (e.comparison_link = link);
          Alcotest.(check bool)
            "physical load order" true
            (e.comparison_orientation = orientation);
          Alcotest.(check bool)
            "source predicate" true
            (e.comparison_predicate = predicate);
          Alcotest.(check bool)
            "ordinary value result" true
            (e.comparison_context = Plan.Comparison_value
            && not e.comparison_negate_result);
          Alcotest.(check int)
            "two transient x87 slots" 2 e.comparison_peak_depth;
          Alcotest.(check bool)
            "comparison consumes all x87 owners" true
            (List.hd (List.rev e.comparison_live_steps) = []);
          Alcotest.(check int)
            "only one retained owner"
            (if link = Plan.Unlinked then 0 else 1)
            (List.length e.comparison_live_before))
        (List.combine [ "<"; ">="; ">"; "<=" ] orientations)
        predicates)
    [
      ( "a",
        "b",
        Plan.Unlinked,
        [ Plan.Right_first; Right_first; Left_first; Left_first ],
        [ E.B; AE; B; AE ] );
      ( "(a+b)",
        "c",
        Plan.Linked_left,
        [ Plan.Left_first; Left_first; Left_first; Left_first ],
        [ E.A; BE; B; AE ] );
      ( "a",
        "(b+c)",
        Plan.Linked_right,
        [ Plan.Right_first; Right_first; Right_first; Right_first ],
        [ E.B; AE; A; BE ] );
      ( "(a+b)",
        "(a+b)",
        Plan.Linked_right,
        [ Plan.Right_first; Right_first; Right_first; Right_first ],
        [ E.B; AE; A; BE ] );
    ];
  List.iter
    (fun (operator, predicate) ->
      let _, _, emission = source_plan ("return (a+b)" ^ operator ^ "c;") in
      let e = only_comparison emission in
      Alcotest.(check bool)
        "equality compares words" true
        (e.comparison_kind = Plan.Bitwise);
      Alcotest.(check bool)
        "integer equality predicate" true
        (e.comparison_predicate = predicate);
      Alcotest.(check bool)
        "equality never retains arithmetic output" true
        (List.for_all
           (fun (a : Plan.entry) -> a.live_after = [])
           emission.arithmetic_entries);
      Alcotest.(check int) "equality uses no x87" 0 e.comparison_peak_depth)
    [ ("==", E.E); ("!=", E.NE) ]

let comparison_conditions () =
  List.iter
    (fun (body, role, negated, predicate, negate_result) ->
      let _, _, emission = source_plan body in
      let e = only_comparison emission in
      Alcotest.(check bool)
        "fused condition predicate" true
        (e.comparison_predicate = predicate);
      Alcotest.(check bool)
        "hosted discriminator polarity" negate_result e.comparison_negate_result;
      Alcotest.(check bool)
        "fused condition load order" true
        (e.comparison_orientation = Plan.Left_first);
      match e.comparison_context with
      | Plan.Comparison_branch c ->
          Alcotest.(check bool) "original condition role" true (c.role = role);
          Alcotest.(check bool) "original NOT parity" negated c.negated
      | _ -> Alcotest.fail "condition lost its source context")
    [
      ( "if(a<b)return 1;return 0;",
        Holyc_lib__Sema.Function_call_resolution.If_condition,
        false,
        E.BE,
        true );
      ( "if(!(a<b))return 1;return 0;",
        Holyc_lib__Sema.Function_call_resolution.If_condition,
        true,
        E.A,
        false );
      ( "do {a=a+1.0;} while(a<b);return 0;",
        Holyc_lib__Sema.Function_call_resolution.Do_while_condition,
        false,
        E.A,
        false );
      ( "do {a=a+1.0;} while(!(a<b));return 0;",
        Holyc_lib__Sema.Function_call_resolution.Do_while_condition,
        true,
        E.BE,
        true );
    ]

let comparison_conversions () =
  List.iter
    (fun (expression, link, predicate) ->
      let _, _, emission =
        source_plan ~parameters:"I64 n,F64 a,F64 b"
          ("return " ^ expression ^ ";")
      in
      let e = only_comparison emission in
      Alcotest.(check bool)
        "conversion selects source template" true (e.comparison_link = link);
      Alcotest.(check bool)
        "conversion predicate" true
        (e.comparison_predicate = predicate);
      Alcotest.(check bool)
        "same-instruction conversion input" true
        (Option.is_some e.comparison_conversion_input);
      Alcotest.(check bool)
        "conversion does not invent earlier ownership" true
        (e.comparison_live_before = []);
      Alcotest.(check bool)
        "conversion stays extended" true
        (List.exists
           (fun (c : Plan.conversion_entry) -> c.deferred)
           emission.conversion_entries))
    [ ("n<a", Plan.Linked_left, E.A); ("a<n", Plan.Linked_right, E.B) ];
  let _, _, emission =
    source_plan ~parameters:"I64 n,F64 a,F64 b" "return n==a;"
  in
  let e = only_comparison emission in
  Alcotest.(check bool)
    "mixed equality materializes conversion" true
    (e.comparison_kind = Plan.Bitwise
    && e.comparison_conversion_input = None
    && List.for_all
         (fun (c : Plan.conversion_entry) -> not c.deferred)
         emission.conversion_entries);
  let _, _, emission = source_plan "return a<1.0;" in
  Alcotest.(check bool)
    "value comparison uses literal m64 bits" true
    ((only_comparison emission).comparison_loads = [ Plan.Bits; Bits ]);
  let _, _, emission = source_plan "if(a<1.0)return 1;return 0;" in
  Alcotest.(check bool)
    "branch comparison selects FLD1" true
    ((only_comparison emission).comparison_loads = [ Plan.Bits; One ])

let comparison_zero_normalization () =
  List.iter
    (fun expression ->
      let plan, _, emission = source_plan ("return " ^ expression ^ ";") in
      let e = only_comparison emission in
      Alcotest.(check bool)
        "public F64 zero comparison retags to signed words" true
        (e.comparison_kind = Plan.Signed_word
        && e.comparison_link = Plan.Unlinked);
      Alcotest.(check int)
        "source zero rewrite needs no x87" 0 (Plan.peak_depth plan);
      Alcotest.(check bool)
        "zero conversion elided" true
        (List.for_all
           (fun (c : Plan.conversion_entry) -> c.elided)
           emission.conversion_entries))
    [ "a<0.0"; "a>=0.0"; "0.0<a"; "0.0>=a"; "a<0"; "0>=a" ];
  List.iter
    (fun expression ->
      let _, _, emission =
        source_plan ("if(" ^ expression ^ ")return 1;return 0;")
      in
      let e = only_comparison emission in
      Alcotest.(check bool)
        "direct branch preserves signed zero phase" true
        (e.comparison_kind = Plan.Signed_word && e.comparison_negate_result))
    [ "a<0.0"; "a>=0.0"; "0.0<a"; "0.0>=a" ];
  let _, _, emission =
    source_plan ~parameters:"F64i a,F64i b,F64i c" "return a<0.0;"
  in
  Alcotest.(check bool)
    "F64i is not the pinned canonical public class" true
    ((only_comparison emission).comparison_kind = Plan.Ordered)

let comparison_negative_zero_literal () =
  List.iter
    (fun (body, expected_load) ->
      let _, _, emission = source_plan body in
      let e = only_comparison emission in
      Alcotest.(check bool)
        "negative zero remains an ordered floating comparison" true
        (e.comparison_kind = Plan.Ordered
        && e.comparison_link = Plan.Linked_left);
      Alcotest.(check bool)
        "negative zero bits survive both load paths" true
        (List.nth e.comparison_loads 1 = expected_load);
      let folded =
        List.find
          (fun (a : Plan.entry) ->
            a.operation = Plan.Literal_bits Int64.min_int)
          emission.arithmetic_entries
      in
      Alcotest.(check bool)
        "folding cannot consume the earlier extended owner" true
        (folded.live_before = folded.live_after
        && List.length folded.live_before = 1);
      Alcotest.(check bool)
        "comparison consumes retained arithmetic" true
        (List.length e.comparison_live_before = 1))
    [
      ("return (a+b)<-0.0;", Plan.Bits);
      ("if((a+b)<-0.0)return 1;return 0;", Plan.Bits);
    ]

let () =
  Alcotest.run "native scalar F64 plan"
    [
      ( "source plans",
        [
          Alcotest.test_case "identities and ownership" `Quick isolated;
          Alcotest.test_case "constant load orientation" `Quick constants;
          Alcotest.test_case "BackFB identity copies" `Quick identities;
          Alcotest.test_case "fail-closed boundaries" `Quick boundaries;
          Alcotest.test_case "comparison template receipts" `Quick
            comparison_templates;
          Alcotest.test_case "original condition receipts" `Quick
            comparison_conditions;
          Alcotest.test_case "comparison conversion events" `Quick
            comparison_conversions;
          Alcotest.test_case "comparison zero normalization" `Quick
            comparison_zero_normalization;
          Alcotest.test_case "negative zero literal receipt" `Quick
            comparison_negative_zero_literal;
        ] );
    ]
