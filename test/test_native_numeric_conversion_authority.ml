open Holyc_lib
module Plan = Holyc_lib__Backend.X86_64_float_plan
module Unit = Holyc_lib__Driver.Integer_unit
module F = Holyc_lib__Ir.Function_body
module G = Ir_block_graph
module S = Ir_instruction_sequence
module O = Ir_opcode

let checked = function
  | Ok x -> x
  | Error _ -> Alcotest.fail "fixture rejected"

let fixture ?(body = "return n+y;") ?contents () =
  let fixture =
    Native_scalar_fixture.compile ~mode:Preprocessor.Aot
      ~path:"numeric-authority.hc"
      ~contents:
        (Option.value contents ~default:("I64 F(I64 n,F64 y){" ^ body ^ "}"))
      ()
    |> checked
  in
  let definition = List.hd (Unit.functions fixture.unit_) in
  let source : Plan.source =
    {
      runtime = Unit.runtime_calls fixture.unit_;
      body = definition.body;
      frame = definition.frame;
    }
  in
  (source, F.body definition.body)

let rejects = function
  | Error (_ :: _) -> ()
  | _ -> Alcotest.fail "unproved conversion admitted"

let mutate graph predicate transform =
  let changed = ref false in
  let rec visit = function
    | [] -> ()
    | instruction :: rest as cell ->
        let d = S.description instruction in
        if (not !changed) && predicate d then (
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform d));
          changed := true)
        else visit rest
  in
  List.iter
    (fun block -> visit (S.instructions (G.instructions block)))
    (G.blocks graph);
  Alcotest.(check bool) "mutation selected original record" true !changed

let canonical () =
  let source, graph = fixture () in
  let plan = Plan.prepare ~source graph |> checked in
  let conversions = Plan.validated_conversions plan ~graph |> Option.get in
  Alcotest.(check int)
    "operand and result conversions" 2 (List.length conversions);
  Alcotest.(check bool)
    "runtime MOV conversion migrates to operand" true
    (List.exists (fun (c : Plan.conversion_entry) -> c.deferred) conversions);
  let source, graph = fixture ~body:"return (n+1)+y;" () in
  let plan = Plan.prepare ~source graph |> checked in
  let entries = Plan.validated_entries plan ~graph |> Option.get in
  Alcotest.(check bool)
    "integer arithmetic conversion remains its own linked event" true
    (List.exists
       (fun (e : Plan.entry) ->
         e.operation = Plan.Convert_to_float && e.live_after <> [])
       entries)

let ownership () =
  let source, graph = fixture () in
  let foreign, _ = fixture () in
  rejects (Plan.prepare graph);
  rejects (Plan.prepare ~source:{ source with runtime = foreign.runtime } graph);
  rejects (Plan.prepare ~source:{ source with frame = foreign.frame } graph);
  rejects (Plan.prepare ~source:{ source with body = foreign.body } graph)

let snapshots () =
  List.iter
    (fun transform ->
      let source, graph = fixture () in
      let plan = Plan.prepare ~source graph |> checked in
      mutate graph (fun d -> d.S.flags = 1L) transform;
      Alcotest.(check bool)
        "changed conversion invalidates cached receipt" true
        (Plan.validated_conversions plan ~graph = None);
      rejects (Plan.prepare ~source graph))
    [
      (fun d -> { d with S.flags = 0L });
      (fun d -> { d with S.flags = 2L });
      (fun d -> { d with S.flags = 3L });
      (fun d -> { d with S.operands = [] });
    ]

let intrinsic_snapshots () =
  let source, graph =
    fixture ~contents:"_intern 28 F64 C(I64 n);F64 F(I64 n){return C(n);}" ()
  in
  let plan = Plan.prepare ~source graph |> checked in
  Alcotest.(check int)
    "one original intrinsic event" 1
    (Plan.validated_intrinsics plan ~graph |> Option.get |> List.length);
  rejects (Plan.prepare graph);
  mutate graph
    (fun d -> d.S.opcode = O.Ic_to_f64)
    (fun d -> { d with S.opcode = O.Ic_to_i64 });
  Alcotest.(check bool)
    "changed intrinsic invalidates cached receipt" true
    (Plan.validated_intrinsics plan ~graph = None);
  rejects (Plan.prepare ~source graph)

let arithmetic_intrinsic () =
  let contents =
    "_intern 27 I64 C(F64 x);I64 F(F64 a,F64 b,F64 c){return C((a+b)+c);}"
  in
  List.iter
    (fun transform ->
      let source, graph = fixture ~contents () in
      let plan = Plan.prepare ~source graph |> checked in
      let receipt =
        Plan.validated_intrinsics plan ~graph |> Option.get |> List.hd
      in
      let entries = Plan.validated_entries plan ~graph |> Option.get in
      Alcotest.(check bool)
        "internal argument link preserved" true
        (List.exists (fun (e : Plan.entry) -> e.live_after <> []) entries);
      let terminal =
        List.find
          (fun (e : Plan.entry) -> e.result_id = receipt.intrinsic_input)
          entries
      in
      Alcotest.(check bool)
        "terminal argument materializes" true
        (terminal.live_after = [] && terminal.result_conversion = None);
      rejects (Plan.prepare graph);
      let foreign, _ = fixture ~contents () in
      rejects
        (Plan.prepare ~source:{ source with runtime = foreign.runtime } graph);
      mutate graph
        (fun d -> d.S.instruction_id = terminal.instruction_id)
        transform;
      Alcotest.(check bool)
        "argument mutation invalidates intrinsic receipt" true
        (Plan.validated_intrinsics plan ~graph = None);
      rejects (Plan.prepare ~source graph))
    [
      (fun d -> { d with S.flags = 2L });
      (fun d -> { d with S.opcode = O.Ic_sub });
      (fun d -> { d with S.operands = List.rev d.operands });
    ]

let intrinsic_consumer () =
  let contents = "_intern 28 F64 C(I64 x);I64 F(I64 n,F64 y){return C(n)+y;}" in
  List.iter
    (fun (select_end, transform) ->
      let source, graph = fixture ~contents () in
      Alcotest.(check bool)
        "ordinary target query remains narrow" true
        (Holyc_lib__Ir.Runtime_call_context.function_call_targets source.runtime
           ~body:source.body
        = None);
      Alcotest.(check bool)
        "numeric body has canonical proof" true
        (Holyc_lib__Ir.Register_variable_analysis
         .authenticates_mixed_scalar_source ~runtime:source.runtime
           ~frame:source.frame ~body:source.body);
      let plan = Plan.prepare ~source graph |> checked in
      let entries = Plan.validated_entries plan ~graph |> Option.get in
      Alcotest.(check bool)
        "consumer has no incoming extended owner" true
        (List.for_all (fun (e : Plan.entry) -> e.live_before = []) entries);
      let receipt =
        Plan.validated_intrinsics plan ~graph |> Option.get |> List.hd
      in
      let foreign, _ = fixture ~contents () in
      rejects
        (Plan.prepare ~source:{ source with runtime = foreign.runtime } graph);
      mutate graph
        (fun d ->
          if select_end then
            Option.fold ~none:false
              ~some:(fun r -> r.S.value_id = receipt.intrinsic_result)
              d.S.result
          else d.S.opcode = O.Ic_add)
        transform;
      Alcotest.(check bool)
        "boundary/consumer mutation invalidates receipt" true
        (Plan.validated_intrinsics plan ~graph = None);
      rejects (Plan.prepare ~source graph))
    [
      (true, fun d -> { d with S.flags = 2L });
      (true, fun d -> { d with S.payload = None });
      (false, fun d -> { d with S.operands = List.rev d.operands });
      (false, fun d -> { d with S.opcode = O.Ic_sub });
    ]

let consumer_source_invalidation () =
  let contents = "_intern 28 F64 C(I64 x);F64 F(I64 n,F64 y){return C(n)+y;}" in
  let source, graph = fixture ~contents () in
  let plan = Plan.prepare ~source graph |> checked in
  Alcotest.(check int)
    "no implicit conversions mask canonical dependency" 0
    (Plan.validated_conversions plan ~graph |> Option.get |> List.length);
  let module T = Holyc_lib__Sema.Function_call_expression_result in
  let module C = Holyc_lib__Sema.Function_call_resolution in
  let original = F.definition_source source.body |> Option.get in
  let binary =
    T.function_all_results original
    |> List.find_map (fun value ->
        match C.argument_expression_source (T.result_source value) with
        | Some (Holyc_lib__Frontend.Ast.Binary_expression binary) -> Some binary
        | _ -> None)
    |> Option.get
  in
  Obj.set_field (Obj.repr binary) 3 (Obj.repr binary.binary_left);
  Alcotest.(check bool)
    "changed source revokes cached canonical consumer" true
    (Plan.validated_intrinsics plan ~graph = None);
  rejects (Plan.prepare ~source graph);
  let source, graph = fixture ~contents () in
  let foreign, _ = fixture ~contents () in
  let plan = Plan.prepare ~source graph |> checked in
  let module Frame = Holyc_lib__Sema.Function_frame_layout in
  Obj.set_field (Obj.repr source.frame) 1
    (Obj.repr (Frame.function_header foreign.frame));
  Alcotest.(check bool)
    "changed frame revokes cached canonical consumer" true
    (Plan.validated_intrinsics plan ~graph = None);
  rejects (Plan.prepare ~source graph)

let () =
  Alcotest.run "numeric conversion authority"
    [
      ( "source proofs",
        [
          Alcotest.test_case "consumer source and frame invalidation" `Quick
            consumer_source_invalidation;
          Alcotest.test_case "materialized intrinsic consumer" `Quick
            intrinsic_consumer;
          Alcotest.test_case "materialized intrinsic argument" `Quick
            arithmetic_intrinsic;
          Alcotest.test_case "canonical ordered events" `Quick canonical;
          Alcotest.test_case "foreign and absent ownership" `Quick ownership;
          Alcotest.test_case "conversion and operand snapshots" `Quick snapshots;
          Alcotest.test_case "intrinsic identity snapshots" `Quick
            intrinsic_snapshots;
        ] );
    ]
