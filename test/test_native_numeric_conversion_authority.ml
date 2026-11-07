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

let () =
  Alcotest.run "numeric conversion authority"
    [
      ( "source proofs",
        [
          Alcotest.test_case "canonical ordered events" `Quick canonical;
          Alcotest.test_case "foreign and absent ownership" `Quick ownership;
          Alcotest.test_case "conversion and operand snapshots" `Quick snapshots;
          Alcotest.test_case "intrinsic identity snapshots" `Quick
            intrinsic_snapshots;
        ] );
    ]
