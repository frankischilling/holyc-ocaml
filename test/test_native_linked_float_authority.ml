(* Authority regressions for the linked F64 plan, pinned to TempleOS
   c26482bb6ad3f80106d28504ec5db3c6a360732c. These mutate caller-visible source
   evidence, never private plan or compiled-image fields. No floating arithmetic
   is used as an expected-value oracle. *)
open Holyc_lib
module Plan = Holyc_lib__Backend.X86_64_float_plan
module Unit = Holyc_lib__Driver.Integer_unit
module F = Holyc_lib__Ir.Function_body
module G = Ir_block_graph
module S = Ir_instruction_sequence
module O = Ir_opcode
module X = Holyc_lib__Ir.X87_stack
module Typed = Holyc_lib__Sema.Function_call_expression_result
module Source = Holyc_lib__Sema.Function_call_resolution

let diagnostics errors =
  String.concat "; "
    (List.map (fun (e : Diagnostic.t) -> e.code ^ ": " ^ e.message) errors)

let checked show = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (show errors)

let plan_errors errors =
  String.concat "; " (List.map (fun (e : Plan.error) -> e.message) errors)

let fixture ?(expression = "(a+b)-c") () =
  let contents = "F64 F(F64 a,F64 b,F64 c){return " ^ expression ^ ";}" in
  let unit_ =
    Native_scalar_fixture.compile ~mode:Preprocessor.Aot
      ~path:"native-linked-float-authority.hc" ~contents ()
    |> checked diagnostics
    |> fun fixture -> fixture.unit_
  in
  let definition = List.hd (Unit.functions unit_) in
  let source : Plan.source =
    {
      runtime = Unit.runtime_calls unit_;
      body = definition.body;
      frame = definition.frame;
    }
  in
  (source, X.graph (F.x87 definition.body))

let descriptions graph =
  G.blocks graph
  |> List.concat_map (fun b ->
      S.instructions (G.instructions b) |> List.map S.description)

let arithmetic (d : S.description) =
  List.mem d.opcode [ O.Ic_add; Ic_sub; Ic_mul; Ic_div; Ic_unary_minus ]

let last_operation graph =
  descriptions graph |> List.filter arithmetic |> List.rev |> List.hd

let prepare source graph = Plan.prepare ~source graph |> checked plan_errors

let rejects label = function
  | Error (_ :: _) -> ()
  | Error [] -> Alcotest.fail (label ^ " returned no explanation")
  | Ok _ -> Alcotest.fail (label ^ " unexpectedly admitted linked arithmetic")

let absent label plan graph description =
  Alcotest.(check bool)
    label true
    (Option.is_none (Plan.find plan ~graph description))

(* Replace a public sequence-list element exactly as the existing IR mutation
   regressions do. The replacement has no access to plan internals. *)
let replace graph predicate transform =
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
  if not !changed then Alcotest.fail "mutation fixture instruction not found"

let owned_sources () =
  List.iter
    (fun expression ->
      let source, graph = fixture ~expression () in
      let plan = prepare source graph in
      let entries =
        descriptions graph |> List.filter_map (Plan.find plan ~graph)
      in
      Alcotest.(check bool)
        (expression ^ " retains an extended producer")
        true
        (List.exists (fun (e : Plan.entry) -> e.live_after <> []) entries);
      Alcotest.(check bool)
        "owned root is admitted" true
        (Option.is_some (Plan.find plan ~graph (last_operation graph))))
    [ "(a+b)-c"; "a-(b/c)"; "(a+2.0)/c" ]

let exact_owners () =
  let source, graph = fixture () in
  let foreign, foreign_graph = fixture () in
  let plan = prepare source graph in
  let root = last_operation graph in
  rejects "no source witness" (Plan.prepare graph);
  rejects "foreign runtime"
    (Plan.prepare ~source:{ source with runtime = foreign.runtime } graph);
  rejects "foreign body"
    (Plan.prepare ~source:{ source with body = foreign.body } graph);
  rejects "foreign frame"
    (Plan.prepare ~source:{ source with frame = foreign.frame } graph);
  rejects "copied frame"
    (Plan.prepare
       ~source:{ source with frame = Obj.obj (Obj.dup (Obj.repr source.frame)) }
       graph);
  rejects "foreign source bundle" (Plan.prepare ~source:foreign graph);
  absent "plan cannot cross equal-text graph ownership" plan foreign_graph
    (last_operation foreign_graph);
  let foreign_plan = prepare foreign foreign_graph in
  absent "foreign plan cannot enter original graph" foreign_plan graph root

let graph_mutations () =
  let mutations =
    [
      ( "ordered producer operands",
        "(a+b)-c",
        arithmetic,
        fun (d : S.description) -> { d with operands = List.rev d.operands } );
      ( "literal payload",
        "(a+2.0)/c",
        (fun d -> d.S.opcode = O.Ic_imm_f64),
        fun d -> { d with S.payload = Some (S.Float_bits 0x4008000000000000L) }
      );
      ( "producer flags",
        "(a+b)-c",
        arithmetic,
        fun d -> { d with S.flags = Int64.logor d.flags 0x1000000L } );
      ( "producer opcode",
        "(a+b)-c",
        arithmetic,
        fun d -> { d with S.opcode = O.Ic_mul } );
    ]
  in
  List.iter
    (fun (label, expression, predicate, transform) ->
      let source, graph = fixture ~expression () in
      let plan = prepare source graph in
      let root = last_operation graph in
      replace graph predicate transform;
      absent
        (label ^ " invalidates retained primitive evidence")
        plan graph root;
      rejects (label ^ " cannot mint a new plan") (Plan.prepare ~source graph))
    mutations

let shared_consumers () =
  let source, graph = fixture () in
  let plan = prepare source graph in
  let root = last_operation graph in
  replace graph
    (fun d -> S.Instruction_id.equal d.S.instruction_id root.instruction_id)
    (fun d ->
      match d.S.operands with
      | first :: _ -> { d with operands = [ first; first ] }
      | [] -> Alcotest.fail "missing linked consumer operands");
  let changed_root = last_operation graph in
  absent "duplicate consumer cannot use old entry" plan graph changed_root;
  absent "old description cannot hide duplicate consumer mutation" plan graph
    root;
  rejects "shared extended producer" (Plan.prepare ~source graph)

let mutate_source_kind source change =
  let original = F.definition_source source.Plan.body |> Option.get in
  let value =
    Typed.function_all_results original
    |> List.find (fun value ->
        match Source.argument_expression_kind (Typed.result_source value) with
        | Source.Float_literal _ -> true
        | _ -> false)
  in
  let expression = Typed.result_source value in
  let block = Obj.repr expression in
  let saved = Obj.field block 0 in
  Fun.protect
    ~finally:(fun () -> Obj.set_field block 0 saved)
    (fun () ->
      Obj.set_field block 0
        (Obj.repr (Source.Float_literal 0x4008000000000000L));
      change ())

let source_snapshots () =
  let source, graph = fixture ~expression:"(a+2.0)/c" () in
  let plan = prepare source graph in
  let root = last_operation graph in
  mutate_source_kind source (fun () ->
      absent "changed typed literal invalidates existing plan" plan graph root;
      rejects "changed typed literal cannot authorize original graph"
        (Plan.prepare ~source graph));
  Alcotest.(check bool)
    "restored original source remains valid" true
    (Option.is_some (Plan.find (prepare source graph) ~graph root))

let tests =
  [
    Alcotest.test_case "linked arithmetic requires exact original source" `Quick
      owned_sources;
    Alcotest.test_case "runtime, body, frame and graph ownership" `Quick
      exact_owners;
    Alcotest.test_case "producer snapshots and ordered operands" `Quick
      graph_mutations;
    Alcotest.test_case "duplicate and shared extended consumer" `Quick
      shared_consumers;
    Alcotest.test_case "typed source primitive snapshot" `Quick source_snapshots;
  ]
