(* Mutations exercise caller-visible source and graph objects, never private
   comparison receipts. Expected rejection follows original source ownership,
   not acceptance by another compiler path. *)
open Holyc_lib
module Plan = Holyc_lib__Backend.X86_64_float_plan
module Unit = Holyc_lib__Driver.Integer_unit
module F = Holyc_lib__Ir.Function_body
module G = Ir_block_graph
module S = Ir_instruction_sequence
module O = Ir_opcode
module Typed = Holyc_lib__Sema.Function_call_expression_result
module Source = Holyc_lib__Sema.Function_call_resolution

let checked = function
  | Ok x -> x
  | Error _ -> Alcotest.fail "original comparison fixture rejected"

let fixture ?(body = "if((a+b)>a)return 1;return 0;") () =
  let contents = "I64 F(F64 a,F64 b){" ^ body ^ "}" in
  let fixture =
    Native_scalar_fixture.compile ~mode:Preprocessor.Aot
      ~path:"comparison-authority.hc" ~contents ()
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

let comparison d =
  List.mem d.S.opcode
    [
      O.Ic_less; Ic_greater_equ; Ic_greater; Ic_less_equ; Ic_equ_equ; Ic_not_equ;
    ]

let rejects label = function
  | Error (_ :: _) -> ()
  | _ -> Alcotest.fail (label ^ " admitted an unproved comparison")

let mutate graph select transform =
  let changed = ref false in
  let rec visit = function
    | [] -> ()
    | instruction :: rest as cell ->
        let d = S.description instruction in
        if (not !changed) && select d then (
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform d));
          changed := true)
        else visit rest
  in
  List.iter
    (fun block -> visit (S.instructions (G.instructions block)))
    (G.blocks graph);
  Alcotest.(check bool) "mutation found original instruction" true !changed

let invalidated label source graph plan =
  Alcotest.(check bool)
    (label ^ " invalidates retained receipt")
    true
    (Plan.validated_comparisons plan ~graph = None);
  rejects (label ^ " rejects a fresh receipt") (Plan.prepare ~source graph)

let owners () =
  let source, graph = fixture () in
  let foreign, foreign_graph = fixture () in
  let plan = Plan.prepare ~source graph |> checked in
  Alcotest.(check int)
    "one original comparison" 1
    (Plan.validated_comparisons plan ~graph |> Option.get |> List.length);
  rejects "missing original source" (Plan.prepare graph);
  List.iter
    (fun (label, source) -> rejects label (Plan.prepare ~source graph))
    [
      ("foreign runtime", { source with runtime = foreign.runtime });
      ("foreign frame", { source with frame = foreign.frame });
      ("foreign body", { source with body = foreign.body });
      ( "copied frame",
        { source with frame = Obj.obj (Obj.dup (Obj.repr source.frame)) } );
    ];
  Alcotest.(check bool)
    "foreign equal-text graph" true
    (Plan.validated_comparisons plan ~graph:foreign_graph = None);
  let copy =
    G.create
      ~entry:(G.block_id (G.entry graph))
      (List.map
         (fun block ->
           ({
              G.block_id = G.block_id block;
              instructions =
                S.instructions (G.instructions block) |> List.map S.description;
            }
             : G.block_description))
         (G.blocks graph))
    |> checked
  in
  rejects "copied graph" (Plan.prepare ~source copy);
  Alcotest.(check bool)
    "copied graph cannot reuse receipt" true
    (Plan.validated_comparisons plan ~graph:copy = None)

let graph_changes () =
  let i64 =
    Semantic_type.make_primitive ~form:Internal_storage
      ~primitive:Primitive_type.I64 ~pointer_depth:0
    |> checked
  in
  let f64 =
    Semantic_type.make_primitive ~form:Internal_storage
      ~primitive:Primitive_type.F64 ~pointer_depth:0
    |> checked
  in
  List.iter
    (fun (label, body, select, transform) ->
      let source, graph = fixture ~body () in
      let plan = Plan.prepare ~source graph |> checked in
      mutate graph select transform;
      invalidated label source graph plan)
    [
      ( "comparison operand order",
        "return a<b;",
        comparison,
        fun d -> { d with S.operands = List.rev d.operands } );
      ( "comparison flags",
        "return a<b;",
        comparison,
        fun d -> { d with S.flags = Int64.logor d.flags 0x1000000L } );
      ( "comparison opcode",
        "return a<b;",
        comparison,
        fun d -> { d with S.opcode = O.Ic_greater } );
      ( "comparison result type",
        "return a<b;",
        comparison,
        fun d -> { d with S.target_type = Some f64 } );
      ( "producer type",
        "return (a+b)>a;",
        (fun d -> d.S.opcode = O.Ic_add),
        fun d -> { d with S.target_type = Some i64 } );
      ( "producer flags",
        "return (a+b)>a;",
        (fun d -> d.S.opcode = O.Ic_add),
        fun d -> { d with S.flags = 2L } );
      ( "literal bits",
        "return a<1.0;",
        (fun d -> d.S.opcode = O.Ic_imm_f64),
        fun d -> { d with S.payload = Some (S.Float_bits 0x4000000000000000L) }
      );
      ( "branch polarity",
        "if(a<b)return 1;return 0;",
        (fun d -> d.S.opcode = O.Ic_br_zero),
        fun d -> { d with S.opcode = O.Ic_br_not_zero } );
      ( "branch operands",
        "if(a<b)return 1;return 0;",
        (fun d -> d.S.opcode = O.Ic_br_zero),
        fun d -> { d with S.operands = [] } );
      ( "branch target",
        "if(a<b)return 1;return 0;",
        (fun d -> d.S.opcode = O.Ic_br_zero),
        fun d -> { d with S.payload = None } );
    ]

let mutate_field target index replacement action =
  let object_ = Obj.repr target in
  let previous = Obj.field object_ index in
  Fun.protect
    ~finally:(fun () -> Obj.set_field object_ index previous)
    (fun () ->
      Obj.set_field object_ index replacement;
      action ())

let source_changes () =
  List.iter
    (fun (label, edit) ->
      let source, graph = fixture () in
      let plan = Plan.prepare ~source graph |> checked in
      let original = F.definition_source source.Plan.body |> Option.get in
      edit source original (fun () -> invalidated label source graph plan);
      ignore (Plan.prepare ~source graph |> checked))
    [
      ( "source condition role",
        fun _ original action ->
          let condition =
            Typed.function_conditions original
            |> List.hd |> Typed.condition_source
          in
          mutate_field condition 1 (Obj.repr Source.Do_while_condition) action
      );
      ( "source binary operator",
        fun _ original action ->
          let value =
            Typed.function_conditions original
            |> List.hd |> Typed.condition_value
          in
          let binary =
            match
              Source.argument_expression_kind (Typed.result_source value)
            with
            | Source.Binary_expression b -> b
            | _ -> Alcotest.fail "expected original source comparison"
          in
          mutate_field binary 0 (Obj.repr O.Ic_less) action );
      ( "typed operand class",
        fun _ original action ->
          let value =
            Typed.function_conditions original
            |> List.hd |> Typed.condition_value
          in
          let left, _ = Typed.result_binary_operands value |> Option.get in
          let i64 =
            Semantic_type.make_primitive ~form:Internal_storage
              ~primitive:Primitive_type.I64 ~pointer_depth:0
            |> checked
          in
          mutate_field left 13 (Obj.repr (Some i64)) action );
      ( "condition expression owner",
        fun _ original action ->
          let foreign, _ =
            fixture ~body:"if(!((a+b)>a))return 1;return 0;" ()
          in
          let foreign_source =
            F.definition_source foreign.Plan.body |> Option.get
          in
          let foreign_expression =
            Typed.function_conditions foreign_source
            |> List.hd |> Typed.condition_source |> Source.condition_expression
          in
          let condition =
            Typed.function_conditions original
            |> List.hd |> Typed.condition_source
          in
          mutate_field condition 3 (Obj.repr foreign_expression) action );
      ( "compiler options",
        fun source _ action ->
          mutate_field source.Plan.body 8
            (Obj.repr
               (Int64.logxor
                  (F.compiler_options source.body)
                  (Compiler_option.mask Compiler_option.No_reg_var)))
            action );
    ]

let successor_changes () =
  let source, graph = fixture () in
  let plan = Plan.prepare ~source graph |> checked in
  let block =
    G.blocks graph |> List.find (fun b -> List.length (G.successors b) > 1)
  in
  mutate_field block 2
    (Obj.repr (List.rev (G.successors block)))
    (fun () -> invalidated "successor order" source graph plan)

let () =
  Alcotest.run "native F64 comparison authority"
    [
      ( "source receipts",
        [
          Alcotest.test_case "original ownership" `Quick owners;
          Alcotest.test_case "instruction and branch snapshots" `Quick
            graph_changes;
          Alcotest.test_case "source context and options" `Quick source_changes;
          Alcotest.test_case "control-flow edge order" `Quick successor_changes;
        ] );
    ]
