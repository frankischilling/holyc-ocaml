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

let normalization_receipts () =
  let module N = Holyc_lib__Ir.Numeric_normalization in
  List.iter
    (fun (expression, deleted) ->
      let source, graph =
        fixture ~contents:("I64 F(I64 n){return " ^ expression ^ ";}") ()
      in
      let receipts =
        N.prepare ~runtime:source.runtime ~body:source.body ~frame:source.frame
          graph
      in
      Alcotest.(check int) "one normalized producer" 1 (List.length receipts);
      let receipt = List.hd receipts in
      Alcotest.(check bool)
        "source normalization receipt valid" true (N.valid receipt ~graph);
      let states = N.states receipt in
      Alcotest.(check int) "ordered phases" 5 (List.length states);
      let normalized = List.nth states 2
      and returned = List.nth states 3
      and migrated = List.nth states 4 in
      Alcotest.(check bool)
        "raw class differs from stack class for deleted identity" true
        ((normalized.raw_class = if deleted then N.Integer64 else N.Float64)
        && normalized.stacked_class = N.Float64);
      Alcotest.(check bool)
        "return conversion classified after normalization" (not deleted)
        returned.producer_to_integer;
      Alcotest.(check bool)
        "MOV conversion migrates to correct operand" true
        ((not migrated.input_to_float)
        && migrated.return_operand_to_float = deleted
        && migrated.arithmetic_operand_to_float = not deleted);
      Alcotest.(check bool)
        "physical truncation only after copy barrier" (not deleted)
        (List.mem N.Store_integer_truncate (N.events receipt));
      let code =
        List.concat_map
          (fun block ->
            List.map S.description (S.instructions (G.instructions block)))
          (G.blocks graph)
      in
      let return =
        List.find (fun (d : S.description) -> d.opcode = O.Ic_return_val) code
      in
      let root =
        List.find
          (fun (d : S.description) -> d.instruction_id = N.instruction receipt)
          code
      in
      let expected_steps =
        if deleted then
          [
            (return.instruction_id, N.Argument1, N.Load_signed_integer);
            (return.instruction_id, N.Argument1, N.Store_binary64);
          ]
        else
          let argument =
            if expression = "1.0*n" then N.Argument2 else N.Argument1
          in
          [
            (root.instruction_id, argument, N.Load_signed_integer);
            (root.instruction_id, argument, N.Store_binary64);
            (root.instruction_id, N.Instruction, N.Copy_binary64);
            (root.instruction_id, N.Result, N.Load_binary64);
            (root.instruction_id, N.Result, N.Store_integer_truncate);
          ]
      in
      let actual_steps =
        List.map
          (fun (step : N.step) -> (step.owner, step.position, step.event))
          (N.steps receipt)
      in
      Alcotest.(check bool)
        (expression ^ " exact ordered event owners and positions")
        true
        (actual_steps = expected_steps);
      let plan = Plan.prepare ~source graph |> checked in
      Alcotest.(check bool)
        "backend consumes shared normalized receipt" true
        (List.exists
           (fun (e : Plan.entry) ->
             match e.operation with
             | Plan.Normalized _ -> true
             | _ -> false)
           (Plan.validated_entries plan ~graph |> Option.get));
      rejects (Plan.prepare graph))
    [
      ("n*1.0", true);
      ("n+0.0", true);
      ("0.0+n", true);
      ("n-0.0", true);
      ("n/1.0", false);
      ("1.0*n", false);
    ]

let normalization_mutations () =
  let module N = Holyc_lib__Ir.Numeric_normalization in
  List.iter
    (fun change ->
      let source, graph = fixture ~contents:"I64 F(I64 n){return n*1.0;}" () in
      let receipt =
        N.prepare ~runtime:source.runtime ~body:source.body ~frame:source.frame
          graph
        |> List.hd
      in
      let plan = Plan.prepare ~source graph |> checked in
      let integer_class =
        List.concat_map
          (fun block -> S.instructions (G.instructions block))
          (G.blocks graph)
        |> List.find_map (fun i ->
            let d = S.description i in
            if d.opcode = O.Ic_deref then d.target_type else None)
      in
      mutate graph
        (fun d -> d.S.instruction_id = N.instruction receipt)
        (fun d ->
          match change with
          | 0 -> { d with S.flags = 0L }
          | 1 -> { d with S.flags = 3L }
          | 2 -> { d with S.target_type = integer_class }
          | 3 -> { d with S.operands = List.rev d.operands }
          | _ -> { d with S.opcode = O.Ic_div });
      Alcotest.(check bool)
        "normalized receipt rejects canonical mutation" false
        (N.valid receipt ~graph);
      Alcotest.(check bool)
        "backend normalization invalidates" true
        (Plan.validated_entries plan ~graph = None))
    [ 0; 1; 2; 3; 4 ]

let normalization_source_invalidation () =
  let module N = Holyc_lib__Ir.Numeric_normalization in
  List.iter
    (fun contents ->
      List.iter
        (fun change_frame ->
          let source, graph = fixture ~contents () in
          let receipt =
            N.prepare ~runtime:source.runtime ~body:source.body
              ~frame:source.frame graph
            |> List.hd
          in
          let plan = Plan.prepare ~source graph |> checked in
          let foreign, _ = fixture ~contents () in
          Alcotest.(check int)
            "foreign runtime cannot prove normalization" 0
            (N.prepare ~runtime:foreign.runtime ~body:source.body
               ~frame:source.frame graph
            |> List.length);
          (if change_frame then
             let module Frame = Holyc_lib__Sema.Function_frame_layout in
             Obj.set_field (Obj.repr source.frame) 1
               (Obj.repr (Frame.function_header foreign.frame))
           else
             let module T = Holyc_lib__Sema.Function_call_expression_result in
             let module C = Holyc_lib__Sema.Function_call_resolution in
             let binary =
               F.definition_source source.body
               |> Option.get |> T.function_all_results
               |> List.find_map (fun value ->
                   match
                     C.argument_expression_source (T.result_source value)
                   with
                   | Some (Holyc_lib__Frontend.Ast.Binary_expression b) ->
                       Some b
                   | _ -> None)
               |> Option.get
             in
             Obj.set_field (Obj.repr binary) 3 (Obj.repr binary.binary_left));
          Alcotest.(check bool)
            "stale normalization source/header rejected" false
            (N.valid receipt ~graph);
          Alcotest.(check bool)
            "stale backend recipe rejected" true
            (Plan.validated_entries plan ~graph = None);
          rejects (Plan.prepare ~source graph))
        [ false; true ])
    [
      "I64 F(I64 n){return n*1.0;}";
      "F64 F(I64 n){return n*1.0;}";
      "I64 F(F64 n){return n*1.0;}";
    ]

let cross_family_normalization () =
  let module N = Holyc_lib__Ir.Numeric_normalization in
  List.iter
    (fun (input_class, return_class, input_name, return_name) ->
      List.iter
        (fun (expression, deleted) ->
          let contents =
            return_name ^ " F(" ^ input_name ^ " n){return " ^ expression ^ ";}"
          in
          let source, graph = fixture ~contents () in
          let receipt =
            N.prepare ~runtime:source.runtime ~body:source.body
              ~frame:source.frame graph
            |> List.hd
          in
          let plan = Plan.prepare ~source graph |> checked in
          let phases = N.states receipt in
          let normalized = List.nth phases 2
          and returned = List.nth phases 3
          and migrated = List.nth phases 4 in
          let integer_input = input_class = N.Integer64 in
          Alcotest.(check bool)
            "original input and declared return remain separate" true
            (List.for_all
               (fun (state : N.state) ->
                 state.input_class = input_class
                 && state.return_class = return_class)
               phases);
          Alcotest.(check bool)
            "cross-family raw survivor and stack class" true
            ((normalized.raw_class = if deleted then input_class else N.Float64)
            && normalized.stacked_class = N.Float64);
          Alcotest.(check bool)
            "cross-family return flags classified once" true
            (returned.producer_to_float = (deleted && integer_input)
            && returned.producer_to_integer = not integer_input);
          Alcotest.(check bool)
            "cross-family migration flags" true
            ((not migrated.input_to_float)
            && (not migrated.producer_to_float)
            && migrated.producer_to_integer
               = ((not deleted) && not integer_input)
            && migrated.return_operand_to_float = (deleted && integer_input)
            && migrated.return_operand_to_integer
               = (deleted && not integer_input)
            && migrated.arithmetic_operand_to_float
               = ((not deleted) && integer_input));
          let code =
            List.concat_map
              (fun block ->
                List.map S.description (S.instructions (G.instructions block)))
              (G.blocks graph)
          in
          let return =
            List.find
              (fun (d : S.description) -> d.opcode = O.Ic_return_val)
              code
          in
          let root = N.instruction receipt in
          let cvt =
            if integer_input then [ N.Load_signed_integer; N.Store_binary64 ]
            else [ N.Load_binary64; N.Store_integer_truncate ]
          in
          let owner, position =
            if deleted then (return.instruction_id, N.Argument1)
            else if integer_input then
              (root, if expression = "1.0*n" then N.Argument2 else N.Argument1)
            else (root, N.Result)
          in
          let conversion =
            List.map (fun event -> (owner, position, event)) cvt
          in
          let copy = [ (root, N.Instruction, N.Copy_binary64) ] in
          let expected =
            if deleted then conversion
            else if integer_input then conversion @ copy
            else copy @ conversion
          in
          Alcotest.(check bool)
            "cross-family exact physical event sequence" true
            (List.map
               (fun (step : N.step) -> (step.owner, step.position, step.event))
               (N.steps receipt)
            = expected);
          let entry =
            Plan.validated_entries plan ~graph
            |> Option.get
            |> List.find (fun (e : Plan.entry) -> e.instruction_id = root)
          in
          Alcotest.(check bool)
            "cross-family single bounded x87 lifetime" true
            (entry.live_before = [] && entry.live_after = []
           && entry.peak_depth = 1
            && List.length entry.live_steps = 3
            && entry.loads
               = [ (if integer_input then Plan.Signed_integer else Plan.Bits) ]
            );
          mutate graph
            (fun d -> d.S.instruction_id = root)
            (fun d -> { d with S.flags = (if integer_input then 2L else 0L) });
          Alcotest.(check bool)
            "cross-family flag mutation revokes shared receipt" false
            (N.valid receipt ~graph);
          Alcotest.(check bool)
            "cross-family flag mutation revokes backend receipt" true
            (Plan.validated_entries plan ~graph = None))
        [
          ("n*1.0", true);
          ("n+0.0", true);
          ("0.0+n", true);
          ("n-0.0", true);
          ("n/1.0", false);
          ("1.0*n", false);
        ])
    [
      (N.Integer64, N.Float64, "I64", "F64");
      (N.Float64, N.Integer64, "F64", "I64");
    ];
  List.iter
    (fun expression ->
      let source, graph =
        fixture ~contents:("F64 F(F64 n){return " ^ expression ^ ";}") ()
      in
      Alcotest.(check int)
        "F64 bit copies retain existing proof" 0
        (N.prepare ~runtime:source.runtime ~body:source.body ~frame:source.frame
           graph
        |> List.length);
      let plan = Plan.prepare ~source graph |> checked in
      Alcotest.(check bool)
        "no numeric recipe inserted into F64 bit copy" true
        (List.for_all
           (fun (e : Plan.entry) ->
             match e.operation with
             | Plan.Normalized _ -> false
             | _ -> true)
           (Plan.validated_entries plan ~graph |> Option.get)))
    [ "n*1.0"; "n+0.0"; "0.0+n"; "n-0.0"; "n/1.0"; "1.0*n" ]

let () =
  Alcotest.run "numeric conversion authority"
    [
      ( "source proofs",
        [
          Alcotest.test_case "cross-family normalized phase and event matrix"
            `Quick cross_family_normalization;
          Alcotest.test_case "normalized source and header invalidation" `Quick
            normalization_source_invalidation;
          Alcotest.test_case "normalized phase receipts" `Quick
            normalization_receipts;
          Alcotest.test_case "normalized flag class and operand mutation" `Quick
            normalization_mutations;
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
