open Holyc_lib
open Yojson.Safe.Util
module A = Test_internal_strlen_authority
module S = Test_source_constant_shifts
module E = Ir_expression_lowering
module Seq = Ir_instruction_sequence
module Op = Ir_opcode
module Graph = Ir_block_graph
module Unit = Holyc_lib__Driver.Integer_unit
module VM = Ir_integer_interpreter
module Native = X86_64_program

let source = "I64 F(I64 x,I64 y,I64 z){if(x<y<z)return 42;return 9;}F(1,2,3);"

let fixture () =
  [
    "oracle/conditional-comparison-chains.json";
    "test/oracle/conditional-comparison-chains.json";
  ]
  |> List.find_opt Sys.file_exists
  |> Option.get |> Yojson.Safe.from_file

let shared_structure () =
  List.iter
    (fun mode ->
      let original = A.fixture ~source mode in
      let graph = S.body original |> Ir_x87_stack.graph in
      let ds = S.descriptions (S.body original) in
      S.dense_ids ds;
      let values =
        List.filter_map
          (fun (d : Seq.description) ->
            Option.map (fun r -> Seq.Value_id.to_int r.Seq.value_id) d.result)
          ds
      in
      Alcotest.(check (list int))
        "dense values despite branch-only instructions"
        (List.init (List.length values) Fun.id)
        (List.sort compare values);
      let comparisons =
        List.filter (fun (d : Seq.description) -> d.opcode = Op.Ic_less) ds
      in
      Alcotest.(check int) "two comparisons" 2 (List.length comparisons);
      let first = List.hd comparisons and second = List.nth comparisons 1 in
      Alcotest.(check bool)
        "original middle survives the branch" true
        (Seq.Value_id.equal
           (List.nth first.operands 1)
           (List.hd second.operands));
      let owner description =
        Graph.blocks graph
        |> List.find (fun block ->
            Graph.instructions block |> Seq.instructions
            |> List.exists (fun i ->
                Seq.Instruction_id.equal (Seq.description i).instruction_id
                  description.Seq.instruction_id))
        |> Graph.block_id
      in
      Alcotest.(check bool)
        "next comparison is in another block" true
        (not (Graph.Block_id.equal (owner first) (owner second)));
      Alcotest.(check bool)
        "condition has no eager conjunction" false
        (List.exists (fun (d : Seq.description) -> d.opcode = Op.Ic_and_and) ds);
      let again = A.fixture ~source mode in
      Alcotest.(check string)
        "deterministic graph" (Graph.human graph)
        (S.body again |> Ir_x87_stack.graph |> Graph.human))
    A.modes

let both_abi_resource_limits () =
  let regressions = fixture () |> member "hosted_regressions" |> to_list in
  List.iter
    (fun mode ->
      List.iter
        (fun case ->
          let source = case |> member "holy_c_source" |> to_string in
          List.iter
            (fun status_abi ->
              let compile ~instructions ~blocks ~bytes ~stack =
                let session = Session.create () in
                let source =
                  Session.add_source session
                    ~path:"conditional-native-limits.hc" ~contents:source
                in
                let config =
                  Preprocessor.Config.create ~compilation_mode:mode ()
                  |> Result.get_ok
                in
                Native_program.compile ~status_abi
                  ~max_ir_instructions:instructions ~max_blocks:blocks
                  ~max_code_bytes:bytes ~max_stack_bytes:stack session ~config
                  ~source
                |> Result.map
                     (fun (checked : Native.t Native_program.checked) ->
                       checked.value)
              in
              let checked result =
                Test_native_program.require_ok A.diagnostics result
              in
              let rejected code = function
                | Error ds ->
                    Alcotest.(check bool)
                      "exact native bound diagnostic" true
                      (List.exists (fun (d : Diagnostic.t) -> d.code = code) ds)
                | Ok _ -> Alcotest.fail "one below native bound compiled"
              in
              let image =
                compile ~instructions:4096 ~blocks:4096 ~bytes:65536 ~stack:4088
                |> checked
              in
              let instructions = Native.ir_instructions image
              and blocks = Native.block_count image
              and bytes = Native.code_bytes image
              and stack = Native.frame_bytes image in
              ignore (compile ~instructions ~blocks ~bytes ~stack |> checked);
              List.iter
                (fun (code, result) -> rejected code result)
                [
                  ( "HCBACK0001",
                    compile ~instructions:(instructions - 1) ~blocks ~bytes
                      ~stack );
                  ( "HCBACK0001",
                    compile ~instructions ~blocks:(blocks - 1) ~bytes ~stack );
                  ( "HCBACK0005",
                    compile ~instructions ~blocks ~bytes:(bytes - 1) ~stack );
                ];
              if stack > 0 then
                ignore
                  (compile ~instructions ~blocks ~bytes ~stack:(stack - 1)
                  |> rejected "HCBACK0004"))
            [ X86_64_encoder.Windows_x64; System_v_x64 ])
        regressions)
    A.modes

let authority () =
  let find_cell original opcode =
    S.body original |> Ir_x87_stack.graph |> Graph.blocks
    |> List.find_map (fun block ->
        let rec find = function
          | [] -> None
          | i :: rest as cell ->
              let d = Seq.description i in
              if d.opcode = opcode then Some (cell, d) else find rest
        in
        find (Seq.instructions (Graph.instructions block)))
    |> Option.get
  in
  List.iter
    (fun mode ->
      let original = A.fixture ~source mode
      and foreign = A.fixture ~source mode in
      A.valid_control ~expected:42L original;
      A.rejects "foreign conditional-chain authority"
        (A.execute ~runtime_calls:(Unit.runtime_calls foreign.unit_) original);
      A.rejects "foreign native conditional-chain authority"
        (A.compile ~runtime_calls:(Unit.runtime_calls foreign.unit_) original);
      List.iter
        (fun (opcode, transform) ->
          let changed = A.fixture ~source mode in
          let cell, d = find_cell changed opcode in
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform d));
          A.rejects "changed shared comparison proof" (A.execute changed);
          A.rejects "changed native shared comparison proof" (A.compile changed))
        [
          ( Op.Ic_less,
            fun (d : Seq.description) -> { d with opcode = Op.Ic_greater } );
          (Op.Ic_less, fun d -> { d with operands = List.rev d.operands });
          (Op.Ic_less, fun d -> { d with operands = List.map Fun.id d.operands });
          (Op.Ic_br_zero, fun d -> { d with opcode = Op.Ic_br_not_zero });
          (Op.Ic_br_zero, fun d -> { d with span = None });
        ])
    A.modes

let atomic_identity_limits () =
  let root = Test_ir_comparison_chains.root "1<2<3;" in
  List.iter
    (fun (instruction, value, block, false_target, code) ->
      let instruction_id =
        Seq.Instruction_id.of_int instruction |> Result.get_ok
      and value_id = Seq.Value_id.of_int value |> Result.get_ok
      and block_id = Seq.Block_id.of_int block |> Result.get_ok
      and false_target = Seq.Block_id.of_int false_target |> Result.get_ok in
      match
        E.lower_condition_chain ~instruction_id ~value_id ~block_id
          ~false_target root
      with
      | Error [ error ] ->
          Alcotest.(check string)
            "atomic conditional allocation" code error.code
      | _ ->
          Alcotest.fail "exhausted or overlapping IDs exposed a partial chain")
    [
      (Int.max_int - 5, 0, 10, 0, "HCIRL0005");
      (0, Int.max_int - 4, 10, 0, "HCIRL0005");
      (0, 0, Int.max_int, 0, "HCIRL0005");
      (0, 0, 0, 0, "HCIRL0004");
    ]

let long_chain () =
  let chain = String.concat "<=" (List.init 2001 (fun _ -> "1")) in
  let source = "I64 F(){if(" ^ chain ^ ")return 42;return 9;}F();" in
  List.iter
    (fun mode ->
      let _, execution = Test_pointer_equality.success mode source in
      Alcotest.(check (option int64))
        "long condition retains its final result" (Some 42L)
        (Option.map (fun w -> w.VM.bits) (VM.final_value execution));
      let limit = VM.executed_steps execution in
      let report = Test_pointer_equality.run ~max_steps:limit mode source in
      Alcotest.(check bool)
        "long chain exact work" true
        (Result.is_ok (integer_program_report_outcome report));
      let report =
        Test_pointer_equality.run ~max_steps:(limit - 1) mode source
      in
      match integer_program_report_outcome report with
      | Error (d :: _) ->
          Alcotest.(check string) "long chain one below" "HCIRVM0007" d.code
      | _ -> Alcotest.fail "long chain exceeded its allowance")
    A.modes

let tests =
  [
    Alcotest.test_case "shared values, source spans and dense IDs" `Quick
      shared_structure;
    Alcotest.test_case "both native ABIs retain exact resource bounds" `Quick
      both_abi_resource_limits;
    Alcotest.test_case "comparison and branch authority" `Quick authority;
    Alcotest.test_case "atomic identity and continuation exhaustion" `Quick
      atomic_identity_limits;
    Alcotest.test_case "long conditional chain and exact execution work" `Quick
      long_chain;
  ]
