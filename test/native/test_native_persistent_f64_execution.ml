open Holyc_lib
module Cases = Persistent_f64_execution_cases
module Image = X86_64_program
module Runtime = Native_program_execution
module Task = Native_source_execution
module Plan = Holyc_lib__Backend.X86_64_float_plan
module Unit = Holyc_lib__Driver.Integer_unit
module Body = Holyc_lib__Ir.Function_body
module Graph = Ir_block_graph
module Sequence = Ir_instruction_sequence
module Opcode = Ir_opcode
module Typed = Holyc_lib__Sema.Function_call_expression_result
module Source = Holyc_lib__Sema.Function_call_resolution

type target = Isolated | Source_task

type observation = {
  context : string;
  outcome : (Image.word_type * int64, Diagnostic.t list) result;
  preparation : int;
  executed : int;
  output : string;
}

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]
let targets = [ Isolated; Source_task ]

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let config mode =
  Preprocessor.Config.create ~compilation_mode:mode () |> checked

let inputs mode text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-persistent-f64-execution.hc"
      ~contents:text
  in
  (session, config mode, source)

let run ?(max_steps = 100_000) ?max_global_bytes ?max_output_bytes
    ?max_initializer_steps ~target ~mode text =
  let session, config, source = inputs mode text in
  let context =
    (match target with
      | Isolated -> "host-jit"
      | Source_task -> "host-jit-task")
    ^ " "
    ^ (match mode with
      | Preprocessor.Jit -> "jit"
      | Preprocessor.Aot -> "aot")
    ^ ": " ^ text
  in
  match target with
  | Isolated ->
      let report =
        Native_program.evaluate ~max_code_bytes:1_048_576 ?max_global_bytes
          ?max_output_bytes ?max_initializer_steps session ~config ~source
          ~max_steps
      in
      let outcome =
        Native_program.outcome report
        |> Result.map
             (fun (result : Native_program.result Native_program.checked) ->
               match result.value.execution.final_value with
               | Some word -> (word.type_, word.bits)
               | None -> Alcotest.fail (context ^ ": missing final value"))
      in
      {
        context;
        outcome;
        preparation = Native_program.preparation_steps report;
        executed =
          Option.value (Native_program.executed_steps report) ~default:0;
        output = Native_program.output_bytes report;
      }
  | Source_task ->
      let report =
        Task.evaluate ~max_code_bytes:1_048_576 ?max_global_bytes
          ?max_output_bytes ?max_initializer_steps session ~config ~source
          ~max_steps
      in
      Option.iter
        (fun progress ->
          Alcotest.(check int)
            "native source commands execute no interpreter instructions" 0
            progress.Integer_task.runtime.executed_steps)
        (Task.source_progress report);
      let outcome =
        Task.outcome report
        |> Result.map (fun (result : Task.result Task.checked) ->
            match result.value.final_value with
            | Some word -> (word.type_, word.bits)
            | None -> Alcotest.fail (context ^ ": missing final value"))
      in
      {
        context;
        outcome;
        preparation = Task.preparation_steps report;
        executed = Task.executed_steps report;
        output = Task.output_bytes report;
      }

let expected_kind (case : Cases.value_case) =
  match case.kind with
  | Cases.I64 -> Image.I64
  | F64 -> Image.F64

let value (case : Cases.value_case) report =
  match report.outcome with
  | Error errors ->
      Alcotest.fail
        (case.name ^ ": " ^ diagnostics errors ^ "\n" ^ report.context)
  | Ok (kind, bits) ->
      Alcotest.(check bool)
        (case.name ^ " scalar class")
        true
        (kind = expected_kind case);
      Alcotest.(check int64) (case.name ^ " exact bits") case.bits bits;
      Alcotest.(check string) (case.name ^ " output") case.output report.output;
      Alcotest.(check bool)
        (case.name ^ " actual native work")
        true (report.executed > 0)

let rejection label report =
  match report.outcome with
  | Error (_ :: _) -> ()
  | Error [] -> Alcotest.fail (label ^ ": missing diagnostic")
  | Ok _ -> Alcotest.fail (label ^ ": rejected source was executed")

let fault (case : Cases.fault_case) report =
  rejection case.name report;
  let errors = Result.fold ~ok:(fun _ -> []) ~error:Fun.id report.outcome in
  Alcotest.(check bool)
    (case.name ^ ": " ^ diagnostics errors)
    true
    (List.exists (fun (error : Diagnostic.t) -> error.code = case.code) errors);
  Alcotest.(check string)
    (case.name ^ " committed output")
    case.output report.output

let all action =
  List.iter
    (fun mode -> List.iter (fun target -> action ~target ~mode) targets)
    modes

let value_matrix (case : Cases.value_case) () =
  all (fun ~target ~mode ->
      let report = run ~target ~mode case.source in
      if case.fisttp && not (Native_execution.fisttp_supported ()) then
        rejection (case.name ^ " requires FISTTP") report
      else value case report)

let fault_matrix (case : Cases.fault_case) () =
  all (fun ~target ~mode -> run ~target ~mode case.source |> fault case)

let jit_fault_matrix (case : Cases.fault_case) () =
  List.iter
    (fun target -> run ~target ~mode:Preprocessor.Jit case.source |> fault case)
    targets

let compile ?max_ir_instructions ?max_code_bytes ?max_global_bytes mode text =
  let session, config, source = inputs mode text in
  Native_program.compile ?max_ir_instructions ?max_code_bytes ?max_global_bytes
    session ~config ~source

let image mode text =
  (compile mode text |> Result.map_error diagnostics |> checked).value

let completed kind bits report =
  match Runtime.outcome report |> checked with
  | Image.Completed result -> (
      match result.final_value with
      | Some word ->
          Alcotest.(check bool) "retained scalar class" true (word.type_ = kind);
          Alcotest.(check int64) "retained exact bits" bits word.bits;
          result.executed_steps
      | None -> Alcotest.fail "retained entry lost its result")
  | Image.Fault result ->
      Alcotest.failf "retained entry faulted after %d steps"
        result.executed_steps

let native_fault expected steps report =
  match Runtime.outcome report |> checked with
  | Image.Fault result ->
      Alcotest.(check bool) "original native fault" true (result.kind = expected);
      Alcotest.(check int) "exact fault charge" steps result.executed_steps;
      Alcotest.(check bool)
        "fault retains source span" true
        (Option.is_some result.span)
  | Image.Completed _ -> Alcotest.fail "expected a checked native fault"

let storage_authority () =
  let fixture () =
    let compiled =
      Native_scalar_fixture.compile ~mode:Preprocessor.Aot
        ~path:"persistent-f64-source-authority.hc"
        ~contents:"F64 values[2];I64 Read(){return values[1]<1.0;}" ()
      |> Result.map_error diagnostics
      |> checked
    in
    let definition = List.hd (Unit.functions compiled.unit_) in
    let source : Plan.source =
      {
        runtime = Unit.runtime_calls compiled.unit_;
        body = definition.body;
        frame = definition.frame;
      }
    in
    (source, Body.body definition.body)
  in
  let prepare source graph =
    Plan.prepare ~source graph
    |> Result.map_error (fun errors ->
        List.map (fun (error : Plan.error) -> error.message) errors
        |> String.concat "; ")
    |> checked
  in
  let rejects name = function
    | Error (_ :: _) -> ()
    | _ -> Alcotest.fail (name ^ ": foreign storage proof was admitted")
  in
  let source, graph = fixture () in
  let foreign, _ = fixture () in
  let plan = prepare source graph in
  Alcotest.(check bool)
    "original stored comparison has a receipt" true
    (Option.is_some (Plan.validated_comparisons plan ~graph));
  List.iter
    (fun (name, changed) -> rejects name (Plan.prepare ~source:changed graph))
    [
      ("foreign runtime storage", { source with runtime = foreign.runtime });
      ("foreign function frame", { source with frame = foreign.frame });
      ("foreign typed body", { source with body = foreign.body });
    ];
  let copied =
    Graph.create
      ~entry:(Graph.block_id (Graph.entry graph))
      (List.map
         (fun block ->
           ({
              Graph.block_id = Graph.block_id block;
              instructions =
                Sequence.instructions (Graph.instructions block)
                |> List.map Sequence.description;
            }
             : Graph.block_description))
         (Graph.blocks graph))
    |> Result.map_error (fun _ -> "copied graph construction failed")
    |> checked
  in
  rejects "copied graph" (Plan.prepare ~source copied);
  Alcotest.(check bool)
    "copied graph cannot reuse a stored comparison receipt" true
    (Plan.validated_comparisons plan ~graph:copied = None);
  let original = Body.definition_source source.body |> Option.get in
  let selected =
    Typed.function_all_results original
    |> List.find_map (fun result ->
        match Typed.result_source result |> Source.argument_expression_kind with
        | Source.Index_expression index -> Some (Source.index_value index)
        | _ -> None)
    |> Option.get
  in
  let object_ = Obj.repr selected in
  let original_kind = Obj.field object_ 0 in
  Fun.protect
    ~finally:(fun () -> Obj.set_field object_ 0 original_kind)
    (fun () ->
      Obj.set_field object_ 0 (Obj.repr (Source.Integer_literal 0L));
      Alcotest.(check bool)
        "changed original index invalidates cached storage receipt" true
        (Plan.validated_comparisons plan ~graph = None);
      rejects "changed original index" (Plan.prepare ~source graph));
  Alcotest.(check bool)
    "restoring the original index restores its own receipt" true
    (Option.is_some (Plan.validated_comparisons plan ~graph));
  let changed = ref false in
  let floating_load (description : Sequence.description) =
    description.opcode <> Opcode.Ic_imm_f64
    && Option.fold ~none:false
         ~some:(fun type_ ->
           Semantic_type.pointer_depth type_ = 0
           &&
           match Semantic_type.base type_ with
           | Semantic_type.Primitive (_, Primitive_type.F64) -> true
           | _ -> false)
         description.target_type
  in
  let rec replace = function
    | [] -> ()
    | instruction :: rest as cell ->
        let description = Sequence.description instruction in
        if (not !changed) && floating_load description then (
          Obj.set_field (Obj.repr cell) 0
            (Obj.repr
               {
                 description with
                 Sequence.flags = Int64.logxor description.flags 2L;
               });
          changed := true)
        else replace rest
  in
  List.iter
    (fun block -> replace (Sequence.instructions (Graph.instructions block)))
    (Graph.blocks graph);
  Alcotest.(check bool) "original F64 load found" true !changed;
  Alcotest.(check bool)
    "changed load flags invalidate cached storage receipt" true
    (Plan.validated_comparisons plan ~graph = None);
  rejects "changed load conversion flags" (Plan.prepare ~source graph)

let emission_limits () =
  List.iter
    (fun mode ->
      let original = image mode Cases.quota_source in
      let ir = Image.ir_instructions original
      and code = Image.code_bytes original in
      Alcotest.(check int)
        "one original F64 allocation" 8
        (Image.global_bytes original);
      ignore
        (compile ~max_ir_instructions:ir ~max_code_bytes:code
           ~max_global_bytes:8 mode Cases.quota_source
        |> Result.map_error diagnostics
        |> checked);
      List.iter
        (fun result ->
          Alcotest.(check bool)
            "one-below emission or storage allowance rejects" true
            (Result.is_error result))
        [
          compile ~max_ir_instructions:(ir - 1) mode Cases.quota_source;
          compile ~max_code_bytes:(code - 1) mode Cases.quota_source;
          compile ~max_global_bytes:7 mode Cases.quota_source;
        ])
    modes

let interrupted_images () =
  List.iter
    (fun mode ->
      let handle = image mode Cases.quota_source |> Runtime.retain |> checked in
      Fun.protect
        ~finally:(fun () -> Runtime.release handle |> checked)
        (fun () ->
          let run limit =
            Runtime.execute_retained_report ~max_steps:limit handle
          in
          let steps = run 100_000 |> completed Image.I64 42L in
          for limit = 1 to steps - 1 do
            run limit |> native_fault Image.Step_limit_exceeded limit;
            ignore (run steps |> completed Image.I64 42L)
          done;
          Gc.compact ();
          ignore (run steps |> completed Image.I64 42L)))
    modes

let retained_storage () =
  let source =
    {|I64 seen=0;F64 counter;
      F64 Tick(){if(!seen){counter=1.0;seen=1;}counter+=0.5;return counter;}Tick();|}
  in
  List.iter
    (fun mode ->
      let original = image mode source in
      for _ = 1 to 2 do
        ignore
          (Runtime.execute_report ~max_steps:100_000 original
          |> completed Image.F64 0x3ff8000000000000L)
      done;
      let first = Runtime.retain original |> checked in
      let second = Runtime.retain original |> checked in
      Fun.protect
        ~finally:(fun () ->
          Runtime.release first |> checked;
          Runtime.release second |> checked)
        (fun () ->
          let run handle bits =
            Runtime.execute_retained_report ~max_steps:100_000 handle
            |> completed Image.F64 bits |> ignore
          in
          run first 0x3ff8000000000000L;
          run second 0x3ff8000000000000L;
          run first 0x4000000000000000L;
          Gc.compact ();
          run first 0x4004000000000000L;
          run second 0x4000000000000000L;
          Runtime.release first |> checked;
          Alcotest.(check bool)
            "released mapping cannot execute" true
            (Runtime.execute_retained_report ~max_steps:100_000 first
            |> Runtime.outcome |> Result.is_error);
          run second 0x4004000000000000L))
    modes

let frame_limits () =
  let source =
    {|F64 value;F64 Add(F64 increment){value=1.0;value+=increment;return value;}Add(0.5);|}
  in
  List.iter
    (fun mode ->
      let handle = image mode source |> Runtime.retain |> checked in
      Fun.protect
        ~finally:(fun () -> Runtime.release handle |> checked)
        (fun () ->
          let run frame =
            Runtime.execute_retained_report ~max_frame_bytes:frame
              ~max_steps:100_000 handle
          in
          ignore (run 8 |> completed Image.F64 0x3ff8000000000000L);
          (match Runtime.outcome (run 7) |> checked with
          | Image.Fault result ->
              Alcotest.(check bool)
                "one-byte-short semantic frame" true
                (result.kind = Image.Frame_limit_exceeded)
          | Image.Completed _ -> Alcotest.fail "short frame was admitted");
          ignore (run 8 |> completed Image.F64 0x3ff8000000000000L)))
    modes

let capabilities () =
  List.iter
    (fun mode ->
      let word_image =
        image mode
          {|F64 value;I64 Check(){(&value)(U64*)[0]=0x7ff0000000000042;return value==value;}Check();|}
      in
      Alcotest.(check bool)
        "raw stored equality needs no x87" false
        (Image.requires_x87 word_image);
      Alcotest.(check bool)
        "raw stored equality needs no FISTTP" false
        (Image.requires_fisttp word_image);
      ignore
        (Runtime.execute_report ~max_steps:100_000 word_image
        |> completed Image.I64 1L);
      let arithmetic = image mode Cases.quota_source in
      Alcotest.(check bool)
        "stored arithmetic requires x87" true
        (Image.requires_x87 arithmetic);
      Alcotest.(check bool)
        "stored comparison needs no FISTTP" false
        (Image.requires_fisttp arithmetic);
      let conversion =
        image mode {|F64 value;I64 Read(){value=3.75;return value;}Read();|}
      in
      Alcotest.(check bool)
        "stored numeric return requires FISTTP" true
        (Image.requires_fisttp conversion);
      if not (Native_execution.fisttp_supported ()) then
        Alcotest.(check bool)
          "unavailable FISTTP rejects retention" true
          (Runtime.retain conversion |> Result.is_error))
    modes

let public_limits () =
  let case = Cases.integer "public persistent quota" Cases.quota_source 42L in
  all (fun ~target ~mode ->
      let baseline = run ~target ~mode case.source in
      value case baseline;
      Alcotest.(check int)
        "runtime-only fixture has no preparation charge" 0 baseline.preparation;
      run ~target ~mode ~max_steps:baseline.executed case.source |> value case;
      let limited =
        run ~target ~mode ~max_steps:(baseline.executed - 1) case.source
      in
      rejection "one-below public runtime allowance" limited;
      Alcotest.(check int)
        "public exact runtime charge" (baseline.executed - 1) limited.executed;
      run ~target ~mode ~max_global_bytes:8 case.source |> value case;
      run ~target ~mode ~max_global_bytes:7 case.source
      |> rejection "one-below public global extent")

let output_limits () =
  let case =
    List.find
      (fun (case : Cases.value_case) ->
        case.name = "argument calls retain reverse effects and stored reads")
      Cases.effect_values
  in
  all (fun ~target ~mode ->
      run ~target ~mode ~max_output_bytes:2 case.source |> value case;
      let limited = run ~target ~mode ~max_output_bytes:1 case.source in
      fault
        (Cases.fault ~output:"A" "second provider exceeds output allowance"
           case.source "HCIRVM0022")
        limited)

let value_tests cases =
  List.map
    (fun (case : Cases.value_case) ->
      Alcotest.test_case case.name `Quick (value_matrix case))
    cases

let () =
  if not (Native_execution.x87_supported ()) then
    print_endline "persistent F64 execution tests: x87 unsupported, skipped"
  else
    Alcotest.run "Native persistent F64 execution"
      [
        ("storage consumers", value_tests Cases.storage_values);
        ("numeric conversions", value_tests Cases.conversion_values);
        ("source precision", value_tests Cases.precision_values);
        ("comparison and truth", value_tests Cases.comparison_values);
        ("effects and repeated calls", value_tests Cases.effect_values);
        ("maintained program", value_tests [ Cases.maintained ]);
        ( "retained source",
          List.map
            (fun (case : Cases.value_case) ->
              Alcotest.test_case case.name `Quick (fun () ->
                  run ~target:Source_task ~mode:Preprocessor.Jit case.source
                  |> value case))
            Cases.jit_task_values );
        ( "storage faults",
          List.map
            (fun (case : Cases.fault_case) ->
              Alcotest.test_case case.name `Quick (fault_matrix case))
            Cases.faults
          @ List.map
              (fun (case : Cases.fault_case) ->
                Alcotest.test_case case.name `Quick (jit_fault_matrix case))
              Cases.jit_faults );
        ( "retained images and limits",
          [
            Alcotest.test_case
              "original storage source, index and load authority" `Quick
              storage_authority;
            Alcotest.test_case "exact emission and original storage allowances"
              `Quick emission_limits;
            Alcotest.test_case "every interrupted floating operation recovers"
              `Quick interrupted_images;
            Alcotest.test_case "independent arenas, GC and released mappings"
              `Quick retained_storage;
            Alcotest.test_case "exact and one-below semantic frame" `Quick
              frame_limits;
            Alcotest.test_case "capabilities follow emitted operations" `Quick
              capabilities;
            Alcotest.test_case "public cumulative native allowances" `Quick
              public_limits;
            Alcotest.test_case "reached output allowance" `Quick output_limits;
          ] );
      ]
