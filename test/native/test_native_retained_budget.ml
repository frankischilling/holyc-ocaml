open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution
module VM = Ir_integer_interpreter

type raw_retained

external raw_retain : Obj.t -> raw_retained = "holyc_native_retain_program"
external raw_release : raw_retained -> unit = "holyc_native_release_program"

external raw_execute :
  raw_retained ->
  Obj.t ->
  Obj.t ->
  Obj.t ->
  (int64 * int64 * int64 * int64 * int64) * string * int
  = "holyc_native_execute_retained_budget_program"

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-retained-budget.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  (session, config, source)

let compile mode contents =
  let session, config, source = inputs mode contents in
  Native_program.compile session ~config ~source
  |> Result.map_error diagnostics
  |> checked
  |> fun value -> value.value

let expect_value expected report =
  match Runtime.outcome report |> checked with
  | Program.Completed result ->
      Alcotest.(check int64)
        "native final value" expected (Option.get result.final_value).bits;
      result.executed_steps
  | Program.Fault fault ->
      Alcotest.failf "unexpected fault at instruction %d" fault.instruction_id

let expect_fault expected report =
  match Runtime.outcome report |> checked with
  | Program.Fault fault ->
      Alcotest.(check bool) "native fault kind" true (expected = fault.kind);
      fault.executed_steps
  | Program.Completed _ -> Alcotest.fail "expected a native fault"

let with_retained image action =
  let retained = Runtime.retain image |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked)
    (fun () -> action retained)

let run = Runtime.execute_retained_budget_report
let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let independently_checked mode contents expected =
  let session, config, source = inputs mode contents in
  let public =
    run_integer_program_report session ~config ~source ~max_steps:100000
  in
  let result =
    integer_program_report_outcome public
    |> Result.map_error diagnostics
    |> checked
    |> fun checked -> checked.value
  in
  Alcotest.(check int64)
    "independent public IR value" expected
    (Option.get (VM.final_value result)).bits;
  let image = compile mode contents in
  let report = Runtime.execute_report ~max_steps:100000 image in
  ignore (expect_value expected report);
  Alcotest.(check string)
    "independent public IR output"
    (integer_program_report_output_bytes public)
    (Runtime.output_bytes report);
  (image, report)

let cumulative_steps_and_original_writes () =
  List.iter
    (fun mode ->
      let image, baseline = independently_checked mode "I64 G=40;++G;" 41L in
      let steps = expect_value 41L baseline in
      with_retained image (fun retained ->
          let budget =
            Runtime.create_budget ~max_steps:(2 * steps) () |> checked
          in
          Alcotest.(check int)
            "first charge" steps
            (run budget retained |> expect_value 41L);
          let first = Runtime.budget_progress budget in
          Alcotest.(check int)
            "second cumulative charge" (2 * steps)
            (run budget retained |> expect_value 42L);
          Alcotest.(check int)
            "exhausted native entry" (2 * steps)
            (run budget retained |> expect_fault Program.Step_limit_exceeded);
          Alcotest.(check int)
            "frozen earlier observation" steps first.executed_steps;
          Alcotest.(check bool)
            "checked exhaustion keeps budget verified" true
            ((Runtime.budget_progress budget).error = None);
          ignore
            (Runtime.execute_retained_report ~max_steps:steps retained
            |> expect_value 43L));
      with_retained image (fun retained ->
          let limit = (2 * steps) - 1 in
          let budget = Runtime.create_budget ~max_steps:limit () |> checked in
          ignore (run budget retained |> expect_value 41L);
          Alcotest.(check int)
            "one-below second activation" limit
            (run budget retained |> expect_fault Program.Step_limit_exceeded);
          Alcotest.(check int)
            "one-below remains consumed" limit
            (Runtime.budget_progress budget).executed_steps;
          ignore
            (Runtime.execute_retained_report ~max_steps:steps retained
            |> expect_value 43L)))
    modes

let output_exhaustion_preserves_prefix () =
  List.iter
    (fun mode ->
      let image, _ =
        independently_checked mode
          "extern U0 PutChars(U64 ch);PutChars('AB');42;" 42L
      in
      let quiet = compile mode "42;" in
      with_retained image (fun retained ->
          with_retained quiet (fun quiet ->
              let budget =
                Runtime.create_budget ~max_steps:100000 ~max_output_bytes:3 ()
                |> checked
              in
              let first = run budget retained in
              ignore (expect_value 42L first);
              Alcotest.(check string)
                "first output" "AB"
                (Runtime.output_bytes first);
              let second = run budget retained in
              ignore (expect_fault Program.Output_limit_exceeded second);
              Alcotest.(check string)
                "reached second prefix" "A"
                (Runtime.output_bytes second);
              let prefix = Runtime.budget_progress budget in
              Alcotest.(check string)
                "ordered cumulative output" "ABA" prefix.output_bytes;
              Bytes.set
                (Bytes.unsafe_of_string (Runtime.output_bytes first))
                0 'X';
              Bytes.set (Bytes.unsafe_of_string prefix.output_bytes) 0 'Y';
              Alcotest.(check string)
                "reports cannot change saved output" "ABA"
                (Runtime.budget_progress budget).output_bytes;
              ignore (run budget quiet |> expect_value 42L);
              let exhausted = run budget retained in
              ignore (expect_fault Program.Output_limit_exceeded exhausted);
              Alcotest.(check string)
                "zero remaining bytes" ""
                (Runtime.output_bytes exhausted);
              Alcotest.(check int)
                "total output never grows past quota" 3
                (Runtime.budget_progress budget).output_byte_length)))
    modes

let output_work_and_atomic_drafts () =
  List.iter
    (fun mode ->
      let image, baseline =
        independently_checked mode
          "extern U0 PutChars(U64 ch);PutChars('A');42;" 42L
      in
      let quiet = compile mode "42;" in
      with_retained image (fun retained ->
          with_retained quiet (fun quiet ->
              let work = Runtime.output_work baseline in
              let budget =
                Runtime.create_budget ~max_steps:100000 ~max_output_work:work ()
                |> checked
              in
              ignore (run budget retained |> expect_value 42L);
              ignore (run budget quiet |> expect_value 42L);
              let exhausted = run budget retained in
              ignore (expect_fault Program.Output_work_limit_exceeded exhausted);
              Alcotest.(check int)
                "exhausted output work adds no work" 0
                (Runtime.output_work exhausted);
              let progress = Runtime.budget_progress budget in
              Alcotest.(check int)
                "exact cumulative output work" work progress.output_work;
              Alcotest.(check string)
                "work fault preserves published prefix" "A"
                progress.output_bytes));
      let atomic =
        compile mode "extern U0 Print(U8 *fmt,...);Print(\"AB\");42;"
      in
      with_retained atomic (fun retained ->
          let budget =
            Runtime.create_budget ~max_steps:100000 ~max_output_bytes:3 ()
            |> checked
          in
          ignore (run budget retained |> expect_value 42L);
          let fault = run budget retained in
          ignore (expect_fault Program.Output_limit_exceeded fault);
          Alcotest.(check string)
            "atomic draft publishes no partial second output" ""
            (Runtime.output_bytes fault);
          Alcotest.(check string)
            "atomic output prefix remains" "AB"
            (Runtime.budget_progress budget).output_bytes))
    modes

let faults_and_preflight () =
  List.iter
    (fun mode ->
      let image =
        compile mode
          "extern U0 PutChars(U64 ch);I64 G=0;I64 \
           F(){++G;if(G==1){PutChars('A');return 1/(G-1);}return G+40;}F();"
      in
      with_retained image (fun retained ->
          let budget = Runtime.create_budget ~max_steps:100000 () |> checked in
          let rejected =
            Runtime.execute_retained_budget_report ~max_global_bytes:7 budget
              retained
          in
          Alcotest.(check bool)
            "storage preflight rejects" true
            (Result.is_error (Runtime.outcome rejected));
          let before = Runtime.budget_progress budget in
          Alcotest.(check int)
            "preflight consumes no steps" 0 before.executed_steps;
          Alcotest.(check bool)
            "preflight preserves allowance" true (before.error = None);
          let first = run budget retained in
          let steps = expect_fault Program.Division_by_zero first in
          Alcotest.(check int)
            "reached fault charged" steps
            (Runtime.budget_progress budget).executed_steps;
          Alcotest.(check string)
            "reached fault output" "A"
            (Runtime.budget_progress budget).output_bytes;
          Alcotest.(check bool)
            "later call consumes remaining allowance" true
            (run budget retained |> expect_value 42L > steps);
          Runtime.release retained |> checked;
          let before = Runtime.budget_progress budget in
          Alcotest.(check bool)
            "released owner rejects" true
            (run budget retained |> Runtime.outcome |> Result.is_error);
          let after = Runtime.budget_progress budget in
          Alcotest.(check int)
            "released owner has no charge" before.executed_steps
            after.executed_steps;
          Alcotest.(check bool)
            "released owner preflight is retryable elsewhere" true
            (after.error = None)))
    modes

let independent_arenas_and_concurrency () =
  let image = compile Preprocessor.Jit "I64 G=40;++G;" in
  let steps =
    Runtime.execute_report ~max_steps:10000 image |> expect_value 41L
  in
  with_retained image (fun first ->
      with_retained image (fun second ->
          let budget =
            Runtime.create_budget ~max_steps:(2 * steps) () |> checked
          in
          ignore (run budget first |> expect_value 41L);
          Alcotest.(check int)
            "separate arena uses shared quota" (2 * steps)
            (run budget second |> expect_value 41L)));
  with_retained image (fun retained ->
      let budget =
        Runtime.create_budget ~max_steps:(32 * steps) () |> checked
      in
      let run_many () =
        List.init 16 (fun _ -> run budget retained |> Runtime.outcome)
      in
      let other = Domain.spawn run_many in
      let results = run_many () @ Domain.join other in
      let values =
        List.filter_map
          (function
            | Ok (Program.Completed result) ->
                Some (Option.get result.final_value).bits
            | Error "retained native budget is already active" -> None
            | Error message -> Alcotest.fail message
            | Ok (Program.Fault _) ->
                Alcotest.fail "unexpected concurrent fault")
          results
        |> List.sort Int64.compare
      in
      List.iteri
        (fun index value ->
          Alcotest.(check int64)
            "each admitted activation writes once"
            (Int64.of_int (41 + index))
            value)
        values;
      Alcotest.(check int)
        "concurrent cumulative charge"
        (List.length values * steps)
        (Runtime.budget_progress budget).executed_steps)

let bounded_capture_chunks () =
  let image =
    compile Preprocessor.Jit "extern U0 PutChars(U64 ch);PutChars('A');42;"
  in
  with_retained image (fun retained ->
      let budget =
        Runtime.create_budget ~max_steps:100000 ~max_output_bytes:257 ()
        |> checked
      in
      for _ = 1 to 257 do
        ignore (run budget retained |> expect_value 42L)
      done;
      Alcotest.(check string)
        "many ordered small captures" (String.make 257 'A')
        (Runtime.budget_progress budget).output_bytes;
      ignore (run budget retained |> expect_fault Program.Output_limit_exceeded))

let configuration_bounds () =
  List.iter
    (fun result ->
      Alcotest.(check bool) "invalid total budget" true (Result.is_error result))
    [
      Runtime.create_budget ~max_steps:0 ();
      Runtime.create_budget ~max_steps:(-1) ();
      Runtime.create_budget ~max_steps:1 ~max_output_bytes:0 ();
      Runtime.create_budget ~max_steps:1
        ~max_output_bytes:(Runtime.hard_max_output_bytes + 1)
        ();
      Runtime.create_budget ~max_steps:1 ~max_output_work:0 ();
    ];
  ignore
    (Runtime.create_budget ~max_steps:max_int ~max_output_work:max_int ()
    |> checked)

let bridge_consumption_bounds () =
  let image = compile Preprocessor.Jit "42;" in
  let abi =
    match Program.status_abi image with
    | Program.Windows_x64 -> 1
    | Program.System_v_x64 -> 2
  in
  let retained =
    raw_retain
      (Obj.repr
         ( Program.code image,
           Array.of_list (Program.windows_unwind_functions image),
           abi,
           Program.entry_stack_bytes image,
           ( Program.global_bytes image,
             Program.literal_bytes image,
             Program.arena_metadata_bytes image,
             Program.global_image image ) ))
  in
  Fun.protect
    ~finally:(fun () -> raw_release retained)
    (fun () ->
      let limits steps =
        Obj.repr
          ( steps,
            1024,
            16,
            65536,
            Program.entry_stack_bytes image,
            1024,
            1024,
            8,
            8 )
      in
      let rejects limits consumed =
        let entered = ref false in
        match raw_execute retained limits consumed (Obj.repr entered) with
        | _ -> Alcotest.fail "malformed native consumption executed"
        | exception Invalid_argument _ ->
            Alcotest.(check bool)
              "preflight did not enter native code" false !entered
      in
      List.iter
        (rejects (limits 16))
        [
          Obj.repr ();
          Obj.repr (0, 0);
          Obj.repr (0, "bad", 0);
          Obj.repr (-1, 0, 0);
          Obj.repr (17, 0, 0);
          Obj.repr (0, -1, 0);
          Obj.repr (0, 9, 9);
          Obj.repr (0, 0, -1);
          Obj.repr (0, 0, 9);
          Obj.repr (0, 1, 0);
        ];
      rejects (limits 0) (Obj.repr (0, 0, 0));
      List.iter
        (fun marker ->
          match
            raw_execute retained (limits 16) (Obj.repr (0, 0, 0)) marker
          with
          | _ -> Alcotest.fail "malformed or used entry marker executed"
          | exception Invalid_argument _ -> ())
        [ Obj.repr (); Obj.repr (ref true); Obj.repr (ref "bad") ];
      let entered = ref false in
      let (kind, site, steps, value_site, bits), output, work =
        raw_execute retained (limits 16)
          (Obj.repr (16, 8, 8))
          (Obj.repr entered)
      in
      Alcotest.(check bool)
        "accepted native entry marks its activation" true !entered;
      (match
         Program.decode_runtime_status image ~max_steps:16 ~kind ~site
           ~executed_steps:steps ~value_site ~bits
         |> checked
       with
      | Program.Fault fault ->
          Alcotest.(check bool)
            "zero remaining native step guard" true
            (fault.kind = Program.Step_limit_exceeded);
          Alcotest.(check int)
            "exhausted cumulative projection" 16 fault.executed_steps
      | Program.Completed _ -> Alcotest.fail "exhausted bridge executed");
      Alcotest.(check string) "zero-size native capture" "" output;
      Alcotest.(check int) "zero remaining native output work" 0 work;
      raw_release retained;
      rejects (limits 16) (Obj.repr (0, 0, 0)))

let () =
  Alcotest.run "Native retained cumulative budgets"
    [
      ( "execution",
        [
          Alcotest.test_case "cumulative steps and original writes" `Quick
            cumulative_steps_and_original_writes;
          Alcotest.test_case "output exhaustion and saved prefix" `Quick
            output_exhaustion_preserves_prefix;
          Alcotest.test_case "output work and atomic drafts" `Quick
            output_work_and_atomic_drafts;
          Alcotest.test_case "faults and retryable preflight" `Quick
            faults_and_preflight;
          Alcotest.test_case "separate arenas and concurrent admission" `Quick
            independent_arenas_and_concurrency;
          Alcotest.test_case "bounded small output captures" `Quick
            bounded_capture_chunks;
          Alcotest.test_case "configuration bounds" `Quick configuration_bounds;
          Alcotest.test_case "native bridge consumed bounds" `Quick
            bridge_consumption_bounds;
        ] );
    ]
