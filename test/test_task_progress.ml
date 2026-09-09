open Holyc_lib
module Task = Integer_task
module T = Test_integer_task
module Stream = Test_task_stream
module VM = Ir_integer_interpreter

let snapshot task = (Task.progress task).runtime

let value expected (progress : VM.task_progress) =
  Alcotest.(check (option int64))
    "outer value latch" expected
    (Option.map (fun (word : VM.word) -> word.bits) progress.final_value)

let run session task source =
  T.run session task source |> Test_integer_program.checked |> ignore

let outer_latch () =
  let session, task = Stream.create () in
  value None (snapshot task);
  run session task "42;";
  value (Some 42L) (snapshot task);
  run session task "I64 N=20;";
  value (Some 42L) (snapshot task);
  (* Retained implicit providers still need their source-read authority. Keep
     that existing boundary explicit, then exercise the supported local header. *)
  T.fault "HCRUN0003" (T.run session task {|"A";|});
  value (Some 42L) (snapshot task);
  run session task {|extern U0 Print(U8 *fmt,...);"A";|};
  value (Some 42L) (snapshot task);
  run session task {|Print("B");|};
  value None (snapshot task);
  Alcotest.(check string)
    "both output paths reached" "AB" (snapshot task).output_bytes

let immutable_fault_snapshot () =
  let session, task = Stream.create () in
  run session task "40;";
  let earlier = snapshot task in
  T.fault "HCIRVM0009" (T.run session task {|Print("A");42;1/0;|});
  let faulted = snapshot task in
  value (Some 40L) earlier;
  value (Some 42L) faulted;
  Alcotest.(check string) "old output remains immutable" "" earlier.output_bytes;
  Alcotest.(check string) "fault retains output" "A" faulted.output_bytes;
  Alcotest.(check int)
    "admitted literal storage includes its terminator"
    (earlier.literal_bytes + 2)
    faulted.literal_bytes;
  Alcotest.(check bool)
    "fault retains reached instructions" true
    (faulted.executed_steps > earlier.executed_steps);
  run session task "21;";
  value (Some 42L) faulted;
  value (Some 21L) (snapshot task);
  Alcotest.(check int)
    "snapshot agrees with cumulative runtime" (Task.executed_steps task)
    (snapshot task).executed_steps;
  T.fault "HCIRVM0009" (T.run session task {|Print("B");1/0;|});
  value None (snapshot task);
  value (Some 42L) faulted;
  Alcotest.(check int)
    "saved literal count remains frozen"
    (earlier.literal_bytes + 2)
    faulted.literal_bytes;
  Alcotest.(check int)
    "later literal allocation is cumulative"
    (faulted.literal_bytes + 2)
    (snapshot task).literal_bytes

let stream_latches () =
  let session, task = Stream.create () in
  run session task "42;";
  let outer = Stream.begin_ task in
  run session task {|1;StreamPrint("outer");Print("A");|};
  value (Some 42L) (snapshot task);
  let inner = Stream.begin_ task in
  T.fault "HCIRVM0009" (T.run session task {|2;StreamPrint("inner");1/0;|});
  value (Some 42L) (snapshot task);
  Stream.abort task inner;
  Alcotest.(check string)
    "outer generation remains separate" "outer" (Stream.finish task outer);
  let progress = snapshot task in
  value (Some 42L) progress;
  Alcotest.(check string)
    "stream ordinary capture survives" "A" progress.output_bytes;
  Alcotest.(check int)
    "finished and aborted generation remains charged" 10
    progress.generated_bytes;
  Alcotest.(check int)
    "shared formatting work" (Task.output_work task) progress.output_work;
  run session task "43;";
  value (Some 43L) (snapshot task)

let preflight_is_read_only () =
  let session = Session.create () in
  let task = Task.create ~max_global_bytes:8 session |> Stream.checked in
  run session task "42;";
  let command = T.compile (Session.fork_frontend session) task "I64 A,B;1;" in
  let before = snapshot task in
  T.fault "HCIRVM0016" (Task.execute task command);
  Alcotest.(check bool)
    "preflight preserves every snapshot field" true
    (before = snapshot task);
  value (Some 42L) (snapshot task);
  let accepted = T.compile (Session.fork_frontend session) task "I64 N=1;" in
  ignore (Task.execute task accepted |> Test_integer_program.checked);
  let admitted = snapshot task in
  Alcotest.(check int)
    "only admitted storage is counted" 8 admitted.global_bytes;
  T.fault "HCIRVM0026" (Task.execute task accepted);
  Alcotest.(check bool)
    "replay preserves every snapshot field" true
    (admitted = snapshot task)

let compilation_and_parse_failures () =
  let session, task = Stream.create () in
  run session task {|Print("A");42;|};
  let before = Task.progress task in
  (match T.run session task "U8 A[2+1];I64 Bad=1/0;" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "invalid initializer compiled");
  let after = Task.progress task in
  value (Some 42L) after.runtime;
  Alcotest.(check string)
    "compile failure retains earlier capture" "A" after.runtime.output_bytes;
  Alcotest.(check bool)
    "dimension work is captured" true
    (after.dimension_work > before.dimension_work);
  Alcotest.(check bool)
    "failed preparation work is captured" true
    (after.runtime.initializer_steps - before.runtime.initializer_steps
    > after.dimension_work - before.dimension_work);
  Alcotest.(check int)
    "unadmitted storage is not allocated" before.runtime.global_bytes
    after.runtime.global_bytes;
  (match T.run session task "I64 Missing=;" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "invalid source parsed");
  Alcotest.(check bool)
    "parse failure preserves reached progress" true
    (after = Task.progress task);
  Alcotest.(check int)
    "snapshot agrees with task preparation"
    (Task.initializer_steps task)
    after.runtime.initializer_steps

let function_and_initializer_latches () =
  let session = Session.create () in
  let task = T.create session in
  run session task "40;";
  run session task "I64 F(){20;return 42;}I64 N=F();";
  value (Some 40L) (snapshot task);
  run session task "F();";
  value (Some 42L) (snapshot task);
  let other = T.create (Session.create ()) in
  value None (snapshot other);
  Alcotest.(check int)
    "fresh task has no runtime work" 0 (snapshot other).executed_steps

let reached_discard_budget () =
  let source = "41;42;" in
  let session = Session.create () in
  let task = T.create session in
  let execution = T.run session task source |> Test_integer_program.checked in
  let exact = VM.executed_steps execution in
  Alcotest.(check int) "two values and stream end" 5 exact;
  List.iter
    (fun (limit, expected) ->
      let session = Session.create () in
      let task = Task.create ~max_steps:limit session |> Stream.checked in
      let outcome = T.run session task source in
      if limit = exact then ignore (Test_integer_program.checked outcome)
      else T.fault "HCIRVM0007" outcome;
      let progress = snapshot task in
      value (Some expected) progress;
      Alcotest.(check int)
        "every reached instruction is charged" limit progress.executed_steps;
      T.fault "HCIRVM0007" (T.run session task "0;");
      Alcotest.(check bool)
        "exhausted preflight preserves reached value" true
        (progress = snapshot task))
    [ (exact, 42L); (exact - 1, 42L); (exact - 2, 41L) ]

let unsigned_and_pointer_values () =
  let session = Session.create () in
  let task = T.create session in
  run session task "U64 N=0xFFFFFFFFFFFFFFFF;N;";
  let progress = snapshot task in
  value (Some Int64.minus_one) progress;
  Alcotest.(check bool)
    "unsigned type is preserved" true
    ((Option.get progress.final_value).type_ = VM.U64);
  run session task "&N;";
  value None (snapshot task);
  value (Some Int64.minus_one) progress

let tests =
  [
    Alcotest.test_case "outer values survive declarations and implicit output"
      `Quick outer_latch;
    Alcotest.test_case "snapshots freeze reached output and fault values" `Quick
      immutable_fault_snapshot;
    Alcotest.test_case "nested stream commands preserve the outer value" `Quick
      stream_latches;
    Alcotest.test_case "preflight and replay do not alter progress" `Quick
      preflight_is_read_only;
    Alcotest.test_case "compile and parse failures retain earlier effects"
      `Quick compilation_and_parse_failures;
    Alcotest.test_case
      "function and initializer internals do not replace outer values" `Quick
      function_and_initializer_latches;
    Alcotest.test_case
      "value capture occurs at the reached discard budget boundary" `Quick
      reached_discard_budget;
    Alcotest.test_case "snapshots retain unsigned values and clear pointers"
      `Quick unsigned_and_pointer_values;
  ]
