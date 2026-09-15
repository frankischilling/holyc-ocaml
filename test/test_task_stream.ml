open Holyc_lib
module Task = Integer_task
module T = Test_integer_task

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let headers =
  {|extern U0 StreamPrint(U8 *fmt,...);extern U0 Print(U8 *fmt,...);|}

let create ?max_output_bytes ?max_output_work ?max_generated_bytes
    ?max_stream_depth ?max_frame_bytes ?max_call_depth () =
  let session = Session.create () in
  let task =
    Task.create ?max_output_bytes ?max_output_work ?max_generated_bytes
      ?max_stream_depth ?max_frame_bytes ?max_call_depth session
    |> checked
  in
  ignore (T.run session task headers |> Test_integer_program.checked);
  (session, task)

let run session task text =
  ignore (T.run session task text |> Test_integer_program.checked)

let begin_ task = Task.begin_stream task |> checked
let finish task stream = Task.finish_stream task stream |> checked
let abort task stream = Task.abort_stream task stream |> checked

let rejected = function
  | Error message ->
      Alcotest.(check bool)
        message true
        (String.starts_with ~prefix:"HCIRVM0027:" message)
  | Ok _ -> Alcotest.fail "foreign, consumed or suspended stream was accepted"

let nested_buffers () =
  let session, task = create () in
  run session task {|U0 Emit(I64 n){StreamPrint("%d",n);}|};
  let outer = begin_ task in
  run session task {|StreamPrint("4");Print("o");|};
  let inner = begin_ task in
  run session task {|Emit(2);Print("k");|};
  Alcotest.(check string) "inner text" "2" (finish task inner);
  run session task {|StreamPrint("%s;","2");|};
  Alcotest.(check string)
    "outer resumes its own buffer" "42;" (finish task outer);
  Alcotest.(check string)
    "ordinary capture is separate" "ok" (Task.output_bytes task)

let exact_token_ownership () =
  let session, task = create () in
  let _, other = create () in
  let outer = begin_ task in
  run session task {|StreamPrint("outer");|};
  let inner = begin_ task in
  run session task {|StreamPrint("inner");|};
  rejected (Task.finish_stream task outer);
  rejected (Task.abort_stream task outer);
  rejected (Task.finish_stream other inner);
  rejected (Task.abort_stream other inner);
  Alcotest.(check string)
    "rejections leave inner intact" "inner" (finish task inner);
  rejected (Task.finish_stream task inner);
  rejected (Task.abort_stream task inner);
  Alcotest.(check string)
    "rejections leave outer intact" "outer" (finish task outer);
  rejected (Task.finish_stream task outer)

let abort_after_fault () =
  let session, task = create () in
  run session task "I64 N=0;";
  let outer = begin_ task in
  run session task {|StreamPrint("keep");|};
  let inner = begin_ task in
  T.fault "HCIRVM0009"
    (T.run session task {|StreamPrint("discard");Print("reached");N=42;1/0;|});
  let work = Task.output_work task in
  abort task inner;
  rejected (Task.finish_stream task inner);
  Alcotest.(check int) "abort retains work" work (Task.output_work task);
  T.value 42L (T.run session task "N;");
  Alcotest.(check string)
    "ordinary effects survive" "reached" (Task.output_bytes task);
  Alcotest.(check string)
    "abort did not inject inner text" "keep" (finish task outer)

let shared_work_limits () =
  List.iter
    (fun limit ->
      let session, task = create ~max_output_work:limit () in
      let stream = begin_ task in
      run session task {|StreamPrint("A");|};
      if limit = 6 then (
        run session task {|Print("B");|};
        Alcotest.(check string) "ordinary exact" "B" (Task.output_bytes task))
      else (
        T.fault "HCIRVM0023" (T.run session task {|Print("B");|});
        Alcotest.(check string)
          "failed Print is atomic" "" (Task.output_bytes task));
      Alcotest.(check int)
        "shared exact work charge" limit (Task.output_work task);
      T.fault "HCIRVM0023" (T.run session task {|StreamPrint("");|});
      Alcotest.(check string)
        "earlier generation survives" "A" (finish task stream))
    [ 6; 5 ]

let shared_work_putchars_prefix () =
  List.iter
    (fun limit ->
      let session, task = create ~max_output_work:limit () in
      run session task "extern U0 PutChars(U64 ch);";
      let stream = begin_ task in
      run session task {|StreamPrint("A");|};
      if limit = 7 then run session task "PutChars('BC');"
      else T.fault "HCIRVM0023" (T.run session task "PutChars('BC');");
      Alcotest.(check int)
        "all output shares work" limit (Task.output_work task);
      Alcotest.(check string)
        "PutChars retains its reached prefix"
        (if limit = 7 then "BC" else "B")
        (Task.output_bytes task);
      Alcotest.(check string)
        "generation remains separate" "A" (finish task stream))
    [ 7; 6 ]

let generated_capacity () =
  List.iter
    (fun limit ->
      let session, task = create ~max_generated_bytes:limit () in
      let outer = begin_ task in
      run session task {|StreamPrint("4");|};
      let inner = begin_ task in
      run session task {|StreamPrint("2");|};
      Alcotest.(check string) "nested text" "2" (finish task inner);
      if limit = 3 then run session task {|StreamPrint(";");|}
      else T.fault "HCIRVM0028" (T.run session task {|StreamPrint(";");|});
      Alcotest.(check string)
        "shared generation capacity"
        (if limit = 3 then "4;" else "4")
        (finish task outer);
      Alcotest.(check int)
        "all nested fragments are charged" limit
        (Task.generated_bytes task);
      let later = begin_ task in
      T.fault "HCIRVM0028" (T.run session task {|StreamPrint("x");|});
      Alcotest.(check string)
        "finished buffers do not reset capacity" "" (finish task later))
    [ 3; 2 ]

let separate_byte_limits () =
  let session, task = create ~max_output_bytes:1 ~max_generated_bytes:3 () in
  let stream = begin_ task in
  run session task {|StreamPrint("42;");Print("o");|};
  T.fault "HCIRVM0022" (T.run session task {|Print("x");|});
  Alcotest.(check string)
    "generation has its own byte budget" "42;" (finish task stream);
  Alcotest.(check string)
    "ordinary bound remains independent" "o" (Task.output_bytes task)

let failed_draft_keeps_remaining_capacity () =
  let session, task = create ~max_generated_bytes:3 () in
  let stream = begin_ task in
  run session task {|StreamPrint("4");|};
  T.fault "HCIRVM0028" (T.run session task {|StreamPrint("23;");|});
  Alcotest.(check int)
    "failed draft is not committed" 1
    (Task.generated_bytes task);
  run session task {|StreamPrint("2;");|};
  Alcotest.(check int)
    "successful fragments consume capacity" 3
    (Task.generated_bytes task);
  Alcotest.(check int)
    "failed scanning and emission stay charged" 14 (Task.output_work task);
  Alcotest.(check string)
    "failed call appended no prefix" "42;" (finish task stream)

let frame_and_call_depth_limits () =
  List.iter
    (fun limit ->
      let session, task = create ~max_frame_bytes:limit () in
      let stream = begin_ task in
      if limit = 24 then run session task {|StreamPrint("%d",42);|}
      else T.fault "HCIRVM0011" (T.run session task {|StreamPrint("%d",42);|});
      Alcotest.(check string)
        "checked service argument slots"
        (if limit = 24 then "42" else "")
        (finish task stream))
    [ 24; 23 ];
  List.iter
    (fun limit ->
      let session, task = create ~max_call_depth:limit () in
      run session task {|U0 Emit(){StreamPrint("42;");}|};
      let stream = begin_ task in
      if limit = 2 then run session task "Emit();"
      else T.fault "HCIRVM0015" (T.run session task "Emit();");
      Alcotest.(check string)
        "service counts within retained call depth"
        (if limit = 2 then "42;" else "")
        (finish task stream))
    [ 2; 1 ]

let abort_keeps_capacity () =
  let session, task = create ~max_generated_bytes:1 () in
  let stream = begin_ task in
  run session task {|StreamPrint("x");|};
  abort task stream;
  let later = begin_ task in
  T.fault "HCIRVM0028" (T.run session task {|StreamPrint("y");|});
  Alcotest.(check string)
    "aborted generation stays charged" "" (finish task later)

let depth_and_zero_bytes () =
  let session, task = create ~max_stream_depth:2 ~max_generated_bytes:0 () in
  let outer = begin_ task in
  let inner = begin_ task in
  (match Task.begin_stream task with
  | Error message ->
      Alcotest.(check bool)
        message true
        (String.starts_with ~prefix:"HCIRVM0029:" message)
  | Ok _ -> Alcotest.fail "generation depth was not bounded");
  run session task {|StreamPrint("");|};
  Alcotest.(check string)
    "empty generation at zero capacity" "" (finish task inner);
  let replacement = begin_ task in
  abort task replacement;
  Alcotest.(check string)
    "depth rejection kept outer intact" "" (finish task outer);
  List.iter
    (fun result ->
      match result with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "invalid task generation limit")
    [
      Task.create ~max_stream_depth:0 (Session.create ());
      Task.create ~max_generated_bytes:(-1) (Session.create ());
    ]

let retained_provider_and_shadow () =
  List.iter
    (fun compatible ->
      let session, task = create () in
      run session task {|I64 N=0;U0 Emit(){++N;StreamPrint("old");}|};
      let original = begin_ task in
      run session task "Emit();";
      Alcotest.(check string)
        "unpublished source retains provider" "old" (finish task original);
      run session task
        (if compatible then {|U0 StreamPrint(U8 *fmt,...){Print("new");}|}
         else {|U0 StreamPrint(U8 *fmt){Print("new");}|});
      let stream = begin_ task in
      (if compatible then run session task "Emit();"
       else
         let result = T.run session task "Emit();" in
         T.fault "HCIRVM0014" result;
         let error = Test_integer_functions.first_error result in
         Alcotest.(check bool)
           "captured header mismatch faults at invocation" true
           (List.mem "stage=execution" error.notes));
      T.value 2L (T.run session task "N;");
      Alcotest.(check string)
        "published source never falls back to provider" "" (finish task stream);
      run session task {|StreamPrint("ignored");|};
      Alcotest.(check string)
        "compatible retained call uses the joined source body"
        (if compatible then "newnew" else "new")
        (Task.output_bytes task))
    [ true; false ]

let retained_literal_uses_current_buffer () =
  let session, task = create () in
  run session task {|U0 Emit(){U8 *p="4";StreamPrint("%s",p);*p='2';}|};
  let first = begin_ task in
  run session task "Emit();";
  Alcotest.(check string) "first call literal" "4" (finish task first);
  let second = begin_ task in
  run session task "Emit();";
  Alcotest.(check string)
    "old mutated literal feeds the new buffer" "2" (finish task second)

let ordinary_report_has_no_generation_authority () =
  List.iter
    (fun mode ->
      let report =
        Test_integer_output.run ~mode
          (headers ^ {|Print("prior");StreamPrint("42;");|})
      in
      ignore (Test_integer_output.fault ~output:"prior" "HCIRVM0027" report);
      Alcotest.(check int)
        "ordinary report preserves only reached formatting" 18
        (integer_program_report_output_work report))
    Test_integer_globals.modes

let formatter_failures () =
  List.iter
    (fun (code, source) ->
      let session, task = create () in
      let stream = begin_ task in
      T.fault code (T.run session task source);
      Alcotest.(check string)
        "failed formatter commits no generated prefix" "" (finish task stream);
      Alcotest.(check bool)
        "failed formatter retains reached work" true
        (Task.output_work task > 0))
    [
      ("HCIRVM0024", {|StreamPrint("prefix%q");|});
      ("HCIRVM0025", {|StreamPrint("%s",42);|});
      ("HCIRVM0012", {|U8 A[1];StreamPrint("%s",A);|});
      ("HCIRVM0019", {|U8 A[1]={'A'};StreamPrint("%s",A);|});
    ]

let inactive_arguments_and_independent_tasks () =
  let session, task = create () in
  run session task "I64 N=0;";
  T.fault "HCIRVM0027" (T.run session task {|StreamPrint("%d",++N);|});
  T.value 1L (T.run session task "N;");
  Alcotest.(check int)
    "inactive buffer follows reached formatting" 4 (Task.output_work task);
  let other_session, other = create () in
  let active = begin_ other in
  run other_session other {|StreamPrint("other");|};
  T.fault "HCIRVM0027" (T.run session task {|StreamPrint("foreign");|});
  Alcotest.(check string)
    "another task cannot select this buffer" "other" (finish other active)

let invalid_headers () =
  List.iter
    (fun header ->
      let session = Session.create () in
      let task = T.create session in
      let stream = begin_ task in
      let result = T.run session task (header ^ {|StreamPrint("42;");|}) in
      T.fault "HCIRVM0030" result;
      let error = Test_integer_functions.first_error result in
      Alcotest.(check bool)
        "unrecognized provider header remains unresolved until invocation" true
        (List.mem "stage=execution" error.notes);
      Alcotest.(check int)
        "invalid header has no formatter work" 0 (Task.output_work task);
      Alcotest.(check string)
        "name cannot authorize a service" "" (finish task stream))
    [
      "extern I64 StreamPrint(U8 *fmt,...);"; "extern U0 StreamPrint(U8 *fmt);";
    ]

let inactive_format_fault_priority () =
  let session, task = create () in
  T.fault "HCIRVM0024" (T.run session task {|StreamPrint("%q");|});
  Alcotest.(check int)
    "format fault precedes inactive context" 2 (Task.output_work task);
  Alcotest.(check int)
    "inactive formatting commits no generated bytes" 0
    (Task.generated_bytes task);
  let session, task = create ~max_output_work:1 () in
  T.fault "HCIRVM0023" (T.run session task {|StreamPrint("A");|});
  Alcotest.(check int)
    "format bound precedes inactive context" 1 (Task.output_work task);
  Alcotest.(check int)
    "failed inactive draft commits no bytes" 0
    (Task.generated_bytes task)

let tests =
  [
    Alcotest.test_case "inactive formatting fault order" `Quick
      inactive_format_fault_priority;
    Alcotest.test_case "nested buffers and ordinary output" `Quick
      nested_buffers;
    Alcotest.test_case "exact LIFO token ownership" `Quick exact_token_ownership;
    Alcotest.test_case "abort preserves reached ordinary effects" `Quick
      abort_after_fault;
    Alcotest.test_case "shared work exact and one below" `Quick
      shared_work_limits;
    Alcotest.test_case "shared work preserves PutChars prefix" `Quick
      shared_work_putchars_prefix;
    Alcotest.test_case "shared generated capacity exact and one below" `Quick
      generated_capacity;
    Alcotest.test_case "ordinary and generated byte limits" `Quick
      separate_byte_limits;
    Alcotest.test_case "failed draft retains remaining capacity" `Quick
      failed_draft_keeps_remaining_capacity;
    Alcotest.test_case "frame and call depth exact and one below" `Quick
      frame_and_call_depth_limits;
    Alcotest.test_case "abort retains generated capacity charge" `Quick
      abort_keeps_capacity;
    Alcotest.test_case "depth and zero byte bounds" `Quick depth_and_zero_bytes;
    Alcotest.test_case "retained provider and source replacement" `Quick
      retained_provider_and_shadow;
    Alcotest.test_case "retained mutable literal follows active buffer" `Quick
      retained_literal_uses_current_buffer;
    Alcotest.test_case "ordinary reports have no generation authority" `Quick
      ordinary_report_has_no_generation_authority;
    Alcotest.test_case "formatter memory and syntax failures" `Quick
      formatter_failures;
    Alcotest.test_case "inactive service and independent tasks" `Quick
      inactive_arguments_and_independent_tasks;
    Alcotest.test_case "unsupported headers grant no service" `Quick
      invalid_headers;
  ]
