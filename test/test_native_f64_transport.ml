open Holyc_lib
module Task = Holyc_lib__Driver.Integer_task
module Dispatch = Task.Native_dispatch
module VM = Ir_integer_interpreter

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let completed = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (diagnostics errors)

let rejected label = function
  | Error _ -> ()
  | Ok _ -> Alcotest.fail (label ^ " unexpectedly succeeded")

let fixture ?(claim = true) capture =
  let session = Session.create () in
  let requests = ref [] in
  let native_dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun _ -> Alcotest.fail "transport test reached an initializer");
      execute_command =
        (fun request ->
          requests := request :: !requests;
          if claim then Dispatch.claim_command_request request |> checked;
          Ok (capture request));
    }
  in
  let task = Task.create ~native_dispatch session |> checked in
  let run contents =
    let source =
      Session.add_source session ~path:"native-f64-transport.hc" ~contents
    in
    Task.run task ~source
  in
  (task, requests, run)

let assert_float task bits =
  match Task.native_final_value task with
  | Some (Dispatch.F64 actual) ->
      Alcotest.(check int64) "native float payload is unchanged" bits actual
  | _ -> Alcotest.fail "native final value lost its F64 kind"

let exact_payloads () =
  List.iter
    (fun bits ->
      let task, requests, run =
        fixture (fun _ -> Dispatch.Captured (Some (Dispatch.F64 bits)))
      in
      let result = run "1.5;" |> completed in
      assert_float task bits;
      Alcotest.(check bool)
        "F64 is not an integer interpreter result" true
        (Option.is_none (VM.final_value result));
      Alcotest.(check bool)
        "F64 is not integer task progress" true
        (Option.is_none (Task.progress task).runtime.final_value);
      Alcotest.(check int)
        "transport executes no interpreter instructions" 0
        (Task.progress task).runtime.executed_steps;
      List.iter
        (fun request ->
          rejected "completed request replay"
            (Dispatch.claim_command_request request))
        !requests)
    [
      0L;
      Int64.min_int;
      0x3ff8000000000000L;
      0x7ff0000000000000L;
      0xfff0000000000000L;
      0x7ff8000000000042L;
      0x7ff0000000000001L;
    ]

let mismatched_kinds () =
  List.iter
    (fun (contents, value) ->
      let task, _, run = fixture (fun _ -> Dispatch.Captured (Some value)) in
      rejected "foreign scalar kind" (run contents);
      Alcotest.(check bool)
        "rejected completion is not published" true
        (Option.is_none (Task.native_final_value task)))
    [
      ("1.5;", Dispatch.I64 42L);
      ("1.5;", Dispatch.U64 42L);
      ("42;", Dispatch.F64 0x3ff8000000000000L);
      ("42;", Dispatch.U64 42L);
    ];
  let task, _, run =
    fixture ~claim:false (fun _ -> Dispatch.Captured (Some (Dispatch.F64 0L)))
  in
  rejected "unentered request" (run "1.5;");
  Alcotest.(check bool)
    "unentered completion is not published" true
    (Option.is_none (Task.native_final_value task))

let preserved_until_capture () =
  let count = ref 0 in
  let bits = 0x7ff8000000000042L in
  let task, _, run =
    fixture (fun _ ->
        incr count;
        if !count = 1 then Dispatch.Captured (Some (Dispatch.F64 bits))
        else if !count = 2 then Dispatch.Unchanged
        else Dispatch.Captured (Some (Dispatch.I64 42L)))
  in
  ignore (run "1.5;2.5;" |> completed);
  assert_float task bits;
  ignore (run "42;" |> completed);
  (match Task.native_final_value task with
  | Some (Dispatch.I64 42L) -> ()
  | _ -> Alcotest.fail "later integer capture did not replace F64");
  match (Task.progress task).runtime.final_value with
  | Some word ->
      Alcotest.(check int64) "integer projection is retained" 42L word.bits
  | None -> Alcotest.fail "later integer completion lost interpreter projection"

let tests =
  [
    Alcotest.test_case "native F64 transport preserves all raw bits" `Quick
      exact_payloads;
    Alcotest.test_case
      "native completion rejects unentered and mismatched kinds" `Quick
      mismatched_kinds;
    Alcotest.test_case "native captures preserve and replace prior values"
      `Quick preserved_until_capture;
  ]
