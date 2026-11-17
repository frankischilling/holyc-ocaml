open Holyc_lib
module Image = X86_64_program
module Runtime = Native_program_execution
module Task = Holyc_lib__Driver.Integer_task
module Source = Holyc_lib__Driver.Integer_source_execution
module Scope = Holyc_lib__Ir.Native_source_suspension

external probe_available : unit -> bool = "holyc_test_f64_cw_available"
external get_cw : unit -> int = "holyc_test_f64_cw_get"
external set_cw : int -> unit = "holyc_test_f64_cw_set"

exception Callback_failure of int ref

let unwrap = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let checked = function
  | Ok value -> value
  | Error errors ->
      Alcotest.fail
        (String.concat "; "
           (List.map
              (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
              errors))

let compiled = function
  | Ok value -> value
  | Error errors ->
      Alcotest.fail
        (String.concat "; "
           (List.map (fun (e : Image.error) -> e.message) errors))

let check_host label = Alcotest.(check int) label 0x047f (get_cw ())

let with_hostile_control action =
  let original = get_cw () in
  Fun.protect
    ~finally:(fun () ->
      let observed = get_cw () in
      (* Restore before any assertion can raise, including on callback failure. *)
      set_cw original;
      Alcotest.(check int)
        "host CW on normal or exceptional exit" 0x047f observed)
    (fun () ->
      set_cw 0x047f;
      action ())

let arithmetic_image () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-f64-callback-child.hc"
      ~contents:"F64 Add(F64 a,F64 b){return a+b;}Add(1.5,2.25);"
  in
  let config = Preprocessor.Config.create () |> unwrap in
  let image =
    (Native_program.compile session ~source ~config |> checked).value
  in
  Alcotest.(check bool)
    "child contains actual x87 instructions" true (Image.requires_x87 image);
  image

let arithmetic image =
  match Runtime.execute ~max_steps:1000 image |> unwrap with
  | Image.Fault _ -> Alcotest.fail "F64 arithmetic faulted"
  | Image.Completed result ->
      let word = Option.get result.final_value in
      Alcotest.(check bool)
        "native result keeps F64 kind" true (word.type_ = Image.F64);
      Alcotest.(check int64)
        "native arithmetic exact bits" 0x400e000000000000L word.bits;
      check_host "host CW after F64 execution"

let callback_case ~raise_callback ~nested () =
  let child = arithmetic_image () in
  let layout =
    Image.create_task_layout_with_literals ~max_global_bytes:64
      ~max_literal_bytes:1024
    |> compiled
  in
  let arena =
    Runtime.create_task_arena ~max_arena_bytes:65_536 layout |> unwrap
  in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> unwrap in
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-f64-callback-parent.hc"
      ~contents:
        {|extern I64 StreamExePrint(U8 *fmt,...);#exe {StreamExePrint("payload%d",42);}42;|}
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit () |> unwrap
  in
  let calls = ref 0 in
  let saved_scope = ref None in
  let marker = ref 73 in
  let execute request =
    let image =
      Image.compile_task_command ~max_code_bytes:524_288 ~layout request
      |> compiled
    in
    let retained = Runtime.retain_task_fragment arena image |> unwrap in
    let callback scope operation =
      incr calls;
      saved_scope := Some scope;
      Scope.check scope |> unwrap;
      check_host "real OCaml callback receives surrounding host CW";
      (match operation with
      | Scope.Execute_source contents ->
          Alcotest.(check string) "actual formatted input" "payload42" contents
      | _ -> Alcotest.fail "unexpected compiler operation");
      if nested then arithmetic child;
      Gc.full_major ();
      check_host "host CW after nested execution and GC";
      if raise_callback then raise (Callback_failure marker);
      Some 42L
    in
    let report =
      Fun.protect
        ~finally:(fun () -> Runtime.release retained |> unwrap)
        (fun () ->
          Runtime.execute_retained_budget_report ~source_callback:callback
            budget retained)
    in
    check_host "host CW after actual callback return";
    match Runtime.outcome report |> unwrap with
    | Image.Fault _ ->
        Alcotest.fail "normal callback did not resume generated caller"
    | Image.Completed result ->
        Ok
          (if Runtime.value_captured report then
             Task.Native_dispatch.Captured
               (Option.map
                  (fun (word : Image.word) ->
                    match word.type_ with
                    | I64 -> Task.Native_dispatch.I64 word.bits
                    | U64 -> Task.Native_dispatch.U64 word.bits
                    | F64 -> Task.Native_dispatch.F64 word.bits)
                  result.final_value)
           else Task.Native_dispatch.Unchanged)
  in
  Fun.protect
    ~finally:(fun () -> Runtime.release_task_arena arena |> unwrap)
    (fun () ->
      with_hostile_control (fun () ->
          let raised =
            try
              let report =
                Source.run session ~source ~config ~max_steps:100_000
                  ~native_dispatch:
                    {
                      Task.Native_dispatch.execute_command = execute;
                      execute_initializer =
                        (fun _ -> Alcotest.fail "unexpected initializer");
                    }
              in
              Source.outcome report |> checked |> ignore;
              Alcotest.(check int)
                "no interpreter execution" 0
                (Option.get (Source.progress report)).runtime.executed_steps;
              false
            with Callback_failure actual ->
              Alcotest.(check bool)
                "original exception identity survives caml_callback_exn" true
                (actual == marker);
              check_host "host CW after real callback exception propagates";
              true
          in
          Alcotest.(check bool)
            "callback exception propagation" raise_callback raised;
          Alcotest.(check int) "actual callback count" 1 !calls;
          Scope.check (Option.get !saved_scope) |> fun result ->
          Alcotest.(check bool)
            "scope expires even after exception" true (Result.is_error result);
          (* An actual fresh F64 execution after either outcome detects damaged
           state/leases. Immediate full-state boundaries are tested separately. *)
          arithmetic child))

let () =
  if not (probe_available () && Native_execution.x87_supported ()) then
    print_endline
      "native F64 callback tests: unsupported x87 host/compiler, skipped"
  else
    Alcotest.run "Native F64 callback FP scopes"
      [
        ( "callbacks",
          [
            Alcotest.test_case "normal callback preserves hostile host CW"
              `Quick
              (callback_case ~raise_callback:false ~nested:false);
            Alcotest.test_case "real OCaml callback exception restores host CW"
              `Quick
              (callback_case ~raise_callback:true ~nested:false);
            Alcotest.test_case "nested native F64 child and callback return"
              `Quick
              (callback_case ~raise_callback:false ~nested:true);
            Alcotest.test_case "nested native F64 child then OCaml exception"
              `Quick
              (callback_case ~raise_callback:true ~nested:true);
          ] );
      ]
