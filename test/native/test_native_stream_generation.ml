open Holyc_lib
module Cases = Stream_generation_cases
module Native = Native_source_execution
module Image = X86_64_program
module VM = Ir_integer_interpreter

module Checkpoint = struct
  type t

  external create : Holyc_lib__Ir.Native_source_suspension.t -> unit ref -> t
    = "holyc_native_source_checkpoint_create"

  external consume :
    t ->
    Holyc_lib__Ir.Native_source_suspension.t ->
    unit ref ->
    int64
    * int
    * string
    * Holyc_lib__Ir.Integer_interpreter.native_generation
      Holyc_lib__Ir.Native_generation_capture.t
    = "holyc_native_source_checkpoint_consume"
end

let describe errors =
  errors
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let checked = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (describe errors)

let unwrap = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let rejects label result =
  Alcotest.(check bool) label true (Result.is_error result)

let inputs ?(mode = Preprocessor.Jit) ?max_generated_bytes text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-stream-generation.hc"
      ~contents:
        ((match mode with
           | Preprocessor.Jit -> Cases.headers
           | Preprocessor.Aot -> "")
        ^ text)
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode ?max_generated_bytes ()
    |> Result.get_ok
  in
  (session, source, config)

let run ?mode ?max_generated_bytes ?max_output_bytes ?max_output_work ?max_steps
    ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes ?max_global_bytes
    ?max_literal_bytes ?max_initializer_steps text =
  let session, source, config = inputs ?mode ?max_generated_bytes text in
  Native.evaluate ~max_code_bytes:524_288 ?max_output_bytes ?max_output_work
    ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes ?max_global_bytes
    ?max_literal_bytes ?max_initializer_steps session ~source ~config
    ~max_steps:(Option.value ~default:100_000 max_steps)

let value report =
  let result = Native.outcome report |> checked in
  Alcotest.(check (option int64))
    "source value" (Some 42L)
    (Option.map (fun word -> word.Native.bits) result.value.final_value);
  Alcotest.(check int)
    "zero interpreted instructions" 0
    (Option.get (Native.source_progress report)).runtime.executed_steps;
  List.iter
    (fun (fragment : Native.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (Image.Completed _)) -> ()
      | _ -> Alcotest.fail "source fragment did not complete in machine code")
    (Native.fragments report)

let generated report =
  (Option.get (Native.source_progress report)).runtime.generated_bytes

let native_compiler_option_authority () =
  let module Task = Holyc_lib__Driver.Integer_task in
  let module Source = Holyc_lib__Driver.Integer_source_execution in
  let module Scope = Holyc_lib__Ir.Native_source_suspension in
  let module Runtime = Native_program_execution in
  let compile = function
    | Ok value -> value
    | Error errors ->
        Alcotest.fail
          (String.concat "; "
             (List.map (fun (e : Image.error) -> e.message) errors))
  in
  List.iter
    (fun mode ->
      let layout =
        Image.create_task_layout_with_literals ~max_global_bytes:64
          ~max_literal_bytes:1024
        |> compile
      in
      let arena =
        Runtime.create_task_arena ~max_arena_bytes:65_536 layout |> unwrap
      in
      let budget = Runtime.create_budget ~max_steps:100_000 () |> unwrap in
      let expired = ref [] in
      let calls = ref 0 in
      let session, source, config =
        inputs ~mode
          (Cases.compiler_option_headers
         ^ {|#exe {Print("before;");Print("%d;",Option(33,1));Print("%d;",GetOption(33));Print("after;");}42;|}
          )
      in
      let execute request =
        let image =
          Image.compile_task_command ~max_code_bytes:524_288 ~layout request
          |> compile
        in
        let retained = Runtime.retain_task_fragment arena image |> unwrap in
        let original = Option.get (Image.source_callback image) in
        let callback scope operation =
          incr calls;
          Scope.check scope |> unwrap;
          Gc.full_major ();
          Gc.compact ();
          Alcotest.(check bool)
            "actual option request survives collection" true
            (Scope.owns_request scope operation |> unwrap);
          let copy = Obj.obj (Obj.dup (Obj.repr operation)) in
          Alcotest.(check bool)
            "option request copy is foreign" false
            (Scope.owns_request scope copy |> unwrap);
          rejects "copied request cannot execute the compiler option"
            (original scope copy);
          let request_block = Obj.repr operation in
          let index = Obj.field request_block 0 in
          Obj.set_field request_block 0 (Obj.repr 34L);
          Alcotest.(check bool)
            "changed option index is foreign" false
            (Scope.owns_request scope operation |> unwrap);
          rejects "changed index cannot execute the compiler option"
            (original scope operation);
          Obj.set_field request_block 0 index;
          (match operation with
          | Scope.Write_option (_, enabled) ->
              Obj.set_field request_block 1 (Obj.repr (not enabled));
              rejects "changed Bool cannot execute the compiler option"
                (original scope operation);
              Obj.set_field request_block 1 (Obj.repr enabled)
          | Scope.Read_option _ -> ()
          | Scope.Execute_source _ ->
              Alcotest.fail "option fixture produced source");
          Domain.join
            (Domain.spawn (fun () ->
                 rejects "option request is domain affine"
                   (Scope.owns_request scope operation)));
          expired := (scope, operation, original) :: !expired;
          original scope operation |> unwrap |> checked |> Option.some
        in
        let report =
          Fun.protect
            ~finally:(fun () -> Runtime.release retained |> unwrap)
            (fun () ->
              Runtime.execute_retained_budget_report ~source_callback:callback
                budget retained)
        in
        match Runtime.outcome report |> unwrap with
        | Image.Fault _ ->
            Alcotest.fail "original option callback did not resume"
        | Image.Completed result ->
            Ok
              (if Runtime.value_captured report then
                 Task.Native_dispatch.Captured
                   (Option.map
                      (fun (word : Image.word) ->
                        match word.type_ with
                        | Image.I64 -> Task.Native_dispatch.I64 word.bits
                        | Image.U64 -> Task.Native_dispatch.U64 word.bits)
                      result.final_value)
               else Unchanged)
      in
      let report =
        Fun.protect
          ~finally:(fun () -> Runtime.release_task_arena arena |> unwrap)
          (fun () ->
            Source.run
              ~native_dispatch:
                {
                  Task.Native_dispatch.execute_command = execute;
                  execute_initializer =
                    (fun _ ->
                      Alcotest.fail "unexpected option fixture initializer");
                }
              session ~source ~config ~max_steps:100_000)
      in
      Source.outcome report |> checked |> ignore;
      Alcotest.(check string)
        "option callback prefix and result" "before;0;1;after;"
        (Runtime.budget_output_bytes budget);
      Alcotest.(check int) "original option callbacks" 2 !calls;
      List.iter
        (fun (scope, operation, original) ->
          rejects "resumed option request expires"
            (Scope.owns_request scope operation);
          rejects "expired option request cannot execute"
            (original scope operation))
        !expired)
    [ Preprocessor.Jit ]

let compiler_options () =
  List.iter
    (fun mode ->
      List.iter
        (fun (name, text, output) ->
          let text =
            (if mode = Preprocessor.Jit then Cases.compiler_option_headers
             else "")
            ^ text
          in
          let report = run ~mode text in
          value report;
          Alcotest.(check string) name output (Native.output_bytes report);
          value (run ~mode ~max_steps:(Native.executed_steps report) text);
          let limited =
            run ~mode ~max_steps:(Native.executed_steps report - 1) text
          in
          match Native.outcome limited with
          | Error errors ->
              Alcotest.(check bool)
                (name ^ " cumulative instruction limit")
                true
                (List.exists
                   (fun (d : Diagnostic.t) -> d.code = "HCIRVM0007")
                   errors)
          | Ok _ ->
              Alcotest.fail "compiler option sequence exceeded exact quota")
        Cases.compiler_options;
      List.iter
        (fun index ->
          let text =
            (if mode = Preprocessor.Jit then Cases.compiler_option_headers
             else "")
            ^ Printf.sprintf
                "#exe {Print(\"before;\");Option(%d,1);Print(\"after;\");}42;"
                index
          in
          let report = run ~mode text in
          (match Native.outcome report with
          | Ok _ -> Alcotest.fail "invalid native option index was accepted"
          | Error errors ->
              Alcotest.(check bool)
                "original option diagnostic propagates" true
                (List.exists
                   (fun (d : Diagnostic.t) -> d.code = "HCEVAL0003")
                   errors));
          Alcotest.(check string)
            "invalid option retains native prefix" "before;"
            (Native.output_bytes report);
          Alcotest.(check bool)
            "original native compiler operation fault" true
            (List.exists
               (fun (fragment : Native.fragment) ->
                 match fragment.native_outcome with
                 | Some (Ok (Image.Fault fault)) ->
                     fault.kind = Image.Compiler_option_failed
                 | _ -> false)
               (Native.fragments report)))
        [ -1; 2; 63 ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let failure code report =
  match Native.outcome report with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors)

let values () =
  List.iter
    (fun (name, text, output, bytes) ->
      let report = run text in
      value report;
      Alcotest.(check string) name output (Native.output_bytes report);
      Alcotest.(check int) (name ^ " generation") bytes (generated report);
      let session, source, config = inputs text in
      let ir =
        run_integer_program_report session ~source ~config ~max_steps:100_000
      in
      let result = integer_program_report_outcome ir |> checked in
      Alcotest.(check (option int64))
        (name ^ " independent IR value")
        (Some 42L)
        (VM.final_value result.value |> Option.map (fun word -> word.VM.bits));
      Alcotest.(check string)
        (name ^ " independent IR output")
        output
        (integer_program_report_output_bytes ir);
      Alcotest.(check int)
        (name ^ " same original formatting work")
        (integer_program_report_output_work ir)
        (Native.output_work report))
    Cases.values

let failures () =
  List.iter
    (fun (name, text, code, output, work, bytes) ->
      let report = run text in
      failure code report;
      Alcotest.(check string) name output (Native.output_bytes report);
      Alcotest.(check int)
        (name ^ " reached work") work
        (Native.output_work report);
      Alcotest.(check int)
        (name ^ " retained generation")
        bytes (generated report);
      Alcotest.(check int)
        (name ^ " no interpreter fallback")
        0 (Option.get (Native.source_progress report)).runtime.executed_steps;
      Alcotest.(check bool)
        (name ^ " reached machine fault")
        true
        (List.exists
           (fun (fragment : Native.fragment) ->
             match fragment.native_outcome with
             | Some (Ok (Image.Fault _)) -> true
             | _ -> false)
           (Native.fragments report)))
    Cases.failures

let quotas () =
  let source = Cases.quota_source in
  let baseline =
    run ~max_generated_bytes:3 ~max_output_bytes:2 ~max_output_work:12 source
  in
  value baseline;
  Alcotest.(check string)
    "ordinary output has its own bound" "ok"
    (Native.output_bytes baseline);
  Alcotest.(check int) "generation has its own bound" 3 (generated baseline);
  value (run ~max_steps:(Native.executed_steps baseline) source);
  failure "HCIRVM0007"
    (run ~max_steps:(Native.executed_steps baseline - 1) source);
  let small = run ~max_generated_bytes:2 source in
  failure "HCIRVM0028" small;
  Alcotest.(check int) "failed draft did not commit" 0 (generated small);
  let work = run ~max_output_work:6 source in
  failure "HCIRVM0023" work;
  Alcotest.(check int) "work failure did not commit" 0 (generated work);
  let partial = run ~max_output_bytes:1 source in
  failure "HCIRVM0022" partial;
  Alcotest.(check string)
    "ordinary failed draft is atomic" ""
    (Native.output_bytes partial);
  Alcotest.(check int) "previous generation is retained" 3 (generated partial);
  value (run ~max_generated_bytes:6 Cases.two_streams);
  let small = run ~max_generated_bytes:5 Cases.two_streams in
  failure "HCIRVM0028" small;
  Alcotest.(check int) "previous stream stays charged" 3 (generated small)

let synchronous_boundary () =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, output) ->
          let report = run ~mode text in
          value report;
          Alcotest.(check string)
            "ordered original native child output" output
            (Native.output_bytes report);
          let session, source, config = inputs ~mode text in
          let independent =
            run_integer_program_report session ~source ~config
              ~max_steps:100_000
          in
          let result = integer_program_report_outcome independent |> checked in
          Alcotest.(check (option int64))
            "independent child value" (Some 42L)
            (VM.final_value result.value
            |> Option.map (fun word -> word.VM.bits));
          Alcotest.(check string)
            "independent child output" output
            (integer_program_report_output_bytes independent);
          Alcotest.(check int)
            "original child formatting work"
            (integer_program_report_output_work independent)
            (Native.output_work report))
        [
          ({|#exe {StreamExePrint("40+2;");}42;|}, "");
          ({|#exe {I64 (*p)(U8 *fmt,...)=&StreamExePrint;p("40+2;");}42;|}, "");
          ( {|#exe {I64 F(I64 (*p)(U8 *fmt,...)=&StreamExePrint){return p("40+2;");}F();}42;|},
            "" );
          ( {|#exe {Print("before;");I64 n=StreamExePrint("Print(\"child;\");42;");Print("after;%d;",n);}42;|},
            "before;child;after;42;" );
          ( {|#exe {I64 n=StreamExePrint("I64 N=2;I64 A[N]={40,2};I64 F(I64 n=A[0]){return n+2;}F();");Print("%d;",n);}42;|},
            "42;" );
          ( {|#exe {Print("before;");I64 n=StreamExePrint("I64 C=40;C+2;");Print("%d;",n);n=StreamExePrint("C+2;");Print("after;%d;",n);}42;|},
            "before;42;after;42;" );
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let executable_children () =
  let fixtures =
    [
      "I64 N=40;I64 F(I64 n=N){return n+2;}F();";
      "I64 F(){static U8 a[3]=\"AB\";return a[0]-23;}F();";
      "I64 N=2;I64 A[N]={40,2};A[0]+A[1];";
      "I64 N=16;class C{U8 a;$$=N;I64 b;};sizeof(C)+18;";
      "I64 Op=0x1e;_intern Op I64 Convert(U8 c);Convert(97)-23;";
      "I64 F(){return 40;}I64 (*old)()=&F;I64 F(){return 2;}old()+F();";
      "#exe {StreamPrint(\"I64 N=40;\");}N+2;";
      "#exe {StreamExePrint(\"class Inner {I64 n;};40+2;\");}42;";
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun child ->
          let text =
            "#exe {Print(\"before;\");Print(\"%d;\",StreamExePrint("
            ^ (Printf.sprintf "%S" child |> String.split_on_char '$'
             |> String.concat "$$")
            ^ "));Print(\"after;\");}42;"
          in
          let report = run ~mode text in
          value report;
          Alcotest.(check string)
            child "before;42;after;"
            (Native.output_bytes report);
          let session, source, config = inputs ~mode text in
          let independent =
            run_integer_program_report session ~source ~config
              ~max_steps:100_000
          in
          integer_program_report_outcome independent |> checked |> ignore;
          Alcotest.(check string)
            "independent child effects"
            (integer_program_report_output_bytes independent)
            (Native.output_bytes report);
          Alcotest.(check int)
            "independent child work"
            (integer_program_report_output_work independent)
            (Native.output_work report))
        fixtures)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let child_quotas () =
  let text =
    {|#exe {Print("before;");StreamExePrint("Print(\"child;\");42;");Print("after;");}42;|}
  in
  List.iter
    (fun mode ->
      let baseline = run ~mode text in
      value baseline;
      let steps = Native.executed_steps baseline in
      let work = Native.output_work baseline in
      value
        (run ~mode ~max_steps:steps ~max_output_bytes:19 ~max_output_work:work
           text);
      failure "HCIRVM0007" (run ~mode ~max_steps:(steps - 1) text);
      List.iter
        (fun (bytes, work_limit, code) ->
          let report =
            run ~mode ~max_output_bytes:bytes ~max_output_work:work_limit text
          in
          Option.iter (fun code -> failure code report) code;
          let session, source, config = inputs ~mode text in
          let independent =
            run_integer_program_report session ~source ~config
              ~max_steps:100_000 ~max_output_bytes:bytes
              ~max_output_work:work_limit
          in
          Alcotest.(check string)
            "child quota output"
            (integer_program_report_output_bytes independent)
            (Native.output_bytes report);
          Alcotest.(check int)
            "child quota work"
            (integer_program_report_output_work independent)
            (Native.output_work report);
          Alcotest.(check int)
            "child quotas have no VM fallback" 0
            (Option.get (Native.source_progress report)).runtime.executed_steps)
        [
          (19, work, None);
          (18, work, Some "HCIRVM0022");
          (19, work - 1, Some "HCIRVM0023");
        ];
      let payload = String.concat "" (List.init 20 (fun _ -> "42;")) in
      let child =
        "#exe {StreamPrint(" ^ Printf.sprintf "%S" payload ^ ");}42;"
      in
      let generated_text =
        "#exe {StreamExePrint(" ^ Printf.sprintf "%S" child ^ ");StreamPrint("
        ^ Printf.sprintf "%S" payload
        ^ ");}42;"
      in
      value (run ~mode ~max_generated_bytes:120 generated_text);
      let short = run ~mode ~max_generated_bytes:119 generated_text in
      failure "HCIRVM0028" short;
      Alcotest.(check int)
        "actual child generation remains charged" 60 (generated short))
    [ Preprocessor.Jit; Preprocessor.Aot ];
  let catalogs =
    {|extern U0 Print(U8 *fmt,...);#exe {I64 D=1;Print("task");StreamExePrint("I64 C=2;42;");}I64 M=40;Print("outer");M+2;|}
  in
  value
    (run ~mode:Preprocessor.Aot ~max_global_bytes:24 ~max_literal_bytes:23
       catalogs);
  failure "HCBACK0004"
    (run ~mode:Preprocessor.Aot ~max_global_bytes:23 catalogs);
  failure "HCBACK0004"
    (run ~mode:Preprocessor.Aot ~max_literal_bytes:22 catalogs)

let suspended_code_and_storage () =
  List.iter
    (fun text ->
      let report = run text in
      value report;
      Alcotest.(check string)
        "suspended original code output" "42;"
        (Native.output_bytes report);
      let session, source, config = inputs text in
      let independent =
        run_integer_program_report session ~source ~config ~max_steps:100_000
      in
      integer_program_report_outcome independent |> checked |> ignore;
      Alcotest.(check string)
        "independent suspended original code"
        (integer_program_report_output_bytes independent)
        (Native.output_bytes report))
    [
      {|#exe {I64 N=0;I64 F(){if(!N){N=1;StreamExePrint("F()+2;");}return 40;}Print("%d;",F()+2);}42;|};
      {|#exe {I64 F(){StreamExePrint("I64 F(){return 2;}40+F();");return 42;}Print("%d;",F());}42;|};
      {|#exe {I64 F(){static I64 N=StreamExePrint("I64 Added=40;Added+2;");return N;}Print("%d;",F());}42;|};
    ]

let physical_child_quotas () =
  let text =
    {|#exe {I64 Parent(){I64 a[2];return StreamExePrint("I64 Child(){I64 a[2];return 42;}Child();");}Print("%d;",Parent());}42;|}
  in
  List.iter
    (fun mode ->
      value (run ~mode ~max_call_depth:2 text);
      failure "HCIRVM0015" (run ~mode ~max_call_depth:1 text);
      value (run ~mode ~max_frame_bytes:32 text);
      failure "HCIRVM0011" (run ~mode ~max_frame_bytes:31 text);
      let rec minimum low high =
        if low = high then low
        else
          let middle = (low + high) / 2 in
          if
            Result.is_ok
              (Native.outcome (run ~mode ~max_active_stack_bytes:middle text))
          then minimum low middle
          else minimum (middle + 1) high
      in
      let stack = minimum 1 4096 in
      let exact = run ~mode ~max_active_stack_bytes:stack text in
      value exact;
      rejects "physical caller stack remains charged during child entry"
        (Native.outcome (run ~mode ~max_active_stack_bytes:(stack - 1) text));
      let largest_entry =
        List.fold_left
          (fun largest (fragment : Native.fragment) ->
            max largest fragment.image.entry_stack_bytes)
          0 (Native.fragments exact)
      in
      Alcotest.(check bool)
        "nested stack is stricter than every fresh entry" true
        (stack > largest_entry))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let executable_child_collection () =
  let saved = Gc.get () in
  Fun.protect
    ~finally:(fun () -> Gc.set saved)
    (fun () ->
      Gc.set
        {
          saved with
          minor_heap_size = 1024;
          space_overhead = 1;
          max_overhead = 0;
        };
      executable_children ();
      suspended_code_and_storage ();
      Gc.full_major ();
      Gc.compact ();
      child_quotas ())

let synchronous_declarations () =
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          let report = run ~mode text in
          value report;
          Alcotest.(check string)
            "declaration children return zero after original source completion"
            "0;"
            (Native.output_bytes report);
          Alcotest.(check int)
            "ordinary child declarations emit no generated bytes" 0
            (generated report);
          let session, source, config = inputs ~mode text in
          let independent =
            run_integer_program_report session ~source ~config
              ~max_steps:100_000
          in
          let result = integer_program_report_outcome independent |> checked in
          Alcotest.(check (option int64))
            "independent saved parser publishes the same class" (Some 42L)
            (VM.final_value result.value
            |> Option.map (fun word -> word.VM.bits));
          Alcotest.(check string)
            "independent declaration child output" "0;"
            (integer_program_report_output_bytes independent))
        [
          {|#exe {Print("%d;",StreamExePrint("class Made {I64 n;};"));}sizeof(Made)+34;|};
          {|#exe {I64 (*p)(U8 *fmt,...)=&StreamExePrint;Print("%d;",p("class Made {I64 n;};"));}sizeof(Made)+34;|};
          {|#exe {I64 n=StreamExePrint("class Made {I64 n;};");Print("%d;",n);}sizeof(Made)+34;|};
          {|#exe {I64 F(){static I64 n=StreamExePrint("class Made {I64 n;};");return n;}Print("%d;",F());}sizeof(Made)+34;|};
          {|#exe {I64 a[StreamExePrint("class Made {I64 n;};")+1];Print("%d;",sizeof(a)-8);}sizeof(Made)+34;|};
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let synchronous_failure_order () =
  let module Visibility = Holyc_lib__Frontend.Symbol_visibility in
  List.iter
    (fun mode ->
      let session, source, config =
        inputs ~mode
          {|#exe {Print("before;");StreamExePrint("class Reached {I64 n;};Print(\"child;\");1/0;");Print("after;");}42;|}
      in
      let report =
        Native.evaluate ~max_code_bytes:524_288 session ~source ~config
          ~max_steps:100_000
      in
      rejects "original child arithmetic fault propagates"
        (Native.outcome report);
      Alcotest.(check string)
        "caller and child output before the fault survive" "before;child;"
        (Native.output_bytes report);
      Alcotest.(check bool)
        "reached original child declaration survives its later entry fault" true
        (Option.is_some
           (Visibility.Environment.find_class (Session.symbols session)
              "Reached"));
      Alcotest.(check int)
        "failure used no interpreter" 0
        (Option.get (Native.source_progress report)).runtime.executed_steps;
      let errors = Native.outcome report |> Result.get_error in
      Alcotest.(check bool)
        "child diagnostics and the caller source fault both survive" true
        (List.exists (fun (d : Diagnostic.t) -> d.code = "HCIRVM0009") errors
        && List.length errors >= 2))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let capture_authority () =
  let module Task = Holyc_lib__Driver.Integer_task in
  let module Source = Holyc_lib__Driver.Integer_source_execution in
  let module Internal_vm = Holyc_lib__Ir.Integer_interpreter in
  let module Capture = Holyc_lib__Ir.Native_generation_capture in
  let module Runtime = Native_program_execution in
  let compiled = function
    | Ok value -> value
    | Error errors ->
        Alcotest.fail
          (String.concat "; "
             (List.map (fun (e : Image.error) -> e.message) errors))
  in
  let layout () =
    Image.create_task_layout_with_literals ~max_global_bytes:64
      ~max_literal_bytes:128
    |> compiled
  in
  let original_layout = layout () in
  let arena =
    Runtime.create_task_arena ~max_arena_bytes:4096 original_layout |> unwrap
  in
  let foreign =
    Runtime.create_task_arena ~max_arena_bytes:4096 (layout ()) |> unwrap
  in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> unwrap in
  let saved = ref None in
  let execute request =
    rejects "foreign domain cannot claim original stream entry"
      (Domain.join
         (Domain.spawn (fun () ->
              Task.Native_dispatch.claim_command_request request)));
    let host, other =
      match Runtime.platform () with
      | Windows_x86_64 -> (Image.Windows_x64, Image.System_v_x64)
      | _ -> (Image.System_v_x64, Image.Windows_x64)
    in
    let wrong =
      Image.compile_task_command ~status_abi:other ~layout:original_layout
        request
      |> compiled
    in
    rejects "foreign ABI cannot retain stream entry"
      (Runtime.retain_task_fragment arena wrong);
    let image =
      Image.compile_task_command ~status_abi:host ~layout:original_layout
        request
      |> compiled
    in
    let target = Option.get (Image.generation image) in
    let active, _, _ = Internal_vm.native_generation_limits target |> unwrap in
    let copy = Obj.obj (Obj.dup (Obj.repr target)) in
    let original_target = Task.Native_dispatch.command_generation request in
    Alcotest.(check bool)
      "request retains its original generation target" true
      (original_target == Task.Native_dispatch.command_generation request);
    let foreign_target = Obj.obj (Obj.dup (Obj.repr original_target)) in
    rejects "generation token belongs to its original domain"
      (Domain.join
         (Domain.spawn (fun () -> Internal_vm.native_generation_limits target)));
    rejects "equal metadata in another arena grants no entry"
      (Runtime.retain_task_fragment foreign image);
    let retained = Runtime.retain_task_fragment arena image |> unwrap in
    let report =
      Fun.protect
        ~finally:(fun () -> Runtime.release retained |> unwrap)
        (fun () ->
          Gc.full_major ();
          Gc.compact ();
          Runtime.execute_retained_budget_report budget retained)
    in
    let result =
      match Runtime.outcome report |> unwrap with
      | Image.Completed value -> value
      | Image.Fault _ -> Alcotest.fail "unexpected stream fault"
    in
    let capture = Option.get (Runtime.generation_capture report) in
    Gc.full_major ();
    Gc.compact ();
    rejects "metadata clone cannot consume an executed capture"
      (Capture.consume capture ~target:copy);
    rejects "another original target cannot consume capture"
      (Capture.consume capture ~target:foreign_target);
    rejects "actual capture was already published exactly once"
      (Capture.consume capture ~target);
    rejects "original target cannot be completed twice"
      (Internal_vm.complete_native_generation target capture);
    rejects "released command cannot enter twice"
      (Task.Native_dispatch.claim_command_request request);
    if active then saved := Some (target, capture);
    Ok
      (Task.Native_dispatch.Captured
         (Option.map
            (fun (word : Image.word) ->
              match word.type_ with
              | I64 -> Task.Native_dispatch.I64 word.bits
              | U64 -> Task.Native_dispatch.U64 word.bits)
            result.final_value))
  in
  let dispatch : Task.Native_dispatch.t =
    {
      execute_initializer =
        (fun _ -> Alcotest.fail "fixture has no initializer");
      execute_command = execute;
    }
  in
  Fun.protect
    ~finally:(fun () ->
      Runtime.release_task_arena foreign |> unwrap;
      Runtime.release_task_arena arena |> unwrap)
    (fun () ->
      let session, source, config = inputs {|#exe {StreamPrint("42;");}|} in
      let report =
        Source.run ~native_dispatch:dispatch session ~source ~config
          ~max_steps:100_000
      in
      Source.outcome report |> checked |> ignore;
      Alcotest.(check bool)
        "original generated source remains executable" true
        (Source.native_final_value report = Some (Task.Native_dispatch.I64 42L));
      Alcotest.(check int)
        "actual generation is charged once" 3
        (Option.get (Source.progress report)).runtime.generated_bytes;
      let target, capture = Option.get !saved in
      Runtime.release_task_arena arena |> unwrap;
      Gc.full_major ();
      Gc.compact ();
      let error =
        match Capture.consume capture ~target with
        | Ok _ -> Alcotest.fail "expired arena authorized source bytes"
        | Error message -> message
      in
      Alcotest.(check bool)
        "capture retains original arena expiry" true
        (String.starts_with ~prefix:"native generation capture has an expired"
           error))

let native_source_callback_scope ?(failure = `None)
    ?(on_callback = fun () -> ())
    ?(fixture =
      {|#exe {I64 Emit(){Print("before;");StreamPrint("40;");Print("%d;%d;after;",StreamExePrint("payload%d",42),StreamExePrint("payload%d",42));StreamPrint("42;");return 42;}Emit;}42;|})
    () =
  let module Task = Holyc_lib__Driver.Integer_task in
  let module Source = Holyc_lib__Driver.Integer_source_execution in
  let module Scope = Holyc_lib__Ir.Native_source_suspension in
  let module Runtime = Native_program_execution in
  let module Internal_vm = Holyc_lib__Ir.Integer_interpreter in
  let module Driver_session = Holyc_lib__Driver.Session in
  let compile = function
    | Ok value -> value
    | Error errors ->
        Alcotest.fail
          (String.concat "; "
             (List.map (fun (error : Image.error) -> error.message) errors))
  in
  let layout =
    Image.create_task_layout_with_literals ~max_global_bytes:64
      ~max_literal_bytes:1024
    |> compile
  in
  let arena =
    Runtime.create_task_arena ~max_arena_bytes:65_536 layout |> unwrap
  in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> unwrap in
  let saved = ref None in
  let saved_checkpoint = ref None in
  let calls = ref 0 in
  let namespace = ref None in
  let foreign_budget = Runtime.create_budget ~max_steps:100_000 () |> unwrap in
  let session, source, config = inputs fixture in
  let callback_error () =
    [
      Diagnostic.make ~code:"HCRUN0004" ~severity:Diagnostic.Error
        ~message:"source callback rejected its input"
        ~primary:(Holyc_lib__Driver.Integer_source.source_span source)
        ();
    ]
  in
  let execute request =
    let image =
      Image.compile_task_command ~max_code_bytes:524_288 ~layout request
      |> compile
    in
    let retained = Runtime.retain_task_fragment arena image |> unwrap in
    let source_callback scope operation =
      let source =
        match operation with
        | Scope.Execute_source contents -> contents
        | _ ->
            Alcotest.fail "source fixture received another compiler operation"
      in
      on_callback ();
      incr calls;
      Alcotest.(check string) "actual formatted source" "payload42" source;
      Scope.check scope |> unwrap;
      Alcotest.(check bool)
        "exact machine request belongs to scope" true
        (Scope.owns_request scope operation |> unwrap);
      let copied_operation = Obj.obj (Obj.dup (Obj.repr operation)) in
      Alcotest.(check bool)
        "copied machine request cannot authorize entry" false
        (Scope.owns_request scope copied_operation |> unwrap);

      Alcotest.(check bool)
        "original cumulative budget" true
        (Runtime.suspension_owns_budget scope budget |> unwrap);
      Alcotest.(check bool)
        "equal allowances have another owner" false
        (Runtime.suspension_owns_budget scope foreign_budget |> unwrap);
      let target = Option.get (Image.generation image) in
      let progress = Runtime.budget_progress budget in
      Alcotest.(check string)
        "caller prefix is admitted before the handler" "before;"
        progress.output_bytes;
      Alcotest.(check bool)
        "caller steps and formatting work are already charged" true
        (progress.executed_steps > 0 && progress.output_work > 7);
      (* These raw calls attack the private FFI. Copying the budget record or
         its counters cannot mint the physical identity held by the C caller. *)
      let identity : unit ref = Obj.obj (Obj.field (Obj.repr budget) 0) in
      let foreign = ref () in
      let raw operation =
        try
          operation ();
          Ok ()
        with Failure message | Invalid_argument message -> Error message
      in
      rejects "checkpoint creation requires the original budget"
        (raw (fun () -> ignore (Checkpoint.create scope foreign)));
      let checkpoint = Checkpoint.create scope identity in
      saved_checkpoint := Some (checkpoint, scope, identity);
      rejects "checkpoint consumption rejects another budget"
        (raw (fun () -> ignore (Checkpoint.consume checkpoint scope foreign)));
      let malformed_scope = Obj.obj (Obj.repr (ref ())) in
      rejects "checkpoint rejects fabricated scope metadata"
        (raw (fun () ->
             ignore (Checkpoint.consume checkpoint malformed_scope identity)));
      rejects "checkpoint cannot cross execution domains"
        (Domain.join
           (Domain.spawn (fun () ->
                raw (fun () ->
                    ignore (Checkpoint.consume checkpoint scope identity)))));
      Gc.full_major ();
      Gc.compact ();
      let steps, work, output, empty =
        Checkpoint.consume checkpoint scope identity
      in
      Alcotest.(check int64)
        "checkpoint has actual cumulative native steps"
        (Int64.of_int progress.executed_steps)
        steps;
      Alcotest.(check int) "already admitted work is not charged again" 0 work;
      Alcotest.(check string)
        "already admitted output is not copied again" "" output;
      Alcotest.(check (pair int int))
        "capture retains the actual generated frontier" (3, 3)
        (Holyc_lib__Ir.Native_generation_capture.bounds empty ~target |> unwrap);
      rejects "active prefix cannot use ordinary capture permission"
        (Holyc_lib__Ir.Native_generation_capture.consume empty ~target);
      Holyc_lib__Ir.Native_generation_capture.consume ~scope empty ~target
      |> unwrap |> ignore;
      rejects "checkpoint cannot be replayed"
        (raw (fun () -> ignore (Checkpoint.consume checkpoint scope identity)));
      Alcotest.(check bool)
        "original generation owner" true
        (Scope.owns_generation scope target |> unwrap);
      let copied = Obj.obj (Obj.dup (Obj.repr target)) in
      Alcotest.(check bool)
        "copied generation metadata is foreign" false
        (Scope.owns_generation scope copied |> unwrap);
      Option.iter
        (fun previous ->
          rejects "earlier callback scope is closed" (Scope.check previous))
        !saved;
      saved := Some scope;
      let steps, frame, depth, stack = Scope.limits scope |> unwrap in
      Alcotest.(check bool)
        "live caller limits" true
        (steps > 0 && steps < 100_000 && frame > 0 && depth > 0 && stack > 0);
      rejects "foreign domain cannot borrow native suspension"
        (Domain.join (Domain.spawn (fun () -> Scope.check scope)));
      rejects "scope is not a callback bridge"
        (try
           Scope.open_raw scope |> ignore;
           Ok ()
         with Invalid_argument message -> Error message);
      rejects "ordinary budget entry is still excluded"
        (Runtime.execute_retained_budget_report budget retained
        |> Runtime.outcome);
      rejects "original caller cannot be released" (Runtime.release retained);
      let ordinary_session = Session.create () in
      let ordinary_source =
        Session.add_source ordinary_session ~path:"unscoped-image.hc"
          ~contents:"42;"
      in
      let ordinary_config = Preprocessor.Config.create () |> unwrap in
      let ordinary_image =
        (Native_program.compile ordinary_session ~source:ordinary_source
           ~config:ordinary_config
        |> checked)
          .value
      in
      let ordinary = Runtime.retain ordinary_image |> unwrap in
      rejects "a scope grants no private-image child entry"
        (Runtime.execute_retained_budget_report ~scope budget ordinary
        |> Runtime.outcome);
      Runtime.release ordinary |> unwrap;
      rejects "scope cannot borrow another original budget"
        (Runtime.execute_retained_budget_report ~scope foreign_budget retained
        |> Runtime.outcome);
      rejects "scope cannot reactivate the original running image"
        (Runtime.execute_retained_budget_report ~scope budget retained
        |> Runtime.outcome);
      rejects "original arena cannot be released"
        (Runtime.release_task_arena arena);
      Gc.full_major ();
      Gc.compact ();
      Scope.check scope |> unwrap;
      Alcotest.(check bool)
        "entered request retains its physical target" true
        (target == Task.Native_dispatch.command_generation request);
      rejects "copied target cannot reserve caller resources"
        (Internal_vm.with_native_source_suspension copied ~scope (fun _ ->
             Alcotest.fail "copied target entered"));
      rejects "another domain cannot reserve caller resources"
        (Domain.join
           (Domain.spawn (fun () ->
                Internal_vm.with_native_source_suspension target ~scope
                  (fun _ -> Alcotest.fail "foreign domain entered"))));
      let evaluate () =
        Internal_vm.with_native_source_suspension target ~scope (fun task ->
            let peer_session = Driver_session.create () in
            let table = Driver_session.semantic_symbols peer_session in
            let peer =
              Internal_vm.create_compiler_namespace_task task ~table |> unwrap
            in
            Alcotest.(check bool)
              "namespace shares original resources" true
              (Internal_vm.task_shares_resources task peer);
            Alcotest.(check bool)
              "namespace retains separate tables" false
              (Internal_vm.task_owns_table task table);
            namespace := Some (peer, table);
            Gc.full_major ();
            Gc.compact ();
            Scope.check scope |> unwrap;
            match failure with
            | `None -> Some 42L
            | `Reject -> None
            | `Raise -> failwith "native source callback exception")
        |> unwrap
      in
      Fun.protect
        ~finally:(fun () ->
          Option.iter
            (fun (peer, table) ->
              match Internal_vm.create_compiler_namespace_task peer ~table with
              | Error message ->
                  Alcotest.(check bool)
                    "reservation closes and peer keeps native authority" true
                    (String.starts_with
                       ~prefix:"native saved compiler input requires" message)
              | Ok _ -> Alcotest.fail "native reservation leaked")
            !namespace)
        evaluate
    in
    let report =
      Fun.protect
        ~finally:(fun () ->
          Option.iter
            (fun scope ->
              rejects "scope closes on normal and exceptional returns"
                (Scope.check scope))
            !saved;
          Runtime.release retained |> unwrap)
        (fun () ->
          Runtime.execute_retained_budget_report ~source_callback budget
            retained)
    in
    Option.iter
      (fun scope ->
        rejects "suspension expires before caller resumes" (Scope.check scope);
        rejects "expired scope cannot read caller limits" (Scope.limits scope))
      !saved;
    Option.iter
      (fun (checkpoint, scope, identity) ->
        rejects "checkpoint expires with its physical callback"
          (try
             ignore (Checkpoint.consume checkpoint scope identity);
             Ok ()
           with Failure message | Invalid_argument message -> Error message))
      !saved_checkpoint;
    match Runtime.outcome report |> unwrap with
    | Image.Fault fault when failure = `Reject ->
        Alcotest.(check bool)
          "reached callback failure" true
          (fault.kind = Image.Stream_exe_source_failed);
        Error (callback_error ())
    | Image.Fault _ ->
        Alcotest.fail "source callback did not resume native caller"
    | Image.Completed result ->
        Ok
          (if Runtime.value_captured report then
             Task.Native_dispatch.Captured
               (Option.map
                  (fun (word : Image.word) ->
                    match word.type_ with
                    | I64 -> Task.Native_dispatch.I64 word.bits
                    | U64 -> Task.Native_dispatch.U64 word.bits)
                  result.final_value)
           else Task.Native_dispatch.Unchanged)
  in
  let dispatch : Task.Native_dispatch.t =
    {
      execute_initializer =
        (fun _ -> Alcotest.fail "fixture has no initializer");
      execute_command = execute;
    }
  in
  Fun.protect
    ~finally:(fun () -> Runtime.release_task_arena arena |> unwrap)
    (fun () ->
      let report =
        try
          Some
            (Source.run ~native_dispatch:dispatch session ~source ~config
               ~max_steps:100_000)
        with
        | Failure message
        when failure = `Raise && message = "native source callback exception"
        ->
          None
      in
      (match (failure, report) with
      | `Raise, None -> ()
      | `None, Some report -> Source.outcome report |> checked |> ignore
      | `Reject, Some report ->
          rejects "source sees callback failure" (Source.outcome report)
      | _ -> Alcotest.fail "callback exception was not propagated after cleanup");
      Alcotest.(check int)
        "physical callbacks"
        (if failure = `None then 2 else 1)
        !calls;
      Alcotest.(check string)
        "GC preserves native prefix and resumed suffix"
        (if failure = `None then "before;42;42;after;" else "before;")
        (Runtime.budget_output_bytes budget);
      Option.iter
        (fun report ->
          Alcotest.(check int)
            "no interpreted instructions" 0
            (Option.get (Source.progress report)).runtime.executed_steps)
        report;
      Option.iter
        (fun report ->
          Alcotest.(check int)
            "GC preserves live generation bytes"
            (if failure = `None then 6 else 3)
            (Option.get (Source.progress report)).runtime.generated_bytes)
        report;
      Alcotest.(check bool)
        "budget remains verified" true
        ((Runtime.budget_progress budget).error = None))

let native_source_callbacks () =
  List.iter
    (fun failure ->
      native_source_callback_scope ~failure ();
      native_source_callback_scope ~failure
        ~fixture:
          {|#exe {I64 Emit(I64 (*p)(U8 *fmt,...)){Print("before;");StreamPrint("40;");Print("%d;%d;after;",p("payload%d",42),p("payload%d",42));StreamPrint("42;");return 42;}Emit(&StreamExePrint);}42;|}
        ())
    [ `None; `Reject; `Raise ];
  ()

let checkpoint_quotas () =
  let text =
    {|#exe {Print("before;");StreamPrint("40;");StreamExePrint("class Made {I64 n;};");Print("after;");StreamPrint("42;");}42;|}
  in
  List.iter
    (fun mode ->
      let baseline = run ~mode text in
      value baseline;
      let work = Native.output_work baseline in
      List.iter
        (fun (max_output_bytes, max_output_work, expected_code) ->
          let report = run ~mode ~max_output_bytes ~max_output_work text in
          Option.iter (fun code -> failure code report) expected_code;
          let session, source, config = inputs ~mode text in
          let independent =
            run_integer_program_report session ~source ~config ~max_output_bytes
              ~max_output_work ~max_steps:100_000
          in
          Alcotest.(check string)
            "checkpoint output matches independent IR"
            (integer_program_report_output_bytes independent)
            (Native.output_bytes report);
          Alcotest.(check int)
            "checkpoint work matches independent IR"
            (integer_program_report_output_work independent)
            (Native.output_work report);
          Alcotest.(check int)
            "checkpoint quotas never fall back to IR" 0
            (Option.get (Native.source_progress report)).runtime.executed_steps)
        [
          (13, work, None);
          (12, work, Some "HCIRVM0022");
          (13, work - 1, Some "HCIRVM0023");
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let native_callback_resource_collection () =
  let module Runtime = Native_program_execution in
  let run failure fixture =
    let cookie = ref 0 in
    let weak = Weak.create 1 in
    Weak.set weak 0 (Some cookie);
    native_source_callback_scope ~failure ?fixture
      ~on_callback:(fun () -> incr cookie)
      ();
    weak
  in
  let callbacks =
    List.concat_map
      (fun failure ->
        [
          run failure None;
          run failure
            (Some
               {|#exe {I64 Emit(I64 (*p)(U8 *fmt,...)){Print("before;");StreamPrint("40;");Print("%d;%d;after;",p("payload%d",42),p("payload%d",42));StreamPrint("42;");return 42;}Emit(&StreamExePrint);}42;|});
        ])
      [ `None; `Reject; `Raise ]
  in
  (* Dead native captures can still root their original task and dispatch.
     Collect them, drain their C resources on another mutator domain, and
     collect the now-unrooted closures. The observer holds only weak cookies. *)
  for _ = 1 to 4 do
    Gc.full_major ();
    Domain.join (Domain.spawn (fun () -> Runtime.platform ())) |> ignore
  done;
  Gc.compact ();
  List.iteri
    (fun index weak ->
      Alcotest.(check bool)
        (Printf.sprintf "discarded callback %d is collectible" index)
        false (Weak.check weak 0))
    callbacks

let () =
  Alcotest.run "Native original stream generation"
    [
      ( "source",
        [
          Alcotest.test_case "values and retained owners" `Quick values;
          Alcotest.test_case "current and child native compiler option controls"
            `Quick compiler_options;
          Alcotest.test_case
            "native compiler option request identity and lifetime" `Quick
            native_compiler_option_authority;
          Alcotest.test_case "reached failures" `Quick failures;
          Alcotest.test_case "shared work and cumulative bytes" `Quick quotas;
          Alcotest.test_case "synchronous original native child execution"
            `Quick synchronous_boundary;
          Alcotest.test_case
            "child defaults, statics, catalogs and nested streams" `Quick
            executable_children;
          Alcotest.test_case "actual child usage joins cumulative quotas" `Quick
            child_quotas;
          Alcotest.test_case
            "suspended original code and storage survive children" `Quick
            suspended_code_and_storage;
          Alcotest.test_case "children inherit physical frame, depth and stack"
            `Quick physical_child_quotas;
          Alcotest.test_case "actual children survive collection and compaction"
            `Quick executable_child_collection;
          Alcotest.test_case "synchronous child declarations publish" `Quick
            synchronous_declarations;
          Alcotest.test_case "synchronous child reached failure order" `Quick
            synchronous_failure_order;
          Alcotest.test_case "executed capture identity and lifetime" `Quick
            capture_authority;
          Alcotest.test_case "native source callback scope and collection"
            `Quick native_source_callbacks;
          Alcotest.test_case "source checkpoints preserve quota faults" `Quick
            checkpoint_quotas;
          Alcotest.test_case "discarded native callback closures collect" `Quick
            native_callback_resource_collection;
        ] );
    ]
