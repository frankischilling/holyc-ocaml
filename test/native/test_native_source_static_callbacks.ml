open Holyc_lib
module Native = Native_source_execution
module Image = X86_64_program

let describe errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let inputs text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-source-static-callbacks.hc"
      ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit () |> checked
  in
  (session, config, source)

let run ?(max_steps = 100_000) ?max_global_bytes ?max_ir_instructions
    ?max_code_bytes ?max_initializer_steps ?max_default_bytes text =
  let session, config, source = inputs text in
  Native.evaluate ?max_global_bytes ?max_ir_instructions ?max_code_bytes
    ?max_initializer_steps ?max_default_bytes session ~config ~source ~max_steps

let value expected report =
  let result = Native.outcome report |> Result.map_error describe |> checked in
  Alcotest.(check int64)
    "actual native value" expected (Option.get result.value.final_value).bits;
  Alcotest.(check int)
    "no interpreter execution" 0
    (Option.get (Native.source_progress report)).runtime.executed_steps;
  List.iter
    (fun (fragment : Native.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (Image.Completed _)) -> ()
      | _ -> Alcotest.fail "original fragment did not execute natively")
    (Native.fragments report)

let diagnostic code report =
  match Native.outcome report with
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (error : Diagnostic.t) -> error.code = code) errors)
  | Ok _ -> Alcotest.fail ("expected " ^ code)

let fault kind report =
  match List.rev (Native.fragments report) with
  | { native_outcome = Some (Ok (Image.Fault reached)); _ } :: _ ->
      Alcotest.(check bool)
        "reached original native fault" true (reached.kind = kind);
      Alcotest.(check int)
        "cumulative fault work" reached.executed_steps
        (Native.executed_steps report)
  | _ -> Alcotest.fail "failure did not reach original native code"

let agrees expected text =
  let native = run text in
  value expected native;
  let session, config, source = inputs text in
  let interpreted =
    run_integer_program_report session ~config ~source ~max_steps:100_000
  in
  let result =
    integer_program_report_outcome interpreted
    |> Result.map_error describe |> checked
  in
  Alcotest.(check int64)
    "independent original IR value" expected
    (Option.get (Ir_integer_interpreter.final_value result.value)).bits;
  Alcotest.(check string)
    "independent original effects"
    (integer_program_report_output_bytes interpreted)
    (Native.output_bytes native)

let cells_arrays_and_returns () =
  List.iter (agrees 42L)
    [
      "I64 F(){return 42;}I64 Run(){static I64 (*p)();p=&F;return p();}Run();";
      "I64 F(){return 42;}I64 Run(){static I64 \
       (*p)()[2];p[0]=&F;p[1]=p[0];p[0]=0;return p[1]();}Run();";
      "I64 F(){return 42;}I64 Run(){static I64 \
       (*p)()[2][2];p[0][1]=&F;p[1][1]=p[0][1];p[0][1]=7;return \
       (*p[1][1])();}Run();";
      "I64 F(){return 42;}I64 Call(I64 (*q)()){return q();}I64 Run(){static \
       I64 (*p)();p=&F;return Call(p);}Run();";
      "I64 Run(){static I64 (*p)();p=-14;return p+=7;}Run();";
      "I64 F(I64 n){return n;}I64 Run(){static I64 (*p)(I64 n);p=&F;return \
       p(p=42);}Run();";
      "I64 F(I64 n,...){return n+argc;}I64 Run(){static I64 (*p)(I64 \
       n=40,...);p=&F;return p(,1,2);}Run();";
      "U0 F(){}I64 Run(){static U0 (*p)();p=&F;p();return 42;}Run();";
      "U8 F(){return 42;}I64 Run(){static U8 (*p)();p=&F;return p();}Run();";
    ];
  agrees 0x800000000000002aL
    "I64 Run(){static I64 (*p)();p=0x800000000000002a;return p;}Run();";
  List.iter
    (fun return_type ->
      agrees 42L
        ("I64 Run(){static " ^ return_type
       ^ " (*p)()[2];p[1]=42;return p[1];}Run();"))
    [ "I8"; "U8"; "U0"; "F64"; "I64 *" ]

let header_effects_and_history () =
  List.iter (agrees 42L)
    [
      "I64 Seed(){return 40;}I64 F(I64 n){return n+2;}I64 Run(){static I64 \
       (*p)(I64 n=Seed());p=&F;return p();}Run();Run();";
      "I64 Counter=41;I64 Seed(){return ++Counter;}I64 Unused(){static I64 \
       (*p)(I64 n=Seed());return 0;}Counter;";
      "I64 F(){return 42;}I64 Call(I64 (*q)()){return q();}I64 Run(){static \
       I64 (*p)(I64 (*q)()=&F);p=&Call;return p();}I64 F(){return 17;}Run();";
      "I64 F(){return 42;}I64 Run(I64 n){static I64 (*p)();if(n)p=&F;return \
       p();}Run(1);I64 G(){return 17;}Run(0);";
      "I64 F(){return 42;}I64 Run(I64 n){static I64 (*p)();if(n)p=&F;return \
       p();}I64 Old(){return Run(0);}Run(1);I64 Run(I64 n){static I64 \
       (*p)();p=17;return p;}Run(0);Old();";
      "I64 Step(I64 n){static I64 (*p)(I64 n);p=&Step;if(n)return \
       p(n-1)+1;return 40;}Step(2);";
    ];
  let text =
    "extern U0 PutChars(U64 ch);I64 Seed(){PutChars('D');return 40;}I64 F(I64 \
     n){return n+2;}I64 Run(){static I64 (*p)(I64 n=Seed());p=&F;return \
     p();}Run();I64 X=99;Run();"
  in
  let report = run text in
  value 42L report;
  Alcotest.(check string)
    "original header runs once before any activation" "D"
    (Native.output_bytes report);
  Alcotest.(check int) "one saved header word" 8 (Native.default_bytes report);
  agrees 42L text;
  Gc.full_major ();
  Gc.compact ();
  agrees 42L text

let reached_faults () =
  List.iter
    (fun (kind, text) -> fault kind (run text))
    [
      ( Image.Uninitialized_read,
        "I64 Run(){static I64 (*p)();return p();}Run();" );
      ( Image.Uninitialized_read,
        "I64 F(){return 42;}I64 Run(){static I64 (*p)()[2];p[0]=&F;return \
         p[1]();}Run();" );
      ( Image.Callback_unowned_address,
        "I64 Run(){static I64 (*p)();p=0;return p();}Run();" );
      ( Image.Callback_unowned_address,
        "I64 Run(){static I64 (*p)();p=42;return p();}Run();" );
      ( Image.Callback_signature_mismatch,
        "I64 F(I64 n){return n;}I64 Run(){static I64 (*p)();p=&F;return \
         p();}Run();" );
      ( Image.Callback_owned_word_escape,
        "I64 F(){return 42;}I64 Run(){static I64 (*p)();p=&F;return p;}Run();"
      );
      ( Image.Address_out_of_bounds,
        "I64 Run(){static I64 (*p)()[2];p[0]=42;return p[2];}Run();" );
    ];
  let report =
    run
      "extern U0 PutChars(U64 ch);I64 Mark(I64 c){PutChars(c);return c;}I64 \
       Run(){static I64 (*p)(I64 a,I64 b);p=0;return \
       p(Mark('A'),Mark('B'));}Run();"
  in
  fault Image.Callback_unowned_address report;
  Alcotest.(check string)
    "reverse arguments finish before reached target fault" "BA"
    (Native.output_bytes report)

let exact_limits () =
  let text =
    "I64 Seed(){return 40;}I64 F(I64 n){return n+2;}I64 Run(){static I64 \
     (*p)(I64 n=Seed())[2];p[1]=&F;return p[1]();}Run();Run();"
  in
  let report = run text in
  value 42L report;
  let code, ir =
    List.fold_left
      (fun (code, ir) (fragment : Native.fragment) ->
        (code + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0) (Native.fragments report)
  in
  value 42L
    (run ~max_global_bytes:16 ~max_default_bytes:8 ~max_code_bytes:code
       ~max_ir_instructions:ir
       ~max_steps:(Native.executed_steps report)
       ~max_initializer_steps:(Native.preparation_steps report)
       text);
  diagnostic "HCIRVM0016" (run ~max_global_bytes:15 text);
  diagnostic "HCIRVM0011" (run ~max_default_bytes:7 text);
  diagnostic "HCBACK0005" (run ~max_code_bytes:(code - 1) text);
  diagnostic "HCBACK0001" (run ~max_ir_instructions:(ir - 1) text);
  fault Image.Step_limit_exceeded
    (run ~max_steps:(Native.executed_steps report - 1) text);
  diagnostic "HCIRVM0007"
    (run ~max_initializer_steps:(Native.preparation_steps report - 1) text)

let arena_and_original_allocation () =
  let module Task = Holyc_lib__Driver.Integer_task in
  let module Dispatch = Task.Native_dispatch in
  let module Request = Task.Native_static_allocation in
  let module Allocation = Holyc_lib__Ir.Integer_static_allocation in
  let module Storage = Holyc_lib__Backend.X86_64_global_storage in
  let module Runtime = Native_program_execution in
  let compile = function
    | Ok value -> value
    | Error errors ->
        Alcotest.fail
          (String.concat "; "
             (List.map (fun (e : Image.error) -> e.message) errors))
  in
  List.iter
    (fun (declarator, bytes, arena_bytes) ->
      List.iter
        (fun capacity ->
          let session = Session.create () in
          let layout =
            Image.create_task_layout ~max_global_bytes:bytes |> compile
          in
          let arena =
            Runtime.create_task_arena ~max_arena_bytes:capacity layout
            |> checked
          in
          let budget = Runtime.create_budget ~max_steps:100_000 () |> checked in
          let allocations = ref 0 and saved = ref None in
          let dispatch : Dispatch.t =
            {
              execute_initializer =
                (fun _ -> Alcotest.fail "unexpected global initializer");
              execute_command =
                (fun request ->
                  List.iter
                    (fun status_abi ->
                      Image.compile_task_command ~status_abi ~layout request
                      |> compile |> ignore)
                    [ Image.Windows_x64; Image.System_v_x64 ];
                  let image =
                    Image.compile_task_command ~layout request |> compile
                  in
                  let retained =
                    Runtime.retain_task_fragment arena image |> checked
                  in
                  Fun.protect
                    ~finally:(fun () -> Runtime.release retained |> checked)
                    (fun () ->
                      let report =
                        Runtime.execute_retained_budget_report budget retained
                      in
                      match Runtime.outcome report |> checked with
                      | Image.Fault _ ->
                          Alcotest.fail "unexpected original task fault"
                      | Image.Completed result ->
                          Ok
                            (if Runtime.value_captured report then
                               Dispatch.Captured
                                 (Option.map
                                    (fun (word : Image.word) ->
                                      Dispatch.I64 word.bits)
                                    result.final_value)
                             else Dispatch.Unchanged)));
            }
          in
          let allocate request =
            incr allocations;
            saved := Some request;
            let allocation = Request.allocation request in
            let pointer = Option.get (Allocation.callback_pointer allocation) in
            Alcotest.(check bool)
              "pending storage keeps its original callback signature" true
              (Option.is_some
                 (Holyc_lib__Sema.Function_type_resolution
                  .function_pointer_source pointer));
            Alcotest.(check int)
              "physical RT_PTR depth" 1
              (Holyc_lib__Sema.Type.pointer_depth (Allocation.type_ allocation));
            let reservation =
              Storage.reserve_static layout request
              |> Result.map_error (fun errors ->
                  String.concat "; "
                    (List.map (fun (e : Storage.error) -> e.message) errors))
              |> checked
            in
            Alcotest.(check int)
              "data, flags and owner lanes are separately reserved" arena_bytes
              (Storage.static_reservation_arena_bytes reservation);
            Runtime.allocate_task_static arena request
            |> Result.map_error (fun message ->
                [
                  Diagnostic.make ~code:"HCBACK0001" ~severity:Diagnostic.Error
                    ~message
                    ~primary:
                      ( Allocation.source allocation
                      |> Holyc_lib__Sema.Compiler_record
                         .static_allocation_receipt
                      |> fun receipt ->
                        receipt.allocation_function.function_name.location.span
                      )
                    ();
                ])
          in
          Fun.protect
            ~finally:(fun () -> Runtime.release_task_arena arena |> checked)
            (fun () ->
              let task =
                Task.create ~native_dispatch:dispatch
                  ~native_static_allocation:allocate session
                |> checked
              in
              let source =
                Session.add_source session ~path:"static-arena.hc"
                  ~contents:
                    ("I64 Run(){static F64 (*p)()" ^ declarator ^ ";"
                    ^ (if declarator = "" then "p=42;return p;"
                       else "p[1]=42;return p[1];")
                    ^ "}Run();")
              in
              let result = Task.run task ~source in
              if capacity = arena_bytes then (
                result |> Result.map_error describe |> checked |> ignore;
                Alcotest.(check bool)
                  "actual native numeric static result" true
                  (Task.native_final_value task = Some (Dispatch.I64 42L));
                Alcotest.(check int)
                  "no VM source instructions" 0
                  (Task.progress task).runtime.executed_steps)
              else
                Alcotest.(check bool)
                  "one fewer owner arena byte rejects before entry" true
                  (Result.is_error result);
              Alcotest.(check int) "one original allocation" 1 !allocations;
              Alcotest.(check bool)
                "expired request cannot be replayed" true
                (Result.is_error
                   (Runtime.allocate_task_static arena (Option.get !saved)))))
        [ arena_bytes; arena_bytes - 1 ])
    [ ("", 8, 17); ("[2]", 16, 48) ]

let () =
  Alcotest.run "Live native static callback allocations"
    [
      ( "original persistent cells",
        [
          Alcotest.test_case
            "cells, fixed arrays and independent return metadata" `Quick
            cells_arrays_and_returns;
          Alcotest.test_case "original header effects, owners and history"
            `Quick header_effects_and_history;
          Alcotest.test_case "reached initialization, bounds and target faults"
            `Quick reached_faults;
          Alcotest.test_case "exact and one-below cumulative limits" `Quick
            exact_limits;
          Alcotest.test_case
            "original allocation, both ABIs and exact owner arena" `Quick
            arena_and_original_allocation;
        ] );
    ]
