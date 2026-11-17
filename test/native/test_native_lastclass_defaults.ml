open Holyc_lib
open Yojson.Safe.Util
module Native = Native_source_execution

let messages errors =
  errors
  |> List.map (fun (e : Diagnostic.t) -> e.code ^ ": " ^ e.message)
  |> String.concat "; "

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let run ?(mode = Preprocessor.Jit) ?max_literal_bytes ?max_default_bytes
    ?(max_steps = 100_000) text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"lastclass-defaults.hc" ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  Native.evaluate ?max_literal_bytes ?max_default_bytes session ~config ~source
    ~max_steps

let value expected report =
  let checked = Native.outcome report |> Result.map_error messages |> checked in
  Alcotest.(check (option int64))
    "native lastclass result" (Some expected)
    (Option.map (fun word -> word.Native.bits) checked.value.final_value);
  match Native.source_progress report with
  | None -> ()
  | Some progress ->
      Alcotest.(check int)
        "no interpreted source execution" 0 progress.runtime.executed_steps

let rejected report =
  match Native.outcome report with
  | Error errors when errors <> [] -> errors
  | _ -> Alcotest.fail "lastclass source unexpectedly completed"

let diagnostic code report =
  let errors = rejected report in
  match
    List.find_opt (fun (error : Diagnostic.t) -> error.code = code) errors
  with
  | Some error -> error
  | None -> Alcotest.fail (messages errors)

let both action = List.iter action [ Preprocessor.Jit; Preprocessor.Aot ]

let class_names () =
  both (fun mode ->
      List.iter
        (fun (text, expected) -> value expected (run ~mode text))
        [
          ( "I64 Name(I64 x,U8 *n=lastclass){return n[0]*100+n[3];}Name(7,);",
            7405L );
          ( "I64 A[1];I64 Name(I64 *x,U8 *n=lastclass){return \
             n[0]*100+n[3];}Name(A);",
            7300L );
          ( "I64 Name(I64 *x,U8 *n=lastclass){return n[0]*100+n[3];}I64 \
             Caller(){I64 a[1];return Name(a);}Caller();",
            7300L );
          ("I64 Name(U8 x,U8 *n=lastclass){return n[2];}Name(7(U8));", 0L);
          ("I64 Name(U8i x,U8 *n=lastclass){return n[2];}Name(7(U8i));", 105L);
          ( "class Student {I64 value;};Student A;I64 Name(Student *x,U8 \
             *n=lastclass){return n[0]*100+n[6];}Name(&A);",
            8416L );
          ( "I64i union Backed {};I64 Name(Backed x,U8 *n=lastclass){return \
             n[3];}Name(7(Backed));",
            105L );
          ( "I64 Name(I64 x,U8 *a=lastclass,I64 ordinary=11,U8 \
             *b=lastclass){return a[3]+b[3]+ordinary;}Name(7,,,);",
            221L );
          ( "I64 Name(I64 x,U8 *a=lastclass,U8 *b=lastclass){return \
             b[0]*100+b[2];}Name(7,\"override\",);",
            8500L );
        ];
      let floating =
        run ~mode
          "I64 Name(F64 x,U8 *n=lastclass){return n[0]*100+n[3];}Name(1.5+0.0);"
      in
      if Native_execution.x87_supported () then (
        value 7000L floating;
        value 105L
          (run ~mode
             "I64 Name(F64i x,U8 *n=lastclass){return n[3];}Name(1.5(F64i));");
        List.iter
          (fun text -> value 0x3ff8_0000_0000_0000L (run ~mode text))
          [
            "F64i Keep(F64i x){return x;}Keep(1.5(F64i));";
            "F64i Keep(F64 x){return x(F64i);}Keep(1.5);";
            "F64 Source(){return 1.5;}Source()(F64i);";
          ])
      else
        let error = diagnostic "HCNATIVE0002" floating in
        Alcotest.(check string)
          "floating predecessor keeps the native capability diagnostic"
          Native_execution.x87_unavailable_message error.message)

let zero_work_headers () =
  both (fun mode ->
      List.iter
        (fun text ->
          let report = run ~mode ~max_default_bytes:1 text in
          value 42L report;
          Alcotest.(check int)
            "lastclass has no declaration expression" 0
            (Native.preparation_steps report);
          Alcotest.(check int)
            "lastclass has no saved payload copy" 0
            (Native.default_bytes report))
        [
          "I64 Unused(U8 *n=lastclass){return 0;}42;";
          "I64 (*Unused)(I64 n=lastclass);42;";
          "I64 (*Unused)(F64 n=lastclass);42;";
        ]);
  let zero = run "I64 Zero(I64 n=lastclass){return n;}Zero;" in
  value 0L zero;
  Alcotest.(check int)
    "missing JIT predecessor leaves zero record word" 0
    (Native.default_bytes zero);
  let floating_zero = run "F64 Missing(F64 n=lastclass){return n;}Missing;" in
  value 0L floating_zero;
  let floating_value =
    Native.outcome floating_zero |> Result.get_ok |> fun checked ->
    Option.get checked.value.final_value
  in
  Alcotest.(check bool)
    "zero record bits retain their selected F64 class" true
    (floating_value.type_ = X86_64_program.F64);
  Alcotest.(check int)
    "F64 lastclass zero has no declaration execution" 0
    (Native.preparation_steps floating_zero);
  Alcotest.(check int)
    "F64 lastclass zero has no saved expression payload" 0
    (Native.default_bytes floating_zero);
  let floating_use =
    run "F64 Missing(F64 n=lastclass){return n+1.0;}Missing;"
  in
  (if Native_execution.x87_supported () then
     value 0x3ff0000000000000L floating_use
   else
     let error = diagnostic "HCNATIVE0002" floating_use in
     Alcotest.(check string)
       "F64 zero arithmetic retains the capability check"
       Native_execution.x87_unavailable_message error.message);
  List.iter
    (fun text ->
      let report = run ~max_default_bytes:1 ~max_literal_bytes:1 text in
      value 42L report;
      Alcotest.(check int)
        "null marker has no preparation" 0
        (Native.preparation_steps report);
      Alcotest.(check int)
        "null marker has no saved payload" 0
        (Native.default_bytes report);
      List.iter
        (fun (fragment : Native.fragment) ->
          Alcotest.(check int)
            "null marker owns no literal bytes" 0 fragment.image.literal_bytes)
        (Native.fragments report))
    [
      "I64 Missing(U8 *n=lastclass){return 42;}Missing;";
      "U8 *Missing(U8 *n=lastclass){return n;}I64 Use(U8 *n){return \
       42;}Use(Missing());";
      "I64 Missing(I64 (*p)(I64 n)=lastclass){return 42;}Missing();";
      "I64 Missing(F64 (*p)()=lastclass){return 42;}Missing();";
    ];
  let null_callback =
    run
      "extern U0 PutChars(U64 c);I64 Mark(){PutChars('A');return 7;}I64 \
       Missing(I64 (*p)(I64 n)=lastclass){return p(Mark());}Missing();"
  in
  let callback_error = diagnostic "HCIRVM0024" null_callback in
  Alcotest.(check string)
    "zero callback acquires no executable owner"
    "the reached callback has no owned executable address"
    callback_error.message;
  Alcotest.(check string)
    "null callback retains reached argument effects" "A"
    (Native.output_bytes null_callback);
  Alcotest.(check int)
    "null callback performs no default preparation" 0
    (Native.preparation_steps null_callback);
  Alcotest.(check int)
    "null callback saves no expression payload" 0
    (Native.default_bytes null_callback);
  ignore
    (diagnostic "HCIRVM0024"
       (run "F64 Missing(F64 (*p)()=lastclass){return p();}Missing();"));
  let null_read = "I64 Missing(U8 *n=lastclass){return n[0];}Missing();" in
  let fault = run null_read in
  let error = diagnostic "HCIRVM0019" fault in
  Alcotest.(check string)
    "null dereference keeps the existing bounds diagnostic"
    "indexed address is outside its declared object extent" error.message;
  let reached = Native.executed_steps fault in
  Alcotest.(check bool)
    "null dereference consumes its source work" true (reached > 0);
  let exact = run ~max_steps:reached null_read in
  ignore (diagnostic "HCIRVM0019" exact);
  Alcotest.(check int)
    "exact allowance reaches the same bounds fault" reached
    (Native.executed_steps exact);
  let limited = run ~max_steps:(reached - 1) null_read in
  ignore (diagnostic "HCIRVM0007" limited);
  Alcotest.(check int)
    "one instruction below stops before the null read" (reached - 1)
    (Native.executed_steps limited);
  List.iter
    (fun report ->
      Alcotest.(check int)
        "null failure adds no preparation work" 0
        (Native.preparation_steps report);
      Alcotest.(check int)
        "null failure saves no default payload" 0
        (Native.default_bytes report))
    [ fault; exact; limited ];
  ignore
    (diagnostic "HCIRL0004"
       (run ~mode:Preprocessor.Aot
          "I64 Missing(U8 *n=lastclass){return 42;}Missing();"))

let callback_headers () =
  both (fun mode ->
      List.iter
        (fun text -> value 105L (run ~mode text))
        [
          "I64 Target(I64 x,U8 *name){return name[3];}I64 (*Selected)(I64 x,U8 \
           *name=lastclass)=&Target;Selected(7,);";
          "I64 Target(I64 x,U8 *name){return name[3];}I64 Caller(I64 (*p)(I64 \
           x,U8 *name=lastclass)){return p(7);}Caller(&Target);";
        ])

let storage_and_limits () =
  let source =
    "I64 Same(I64 x,U8 *a=lastclass,U8 *b=lastclass){return a==b;}Same(7);"
  in
  both (fun mode ->
      let expected, bytes =
        if mode = Preprocessor.Jit then (1L, 5) else (0L, 10)
      in
      value expected
        (run ~mode ~max_literal_bytes:bytes ~max_default_bytes:1 source);
      ignore (rejected (run ~mode ~max_literal_bytes:(bytes - 1) source)));
  let mutable_name =
    "I64 Change(I64 x,U8 *n=lastclass){n[0]='Z';return 0;}I64 Inspect(I64 x,U8 \
     *n=lastclass){return n[0];}Change(1);Inspect(2);"
  in
  value 90L (run mutable_name);
  value 73L (run ~mode:Preprocessor.Aot mutable_name);
  both (fun mode ->
      value 85L
        (run ~mode
           "I64 Change(U8 x,U8 *n=lastclass){n[0]='Z';return 0;}I64 \
            Inspect(U8i x,U8 *n=lastclass){return \
            n[0];}Change(1(U8));Inspect(2(U8i));"))

let effects_and_failure () =
  both (fun mode ->
      let report =
        run ~mode
          "extern U0 PutChars(U64 c);I64 Mark(){PutChars('A');return 7;}I64 \
           Name(I64 x,U8 *n=lastclass){PutChars(n[0]);I64 z=0;return \
           7/z;}Name(Mark());"
      in
      let errors = rejected report in
      Alcotest.(check bool)
        (messages errors) true
        (List.exists (fun (e : Diagnostic.t) -> e.code = "HCIRVM0009") errors);
      Alcotest.(check string)
        "provided expression executes once before callee failure" "AI"
        (Native.output_bytes report);
      Alcotest.(check int)
        "lastclass adds no preparation work on failure" 0
        (Native.preparation_steps report);
      Alcotest.(check int)
        "lastclass adds no saved default bytes" 0
        (Native.default_bytes report))

let implicit_and_historical () =
  both (fun mode ->
      let report =
        run ~mode
          "I64 Seen;U0 Print(U8 *text,U8 \
           *name=lastclass){Seen=name[0]*100+name[2];}\"x\",;Seen;"
      in
      value 8500L report);
  value 105073L
    (run
       "I64 Name(I64 x,U8 *n=lastclass){return n[3];}I64 Early(){return \
        Name(1);}I64 Name(I64 x,U8 *n=lastclass){return \
        n[0];}Early()*1000+Name(2);")

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let cli_example compiler example () =
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          let stdout = Filename.temp_file "holyc-lastclass-" ".out" in
          let stderr = Filename.temp_file "holyc-lastclass-" ".err" in
          Fun.protect
            ~finally:(fun () ->
              Sys.remove stdout;
              Sys.remove stderr)
            (fun () ->
              let out_fd =
                Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let err_fd =
                Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let pid =
                Fun.protect
                  ~finally:(fun () ->
                    Unix.close out_fd;
                    Unix.close err_fd)
                  (fun () ->
                    Unix.create_process compiler
                      [|
                        compiler;
                        "run";
                        "--target=" ^ target;
                        "--mode=" ^ mode;
                        "--format=json";
                        example;
                      |]
                      Unix.stdin out_fd err_fd)
              in
              let _, status = Unix.waitpid [] pid in
              let output = read_file stdout and errors = read_file stderr in
              Alcotest.(check bool)
                (target ^ "/" ^ mode ^ ": " ^ errors ^ output)
                true (status = Unix.WEXITED 0);
              Alcotest.(check string) "CLI has no stderr" "" errors;
              let report = Yojson.Safe.from_string output in
              Alcotest.(check string)
                "maintained lastclass example" "0x00000000000000dd"
                (report |> member "final_value" |> member "bits" |> to_string);
              Alcotest.(check int)
                "only ordinary default stores a payload" 8
                (report |> member "prepared_default_bytes" |> to_int)))
        [ "jit"; "aot" ])
    [ "host-jit-task"; "host-jit" ]

let () =
  if Array.length Sys.argv <> 3 then
    failwith "expected compiler and lastclass example paths";
  if Native_execution.platform () <> Native_execution.Unsupported then
    Alcotest.run
      ~argv:[| Sys.argv.(0) |]
      "Native lastclass defaults"
      [
        ( "source",
          [
            Alcotest.test_case "provided class and skipped slots" `Quick
              class_names;
            Alcotest.test_case "zero-work declarations and absent predecessor"
              `Quick zero_work_headers;
            Alcotest.test_case "anonymous and parameter callback headers" `Quick
              callback_headers;
            Alcotest.test_case "JIT shared names, AOT copies and literal quotas"
              `Quick storage_and_limits;
            Alcotest.test_case "effects and original failure" `Quick
              effects_and_failure;
            Alcotest.test_case "implicit and historical calls" `Quick
              implicit_and_historical;
            Alcotest.test_case "maintained example through public CLI" `Quick
              (cli_example Sys.argv.(1) Sys.argv.(2));
          ] );
      ]
