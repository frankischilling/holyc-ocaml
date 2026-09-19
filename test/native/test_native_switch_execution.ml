open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution
module VM = Ir_integer_interpreter

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let vm_errors_text errors =
  errors
  |> List.map (fun (error : VM.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let inputs ~mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-switch-execution.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let public ?max_switch_work ?(max_steps = 100_000) ~mode contents =
  let session, config, source = inputs ~mode contents in
  run_integer_program ?max_switch_work session ~config ~source ~max_steps

let native ?max_switch_work ?(max_steps = 100_000) ~mode contents =
  let session, config, source = inputs ~mode contents in
  Native_program.evaluate ?max_switch_work session ~config ~source ~max_steps

let first_native_error report =
  match Native_program.outcome report with
  | Ok _ -> Alcotest.fail "native switch source unexpectedly succeeded"
  | Error [] -> Alcotest.fail "native switch source returned no diagnostic"
  | Error (first :: _) -> first

let check_vm_word label expected result =
  match VM.final_value result with
  | Some word ->
      Alcotest.(check bool) (label ^ " VM type") true (word.type_ = VM.I64);
      Alcotest.(check int64) (label ^ " VM bits") expected word.bits
  | None -> Alcotest.failf "%s VM result has no word" label

let check_native_word label expected = function
  | Some (word : Program.word) ->
      Alcotest.(check bool)
        (label ^ " native type") true (word.type_ = Program.I64);
      Alcotest.(check int64) (label ^ " native bits") expected word.bits
  | None -> Alcotest.failf "%s native result has no word" label

let batch ~mode contents =
  Native_scalar_fixture.compile ~mode ~path:"native-switch-batch.hc" ~contents
    ()
  |> require_ok diagnostics_text

let compare ~mode ~label ~expected contents =
  let public_result =
    public ~mode contents |> require_ok diagnostics_text |> fun checked ->
    checked.value
  in
  check_vm_word (label ^ " public") expected public_result;
  let fixture = batch ~mode contents in
  let checked =
    Native_scalar_fixture.execute ~max_steps:100_000 fixture
    |> require_ok vm_errors_text
  in
  check_vm_word (label ^ " checked batch") expected checked;
  let report = native ~mode contents in
  let result =
    match Native_program.outcome report with
    | Ok checked -> checked.value
    | Error diagnostics -> Alcotest.fail (diagnostics_text diagnostics)
  in
  check_native_word label expected result.execution.final_value;
  Alcotest.(check int)
    (label ^ " checked/native steps")
    (VM.executed_steps checked)
    result.execution.executed_steps;
  report

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let successful_differentials () =
  let cases =
    [
      ( "dft function label",
        "I64 F(){goto dft;switch(0){case 0:return 0;dft:return 42;}return \
         -1;}F();" );
      ( "closed floating case",
        "I64 F(I64 n){switch(n){case 2.75:return 42;default:return 0;}return \
         -1;}F(2);" );
      ( "signed narrow",
        "I64 F(I8 n){switch(n){case -1:return 42;default:return 0;}return \
         -1;}F(255);" );
      ( "unsigned narrow",
        "I64 F(U8 n){switch(n){case 255:return 42;default:return 0;}return \
         -1;}F(-1);" );
      ( "simple",
        "I64 F(I64 n){switch(n){case 1:return 42;default:return 0;}return \
         -1;}F(1);" );
      ( "hole/default",
        "I64 F(I64 n){switch(n){case 1:return 7;case 3:return 9;default:return \
         42;}return -1;}F(2);" );
      ( "negative",
        "I64 F(I64 n){switch(n){case -2:return 42;default:return 0;}return \
         -1;}F(-2);" );
      ( "high bit",
        "I64 F(U64 n){switch(n){case 0x8000000000000000:return \
         42;default:return 0;}return -1;}F(0x8000000000000000);" );
      ( "selector once",
        "I64 F(){I64 n=0;switch(++n){case 1:n+=41;break;default:return \
         0;}return n;}F();" );
      ( "implicit starts zero",
        "I64 F(I64 n){switch(n){case:return 42;default:return 0;}return \
         -1;}F(0);" );
      ( "implicit",
        "I64 F(I64 n){switch(n){case 4:return 7;case:return 42;default:return \
         0;}return -1;}F(5);" );
      ( "reversed range",
        "I64 F(I64 n){switch(n){case 5...3:return 42;default:return 0;}return \
         -1;}F(4);" );
      ( "fallthrough default middle",
        "I64 F(I64 n){I64 v=0;switch(n){case 1:return 7;default:v=40;case \
         3:v+=2;break;}return v;}F(2);" );
      ( "nested break",
        "I64 F(I64 n){while(1){switch(n){case \
         1:n=42;break;default:n=0;break;}break;}return n;}F(1);" );
      ( "nested switch breaks",
        "I64 F(){I64 n=0;switch(0){case 0:switch(1){case \
         1:n=40;break;default:return 0;}n+=2;break;default:return 0;}return \
         n;}F();" );
      ( "switch goto outside",
        "I64 F(I64 n){switch(n){case 1:goto done;default:return 0;}done:return \
         42;}F(1);" );
      ( "goto enters switch",
        "I64 F(){goto inside;switch(0){case 0:inside:return 42;default:return \
         0;}return -1;}F();" );
      ( "unselected arithmetic fault",
        "I64 F(I64 n){switch(n){case 1:return 42;case 2:return \
         1/0;default:return 0;}return -1;}F(1);" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) ->
          ignore (compare ~mode ~label ~expected:42L source))
        cases)
    modes

let scalar_selector_widths () =
  List.iter
    (fun mode ->
      List.iter
        (fun type_name ->
          let source =
            Printf.sprintf
              "I64 F(%s n){switch(n){case 1:return 42;default:return 0;}return \
               -1;}F(1);"
              type_name
          in
          ignore
            (compare ~mode ~label:(type_name ^ " selector") ~expected:42L source))
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ])
    modes

let switch_work_limit_and_fresh_runs () =
  let source =
    "I64 F(I64 n){switch(n){case 2+3:return 42;default:return 0;}return \
     -1;}F(5);"
  in
  List.iter
    (fun mode ->
      let exact = native ~mode ~max_switch_work:3 source in
      (match Native_program.outcome exact with
      | Ok checked ->
          check_native_word "exact switch work" 42L
            checked.value.execution.final_value
      | Error ds -> Alcotest.fail (diagnostics_text ds));
      Alcotest.(check int)
        "native exact switch work" 3
        (Native_program.switch_work exact);
      let one_below = native ~mode ~max_switch_work:2 source in
      Alcotest.(check string)
        "native switch work one-below" "HCSW0003"
        (first_native_error one_below).code;
      Alcotest.(check int)
        "native switch work progress" 2
        (Native_program.switch_work one_below);
      Alcotest.(check bool)
        "preparation failure has no image" true
        (Option.is_none (Native_program.image one_below));
      let fresh = native ~mode source in
      (match Native_program.outcome fresh with
      | Ok checked ->
          check_native_word "fresh switch" 42L
            checked.value.execution.final_value
      | Error ds -> Alcotest.fail (diagnostics_text ds));
      Alcotest.(check int)
        "fresh switch preparation repeats once" 3
        (Native_program.switch_work fresh);
      List.iter
        (fun limit ->
          let invalid = native ~mode ~max_switch_work:limit "@invalid" in
          Alcotest.(check string)
            "invalid native switch limit" "HCIRVM0001"
            (first_native_error invalid).code;
          Alcotest.(check int)
            "invalid native switch work" 0
            (Native_program.switch_work invalid);
          Alcotest.(check bool)
            "invalid native limit has no image" true
            (Option.is_none (Native_program.image invalid)))
        [ 0; -1 ])
    modes

let selected_runtime_fault_retains_native_image () =
  let source =
    "I64 Pick(I64 n){switch(n){case 1:return 84/0;default:return 0;}return \
     -1;}Pick(1);"
  in
  List.iter
    (fun mode ->
      let fixture = batch ~mode source in
      let vm_fault =
        match Native_scalar_fixture.execute ~max_steps:100_000 fixture with
        | Error (first :: _) -> first
        | Error [] -> Alcotest.fail "checked selected switch fault has no error"
        | Ok _ -> Alcotest.fail "checked selected switch fault completed"
      in
      Alcotest.(check string)
        "checked selected fault code" "HCIRVM0009" vm_fault.code;
      let report = native ~mode source in
      Alcotest.(check string)
        "native selected fault code" "HCIRVM0009"
        (first_native_error report).code;
      Alcotest.(check int)
        "selected fault switch preparation" 1
        (Native_program.switch_work report);
      Alcotest.(check bool)
        "selected runtime fault retains image" true
        (Option.is_some (Native_program.image report));
      (match Native_program.native_outcome report with
      | Some (Program.Fault fault) ->
          Alcotest.(check bool)
            "selected fault kind" true
            (fault.kind = Program.Division_by_zero);
          Alcotest.(check int)
            "checked/native selected fault step" vm_fault.executed_steps
            fault.executed_steps;
          Alcotest.(check (option string))
            "selected fault owner" (Some "Pick") fault.function_name;
          Alcotest.(check bool)
            "selected fault keeps a source site" true
            (Option.is_some fault.span)
      | Some (Program.Completed _) ->
          Alcotest.fail "selected native switch fault completed"
      | None ->
          Alcotest.fail "selected native switch fault has no runtime outcome");
      ignore
        (compare ~mode ~label:"fresh switch after runtime fault" ~expected:42L
           "I64 Healthy(I64 n){switch(n){case 1:return 42;default:return \
            0;}return -1;}Healthy(1);"))
    modes

let invalid_source_rejects_before_native_entry () =
  let cases =
    [
      ( "duplicate",
        "I64 Bad(I64 n){switch(n){case 1:return 1;case 1:return \
         2;default:return 0;}return -1;}42;",
        "HCSW0002" );
      ( "no-bound",
        "I64 Bad(I64 n){switch[n]{case 1:return 1;default:return 0;}return \
         -1;}42;",
        "HCRUN0001" );
      ( "subswitch",
        "I64 Bad(I64 n){switch(n){start:case 1:return 1;end:default:return \
         0;}return -1;}42;",
        "HCRUN0001" );
      ( "effectful",
        "I64 Bad(I64 n){switch(n){case n++:return 1;default:return 0;}return \
         -1;}42;",
        "HCRUN0001" );
      ( "duplicate default",
        "I64 Bad(I64 n){switch(n){case 1:return 1;default:return \
         0;default:return 2;}return -1;}42;",
        "HCRUN0001" );
      ( "range",
        "I64 Bad(I64 n){switch(n){case 0...65535:return 1;default:return \
         0;}return -1;}42;",
        "HCSW0001" );
      ( "table cap",
        "I64 A(I64 n){switch(n){case 0...39999:return 1;default:return \
         0;}return -1;} I64 B(I64 n){switch(n){case 0...39999:return \
         2;default:return 0;}return -1;} 42;",
        "HCSW0004" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, code) ->
          let report = native ~mode source in
          Alcotest.(check string) label code (first_native_error report).code;
          Alcotest.(check bool)
            (label ^ " has no image") true
            (Option.is_none (Native_program.image report));
          Alcotest.(check (option int))
            (label ^ " has no runtime steps")
            None
            (Native_program.executed_steps report))
        cases)
    modes

let () =
  match Runtime.platform () with
  | Runtime.Unsupported -> Alcotest.fail "native switch tests require x86-64"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 ->
      Alcotest.run "holyc native switch execution"
        [
          ( "switch execution",
            [
              Alcotest.test_case "successful switch programs match checked IR"
                `Quick successful_differentials;
              Alcotest.test_case "all scalar selector widths" `Quick
                scalar_selector_widths;
              Alcotest.test_case "switch work limits and fresh runs" `Quick
                switch_work_limit_and_fresh_runs;
              Alcotest.test_case "selected runtime fault keeps image and site"
                `Quick selected_runtime_fault_retains_native_image;
              Alcotest.test_case "invalid source rejects before native entry"
                `Quick invalid_source_rejects_before_native_entry;
            ] );
        ]
