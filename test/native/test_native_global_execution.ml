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

let source_inputs ~mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-globals-execution.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let public_run ?(max_steps = 10_000) ~mode contents =
  let session, config, source = source_inputs ~mode contents in
  run_integer_program session ~config ~source ~max_steps

let native_report ?(max_steps = 10_000) ~mode contents =
  let session, config, source = source_inputs ~mode contents in
  Native_program.evaluate session ~config ~source ~max_steps

let native_success ?max_steps ~mode contents =
  let report = native_report ?max_steps ~mode contents in
  match Native_program.outcome report with
  | Ok checked -> (report, checked.value)
  | Error diagnostics -> Alcotest.fail (diagnostics_text diagnostics)

let batch_fixture ~mode contents =
  Native_scalar_fixture.compile ~mode ~path:"native-globals-batch.hc" ~contents
    ()
  |> require_ok diagnostics_text

let batch_success ?(max_steps = 10_000) ~mode contents =
  let fixture = batch_fixture ~mode contents in
  let execution =
    Native_scalar_fixture.execute ~max_steps fixture
    |> require_ok vm_errors_text
  in
  (fixture, execution)

let first_native_diagnostic report =
  match Native_program.outcome report with
  | Ok _ -> Alcotest.fail "native goto execution unexpectedly succeeded"
  | Error [] -> Alcotest.fail "native goto execution returned no diagnostic"
  | Error (first :: _) -> first

let vm_type_name = function
  | VM.I64 -> "I64"
  | VM.U64 -> "U64"

let program_type_name = function
  | Program.I64 -> "I64"
  | Program.U64 -> "U64"

let check_vm_word label expected_type expected_bits result =
  match VM.final_value result with
  | None -> Alcotest.failf "%s: missing checked-IR final value" label
  | Some word ->
      Alcotest.(check string)
        (label ^ " checked-IR type")
        expected_type (vm_type_name word.type_);
      Alcotest.(check int64)
        (label ^ " checked-IR bits")
        expected_bits word.bits

let check_native_word label expected_type expected_bits = function
  | None -> Alcotest.failf "%s: missing native final value" label
  | Some (word : Program.word) ->
      Alcotest.(check string)
        (label ^ " native type") expected_type
        (program_type_name word.type_);
      Alcotest.(check int64) (label ^ " native bits") expected_bits word.bits

let check_public_word label expected_type expected_bits = function
  | Error diagnostics -> Alcotest.fail (diagnostics_text diagnostics)
  | Ok checked ->
      check_vm_word (label ^ " public source") expected_type expected_bits
        checked.value

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let prepared_initializers () =
  let cases =
    [
      ("entry", "I64 G=40;G+=2;G;", "I64", 42L);
      ("declarator list", "I64 A=20,B=22;A+B;", "I64", 42L);
      ("primitive size", "I64 G=sizeof(I64)+34;G;", "I64", 42L);
      ("calls", "I64 G=40;I64 Add(I64 n=2){G+=n;return G;}Add();", "I64", 42L);
      ( "recursion",
        "I64 G=21;I64 F(I64 n){if(n){G+=n;return F(n-1);}return G;}F(6);",
        "I64",
        42L );
      ( "all widths",
        "I8 A=255;U8 B=258;I16 C=65533;U16 D=65540;I32 E=4294967291;U32 \
         F=4294967302;I64 G=-7;U64 H=46;A+B+C+D+E+F+G+H;",
        "U64",
        42L );
      ("high bits", "U64 G=0x8000000000000000;G;", "U64", Int64.min_int);
      ("unused", "I64 Unused=6*7;42;", "I64", 42L);
      ("shadow", "I64 G=40;I64 F(){I64 G=2;return G;}G+F();", "I64", 42L);
      ( "switch",
        "I64 G=40;switch(G){case 40:G+=2;break;default:G=0;}G;",
        "I64",
        42L );
      ("eager values", "I64 G=0||2;G+41;", "I64", 42L);
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, contents, type_, bits) ->
          let report, native = native_success ~mode contents in
          check_native_word label type_ bits native.execution.final_value;
          check_public_word label type_ bits (public_run ~mode contents);
          Alcotest.(check bool)
            "declaration work retained" true
            (Native_program.preparation_steps report > 0);
          for _ = 1 to 3 do
            match
              Runtime.execute ~max_steps:10000 native.image |> require_ok Fun.id
            with
            | Program.Completed result ->
                check_native_word "fresh prepared image" type_ bits
                  result.final_value
            | Program.Fault _ -> Alcotest.fail "fresh prepared image faulted"
          done)
        cases)
    modes

let initializer_limits () =
  List.iter
    (fun mode ->
      let contents = "I8 A=255;I64 B=6*7-1;I64 F(I64 n=2){return A+B+n;}F();" in
      let report, native = native_success ~mode contents in
      let steps = Native_program.preparation_steps report in
      Alcotest.(check int)
        "only default payload" 8
        (Native_program.default_bytes report);
      let evaluate limit bytes =
        let session, config, source = source_inputs ~mode contents in
        Native_program.evaluate ~max_initializer_steps:limit
          ~max_global_bytes:bytes ~max_default_bytes:8 session ~config ~source
          ~max_steps:1000
      in
      let exact = evaluate steps 9 in
      let result =
        Native_program.outcome exact |> require_ok diagnostics_text
      in
      check_native_word "exact shared preparation and storage limits" "I64" 42L
        result.value.execution.final_value;
      List.iter
        (fun failed ->
          Alcotest.(check bool)
            "quota rejects before entry" true
            (Option.is_none (Native_program.image failed));
          Alcotest.(check bool)
            "work survives quota failure" true
            (Native_program.preparation_steps failed > 0))
        [ evaluate (steps - 1) 9; evaluate steps 8 ];
      let repeated, _ = native_success ~mode (contents ^ "F();F();") in
      Alcotest.(check int)
        "calls never reprepare" steps
        (Native_program.preparation_steps repeated);
      let session, config, source = source_inputs ~mode "I64 G=40;G+=2;1/0;" in
      let failure =
        Native_program.evaluate session ~config ~source ~max_steps:1000
      in
      let image = Native_program.image failure |> Option.get in
      for _ = 1 to 2 do
        match Runtime.execute ~max_steps:1000 image |> require_ok Fun.id with
        | Program.Fault fault when fault.kind = Program.Division_by_zero -> ()
        | _ -> Alcotest.fail "prepared fault image did not unwind"
      done;
      ignore native)
    modes

let scalar_globals () =
  let cases =
    [
      ("entry only", "I64 G;G=42;G;", "I64", 42L);
      ( "shared calls",
        "I64 G;I64 Add(I64 n){G+=n;return G;}G=0;Add(20);Add(22);G;",
        "I64",
        42L );
      ( "recursive sharing",
        "I64 G;I64 Sum(I64 n){if(n){G+=n;return Sum(n-1);}return \
         G;}G=0;Sum(6);G*2;",
        "I64",
        42L );
      ("local shadow", "I64 G;I64 F(){I64 G=2;return G;}G=40;G+F();", "I64", 42L);
      ( "RHS writes destination",
        "I64 G;I64 F(){G=40;return 2;}G=1;G+=F();G;",
        "I64",
        42L );
      ( "saved default",
        "I64 G;I64 F(I64 x=21){G+=x;return G;}G=0;F();F();",
        "I64",
        42L );
      ( "goto sharing",
        "I64 G;U0 F(){goto done;G=0;done:G+=2;}G=40;F();G;",
        "I64",
        42L );
      ("assignment bits", "I8 G;G=255;", "I64", 255L);
      ("compound bits", "I8 G;G=127;G+=1;", "I64", 128L);
      ("compound store", "I8 G;G=127;G+=1;G;", "I64", -128L);
      ("prefix normalization", "I8 G;G=127;++G;", "I64", -128L);
      ("postfix old value", "I8 G;G=127;G++;", "I64", 127L);
      ( "adjacent widths",
        "I8 A;U8 B;I16 C;U16 D;I32 E;U32 F;I64 G;U64 \
         H;A=-1;B=2;C=-3;D=4;E=-5;F=6;G=-7;H=46;A+B+C+D+E+F+G+H;",
        "U64",
        42L );
      ("unsigned shift", "U8 G;G=255;G>>=7;G;", "U64", 1L);
      ("signed shift", "I8 G;G=-128;G>>=7;G;", "I64", -1L);
      ("unused global", "I64 G;42;", "I64", 42L);
      ("skipped read", "I64 G;if(0)G;42;", "I64", 42L);
      ( "high register pressure",
        "I64 G;G=2;(G+1)*((G+2)*((G+3)*((G+4)*(G+5))));",
        "I64",
        2520L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, contents, expected_type, expected_bits) ->
          let report, native = native_success ~mode contents in
          check_native_word label expected_type expected_bits
            native.execution.final_value;
          check_public_word label expected_type expected_bits
            (public_run ~mode contents);
          let _, batch = batch_success ~mode contents in
          check_vm_word label expected_type expected_bits batch;
          Alcotest.(check int)
            (label ^ " meter") (VM.executed_steps batch)
            native.execution.executed_steps;
          ignore report)
        cases)
    modes

let fresh_storage_and_faults () =
  List.iter
    (fun mode ->
      let text =
        "I64 G;I64 F(I64 n){if(n)return F(n-1);return G;}if(0)G=42;F(3);"
      in
      let report = native_report ~mode text in
      (match mode with
      | Preprocessor.Aot ->
          let result =
            Native_program.outcome report |> require_ok diagnostics_text
          in
          check_native_word "AOT zero" "I64" 0L
            result.value.execution.final_value
      | Preprocessor.Jit ->
          let error = first_native_diagnostic report in
          Alcotest.(check string) "unknown JIT read" "HCIRVM0012" error.code;
          let fixture = batch_fixture ~mode text in
          let errors =
            match Native_scalar_fixture.execute ~max_steps:10000 fixture with
            | Error e -> e
            | Ok _ -> Alcotest.fail "VM accepted unknown read"
          in
          Alcotest.(check string)
            "matching VM fault" "HCIRVM0012" (List.hd errors).code);
      let _, result = native_success ~mode "I64 G;G=42;G;" in
      for _ = 1 to 3 do
        match
          Runtime.execute ~max_steps:10000 result.image |> require_ok Fun.id
        with
        | Program.Completed result ->
            check_native_word "fresh recovery" "I64" 42L result.final_value
        | Program.Fault _ -> Alcotest.fail "fresh recovery faulted"
      done)
    modes;
  let _, result = native_success ~mode:Preprocessor.Aot "I64 G;G+=1;G;" in
  for _ = 1 to 3 do
    match
      Runtime.execute ~max_steps:10000 result.image |> require_ok Fun.id
    with
    | Program.Completed result ->
        check_native_word "fresh AOT image" "I64" 1L result.final_value
    | Program.Fault _ -> Alcotest.fail "fresh image faulted"
  done

let quota_boundaries () =
  List.iter
    (fun mode ->
      let session, config, source =
        source_inputs ~mode "I8 A;I64 Unused;A=42;A;"
      in
      let image =
        Native_program.compile ~max_global_bytes:9 session ~config ~source
        |> require_ok diagnostics_text
      in
      Alcotest.(check int) "declared bytes" 9 (Program.global_bytes image.value);
      Alcotest.(check int)
        "private arena bytes" 11
        (String.length (Program.global_image image.value));
      (match
         Native_program.compile ~max_global_bytes:8 session ~config ~source
       with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "unused global escaped quota");
      (match
         Runtime.execute ~max_global_bytes:8 ~max_steps:10000 image.value
       with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "runtime quota escaped");
      let result =
        Runtime.execute ~max_global_bytes:9 ~max_steps:10000 image.value
        |> require_ok Fun.id
      in
      let steps =
        match result with
        | Program.Completed r -> r.executed_steps
        | _ -> Alcotest.fail "exact storage quota failed"
      in
      (match
         Runtime.execute ~max_steps:steps image.value |> require_ok Fun.id
       with
      | Program.Completed _ -> ()
      | _ -> Alcotest.fail "exact steps failed");
      match
        Runtime.execute ~max_steps:(steps - 1) image.value |> require_ok Fun.id
      with
      | Program.Fault f when f.kind = Program.Step_limit_exceeded ->
          Alcotest.(check int) "reached budget" (steps - 1) f.executed_steps
      | _ -> Alcotest.fail "one-below steps succeeded")
    modes

let updates_and_widths () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_, input, expected, tag) ->
          let text = Printf.sprintf "%s G;G=%s;G;" type_ input in
          let _, native = native_success ~mode text in
          check_native_word type_ tag expected native.execution.final_value;
          check_public_word type_ tag expected (public_run ~mode text))
        [
          ("I8", "255", -1L, "I64");
          ("U8", "257", 1L, "U64");
          ("I16", "65535", -1L, "I64");
          ("U16", "65537", 1L, "U64");
          ("I32", "4294967295", -1L, "I64");
          ("U32", "4294967297", 1L, "U64");
          ("I64", "-1", -1L, "I64");
          ("U64", "0xffffffffffffffff", -1L, "U64");
        ];
      List.iter
        (fun (operator, expected) ->
          let text = Printf.sprintf "I16 G;G=42;G%s2;G;" operator in
          let _, native = native_success ~mode text in
          check_native_word operator "I64" expected native.execution.final_value;
          check_public_word operator "I64" expected (public_run ~mode text))
        [
          ("+=", 44L);
          ("-=", 40L);
          ("*=", 84L);
          ("/=", 21L);
          ("%=", 0L);
          ("&=", 2L);
          ("|=", 42L);
          ("^=", 40L);
          ("<<=", 168L);
          (">>=", 10L);
        ];
      let text =
        "I64 G;I64 F(I64 n){if(n)return F(n-1);G/=0;return G;}G=42;F(2);"
      in
      let report = native_report ~mode text in
      (match Native_program.native_outcome report with
      | Some (Program.Fault f) when f.kind = Program.Division_by_zero ->
          Alcotest.(check (option string))
            "nested fault owner" (Some "F") f.function_name
      | _ -> Alcotest.fail "nested global division failed to guard");
      let image = Native_program.image report |> Option.get in
      for _ = 1 to 2 do
        match Runtime.execute ~max_steps:10000 image |> require_ok Fun.id with
        | Program.Fault f when f.kind = Program.Division_by_zero -> ()
        | _ -> Alcotest.fail "repeated fault did not clean up"
      done)
    modes

let () =
  if Runtime.platform () = Runtime.Unsupported then
    failwith "native globals tests require x86-64";
  Alcotest.run "Native global execution"
    [
      ( "globals",
        [
          Alcotest.test_case
            "width edges, compound operations and nested faults" `Quick
            updates_and_widths;
          Alcotest.test_case "scalar storage and sharing" `Quick scalar_globals;
          Alcotest.test_case "original scalar initializer preparation" `Quick
            prepared_initializers;
          Alcotest.test_case
            "shared initializer/default limits and fault cleanup" `Quick
            initializer_limits;
          Alcotest.test_case "fresh images and nested fault unwind" `Quick
            fresh_storage_and_faults;
          Alcotest.test_case "storage and step boundaries" `Quick
            quota_boundaries;
        ] );
    ]
