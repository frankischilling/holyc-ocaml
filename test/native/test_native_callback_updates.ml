open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution
module VM = Ir_integer_interpreter

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
    Session.add_source session ~path:"native-callback-updates.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  (session, config, source)

let native ?(max_steps = 100_000) mode contents =
  let session, config, source = inputs mode contents in
  Native_program.evaluate session ~config ~source ~max_steps

let ir ?(max_steps = 100_000) mode contents =
  let session, config, source = inputs mode contents in
  run_integer_program_report session ~config ~source ~max_steps

let expect_error expected = function
  | Ok _ -> Alcotest.fail "expected a checked execution failure"
  | Error errors ->
      Alcotest.(check bool)
        ("expected " ^ expected ^ "; received " ^ diagnostics errors)
        true
        (List.exists
           (fun (error : Diagnostic.t) -> error.code = expected)
           errors)

let native_value expected report =
  let result =
    Native_program.outcome report |> Result.map_error diagnostics |> checked
    |> fun checked -> checked.value.execution
  in
  Alcotest.(check int64)
    "native final word" expected (Option.get result.final_value).bits;
  result.executed_steps

let compare mode contents expected =
  let public = ir mode contents in
  let public_result =
    integer_program_report_outcome public
    |> Result.map_error diagnostics
    |> checked
    |> fun checked -> checked.value
  in
  Alcotest.(check int64)
    "public IR final word" expected
    (Option.get (VM.final_value public_result)).bits;
  let report = native mode contents in
  let native_steps = native_value expected report in
  Alcotest.(check string)
    "native output agrees with public IR"
    (integer_program_report_output_bytes public)
    (Native_program.output_bytes report);
  (VM.executed_steps public_result, native_steps)

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let storage_shapes_and_metadata () =
  let rows =
    [
      "I64 F(){I64 (*p)(I64 n);p=34;p++;return p==42;}F();";
      "I64 F(I64 (*p)(I64 n)){p++;return p==42;}F(34);";
      "I64 F(){static I64 (*p)(I64 n);p=34;p++;return p==42;}F();";
      "I64 (*P)(I64 n);I64 F(){P=34;P++;return P==42;}F();";
      "I64 F(){I64 (*p)(I64 n)[2][3];p[1][2]=34;p[1][2]++;return \
       p[1][2]==42;}F();";
      "I64 F(){static I64 (*p)(I64 n)[2];p[1]=50;--p[1];return p[1]==42;}F();";
      "I64 F(){F64 (*p)(I64 n);p=34;p++;return p==42;}F();";
      "I64 F(){U8 *(*p)(I64 n);p=34;p++;return p==42;}F();";
    ]
  in
  List.iter
    (fun mode -> List.iter (fun source -> ignore (compare mode source 1L)) rows)
    modes

let stride_canceled_star_and_compounds () =
  let rows =
    [
      ( "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=34;return \
         Id(++p);}F();",
        42L );
      ( "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=34;return \
         Id(p++);}F();",
        34L );
      ( "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=26;return \
         Id(p+=2);}F();",
        42L );
      ( "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=58;return \
         Id(p-=2);}F();",
        42L );
      ( "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);U64 n=2;p=26;return \
         Id(p+=n);}F();",
        42L );
      ( "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);U64 n=2;p=58;return \
         Id(p-=n);}F();",
        42L );
      ( "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=-84;return \
         Id(p/=-2);}F();",
        42L );
      ( "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=-8;return \
         Id(p>>=1);}F();",
        -4L );
      ( "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);U64 d=2;p=-84;return \
         Id(p/=d);}F();",
        -42L );
      ( "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);U64 n=1;p=-8;return \
         Id(p>>=n);}F();",
        -4L );
      ( "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=34;return \
         Id(++*p);}F();",
        42L );
      ("I64 F(){I64 (*p)(I64 n);p=50;(*p)--;return p==42;}F();", 1L);
      ("I64 F(){I64 (*p)(I64 n);p=26;*p+=2;return p==42;}F();", 1L);
      ( "I64 (*P)(I64 n)[2][2];I64 F(){P[1][1]=50;I64 old=P[1][1]--;return \
         old*100+(P[1][1]==42);}F();",
        5001L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) -> ignore (compare mode source expected))
        rows)
    modes

let rhs_effect_and_value_flow () =
  let rows =
    [
      ( "I64 (*P)(I64 n);I64 Side(){P=40;return 1;}I64 \
         F(){P=34;P+=Side();return P==48;}F();",
        1L );
      ( "I64 Check(I64 (*q)(I64 n)){return q==42;}I64 F(){I64 (*p)(I64 \
         n);p=34;return Check(++p);}F();",
        1L );
      ("I64 F(){I64 (*p)(I64 n);p=26;I64 n=(p+=2);return n;}F();", 42L);
      ("I64 F(){I64 (*p)(I64 n);p=34;return ++p;}F();", 42L);
      ("I64 F(){I64 (*p)(I64 n);I64 n;p=26;n=(p+=2);return n;}F();", 42L);
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) -> ignore (compare mode source expected))
        rows)
    modes

let faults_and_bounds_match_public_ir () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, code) ->
          let public = ir mode source in
          expect_error code (integer_program_report_outcome public);
          let report = native mode source in
          expect_error code (Native_program.outcome report))
        [
          ("I64 F(){I64 (*p)(I64 n);p++;return 42;}F();", "HCIRVM0012");
          ("I64 F(){I64 (*p)(I64 n)[2];p[2]++;return 42;}F();", "HCIRVM0019");
          ( "I64 Target(I64 n){return n+2;}I64 (*P)(I64 n)[2][2];I64 \
             F(){P[1][1]=&Target;P[1][1]++;return 42;}F();",
            "HCIRVM0024" );
        ])
    modes

let owned_code_fault_matches_public_ir () =
  let source =
    "extern U0 PutChars(U64 ch);I64 Target(I64 n){return n+2;}I64 F(){I64 \
     (*p)(I64 n);p=&Target;PutChars('B');p++;PutChars('X');return 42;}F();"
  in
  List.iter
    (fun mode ->
      let public = ir mode source in
      expect_error "HCIRVM0024" (integer_program_report_outcome public);
      Alcotest.(check string)
        "public IR keeps effects before the owned-address update" "B"
        (integer_program_report_output_bytes public);
      let report = native mode source in
      expect_error "HCIRVM0024" (Native_program.outcome report);
      Alcotest.(check string)
        "native keeps effects before the owned-address update" "B"
        (Native_program.output_bytes report);
      let fault =
        match Native_program.native_outcome report with
        | Some (Program.Fault fault) -> fault
        | _ -> Alcotest.fail "native update did not expose its reached fault"
      in
      Alcotest.(check bool)
        "native reports the dedicated owned-address update fault" true
        (fault.kind = Program.Callback_update_owned_address);
      let image =
        match Native_program.image report with
        | Some image -> image
        | None -> Alcotest.fail "native update fault lost its compiled image"
      in
      let decode site =
        Program.decode_runtime_status image ~max_steps:100_000 ~kind:22L ~site
          ~executed_steps:(Int64.of_int fault.executed_steps)
          ~value_site:0L ~bits:0L
      in
      let site = Int64.of_int (fault.global_position + 1) in
      (match decode site |> checked with
      | Program.Fault decoded ->
          Alcotest.(check bool)
            "status 22 decodes only as the authenticated callback-update fault"
            true
            (decoded.kind = Program.Callback_update_owned_address)
      | Program.Completed _ ->
          Alcotest.fail "status 22 decoded as successful completion");
      let wrong_site = if site = 1L then 2L else 1L in
      Alcotest.(check bool)
        "status 22 is rejected at a non-update site" true
        (decode wrong_site |> Result.is_error))
    modes

let exact_work () =
  let source = "I64 F(){I64 (*p)(I64 n);p=34;p++;return p==42;}F();" in
  List.iter
    (fun mode ->
      let public_steps, native_steps = compare mode source 1L in
      let public = ir ~max_steps:public_steps mode source in
      ignore
        (integer_program_report_outcome public
        |> Result.map_error diagnostics
        |> checked);
      expect_error "HCIRVM0007"
        (ir ~max_steps:(public_steps - 1) mode source
        |> integer_program_report_outcome);
      ignore (native ~max_steps:native_steps mode source |> native_value 1L);
      expect_error "HCIRVM0007"
        (native ~max_steps:(native_steps - 1) mode source
        |> Native_program.outcome))
    modes

let retained_static_updates_survive_gc () =
  let source =
    "I64 Next(){static I64 ready=0;static I64 (*p)(I64 \
     n);if(!ready){ready=1;p=26;}p+=1;return (p==34)*34+(p==42)*42;}Next;"
  in
  List.iter
    (fun mode ->
      let session, config, source = inputs mode source in
      let image =
        Native_program.compile session ~config ~source
        |> Result.map_error diagnostics
        |> checked
        |> fun checked -> checked.value
      in
      let retained = Runtime.retain image |> checked in
      Fun.protect
        ~finally:(fun () -> Runtime.release retained |> checked)
        (fun () ->
          Gc.full_major ();
          Gc.compact ();
          List.iter
            (fun expected ->
              match
                Runtime.execute_retained_report ~max_steps:100_000 retained
                |> Runtime.outcome |> checked
              with
              | Program.Completed result ->
                  Alcotest.(check int64)
                    "retained static callback update" expected
                    (Option.get result.final_value).bits
              | Program.Fault fault ->
                  Alcotest.failf
                    "retained numeric callback faulted at site %d instruction \
                     %d"
                    fault.global_position fault.instruction_id)
            [ 34L; 42L ]))
    modes

let retained_owned_fault_does_not_corrupt_target () =
  let source =
    "I64 Target(I64 n){return n+2;}I64 F(){static I64 ready=0;static I64 \
     (*p)(I64 n);if(!ready){ready=1;p=&Target;p++;}return p(40);}F();"
  in
  List.iter
    (fun mode ->
      let session, config, source = inputs mode source in
      let image =
        Native_program.compile session ~config ~source
        |> Result.map_error diagnostics
        |> checked
        |> fun checked -> checked.value
      in
      let retained = Runtime.retain image |> checked in
      Fun.protect
        ~finally:(fun () -> Runtime.release retained |> checked)
        (fun () ->
          (match
             Runtime.execute_retained_report ~max_steps:100_000 retained
             |> Runtime.outcome |> checked
           with
          | Program.Fault { kind = Program.Callback_update_owned_address; _ } ->
              ()
          | Program.Fault fault ->
              Alcotest.failf
                "expected owned-address fault; got another fault at site %d \
                 instruction %d"
                fault.global_position fault.instruction_id
          | Program.Completed _ ->
              Alcotest.fail
                "expected the first retained owned-address update to fault");
          Gc.full_major ();
          Gc.compact ();
          match
            Runtime.execute_retained_report ~max_steps:100_000 retained
            |> Runtime.outcome |> checked
          with
          | Program.Completed result ->
              Alcotest.(check int64)
                "the original owned target remains callable after the update \
                 fault"
                42L (Option.get result.final_value).bits
          | Program.Fault fault ->
              Alcotest.failf
                "owned target recovery faulted at site %d instruction %d"
                fault.global_position fault.instruction_id))
    modes

let retained_division_fault_keeps_numeric_cell () =
  let source =
    "I64 F(){static I64 ready=0;static I64 (*p)(I64 \
     n);if(!ready){ready=1;p=42;p/=0;}return p==42;}F();"
  in
  List.iter
    (fun mode ->
      let session, config, source = inputs mode source in
      let image =
        Native_program.compile session ~config ~source
        |> Result.map_error diagnostics
        |> checked
        |> fun checked -> checked.value
      in
      let retained = Runtime.retain image |> checked in
      Fun.protect
        ~finally:(fun () -> Runtime.release retained |> checked)
        (fun () ->
          (match
             Runtime.execute_retained_report ~max_steps:100_000 retained
             |> Runtime.outcome |> checked
           with
          | Program.Fault { kind = Program.Division_by_zero; _ } -> ()
          | Program.Fault fault ->
              Alcotest.failf
                "expected division-by-zero; got another fault at site %d \
                 instruction %d"
                fault.global_position fault.instruction_id
          | Program.Completed _ ->
              Alcotest.fail
                "expected the first retained callback division to fault");
          Gc.full_major ();
          Gc.compact ();
          match
            Runtime.execute_retained_report ~max_steps:100_000 retained
            |> Runtime.outcome |> checked
          with
          | Program.Completed result ->
              Alcotest.(check int64)
                "faulting division leaves the persistent callback word \
                 unchanged"
                1L (Option.get result.final_value).bits
          | Program.Fault fault ->
              Alcotest.failf
                "numeric callback recovery faulted at site %d instruction %d"
                fault.global_position fault.instruction_id))
    modes

let () =
  match Runtime.platform () with
  | Runtime.Unsupported -> Alcotest.fail "native callback tests require x86-64"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 ->
      Alcotest.run "holyc native callback updates"
        [
          ( "callback updates",
            [
              Alcotest.test_case "storage shapes and callback metadata" `Quick
                storage_shapes_and_metadata;
              Alcotest.test_case "stride, canceled star, and compounds" `Quick
                stride_canceled_star_and_compounds;
              Alcotest.test_case "RHS effects and update-result flow" `Quick
                rhs_effect_and_value_flow;
              Alcotest.test_case "faults and bounds match public IR" `Quick
                faults_and_bounds_match_public_ir;
              Alcotest.test_case "owned-address fault matches public IR" `Quick
                owned_code_fault_matches_public_ir;
              Alcotest.test_case "exact update work" `Quick exact_work;
              Alcotest.test_case "retained static updates survive GC" `Quick
                retained_static_updates_survive_gc;
              Alcotest.test_case "owned-address fault leaves target intact"
                `Quick retained_owned_fault_does_not_corrupt_target;
              Alcotest.test_case "division fault leaves numeric callback intact"
                `Quick retained_division_fault_keeps_numeric_cell;
            ] );
        ]
