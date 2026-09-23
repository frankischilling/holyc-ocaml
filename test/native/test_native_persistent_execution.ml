open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution
module VM = Ir_integer_interpreter

let require_ok show = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (show errors)

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let vm_errors_text errors =
  errors
  |> List.map (fun (error : VM.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-persistent-execution.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let report ?max_global_bytes ?max_initializer_steps ?status_abi mode contents =
  let session, config, source = inputs mode contents in
  Native_program.evaluate ?max_global_bytes ?max_initializer_steps ?status_abi
    session ~config ~source ~max_steps:10000

let success mode contents =
  let report = report mode contents in
  let checked = Native_program.outcome report |> require_ok diagnostics_text in
  (report, checked.value)

let check_native label expected = function
  | None -> Alcotest.failf "%s: native result is absent" label
  | Some (word : Program.word) ->
      Alcotest.(check int64) label expected word.bits

let check_vm label expected execution =
  match VM.final_value execution with
  | None -> Alcotest.failf "%s: interpreter result is absent" label
  | Some word -> Alcotest.(check int64) label expected word.bits

let fixture mode contents =
  Native_scalar_fixture.compile ~mode ~path:"native-persistent-batch.hc"
    ~contents ()
  |> require_ok diagnostics_text

let compare mode label expected contents =
  let session, config, source = inputs mode contents in
  let public =
    run_integer_program session ~config ~source ~max_steps:10000
    |> require_ok diagnostics_text
  in
  check_vm (label ^ " public") expected public.value;
  let batch =
    Native_scalar_fixture.execute ~max_steps:10000 (fixture mode contents)
    |> require_ok vm_errors_text
  in
  check_vm (label ^ " checked batch") expected batch;
  let report, native = success mode contents in
  check_native (label ^ " native") expected native.execution.final_value;
  Alcotest.(check int)
    (label ^ " execution work")
    (VM.executed_steps batch) native.execution.executed_steps;
  (report, native)

let widths =
  [
    ("I8", "255", "+1");
    ("U8", "255", "-255");
    ("I16", "65535", "+1");
    ("U16", "65535", "-65535");
    ("I32", "4294967295", "+1");
    ("U32", "4294967295", "-4294967295");
    ("I64", "-1", "+1");
    ("U64", "0xffffffffffffffff", "-0xffffffffffffffff");
  ]

let storage_source ~static type_name statements =
  if static then
    Printf.sprintf "I64 F(){static %s a[3];%s}F();" type_name statements
  else Printf.sprintf "%s a[3];I64 F(){%s}F();" type_name statements

let all_widths () =
  List.iter
    (fun mode ->
      List.iter
        (fun static ->
          List.iter
            (fun (type_name, stored, adjustment) ->
              let source =
                storage_source ~static type_name
                  (Printf.sprintf
                     "a[0]=13;a[2]=29;a[1]=%s;return a[0]+a[1]+a[2]%s;" stored
                     adjustment)
              in
              ignore (compare mode type_name 42L source);
              List.iter
                (fun abi ->
                  let session, config, source = inputs mode source in
                  let image =
                    Native_program.compile ~status_abi:abi session ~config
                      ~source
                    |> require_ok diagnostics_text
                  in
                  Alcotest.(check bool)
                    "requested status ABI" true
                    (Program.status_abi image.value = abi))
                [ Program.Windows_x64; Program.System_v_x64 ])
            widths)
        [ false; true ])
    modes

let updates () =
  let cases =
    [
      (40, "+=2");
      (44, "-=2");
      (21, "*=2");
      (84, "/=2");
      (126, "%=84");
      (21, "<<=1");
      (84, ">>=1");
      (58, "&=47");
      (40, "|=2");
      (40, "^=2");
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun static ->
          List.iter
            (fun (type_name, _, _) ->
              List.iter
                (fun (initial, operation) ->
                  let source =
                    storage_source ~static type_name
                      (Printf.sprintf
                         "a[0]=13;a[2]=29;a[1]=%d;a[1]%s;return \
                          a[0]+a[1]+a[2]-42;"
                         initial operation)
                  in
                  ignore (compare mode (type_name ^ operation) 42L source))
                cases;
              List.iter
                (fun (initial, expression, adjustment) ->
                  let source =
                    storage_source ~static type_name
                      (Printf.sprintf
                         "a[0]=13;a[2]=29;a[1]=%d;%s *p=&a[2];I64 n=%s;return \
                          n+a[1]+a[0]+a[2]-%d;"
                         initial type_name expression adjustment)
                  in
                  ignore (compare mode expression 42L source))
                [
                  (41, "++p[-1]", 84);
                  (43, "--a[1]", 84);
                  (42, "p[-1]++", 85);
                  (42, "a[1]--", 83);
                ])
            widths)
        [ false; true ])
    modes

let identities_and_flat_offsets () =
  let cases =
    [
      ("entry", "I64 a[2];a[0]=40;a[1]=2;a[0]+a[1];");
      ("global cross row", "I64 a[2][3];a[0][3]=42;a[1][0];");
      ("global restored offset", "I64 a[2][3];a[-1][3]=42;a[0][0];");
      ( "static restored offset",
        "I64 F(){static I64 a[2][3];a[2][-1]=42;return a[1][2];}F();" );
      ( "partial row",
        "I64 a[2][3];I64 Read(I64 *p){return \
         p[-1]+p[0];}a[0][2]=40;a[1][0]=2;Read(a[1]);" );
      ( "saved global alias",
        "I64 a[2];I64 F(){I64 *p,*q;I64 i=0;while(i<2){I64 \
         *r=&a[i];if(i)q=r;else p=r;i++;}*p=40;*q=2;return a[0]+a[1];}F();" );
      ( "saved static alias",
        "I64 F(){static I64 a[2];I64 *p,*q;I64 i=0;while(i<2){I64 \
         *r=&a[i];if(i)q=r;else p=r;i++;}*p=40;*q=2;return a[0]+a[1];}F();" );
      ( "RHS mutates destination",
        "I64 a[2];I64 Set(){a[0]=40;return 2;}I64 \
         F(){a[0]=1;a[0]+=Set();return a[0];}F();" );
      ( "nested pointer rebinding",
        "I64 a[2],b[2];I64 F(){a[0]=40;b[0]=7;I64 \
         *p=a;p[((p=b)[0]=0)+0]+=2;return a[0];}F();" );
      ( "one-past return to object",
        "I64 a[2];I64 F(){a[1]=42;I64 *p=&a[2];return p[-1];}F();" );
      ( "distinct static owners",
        "I64 F(){static I8 a[2];a[0]=20;return a[0];}I64 G(){static I8 \
         a[2];a[0]=22;return a[0];}F()+G();" );
      ( "recursive static sharing",
        "I64 F(I64 n){static I16 \
         a[2];if(n==6){a[0]=21;a[1]=0;}if(n){a[0]+=n;return F(n-1);}return \
         a[0];}F(6);" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) -> ignore (compare mode label 42L source))
        cases)
    modes

let initial_state_and_fresh_images () =
  List.iter
    (fun static ->
      let source = storage_source ~static "I8" "a[0]=42;return a[1];" in
      let unknown = report Preprocessor.Jit source in
      (match Native_program.native_outcome unknown with
      | Some (Program.Fault f) when f.kind = Program.Uninitialized_read -> ()
      | _ -> Alcotest.fail "writing one cell initialized an adjacent JIT cell");
      let _, native = compare Preprocessor.Aot "untouched AOT cell" 0L source in
      for _ = 1 to 3 do
        match
          Runtime.execute ~max_steps:10000 native.image |> require_ok Fun.id
        with
        | Program.Completed r -> check_native "fresh AOT arena" 0L r.final_value
        | _ -> Alcotest.fail "fresh AOT arena faulted"
      done;
      let source = storage_source ~static "U8" "a[0]++;return a[0];" in
      let _, native =
        compare Preprocessor.Aot "zeroed array increment" 1L source
      in
      for _ = 1 to 3 do
        match
          Runtime.execute ~max_steps:10000 native.image |> require_ok Fun.id
        with
        | Program.Completed r -> check_native "fresh mutation" 1L r.final_value
        | _ -> Alcotest.fail "fresh array mutation faulted"
      done)
    [ false; true ]

let faults_and_padding () =
  let cases =
    [
      ("negative", "I8 a[3];a[-1];", Program.Address_out_of_bounds);
      ("one past", "I8 a[3];a[3];", Program.Address_out_of_bounds);
      ( "static padding",
        "I64 F(){static I8 a[3];return a[3];}F();",
        Program.Address_out_of_bounds );
      ( "adjacent global",
        "I8 a[3],b[3];b[0]=42;a[3];",
        Program.Address_out_of_bounds );
      ( "scale overflow",
        "I64 a[2];a[0x1000000000000000];",
        Program.Index_scale_overflow );
      ( "RHS before final bounds",
        "I64 a[2];a[-1]=1/0;",
        Program.Division_by_zero );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, contents, expected) ->
          let result = report mode contents in
          let fault =
            match Native_program.native_outcome result with
            | Some (Program.Fault f) -> f
            | _ ->
                Alcotest.failf "%s: missing native execution fault: %s" label
                  (match Native_program.outcome result with
                  | Error e -> diagnostics_text e
                  | Ok _ -> "completed")
          in
          Alcotest.(check bool) (label ^ " kind") true (fault.kind = expected);
          let error =
            match
              Native_scalar_fixture.execute ~max_steps:10000
                (fixture mode contents)
            with
            | Error (error :: _) -> error
            | _ -> Alcotest.failf "%s: checked interpreter did not fault" label
          in
          Alcotest.(check int)
            (label ^ " consumed work") error.executed_steps fault.executed_steps;
          Alcotest.(check (option int))
            (label ^ " original instruction")
            error.instruction_id (Some fault.instruction_id))
        cases)
    modes

let storage_limits () =
  List.iter
    (fun mode ->
      let contents =
        "I8 a[3];I64 Unused;I64 F(){static I8 s[3];a[0]=40;s[0]=2;return \
         a[0]+s[0];}F();"
      in
      let exact = report ~max_global_bytes:19 mode contents in
      let native =
        Native_program.outcome exact |> require_ok diagnostics_text
      in
      check_native "exact declared storage" 42L
        native.value.execution.final_value;
      Alcotest.(check int)
        "declared allocation includes unused and static padding" 19
        (Program.global_bytes native.value.image);
      Alcotest.(check bool)
        "private flags excluded from declared bytes" true
        (String.length (Program.global_image native.value.image) > 19);
      let below = report ~max_global_bytes:18 mode contents in
      Alcotest.(check bool)
        "compile quota checked before entry" true
        (Option.is_none (Native_program.image below));
      (match
         Runtime.execute ~max_global_bytes:18 ~max_steps:10000
           native.value.image
       with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "runtime accepted one-below storage quota");
      let oversized =
        report ~max_global_bytes:4194304 mode "U8 Huge[4194304];42;"
      in
      Alcotest.(check bool)
        "array metadata bounded before expansion" true
        (Option.is_none (Native_program.image oversized)))
    modes

let initialized_widths () =
  let rows =
    [
      ("I8", "255", "-1");
      ("U8", "511", "255");
      ("I16", "65535", "-1");
      ("U16", "131071", "65535");
      ("I32", "4294967295", "-1");
      ("U32", "8589934591", "4294967295");
      ("I64", "-9223372036854775808", "-9223372036854775808");
      ("U64", "0xffffffffffffffff", "0xffffffffffffffff");
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (type_name, value, expected) ->
          let source =
            Printf.sprintf
              "%s G[3]={13,%s,29};I64 F(){static %s \
               s[3]={13,%s,29};if(G[0]!=13||G[1]!=%s||G[2]!=29||s[0]!=13||s[1]!=%s||s[2]!=29)return \
               0;return 42;}F();"
              type_name value type_name value expected expected
          in
          let prepared, native =
            compare mode (type_name ^ " initialized image") 42L source
          in
          let work = Native_program.preparation_steps prepared in
          let bytes = Program.global_bytes native.image in
          let exact =
            report ~max_initializer_steps:work ~max_global_bytes:bytes mode
              source
          in
          let exact =
            Native_program.outcome exact |> require_ok diagnostics_text
          in
          check_native "exact initializer and object limits" 42L
            exact.value.execution.final_value;
          let below = report ~max_initializer_steps:(work - 1) mode source in
          Alcotest.(check int)
            "numeric initializer exhaustion keeps its consumed budget" (work - 1)
            (Native_program.preparation_steps below);
          Alcotest.(check bool)
            "one-below preparation cannot publish an image" true
            (Option.is_none (Native_program.image below));
          let below_bytes = report ~max_global_bytes:(bytes - 1) mode source in
          Alcotest.(check bool)
            "one-below initialized storage cannot enter" true
            (Option.is_none (Native_program.image below_bytes));
          List.iter
            (fun abi ->
              let session, config, source = inputs mode source in
              let image =
                Native_program.compile ~status_abi:abi session ~config ~source
                |> require_ok diagnostics_text
              in
              Alcotest.(check bool)
                "initialized array requested ABI" true
                (Program.status_abi image.value = abi))
            [ Program.Windows_x64; Program.System_v_x64 ])
        rows)
    modes

let initializer_shapes_and_copies () =
  let cases =
    [
      ( "nested numeric leaves",
        "I16 G[2][2]={{10,10},{20,2}};I64 F(){static I16 \
         s[2][2]={{10,10},{20,2}};return \
         G[0][0]+G[0][1]+G[1][0]+G[1][1]+s[0][0]+s[0][1]+s[1][0]+s[1][1]-42;}F();"
      );
      ( "flat numeric leaves",
        "I16 G[2][2]=10,10,20,2;I64 F(){static I16 s[2][2]={10,10,20,2};return \
         G[1][0]+G[1][1]+s[0][0]+s[0][1];}F();" );
      ( "copied signed byte and untouched neighbors",
        "I8 G[2]=\"\\xFF\";I64 F(){static I8 s[2]=\"\\xFF\";return \
         G[0]+s[0]+G[1]+s[1]+44;}F();" );
      ( "embedded NUL retained in row copies",
        "U8 G[2][3]={\"A\\0\",\"BC\"};I64 F(){static U8 \
         s[2][3]={\"A\\0\",\"BC\"};return \
         G[0][1]+G[0][2]+G[1][2]+s[0][1]+s[0][2]+s[1][2]+42;}F();" );
      ( "truncated copy has no invented terminator",
        "U8 G[2]=\"ABC\";I64 F(){static U8 s[2]=\"ABC\";return \
         G[1]+s[1]-90;}F();" );
      ( "outer-rank copy preserves original prefix",
        "U8 G[2][3]=\"42\";I64 F(){static U8 s[2][3]=\"42\";return \
         (G[0][0]-48)*10+G[0][1]-48+s[0][0]+s[0][1]-102;}F();" );
      ( "distinct initialized static owners",
        "I64 F(){static I16 a[1]={20};return ++a[0];}I64 G(){static I16 \
         a[1]={20};return ++a[0];}F()+G();" );
      ( "recursive initialized static sharing",
        "I64 F(I64 n){static I16 a[2]={36,0};if(n){a[0]++;return \
         F(n-1);}return a[0];}F(6);" );
      ( "unused and unreachable initializers",
        "I16 Unused[2]={40,2};I64 Never(){return 0;static I8 s[2]={40,2};}42;"
      );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) -> ignore (compare mode label 42L source))
        cases;
      List.iter
        (fun source ->
          let result = report mode source in
          match (mode, Native_program.native_outcome result) with
          | Preprocessor.Jit, Some (Program.Fault fault) ->
              Alcotest.(check bool)
                "partial copy leaves tail unknown" true
                (fault.kind = Program.Uninitialized_read)
          | Preprocessor.Aot, Some (Program.Completed result) ->
              check_native "partial AOT copy leaves zero tail" 42L
                result.final_value
          | _ -> Alcotest.fail "partial copied array has incorrect tail state")
        [
          "U8 G[2][3]=\"42\";G[0][2]+42;";
          "I64 F(){static U8 s[2][3]=\"42\";return s[0][2]+42;}F();";
        ])
    modes

let initialized_image_recovery () =
  let source =
    "I16 G[2]={38,0};I64 F(){static U8 s[2]={1,0};G[0]++;s[0]++;return \
     G[0]+s[0];}F();F();"
  in
  List.iter
    (fun mode ->
      let prepared, native =
        compare mode "initialized repeated calls" 43L source
      in
      let image = native.image in
      let work = Native_program.preparation_steps prepared in
      let steps = native.execution.executed_steps in
      (match
         Runtime.execute ~max_steps:(steps - 1) image |> require_ok Fun.id
       with
      | Program.Fault fault ->
          Alcotest.(check bool)
            "late fault stops after mutations" true
            (fault.kind = Program.Step_limit_exceeded)
      | _ -> Alcotest.fail "one-below complete execution unexpectedly succeeded");
      for _ = 1 to 3 do
        match Runtime.execute ~max_steps:steps image |> require_ok Fun.id with
        | Program.Completed result ->
            check_native "fresh prepared arena after a fault" 43L
              result.final_value;
            Alcotest.(check int)
              "repeated execution work" steps result.executed_steps
        | _ -> Alcotest.fail "fresh prepared image unexpectedly faulted"
      done;
      Alcotest.(check int)
        "repeated entry does not reprepare source" work
        (Native_program.preparation_steps prepared))
    modes

let () =
  if Runtime.platform () = Runtime.Unsupported then
    failwith "native persistent array tests require x86-64";
  Alcotest.run "Native persistent arrays"
    [
      ( "persistent arrays",
        [
          Alcotest.test_case "all widths and ABI images" `Quick all_widths;
          Alcotest.test_case "all updates preserve adjacent cells" `Quick
            updates;
          Alcotest.test_case "owners, aliases and flat offsets" `Quick
            identities_and_flat_offsets;
          Alcotest.test_case "per-cell initial state and fresh images" `Quick
            initial_state_and_fresh_images;
          Alcotest.test_case "fault order and inaccessible padding" `Quick
            faults_and_padding;
          Alcotest.test_case "declared and private storage limits" `Quick
            storage_limits;
          Alcotest.test_case
            "all-width initialized images and exact preparation quotas" `Quick
            initialized_widths;
          Alcotest.test_case
            "initialized shapes copied bytes and static sharing" `Quick
            initializer_shapes_and_copies;
          Alcotest.test_case "prepared images reset after late execution faults"
            `Quick initialized_image_recovery;
        ] );
    ]
