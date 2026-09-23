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
    Session.add_source session ~path:"native-owned-literals.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let report ?max_literal_bytes ?max_global_bytes ?status_abi mode contents =
  let session, config, source = inputs mode contents in
  Native_program.evaluate ?max_literal_bytes ?max_global_bytes ?status_abi
    session ~config ~source ~max_steps:10000

let native_value expected = function
  | Some (word : Program.word) ->
      Alcotest.(check int64) "native expected bits" expected word.bits
  | None -> Alcotest.fail "native literal program has no result"

let vm_value label expected execution =
  match VM.final_value execution with
  | Some word -> Alcotest.(check int64) label expected word.bits
  | None -> Alcotest.fail (label ^ " has no result")

let fixture mode contents =
  Native_scalar_fixture.compile ~mode ~path:"native-literal-batch.hc" ~contents
    ()
  |> require_ok diagnostics_text

let compare mode expected contents =
  let session, config, source = inputs mode contents in
  let public =
    run_integer_program session ~config ~source ~max_steps:10000
    |> require_ok diagnostics_text
  in
  vm_value "public source expected bits" expected public.value;
  let batch =
    Native_scalar_fixture.execute ~max_steps:10000 (fixture mode contents)
    |> require_ok vm_errors_text
  in
  vm_value "checked batch expected bits" expected batch;
  let native =
    Native_program.outcome (report mode contents) |> require_ok diagnostics_text
  in
  native_value expected native.value.execution.final_value;
  Alcotest.(check int)
    "original execution work" (VM.executed_steps batch)
    native.value.execution.executed_steps;
  native.value

let sources =
  [
    ("entry literal", "(\"*\")[0];", 42L);
    ("empty terminator", "(\"\")[0]+42;", 42L);
    ( "mutable literal",
      "I64 F(){U8 *s=\"41\";s[1]++;return (s[0]-48)*10+s[1]-48;}F();",
      42L );
    ( "same producer across calls",
      "I64 F(){U8 *s=\"0\";return ++s[0]-48;}F();F();",
      2L );
    ( "same producer in recursion",
      "I64 F(I64 n){U8 *s=\"0\";(*s)++;if(n)return F(n-1);return *s-48;}F(41);",
      42L );
    ( "different equal-text producers",
      "I64 F(){U8 *a=\"*\",*b=\"*\";*a=0;return *b;}F();",
      42L );
    ( "different function owners",
      "I64 F(){U8 *s=\"*\";*s=1;return 0;}I64 G(){U8 *s=\"*\";return \
       *s;}F();G();",
      42L );
    ( "embedded NUL",
      "I64 F(){U8 *s=\"A\\0B\";return s[0]*10000+s[1]*100+s[2]+s[3];}F();",
      650066L );
    ( "adjacent literal concatenation",
      "I64 F(){U8 *s=\"A\" \"\\0B\";return s[0]*10000+s[1]*100+s[2]+s[3];}F();",
      650066L );
    ( "terminator is writable",
      "I64 F(){U8 *s=\"A\";s[1]=42;return s[1];}F();",
      42L );
    ( "byte wrap",
      "I64 F(){U8 *s=\"*\";s[0]=255;s[0]++;return s[0]+42;}F();",
      42L );
    ( "fixed pointer parameter",
      "I64 Add(U8 *p){*p+=2;return *p;}Add(\"(\");",
      42L );
    ( "pointer alias capture",
      "I64 F(){U8 *s=\"ab\",*p,*q;I64 i=0;while(i<2){U8 *r=&s[i];if(i)q=r;else \
       p=r;i++;}*p=40;*q=2;return s[0]+s[1];}F();",
      42L );
    ( "one-past reference",
      "I64 F(){U8 *s=\"*\";U8 *p=&s[2];return p[-2];}F();",
      42L );
    ( "mixed global and literal arenas",
      "I8 G;I64 F(){static U8 a[2];U8 *s=\"(\";G=2;a[0]=s[0];return \
       a[0]+G;}F();",
      42L );
  ]

let source_gates () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, expected) ->
          let native = compare mode expected source in
          Alcotest.(check bool)
            (label ^ " has owned literal bytes")
            true
            (Program.literal_bytes native.image > 0);
          List.iter
            (fun abi ->
              let session, config, source = inputs mode source in
              let image =
                Native_program.compile ~status_abi:abi session ~config ~source
                |> require_ok diagnostics_text
              in
              Alcotest.(check bool)
                "requested ABI image" true
                (Program.status_abi image.value = abi))
            [ Program.Windows_x64; Program.System_v_x64 ])
        sources)
    modes

let fresh_images () =
  List.iter
    (fun mode ->
      let native =
        compare mode 2L "I64 F(){U8 *s=\"0\";return ++s[0]-48;}F();F();"
      in
      for _ = 1 to 3 do
        match
          Runtime.execute ~max_steps:10000 native.image |> require_ok Fun.id
        with
        | Program.Completed execution -> native_value 2L execution.final_value
        | _ -> Alcotest.fail "fresh literal image faulted"
      done;
      let exported = Program.global_image native.image in
      let expected = Bytes.to_string (Bytes.of_string exported) in
      Bytes.fill
        (Bytes.unsafe_of_string exported)
        0 (String.length exported) '\255';
      Alcotest.(check string)
        "exported byte image is independent" expected
        (Program.global_image native.image);
      let faulting =
        report mode
          "I64 F(){U8 *s=\"(\";if(s[0]!=40)return 42;s[0]++;return 1/0;}F();"
      in
      let image = Native_program.image faulting |> Option.get in
      for _ = 1 to 3 do
        match Runtime.execute ~max_steps:10000 image |> require_ok Fun.id with
        | Program.Fault fault when fault.kind = Program.Division_by_zero -> ()
        | _ -> Alcotest.fail "fault unwind retained previous literal mutations"
      done)
    modes

let bounds_and_fault_order () =
  let cases =
    [
      ("(\"*\")[-1];", Program.Address_out_of_bounds);
      ("(\"*\")[2];", Program.Address_out_of_bounds);
      ("I64 F(){U8 *s=\"*\";return s[2];}F();", Program.Address_out_of_bounds);
      ("I64 F(){U8 *s=\"*\";s[2]=1/0;return 0;}F();", Program.Division_by_zero);
      ( "I64 F(){U8 *s=\"*\";s[2]+=0;return 0;}F();",
        Program.Address_out_of_bounds );
      ( "I64 F(){U8 *s=\"*\",*p=&s[3];return 0;}F();",
        Program.Address_out_of_bounds );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) ->
          let result = report mode source in
          let fault =
            match Native_program.native_outcome result with
            | Some (Program.Fault fault) -> fault
            | _ -> Alcotest.fail "literal fault did not reach native execution"
          in
          Alcotest.(check bool)
            "expected fault kind" true (fault.kind = expected);
          let error =
            match
              Native_scalar_fixture.execute ~max_steps:10000
                (fixture mode source)
            with
            | Error (error :: _) -> error
            | _ -> Alcotest.fail "literal checked batch did not fault"
          in
          Alcotest.(check int)
            "fault work" error.executed_steps fault.executed_steps;
          Alcotest.(check (option int))
            "original fault instruction" error.instruction_id
            (Some fault.instruction_id))
        cases)
    modes

let literal_limits () =
  List.iter
    (fun mode ->
      let source = "(\"41\")[0]-10;" in
      let native =
        Native_program.outcome
          (report ~max_literal_bytes:3 ~max_global_bytes:1 mode source)
        |> require_ok diagnostics_text
      in
      native_value 42L native.value.execution.final_value;
      Alcotest.(check int)
        "literal includes one terminator" 3
        (Program.literal_bytes native.value.image);
      Alcotest.(check int)
        "literal does not consume global quota" 0
        (Program.global_bytes native.value.image);
      Alcotest.(check int)
        "data and metadata account for the entire arena"
        (String.length (Program.global_image native.value.image))
        (Program.global_bytes native.value.image
        + Program.literal_bytes native.value.image
        + Program.arena_metadata_bytes native.value.image);
      Alcotest.(check bool)
        "one-below compile quota rejects before native entry" true
        (Option.is_none
           (Native_program.image (report ~max_literal_bytes:2 mode source)));
      (match
         Runtime.execute ~max_literal_bytes:2 ~max_steps:10000
           native.value.image
       with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "runtime admitted one-below literal quota");
      (match
         Runtime.execute ~max_literal_bytes:3 ~max_steps:10000
           native.value.image
         |> require_ok Fun.id
       with
      | Program.Completed execution -> native_value 42L execution.final_value
      | _ -> Alcotest.fail "exact literal quota failed");
      let long_source =
        "I64 F(){U8 *s=\"" ^ String.make 2048 '*'
        ^ "\";return s[2047]+s[2048];}F();"
      in
      let long_native = compare mode 42L long_source in
      Alcotest.(check bool)
        "literal tables do not consume the automatic frame" true
        (Program.frame_bytes long_native.image < 4088);
      Alcotest.(check int)
        "full long literal extent" 2049
        (Program.literal_bytes long_native.image);
      let unused = "I64 Unused(){U8 *s=\"1234\";return *s;}42;" in
      Alcotest.(check bool)
        "unreachable original producer still consumes literal quota" true
        (Option.is_none
           (Native_program.image (report ~max_literal_bytes:4 mode unused))))
    modes

let () =
  if Runtime.platform () = Runtime.Unsupported then
    failwith "native literal tests require x86-64";
  Alcotest.run "Native owned literals"
    [
      ( "literals",
        [
          Alcotest.test_case "owned mutable source gates" `Quick source_gates;
          Alcotest.test_case "fresh execution and fault images" `Quick
            fresh_images;
          Alcotest.test_case "bounds and original fault phase" `Quick
            bounds_and_fault_order;
          Alcotest.test_case "literal and arena quotas" `Quick literal_limits;
        ] );
    ]
