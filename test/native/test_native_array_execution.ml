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

let source_inputs ~mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-array-execution.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let native_report ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes
    ?max_dimension_work ?max_global_bytes ?status_abi ~mode ~max_steps contents
    =
  let session, config, source = source_inputs ~mode contents in
  Native_program.evaluate ?max_frame_bytes ?max_call_depth
    ?max_active_stack_bytes ?max_dimension_work ?max_global_bytes ?status_abi
    session ~config ~source ~max_steps

let native_success ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes
    ?max_dimension_work ?max_global_bytes ?status_abi ~mode ~max_steps contents
    =
  let report =
    native_report ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes
      ?max_dimension_work ?max_global_bytes ?status_abi ~mode ~max_steps
      contents
  in
  match Native_program.outcome report with
  | Ok checked -> (report, checked.value)
  | Error diagnostics -> Alcotest.fail (diagnostics_text diagnostics)

let native_fault ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes
    ?max_dimension_work ?max_global_bytes ~mode ~max_steps contents =
  let report =
    native_report ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes
      ?max_dimension_work ?max_global_bytes ~mode ~max_steps contents
  in
  let diagnostics =
    match Native_program.outcome report with
    | Ok _ -> Alcotest.fail "native array source unexpectedly completed"
    | Error diagnostics -> diagnostics
  in
  let fault =
    match Native_program.native_outcome report with
    | Some (Program.Fault fault) -> fault
    | Some (Program.Completed _) ->
        Alcotest.fail "native array source reported an error after completion"
    | None -> Alcotest.fail "native array source failed before native entry"
  in
  (report, fault, diagnostics)

let native_image ?status_abi ~mode contents =
  let session, config, source = source_inputs ~mode contents in
  Native_program.compile ?status_abi session ~config ~source
  |> require_ok diagnostics_text
  |> fun checked -> checked.value

let vm_success ~mode ~max_steps contents =
  let session, config, source = source_inputs ~mode contents in
  run_integer_program session ~config ~source ~max_steps
  |> require_ok diagnostics_text
  |> fun checked -> checked.value

let batch_fixture ~mode contents =
  Native_scalar_fixture.compile ~mode ~path:"native-array-batch.hc" ~contents ()
  |> require_ok diagnostics_text

let batch_success ?max_frame_bytes ?max_call_depth ~mode ~max_steps contents =
  let fixture = batch_fixture ~mode contents in
  Native_scalar_fixture.execute ?max_frame_bytes ?max_call_depth ~max_steps
    fixture
  |> require_ok vm_errors_text

let batch_failure ?max_frame_bytes ?max_call_depth ~mode ~max_steps contents =
  let fixture = batch_fixture ~mode contents in
  match
    Native_scalar_fixture.execute ?max_frame_bytes ?max_call_depth ~max_steps
      fixture
  with
  | Ok _ -> Alcotest.fail "checked array IR unexpectedly completed"
  | Error [] -> Alcotest.fail "checked array IR returned no error"
  | Error (first :: _) -> first

let program_type_name = function
  | Program.I64 -> "I64"
  | Program.U64 -> "U64"

let vm_type_name = function
  | VM.I64 -> "I64"
  | VM.U64 -> "U64"

let check_native_word label expected_type expected_bits = function
  | None -> Alcotest.failf "%s: missing native final word" label
  | Some (word : Program.word) ->
      Alcotest.(check string)
        (label ^ " native type") expected_type
        (program_type_name word.type_);
      Alcotest.(check int64) (label ^ " native bits") expected_bits word.bits

let check_vm_word label expected_type expected_bits result =
  match VM.final_value result with
  | None -> Alcotest.failf "%s: missing VM final word" label
  | Some word ->
      Alcotest.(check string)
        (label ^ " VM type") expected_type (vm_type_name word.type_);
      Alcotest.(check int64) (label ^ " VM bits") expected_bits word.bits

let compare_source ?(max_steps = 10_000) ~mode ~label ~expected_type
    ~expected_bits contents =
  let interpreted = vm_success ~mode ~max_steps contents in
  check_vm_word label expected_type expected_bits interpreted;
  let batch = batch_success ~mode ~max_steps contents in
  check_vm_word (label ^ " checked batch") expected_type expected_bits batch;
  let report, native = native_success ~mode ~max_steps contents in
  check_native_word label expected_type expected_bits
    native.execution.final_value;
  Alcotest.(check int)
    (label ^ " native work matches checked batch")
    (VM.executed_steps batch) native.execution.executed_steps;
  (report, native, interpreted)

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let gates =
  [
    ("local-elements", "I64 F(){I64 a[2];a[0]=40;a[1]=2;return a[0]+a[1];}F();");
    ( "caller-element",
      "I64 Set(I64 *p){*p+=2;return 0;}I64 F(){I64 \
       a[2];a[0]=40;Set(&a[0]);return a[0];}F();" );
    ("row-major", "I64 F(){I64 a[2][3];a[1][2]=42;return a[1][2];}F();");
    ( "dynamic-loop",
      "I64 F(){I64 a[3];I64 i=0;while(i<3){a[i]=14;i++;}return \
       a[0]+a[1]+a[2];}F();" );
    ( "pointer-index",
      "I64 F(){I64 a[2];a[0]=40;a[1]=2;I64 *p=a;return p[0]+p[1];}F();" );
    ("cross-row", "I64 F(){I64 a[2][3];a[0][3]=42;return a[1][0];}F();");
    ( "indexed-rhs",
      "I64 Bump(I64 *p){*p+=1;return *p;}I64 F(){I64 a[2];I64 \
       i=0;a[0]=20;a[i++]+=Bump(&a[0]);return a[0]+i-1;}F();" );
    ( "interior-pointer",
      "I64 F(){I64 a[2];a[0]=42;I64 *p=&a[1];return p[-1];}F();" );
  ]

let alias_loop =
  "I64 F(){I64 a[2];I64 *p,*q;I64 i=0;while(i<2){I64 *r=&a[i];if(i)q=r;else \
   p=r;i++;}*p=40;*q=2;return a[0]+a[1];}F();"

let width_rows =
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

let width_source type_name stored adjustment =
  Printf.sprintf
    "I64 F(){%s a[3];a[0]=13;a[2]=29;a[1]=%s;return a[0]+a[1]+a[2]%s;}F();"
    type_name stored adjustment

let all_widths_and_abi_images () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_name, stored, adjustment) ->
          let source = width_source type_name stored adjustment in
          ignore
            (compare_source ~mode
               ~label:(type_name ^ " array storage")
               ~expected_type:"I64" ~expected_bits:42L source);
          List.iter
            (fun abi ->
              let image = native_image ~status_abi:abi ~mode source in
              Alcotest.(check bool)
                (type_name ^ " requested ABI image")
                true
                (Program.status_abi image = abi))
            [ Program.Windows_x64; Program.System_v_x64 ])
        width_rows)
    modes

let update_sources type_name =
  let direct initial update =
    Printf.sprintf
      "I64 F(){%s a[3];a[0]=13;a[2]=29;a[1]=%d;a[1]%s;return \
       a[1]+a[0]+a[2]-42;}F();"
      type_name initial update
  in
  [
    ( "negative interior +=",
      Printf.sprintf
        "I64 F(){%s a[3];a[0]=13;a[2]=29;a[1]=40;%s *p=&a[2];p[-1]+=2;return \
         p[-1]+a[0]+a[2]-42;}F();"
        type_name type_name );
    ( "pointer-index -=",
      Printf.sprintf
        "I64 F(){%s a[3];a[0]=13;a[2]=29;a[1]=44;%s *p=a;p[1]-=2;return \
         a[1]+a[0]+a[2]-42;}F();"
        type_name type_name );
    ("*=", direct 21 "*=2");
    ("/=", direct 84 "/=2");
    ("%=", direct 126 "%=84");
    ("<<=", direct 21 "<<=1");
    (">>=", direct 84 ">>=1");
    ("&=", direct 58 "&=47");
    ("|=", direct 40 "|=2");
    ("^=", direct 40 "^=2");
    ( "prefix ++",
      Printf.sprintf
        "I64 F(){%s a[3];a[0]=13;a[2]=29;a[1]=41;%s *p=&a[2];I64 \
         n=++p[-1];return n+a[1]+a[0]+a[2]-84;}F();"
        type_name type_name );
    ( "prefix --",
      Printf.sprintf
        "I64 F(){%s a[3];a[0]=13;a[2]=29;a[1]=43;I64 n=--a[1];return \
         n+a[1]+a[0]+a[2]-84;}F();"
        type_name );
    ( "postfix ++",
      Printf.sprintf
        "I64 F(){%s a[3];a[0]=13;a[2]=29;a[1]=42;%s *p=a;I64 n=p[1]++;return \
         n+a[1]+a[0]+a[2]-85;}F();"
        type_name type_name );
    ( "postfix --",
      Printf.sprintf
        "I64 F(){%s a[3];a[0]=13;a[2]=29;a[1]=42;I64 n=a[1]--;return \
         n+a[1]+a[0]+a[2]-83;}F();"
        type_name );
  ]

let all_width_array_updates () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_name, _, _) ->
          List.iter
            (fun (label, source) ->
              ignore
                (compare_source ~mode
                   ~label:(type_name ^ " " ^ label)
                   ~expected_type:"I64" ~expected_bits:42L source))
            (update_sources type_name))
        width_rows)
    modes

let call_decay_sources type_name =
  let one_dimensional argument =
    Printf.sprintf
      "I64 Set(%s *p){p[0]=40;p[1]=2;return 0;}I64 F(){%s \
       a[3];a[2]=17;Set(%s);return a[0]+a[1]+a[2]-17;}F();"
      type_name type_name argument
  in
  [
    ("direct array decay", one_dimensional "a");
    ("grouped array decay", one_dimensional "(a)");
    ( "partial-row decay retains full extent",
      Printf.sprintf
        "I64 Set(%s *p){p[-1]=40;p[2]=2;return 0;}I64 F(){%s \
         a[2][3];a[1][0]=11;a[1][1]=13;Set(a[1]);return \
         a[0][2]+a[1][2]+a[1][0]+a[1][1]-24;}F();"
        type_name type_name );
  ]

let all_width_call_decay () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_name, _, _) ->
          List.iter
            (fun (label, source) ->
              ignore
                (compare_source ~mode
                   ~label:(type_name ^ " " ^ label)
                   ~expected_type:"I64" ~expected_bits:42L source))
            (call_decay_sources type_name))
        width_rows)
    modes

let required_source_gates () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) ->
          ignore
            (compare_source ~mode ~label ~expected_type:"I64" ~expected_bits:42L
               source))
        gates;
      ignore
        (compare_source ~mode ~label:"same indexed producer retains aliases"
           ~expected_type:"I64" ~expected_bits:42L alias_loop))
    modes

let grouping_decay_and_restored_addresses () =
  let cases =
    [
      ( "one-dimensional decay",
        "I64 Set(I64 *p){p[0]=42;return 0;}I64 F(){I64 a[2];Set(a);return \
         a[0];}F();" );
      ( "flat multidimensional decay",
        "I64 Set(I64 *p){p[5]=42;return 0;}I64 F(){I64 a[2][3];Set((a));return \
         a[1][2];}F();" );
      ( "partial row decay",
        "I64 F(){I64 a[2][3];I64 *p=a[1];p[2]=42;return a[1][2];}F();" );
      ( "cross-row grouping",
        "I64 F(){I64 a[2][3];a[0][3]=42;return (a[1])[0];}F();" );
      ( "intermediate address leaves object then returns",
        "I64 F(){I64 a[2][3];a[3][-4]=42;return a[1][2];}F();" );
      ( "one-past pointer may index backward",
        "I64 F(){I64 a[2];a[1]=42;I64 *p=&a[2];return p[-1];}F();" );
      ( "bare multidimensional array dereference",
        "I64 F(){I64 a[2][3];*a=42;return *a;}F();" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) ->
          ignore
            (compare_source ~mode ~label ~expected_type:"I64" ~expected_bits:42L
               source))
        cases)
    modes

let side_effect_order_and_calls () =
  let cases =
    [
      ( "index evaluated once",
        "I64 F(){I64 a[2];I64 i=0;a[i++]=i;return 40+a[0]+i;}F();" );
      ( "base snapshot survives index side effect",
        "I64 F(){I64 a[2],b[2];a[0]=40;b[0]=7;I64 \
         *p=a;p[((p=b)[0]=0)+0]+=2;return a[0];}F();" );
      ("compound read follows RHS", List.assoc "indexed-rhs" gates);
      ( "pointer local crosses fixed call",
        "I64 Set(I64 *p){p[-1]=42;return 0;}I64 F(){I64 a[2];I64 \
         *p=&a[2];Set(p);return a[1];}F();" );
      ( "recursive activations retain array ownership",
        "I64 R(I64 n,I64 *p){I64 a[2];a[0]=n;if(n)R(n-1,a);p[0]+=a[0];return \
         0;}I64 F(){I64 a[2];a[0]=39;R(2,a);return a[0];}F();" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) ->
          ignore
            (compare_source ~mode ~label ~expected_type:"I64" ~expected_bits:42L
               source))
        cases)
    modes

let check_fault_against_vm ~mode ~label ~expected_kind ~expected_code source =
  let max_steps = 10_000 in
  let batch = batch_failure ~mode ~max_steps source in
  let report, fault, diagnostics = native_fault ~mode ~max_steps source in
  let diagnostic = List.hd diagnostics in
  Alcotest.(check string) (label ^ " VM/native code") batch.code diagnostic.code;
  Alcotest.(check string)
    (label ^ " expected code") expected_code diagnostic.code;
  Alcotest.(check int)
    (label ^ " exact fault work")
    batch.executed_steps fault.executed_steps;
  Alcotest.(check bool)
    (label ^ " native fault kind")
    true
    (fault.kind = expected_kind);
  (report, fault)

let negative_destination_rhs_phase () =
  List.iter
    (fun mode ->
      let _, rhs_fault =
        check_fault_against_vm ~mode ~label:"RHS faults before negative store"
          ~expected_kind:Program.Division_by_zero ~expected_code:"HCIRVM0009"
          "I64 F(){I64 a[2];a[-1]=1/0;return 42;}F();"
      in
      Alcotest.(check bool)
        "negative destination was retained until after RHS" true
        (rhs_fault.kind = Program.Division_by_zero))
    modes

let unknown_elements_and_fresh_activations () =
  List.iter
    (fun mode ->
      ignore
        (check_fault_against_vm ~mode
           ~label:"recursive per-element unknown state"
           ~expected_kind:Program.Uninitialized_read ~expected_code:"HCIRVM0012"
           "I64 F(I64 n){I64 a[1];if(n){a[0]=42;return F(0);}return a[0];}F(1);");
      let source = "I64 F(){I64 a[2];a[0]=40;a[1]=2;return a[0]+a[1];}F();" in
      let image = native_image ~mode source in
      let batch = batch_success ~mode ~max_steps:10_000 source in
      for run = 1 to 3 do
        match Runtime.execute ~max_steps:10_000 image |> require_ok Fun.id with
        | Program.Completed execution ->
            check_native_word
              (Printf.sprintf "fresh array image run %d" run)
              "I64" 42L execution.final_value;
            Alcotest.(check int)
              "repeated image work is stable" (VM.executed_steps batch)
              execution.executed_steps
        | Program.Fault _ -> Alcotest.fail "fresh array image faulted"
      done)
    modes

let byte unwind index = Char.code unwind.[index]

let callable_frame_bytes label unwind =
  let length = String.length unwind in
  if length <> 8 && length <> 12 then
    Alcotest.failf "%s: unexpected callable unwind length %d" label length;
  let count = byte unwind 2 in
  if count = 1 && byte unwind 4 = 1 && byte unwind 5 lsr 4 = 5 then 0
  else if count = 2 && byte unwind 4 = 11 && byte unwind 5 land 0xf = 2 then
    ((byte unwind 5 lsr 4) + 1) * 8
  else if count = 3 && byte unwind 4 = 11 && byte unwind 5 land 0xf = 1 then
    byte unwind 6 lor (byte unwind 7 lsl 8) * 8
  else Alcotest.failf "%s: malformed callable unwind metadata" label

let one_named_physical_cost image =
  match Program.windows_unwind_functions image with
  | [ _entry; (_, _, unwind) ] ->
      16 + callable_frame_bytes "array function" unwind
  | records ->
      Alcotest.failf "array image has %d unwind records" (List.length records)

let exact_runtime_quotas () =
  let local = List.assoc "local-elements" gates in
  let nested = List.assoc "caller-element" gates in
  List.iter
    (fun mode ->
      let local_batch = batch_success ~mode ~max_steps:10_000 local in
      let local_steps = VM.executed_steps local_batch in
      let _, exact =
        native_success ~mode ~max_steps:local_steps ~max_frame_bytes:16
          ~max_call_depth:1 ~max_global_bytes:1 local
      in
      check_native_word "exact array semantic quotas" "I64" 42L
        exact.execution.final_value;
      let _, frame_fault, frame_diagnostics =
        native_fault ~mode ~max_steps:local_steps ~max_frame_bytes:15
          ~max_call_depth:1 local
      in
      Alcotest.(check bool)
        "array frame one below" true
        (frame_fault.kind = Program.Frame_limit_exceeded);
      Alcotest.(check string)
        "array frame one below code" "HCIRVM0011"
        (List.hd frame_diagnostics).code;
      let _, step_fault, step_diagnostics =
        native_fault ~mode ~max_steps:(local_steps - 1) ~max_frame_bytes:16
          ~max_call_depth:1 local
      in
      Alcotest.(check bool)
        "array step one below" true
        (step_fault.kind = Program.Step_limit_exceeded);
      Alcotest.(check string)
        "array step one below code" "HCIRVM0007" (List.hd step_diagnostics).code;
      let nested_batch = batch_success ~mode ~max_steps:10_000 nested in
      let nested_steps = VM.executed_steps nested_batch in
      ignore
        (native_success ~mode ~max_steps:nested_steps ~max_frame_bytes:24
           ~max_call_depth:2 nested);
      let _, depth_fault, depth_diagnostics =
        native_fault ~mode ~max_steps:nested_steps ~max_frame_bytes:24
          ~max_call_depth:1 nested
      in
      Alcotest.(check bool)
        "array call depth one below" true
        (depth_fault.kind = Program.Call_depth_exceeded);
      Alcotest.(check string)
        "array call depth one below code" "HCIRVM0015"
        (List.hd depth_diagnostics).code;
      let image = native_image ~mode local in
      Alcotest.(check int)
        "automatic arrays do not consume the global arena" 0
        (Program.global_bytes image);
      let physical =
        Program.entry_stack_bytes image + one_named_physical_cost image
      in
      ignore
        (native_success ~mode ~max_steps:local_steps ~max_frame_bytes:16
           ~max_call_depth:1 ~max_active_stack_bytes:physical local);
      let _, stack_fault, stack_diagnostics =
        native_fault ~mode ~max_steps:local_steps ~max_frame_bytes:16
          ~max_call_depth:1 ~max_active_stack_bytes:(physical - 1) local
      in
      Alcotest.(check bool)
        "array physical stack one below" true
        (stack_fault.kind = Program.Native_stack_limit_exceeded);
      Alcotest.(check string)
        "array physical stack one below code" "HCNATIVE0006"
        (List.hd stack_diagnostics).code;
      let dimensions = "I64 F(){I64 a[2][3];a[1][2]=42;return a[1][2];}F();" in
      let exact_dimensions =
        native_report ~mode ~max_steps:10_000 ~max_dimension_work:2 dimensions
      in
      ignore
        (Native_program.outcome exact_dimensions |> require_ok diagnostics_text);
      Alcotest.(check int)
        "two original dimensions consume two work units" 2
        (Native_program.dimension_work exact_dimensions);
      let below =
        native_report ~mode ~max_steps:10_000 ~max_dimension_work:1 dimensions
      in
      Alcotest.(check bool)
        "dimension work one below rejects before native image" true
        (Result.is_error (Native_program.outcome below)
        && Option.is_none (Native_program.image below));
      Alcotest.(check int)
        "dimension work one below retains reached work" 1
        (Native_program.dimension_work below))
    modes

let () =
  match Runtime.platform () with
  | Runtime.Unsupported ->
      Alcotest.fail "native array tests require Windows x86-64 or Linux x86-64"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 ->
      Alcotest.run "holyc native arrays"
        [
          ( "native arrays",
            [
              Alcotest.test_case "all scalar widths and both ABI images" `Quick
                all_widths_and_abi_images;
              Alcotest.test_case
                "all-width indexed compound and prefix/postfix updates" `Quick
                all_width_array_updates;
              Alcotest.test_case
                "all-width direct grouped and partial-row call decay" `Quick
                all_width_call_decay;
              Alcotest.test_case "eight #613 gates and stable loop aliases"
                `Quick required_source_gates;
              Alcotest.test_case
                "flat grouping decay and restored intermediate addresses" `Quick
                grouping_decay_and_restored_addresses;
              Alcotest.test_case
                "base index RHS order fixed calls and recursive ownership"
                `Quick side_effect_order_and_calls;
              Alcotest.test_case
                "negative destination is retained while RHS faults" `Quick
                negative_destination_rhs_phase;
              Alcotest.test_case
                "per-element unknown state and fresh repeated images" `Quick
                unknown_elements_and_fresh_activations;
              Alcotest.test_case
                "semantic physical dimension and execution quotas are exact"
                `Quick exact_runtime_quotas;
            ] );
        ]
