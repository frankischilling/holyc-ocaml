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

let source_inputs ~mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-scalar-function-execution.hc"
      ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let native_report ?max_initializer_steps ?max_default_bytes ?max_frame_bytes
    ?max_call_depth ?max_active_stack_bytes ~mode ~max_steps contents =
  let session, config, source = source_inputs ~mode contents in
  Native_program.evaluate ?max_initializer_steps ?max_default_bytes
    ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes session ~config
    ~source ~max_steps

let native_success_report ?max_initializer_steps ?max_default_bytes
    ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes ~mode ~max_steps
    contents =
  let report =
    native_report ?max_initializer_steps ?max_default_bytes ?max_frame_bytes
      ?max_call_depth ?max_active_stack_bytes ~mode ~max_steps contents
  in
  match Native_program.outcome report with
  | Ok checked -> (report, checked.value)
  | Error diagnostics -> Alcotest.fail (diagnostics_text diagnostics)

let native_fault ?max_initializer_steps ?max_default_bytes ?max_frame_bytes
    ?max_call_depth ?max_active_stack_bytes ~mode ~max_steps contents =
  let report =
    native_report ?max_initializer_steps ?max_default_bytes ?max_frame_bytes
      ?max_call_depth ?max_active_stack_bytes ~mode ~max_steps contents
  in
  let diagnostics =
    match Native_program.outcome report with
    | Ok _ -> Alcotest.fail "native source unexpectedly completed"
    | Error diagnostics -> diagnostics
  in
  let fault =
    match Native_program.native_outcome report with
    | Some (Program.Fault fault) -> Some fault
    | Some (Program.Completed _) ->
        Alcotest.fail
          "native source reported an error after successful execution"
    | None -> None
  in
  (report, fault, diagnostics)

let native_image ~mode contents =
  let session, config, source = source_inputs ~mode contents in
  Native_program.compile session ~config ~source |> require_ok diagnostics_text
  |> fun checked -> checked.value

let vm_success ?max_frame_bytes ?max_call_depth ~mode ~max_steps contents =
  let session, config, source = source_inputs ~mode contents in
  run_integer_program ?max_frame_bytes ?max_call_depth session ~config ~source
    ~max_steps
  |> require_ok diagnostics_text
  |> fun checked -> checked.value

let vm_errors_text errors =
  errors
  |> List.map (fun (error : VM.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let batch_fixture ~mode contents =
  Native_scalar_fixture.compile ~mode
    ~path:"native-scalar-function-batch-execution.hc" ~contents ()
  |> require_ok diagnostics_text

let batch_success ?max_frame_bytes ?max_call_depth ~mode ~max_steps contents =
  let fixture = batch_fixture ~mode contents in
  let execution =
    Native_scalar_fixture.execute ?max_frame_bytes ?max_call_depth ~max_steps
      fixture
    |> require_ok vm_errors_text
  in
  (fixture, execution)

let batch_failure ?max_frame_bytes ?max_call_depth ~mode ~max_steps contents =
  let fixture = batch_fixture ~mode contents in
  match
    Native_scalar_fixture.execute ?max_frame_bytes ?max_call_depth ~max_steps
      fixture
  with
  | Ok _ -> Alcotest.fail "checked native-bundle IR unexpectedly completed"
  | Error [] -> Alcotest.fail "checked native-bundle IR returned no error"
  | Error (first :: _) -> (fixture, first)

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
  (* Fresh public source execution is the independent semantic oracle. In JIT,
     defaults can activate a stateful command stream whose bookkeeping is not
     part of the native batch image, so its runtime meter is intentionally not a
     native-work oracle. *)
  let vm = vm_success ~mode ~max_steps contents in
  check_vm_word label expected_type expected_bits vm;
  let fixture, batch = batch_success ~mode ~max_steps contents in
  check_vm_word (label ^ " checked batch") expected_type expected_bits batch;
  let report, native = native_success_report ~mode ~max_steps contents in
  check_native_word label expected_type expected_bits
    native.execution.final_value;
  Alcotest.(check int)
    (label ^ " exact checked-bundle/native runtime work")
    (VM.executed_steps batch) native.execution.executed_steps;
  Alcotest.(check int)
    (label ^ " native preparation matches its isolated batch preparation")
    fixture.preparation_steps
    (Native_program.preparation_steps report);
  Alcotest.(check int)
    (label ^ " native saved defaults match its isolated batch preparation")
    fixture.default_bytes
    (Native_program.default_bytes report);
  (report, native, vm)

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let parameter_rows =
  [
    ("I8", "255", "I64", -1L);
    ("U8", "255", "U64", 255L);
    ("I16", "65535", "I64", -1L);
    ("U16", "65535", "U64", 65535L);
    ("I32", "4294967295", "I64", -1L);
    ("U32", "4294967295", "U64", 4294967295L);
    ("I64", "-1", "I64", -1L);
    ("U64", "0xffffffffffffffff", "U64", -1L);
  ]

let return_rows =
  [
    ("I8", "298", "I64", 298L);
    ("U8", "298", "U64", 298L);
    ("I16", "65578", "I64", 65578L);
    ("U16", "65578", "U64", 65578L);
    ("I32", "4294967338", "I64", 4294967338L);
    ("U32", "4294967338", "U64", 4294967338L);
    ("I64", "9007199254740993", "I64", 9007199254740993L);
    ("U64", "0xffffffffffffffff", "U64", -1L);
  ]

let all_scalar_entries_locals_and_returns () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_name, input, expected_type, expected_bits) ->
          let source =
            Printf.sprintf "%s Echo(%s n){%s saved=n;return saved;}\nEcho(%s);"
              type_name type_name type_name input
          in
          ignore
            (compare_source ~mode
               ~label:(type_name ^ " parameter/local entry")
               ~expected_type ~expected_bits source))
        parameter_rows;
      List.iter
        (fun (type_name, value, expected_type, expected_bits) ->
          let source =
            Printf.sprintf "%s Wide(){return %s;}\nWide();" type_name value
          in
          ignore
            (compare_source ~mode
               ~label:(type_name ^ " full register return")
               ~expected_type ~expected_bits source))
        return_rows)
    modes

let adjacent_storage_assignments_and_updates () =
  let cases =
    [
      ( "adjacent narrow automatic slots",
        "I64 F(){I8 a=1;U8 b=40;I16 c=2;U16 d=1;I32 e=3;U32 \
         f=1;a=-1;c=-1;e=-1;return b+d+f;}\n\
         F();",
        "I64",
        42L );
      ( "plain assignment returns register payload before U8 storage narrowing",
        "U8 F(){U8 n=0;return (n=298);}\nF();",
        "U64",
        298L );
      ( "plain assignment stores only the U8 low byte",
        "U8 F(){U8 n=0;n=298;return n;}\nF();",
        "U64",
        42L );
      ( "compound assignment returns the full register payload",
        "U8 F(){U8 n=250;return (n+=48);}\nF();",
        "U64",
        298L );
      ( "compound assignment stores the normalized payload",
        "U8 F(){U8 n=250;n+=48;return n;}\nF();",
        "U64",
        42L );
      ( "prefix returns the normalized new stored I8 value",
        "I8 F(){I8 n=127;return ++n;}\nF();",
        "I64",
        -128L );
      ( "postfix returns the normalized old stored I8 value",
        "I8 F(){I8 n=127;return n++;}\nF();",
        "I64",
        127L );
      ( "U8 prefix wraps before returning the new stored value",
        "U8 F(){U8 n=255;return ++n;}\nF();",
        "U64",
        0L );
      ( "U8 postfix returns the old value across storage wrap",
        "U8 F(){U8 n=255;return n++;}\nF();",
        "U64",
        255L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, expected_type, expected_bits) ->
          ignore
            (compare_source ~mode ~label ~expected_type ~expected_bits source))
        cases)
    modes

let unsigned_call_computation_classes () =
  let rows =
    [
      ("U8", "I8", -42L);
      ("U16", "I16", 9223372036854775766L);
      ("U32", "I32", 9223372036854775766L);
      ("U64", "I64", 9223372036854775766L);
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (unsigned, signed, expected_bits) ->
          let source =
            Printf.sprintf
              "%s V(){return 84;}%s D(){return 2;}I64 F(){return -V()/D();}\n\
               F();"
              unsigned signed
          in
          ignore
            (compare_source ~mode
               ~label:(unsigned ^ " public call class")
               ~expected_type:"I64" ~expected_bits source))
        rows)
    modes

let default_rows =
  [
    ("I8", "255", "I64", -1L, 3);
    ("U8", "554", "U64", 42L, 3);
    ("I16", "65535", "I64", -1L, 3);
    ("U16", "65578", "U64", 42L, 3);
    ("I32", "4294967295", "I64", -1L, 3);
    ("U32", "4294967338", "U64", 42L, 3);
    (* The literal and unary-minus node are separately prepared. *)
    ("I64", "-1", "I64", -1L, 4);
    ("U64", "0xffffffffffffffff", "U64", -1L, 3);
  ]

let scalar_defaults_prepare_once_and_enter_declared_storage () =
  List.iter
    (fun mode ->
      List.iter
        (fun ( type_name,
               literal,
               expected_type,
               expected_bits,
               preparation_steps ) ->
          let declaration =
            Printf.sprintf "%s Value(%s n=%s){return n;}" type_name type_name
              literal
          in
          let report, _, _ =
            compare_source ~mode
              ~label:(type_name ^ " omitted default")
              ~expected_type ~expected_bits
              (declaration ^ "\nValue();")
          in
          Alcotest.(check int)
            (type_name ^ " exact default preparation work")
            preparation_steps
            (Native_program.preparation_steps report);
          Alcotest.(check int)
            (type_name ^ " one default saves one eight-byte payload")
            8
            (Native_program.default_bytes report))
        default_rows;
      let explicit_report, _, _ =
        compare_source ~mode ~label:"U8 supplied argument with default header"
          ~expected_type:"U64" ~expected_bits:42L
          "U8 Value(U8 n=554){return n;}\nValue(42);"
      in
      Alcotest.(check int)
        "supplied U8 argument does not suppress declaration preparation" 3
        (Native_program.preparation_steps explicit_report);
      Alcotest.(check int)
        "supplied U8 argument retains one saved declaration payload" 8
        (Native_program.default_bytes explicit_report);
      let unused_report, _, _ =
        compare_source ~mode ~label:"unused U8 default-bearing function"
          ~expected_type:"I64" ~expected_bits:42L
          "U8 Value(U8 n=554){return n;}\n42;"
      in
      Alcotest.(check int)
        "unused U8 function still prepares its declaration default" 3
        (Native_program.preparation_steps unused_report);
      Alcotest.(check int)
        "unused U8 function still retains its raw saved payload" 8
        (Native_program.default_bytes unused_report);
      let repeated_report, _, _ =
        compare_source ~mode
          ~label:"repeated U8 omitted calls reuse one default"
          ~expected_type:"U64" ~expected_bits:84L
          "U8 Value(U8 n=554){return n;}\nValue()+Value();"
      in
      Alcotest.(check int)
        "repeated U8 calls prepare their declaration once" 3
        (Native_program.preparation_steps repeated_report);
      Alcotest.(check int)
        "repeated U8 calls reuse one eight-byte saved payload" 8
        (Native_program.default_bytes repeated_report);
      let unused_pair_report, _, _ =
        compare_source ~mode ~label:"unused two-default header prepares once"
          ~expected_type:"I64" ~expected_bits:42L
          "I64 Pair(I64 left=20,U64 right=22){return left+right;}\n42;"
      in
      Alcotest.(check int)
        "unused two-default header performs both constant preparations" 6
        (Native_program.preparation_steps unused_pair_report);
      Alcotest.(check int)
        "unused two-default header retains two saved words" 16
        (Native_program.default_bytes unused_pair_report);
      let mixed_report, _, _ =
        compare_source ~mode ~label:"mixed I64/U64 defaults feed narrow call"
          ~expected_type:"I64" ~expected_bits:42L
          "I8 Narrow(I8 n){return n;}\n\
           I64 Mixed(I64 left=40,U64 right=2){I8 n=Narrow(left+right);return n;}\n\
           Mixed();"
      in
      Alcotest.(check int)
        "two literal defaults have independent six-step preparation" 6
        (Native_program.preparation_steps mixed_report);
      Alcotest.(check int)
        "two defaults retain two eight-byte payloads" 16
        (Native_program.default_bytes mixed_report);
      let exact_report, exact =
        native_success_report ~max_initializer_steps:3 ~max_default_bytes:8
          ~mode ~max_steps:10_000 "U8 Value(U8 n=554){return n;}\nValue();"
      in
      Alcotest.(check int)
        "exact U8 preparation work" 3
        (Native_program.preparation_steps exact_report);
      Alcotest.(check int)
        "exact U8 payload bytes" 8
        (Native_program.default_bytes exact_report);
      check_native_word "exact U8 default quota" "U64" 42L
        exact.execution.final_value;
      let byte_report, byte_fault, byte_diagnostics =
        native_fault ~max_initializer_steps:3 ~max_default_bytes:7 ~mode
          ~max_steps:10_000 "U8 Value(U8 n=554){return n;}\nValue();"
      in
      Alcotest.(check bool)
        "one-below default byte limit fails before native entry" true
        (Option.is_none byte_fault);
      Alcotest.(check string)
        "one-below default byte diagnostic" "HCIRVM0011"
        (List.hd byte_diagnostics).code;
      Alcotest.(check int)
        "byte quota rejects before charging default preparation" 0
        (Native_program.preparation_steps byte_report);
      Alcotest.(check int)
        "byte quota retains no partial payload" 0
        (Native_program.default_bytes byte_report);
      let prep_report, prep_fault, prep_diagnostics =
        native_fault ~max_initializer_steps:2 ~max_default_bytes:8 ~mode
          ~max_steps:10_000 "U8 Value(U8 n=554){return n;}\nValue();"
      in
      Alcotest.(check bool)
        "one-below preparation fails before native entry" true
        (Option.is_none prep_fault);
      Alcotest.(check string)
        "one-below preparation diagnostic" "HCIRVM0007"
        (List.hd prep_diagnostics).code;
      Alcotest.(check int)
        "one-below preparation retains reached work" 2
        (Native_program.preparation_steps prep_report);
      Alcotest.(check int)
        "failed preparation publishes no saved payload" 0
        (Native_program.default_bytes prep_report))
    modes

let u0_control_recursion_and_final_latch () =
  let word_cases =
    [
      ( "U0 fallthrough resumes caller",
        "U0 V(I8 n){I8 saved=n;saved;}\nV(42);42;",
        42L );
      ( "U0 early return resumes caller",
        "U0 V(I16 n){if(n)return;I16 saved=0;saved;}\nV(1);42;",
        42L );
      ( "nested recursive U0 calls restore every caller",
        "U0 R(I32 n){if(n){R(n-1);return;}}\nR(4);42;",
        42L );
      ( "nested word call inside U0 cannot become its return value",
        "I64 Word(){return 7;}U0 V(){Word();}\nV();42;",
        42L );
      ( "U0i spelling retains no-word completion",
        "U0i V(){return;}\nV();42;",
        42L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, bits) ->
          ignore
            (compare_source ~mode ~label ~expected_type:"I64"
               ~expected_bits:bits source))
        word_cases;
      List.iter
        (fun spelling ->
          let source = Printf.sprintf "%s V(){}\n42;V();" spelling in
          let vm = vm_success ~mode ~max_steps:10_000 source in
          Alcotest.(check bool)
            (spelling
           ^ " public VM clears an earlier word after no-value discard")
            true
            (Option.is_none (VM.final_value vm));
          let _, batch = batch_success ~mode ~max_steps:10_000 source in
          let _, native =
            native_success_report ~mode ~max_steps:10_000 source
          in
          Alcotest.(check bool)
            (spelling ^ " native discard clears the final latch")
            true
            (Option.is_none native.execution.final_value);
          Alcotest.(check int)
            (spelling ^ " no-value path has exact batch/native work")
            (VM.executed_steps batch) native.execution.executed_steps)
        [ "U0"; "U0i" ])
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

let named_physical_costs image =
  match Program.windows_unwind_functions image with
  | [] -> Alcotest.fail "callable image has no entry unwind record"
  | _entry :: named ->
      List.mapi
        (fun index (_, _, unwind) ->
          16 + callable_frame_bytes (Printf.sprintf "function %d" index) unwind)
        named

let active_physical_stack_boundaries () =
  let recursive_u0 = "U0 R(I32 n){if(n){R(n-1);return;}}\nR(4);" in
  let mixed =
    "I8 Leaf(I8 n){I16 saved=n;return saved;}\n\
     I64 Outer(U16 n){I32 keep=n;return Leaf(keep);}\n\
     Outer(42);"
  in
  List.iter
    (fun mode ->
      let u0_image = native_image ~mode recursive_u0 in
      let u0_cost =
        match named_physical_costs u0_image with
        | [ cost ] -> cost
        | costs ->
            Alcotest.failf "recursive U0 image has %d named stack records"
              (List.length costs)
      in
      let u0_exact = Program.entry_stack_bytes u0_image + (5 * u0_cost) in
      let _, u0_ok =
        native_success_report ~max_active_stack_bytes:u0_exact ~mode
          ~max_steps:10_000 recursive_u0
      in
      Alcotest.(check bool)
        "recursive U0 exact physical stack completes without a word" true
        (Option.is_none u0_ok.execution.final_value);
      let _, u0_fault, u0_diagnostics =
        native_fault ~max_active_stack_bytes:(u0_exact - 1) ~mode
          ~max_steps:10_000 recursive_u0
      in
      let u0_fault = Option.get u0_fault in
      Alcotest.(check string)
        "recursive U0 one-below physical stack diagnostic" "HCNATIVE0006"
        (List.hd u0_diagnostics).code;
      Alcotest.(check (option string))
        "recursive U0 stack fault stays at its call owner" (Some "R")
        u0_fault.function_name;
      let _, u0_recovered =
        native_success_report ~max_active_stack_bytes:u0_exact ~mode
          ~max_steps:10_000 recursive_u0
      in
      Alcotest.(check bool)
        "fresh recursive U0 call recovers after physical stack fault" true
        (Option.is_none u0_recovered.execution.final_value);
      let mixed_image = native_image ~mode mixed in
      let leaf_cost, outer_cost =
        match named_physical_costs mixed_image with
        | [ leaf; outer ] -> (leaf, outer)
        | costs ->
            Alcotest.failf "mixed narrow image has %d named stack records"
              (List.length costs)
      in
      let mixed_exact =
        Program.entry_stack_bytes mixed_image + outer_cost + leaf_cost
      in
      let _, mixed_ok =
        native_success_report ~max_active_stack_bytes:mixed_exact ~mode
          ~max_steps:10_000 mixed
      in
      check_native_word "mixed narrow exact physical stack" "I64" 42L
        mixed_ok.execution.final_value;
      let _, mixed_fault, mixed_diagnostics =
        native_fault ~max_active_stack_bytes:(mixed_exact - 1) ~mode
          ~max_steps:10_000 mixed
      in
      let mixed_fault = Option.get mixed_fault in
      Alcotest.(check string)
        "mixed narrow one-below physical stack diagnostic" "HCNATIVE0006"
        (List.hd mixed_diagnostics).code;
      Alcotest.(check (option string))
        "mixed narrow stack fault stays at Outer call site" (Some "Outer")
        mixed_fault.function_name;
      let _, mixed_recovered =
        native_success_report ~max_active_stack_bytes:mixed_exact ~mode
          ~max_steps:10_000 mixed
      in
      check_native_word "mixed narrow physical-stack recovery" "I64" 42L
        mixed_recovered.execution.final_value)
    modes

let budgets_faults_and_recovery () =
  let recursion =
    "I8 Recur(I8 n){if(n)return Recur(n-1);return 42;}\nRecur(3);"
  in
  let divide_fault =
    "I16 Leaf(I16 n){I16 x=84;x/=n;return x;}\n\
     I64 Outer(I16 n){return Leaf(n);}\n\
     Outer(0);"
  in
  let healthy =
    "I16 Leaf(I16 n){I16 x=84;x/=n;return x;}\n\
     I64 Outer(I16 n){return Leaf(n);}\n\
     Outer(2);"
  in
  List.iter
    (fun mode ->
      let _, batch = batch_success ~mode ~max_steps:10_000 recursion in
      let steps = VM.executed_steps batch in
      let _, exact =
        native_success_report ~max_frame_bytes:32 ~max_call_depth:4 ~mode
          ~max_steps:steps recursion
      in
      check_native_word "exact recursion budgets" "I64" 42L
        exact.execution.final_value;
      Alcotest.(check int)
        "exact step allowance is derived from the same checked native-bundle IR"
        steps exact.execution.executed_steps;
      let _, batch_step_fault =
        batch_failure ~mode ~max_steps:(steps - 1) recursion
      in
      Alcotest.(check string)
        "checked native-bundle IR has the same one-below step boundary"
        "HCIRVM0007" batch_step_fault.code;
      Alcotest.(check int)
        "checked native-bundle IR consumes the one-below step allowance"
        (steps - 1) batch_step_fault.executed_steps;
      let _, step_fault, step_diagnostics =
        native_fault ~mode ~max_steps:(steps - 1) recursion
      in
      let step_fault = Option.get step_fault in
      Alcotest.(check string)
        "narrow recursion one-below step code" "HCIRVM0007"
        (List.hd step_diagnostics).code;
      Alcotest.(check int)
        "narrow recursion consumes the complete one-below budget" (steps - 1)
        step_fault.executed_steps;
      let _, batch_frame =
        batch_failure ~max_frame_bytes:31 ~max_call_depth:4 ~mode
          ~max_steps:10_000 recursion
      in
      let _, frame_fault, frame_diagnostics =
        native_fault ~max_frame_bytes:31 ~max_call_depth:4 ~mode
          ~max_steps:10_000 recursion
      in
      let frame_fault = Option.get frame_fault in
      Alcotest.(check string)
        "narrow parameter frame one-below matches checked batch IR"
        batch_frame.code (List.hd frame_diagnostics).code;
      Alcotest.(check (option string))
        "frame fault retains recursive owner" (Some "Recur")
        frame_fault.function_name;
      let _, batch_depth =
        batch_failure ~max_frame_bytes:32 ~max_call_depth:3 ~mode
          ~max_steps:10_000 recursion
      in
      let _, depth_fault, depth_diagnostics =
        native_fault ~max_frame_bytes:32 ~max_call_depth:3 ~mode
          ~max_steps:10_000 recursion
      in
      let depth_fault = Option.get depth_fault in
      Alcotest.(check string)
        "narrow recursion call-depth one-below matches checked batch IR"
        batch_depth.code (List.hd depth_diagnostics).code;
      Alcotest.(check (option string))
        "depth fault retains recursive owner" (Some "Recur")
        depth_fault.function_name;
      let _, batch_division =
        batch_failure ~mode ~max_steps:10_000 divide_fault
      in
      let _, division_fault, division_diagnostics =
        native_fault ~mode ~max_steps:10_000 divide_fault
      in
      let division_fault = Option.get division_fault in
      Alcotest.(check string)
        "nested narrow arithmetic fault matches checked batch IR diagnostic"
        batch_division.code (List.hd division_diagnostics).code;
      Alcotest.(check (option string))
        "nested arithmetic fault retains Leaf owner" (Some "Leaf")
        division_fault.function_name;
      ignore
        (compare_source ~mode ~label:"healthy execution after narrow faults"
           ~expected_type:"I64" ~expected_bits:42L healthy))
    modes

let () =
  match Runtime.platform () with
  | Runtime.Unsupported ->
      Alcotest.fail
        "native scalar function tests require Windows x86-64 or Linux x86-64"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 ->
      Alcotest.run "holyc native scalar functions"
        [
          ( "native scalar functions",
            [
              Alcotest.test_case
                "all widths normalize parameters/locals and preserve return \
                 bits"
                `Quick all_scalar_entries_locals_and_returns;
              Alcotest.test_case
                "adjacent storage assignment compound and prefix/postfix \
                 semantics"
                `Quick adjacent_storage_assignments_and_updates;
              Alcotest.test_case "public unsigned call computation classes"
                `Quick unsigned_call_computation_classes;
              Alcotest.test_case
                "all-width defaults prepare once and normalize at parameter \
                 entry"
                `Quick scalar_defaults_prepare_once_and_enter_declared_storage;
              Alcotest.test_case
                "U0 fallthrough early return recursion and final latch" `Quick
                u0_control_recursion_and_final_latch;
              Alcotest.test_case
                "U0 recursion and mixed narrow calls have exact physical stack \
                 bounds"
                `Quick active_physical_stack_boundaries;
              Alcotest.test_case
                "scalar budgets faults unwind and recover against public VM"
                `Quick budgets_faults_and_recovery;
            ] );
        ]
