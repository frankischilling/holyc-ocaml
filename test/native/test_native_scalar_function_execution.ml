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

let compare_source ?(initializer_steps = 0) ?(max_steps = 10_000) ~mode ~label
    ~expected_type ~expected_bits contents =
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
    (fixture.preparation_steps + initializer_steps)
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

let pointer_alias_semantics () =
  let cases =
    [
      ( "unknown object can be addressed and initialized",
        "I64 Set(I64 *p){*p=42;return *p;}I64 F(){I64 x;I64 *p=&x;return \
         Set(p);}F();",
        42L );
      ( "copy rebinding and address cancellation",
        "I64 F(){I64 a=20;I64 b=40;I64 *p=&a;I64 \
         *q=p;p=&b;*q+=2;*(&*p)+=*q;return b-a+2;}F();",
        42L );
      ( "pointer parameter rebind stays local",
        "U0 Set(I64 *p,I64 *q){p=q;*p+=2;}I64 F(){I64 a=9;I64 b=31;I64 \
         *p=&a;Set(p,&b);return *p+b;}F();",
        42L );
      ( "address of parameter retains its own storage",
        "I64 F(I64 x){I64 *p=&x;*p+=2;return x;}F(40);",
        42L );
      ( "compound load follows RHS alias effect",
        "I64 Set(I64 *p){*p=40;return 2;}I64 F(){I64 x=1;I64 \
         *p=&x;*p+=Set(p);return x;}F();",
        42L );
      ( "destination survives RHS pointer rebind",
        "I64 F(){I64 x=40;I64 y=2;I64 *p=&x;*p+=*(p=&y);return x;}F();",
        42L );
      ( "right-to-left arguments observe the right argument's rebind",
        "I64 Both(I64 *first,I64 *second){*first=40;*second=2;return \
         *first+*second;}I64 F(){I64 a=0;I64 b=0;I64 *p=&a;return \
         Both(p,p=&b);}F();",
        4L );
      ( "captured right argument survives left argument rebinding",
        "I64 Both(I64 *first,I64 *second){*first=40;*second=2;return \
         *first+*second;}I64 F(){I64 a=0;I64 b=0;I64 *p=&a;return \
         Both(p=&b,p);}F();",
        42L );
      ( "pointer assignment results retain their own values",
        "I64 Both(I64 *first,I64 *second){*first=40;*second=2;return \
         *first+*second;}I64 F(){I64 a=0;I64 b=0;I64 *p;return \
         Both(p=&a,p=&b);}F();",
        42L );
      ( "pointer parameter values survive nested argument rebinding",
        "I64 Both(I64 *first,I64 *second){*first=40;*second=2;return \
         *first+*second;}I64 Pass(I64 *p,I64 *q){return Both(p=q,p);}I64 \
         F(){I64 a=0;I64 b=0;return Pass(&a,&b);}F();",
        42L );
      ( "indexed base survives pointer rebinding in its index",
        "I64 Zero(I64 *p){return 0;}I64 F(){I64 a[2];a[0]=40;a[1]=2;I64 \
         *p=&a[0];return p[Zero(p=&a[1])]+*p;}F();",
        42L );
      ( "interior base survives index rebinding and staged RHS arguments",
        "I64 Sum(I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 h){return \
         a+b+c+d+e+f+g+h;}I64 Zero(I64 *p){return 0;}I64 F(){I64 \
         a[2];a[0]=1;a[1]=100;I64 \
         *p=&a[1];p[Zero(p=&a[0])-1]+=Sum(2,3,4,5,6,7,8,6);return \
         a[0]+a[1]-100;}F();",
        42L );
      ( "recursive activation addresses stay distinct",
        "I64 R(I64 n,I64 *p){I64 x=n;if(n){R(n-1,&x);*p+=x;}else *p+=1;return \
         0;}I64 F(){I64 x=35;R(3,&x);return x;}F();",
        42L );
      ( "global and static aliases cross function ownership",
        "I64 G=20;U0 Add(I64 *p){*p+=2;}I64 F(){static I64 \
         x=18;Add(&x);Add(&G);return x+G;}F();",
        42L );
      ( "entry global address uses private descriptor frame",
        "I64 G=40;I64 Add(I64 *p){return *p+=2;}Add(&G);",
        42L );
      ( "pointer plus scalar default",
        "I64 Add(I64 *p,I64 n=2){return *p+=n;}I64 F(){I64 x=40;return \
         Add(&x);}F();",
        42L );
      ( "loop goto switch preserve reference slots",
        "I64 F(){I64 x=40;I64 *p=&x;I64 n=0;again:switch(n){case \
         0:++*p;break;case 1:++*p;break;default:return x;}n++;goto again;}F();",
        42L );
      ( "all indirect compound families",
        "I64 F(){I64 x=100;I64 \
         *p=&x;*p/=4;*p%=20;*p*=8;*p-=2;*p|=8;*p&=63;*p^=4;*p<<=1;*p>>=1;return \
         x;}F();",
        42L );
      ( "postfix and prefix results",
        "I64 F(){I64 x=39;I64 *p=&x;I64 a=(*p)++;I64 b=++*p;--*p;(*p)--;return \
         a+b-x+1;}F();",
        42L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, bits) ->
          ignore
            (compare_source ~mode ~label ~expected_type:"I64"
               ~expected_bits:bits
               ~initializer_steps:
                 (if
                    label = "global and static aliases cross function ownership"
                  then 6
                  else if
                    label = "entry global address uses private descriptor frame"
                  then 3
                  else 0)
               source))
        cases;
      List.iter
        (fun (type_name, initial, expected_type, expected_bits) ->
          let source =
            Printf.sprintf
              "%s Read(%s *p){return *p;}%s F(){%s left=13;%s x;%s right=29;%s \
               *p=&x;*p=%s;return Read(&*p)+left+right-42;}F();"
              type_name type_name type_name type_name type_name type_name
              type_name initial
          in
          ignore
            (compare_source ~mode
               ~label:(type_name ^ " reference width")
               ~expected_type ~expected_bits source))
        parameter_rows;
      List.iter
        (fun type_name ->
          let source =
            Printf.sprintf
              "I64 F(){%s x=255;%s *p=&x;I64 old=(*p)++;return ++*p+old;}F();"
              type_name type_name
          in
          let expected_bits = if type_name = "I8" then 0L else 256L in
          ignore
            (compare_source ~mode
               ~label:(type_name ^ " wrapped indirect prefix")
               ~expected_type:"I64" ~expected_bits source))
        [ "I8"; "U8" ])
    modes

let array_update_wrap_results () =
  let rows =
    [
      ("I8", "-128", "127");
      ("U8", "0", "255");
      ("I16", "-32768", "32767");
      ("U16", "0", "65535");
      ("I32", "-2147483648", "2147483647");
      ("U32", "0", "4294967295");
      ("I64", "-9223372036854775808", "9223372036854775807");
      ("U64", "0", "0xffffffffffffffff");
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (type_name, minimum, maximum) ->
          List.iter
            (fun target ->
              List.iter
                (fun (label, initial, expression, returned, stored) ->
                  let source =
                    Printf.sprintf
                      "I64 F(){%s a[3];a[0]=13;a[2]=29;a[1]=%s;%s *p=&a[2];I64 \
                       n=%s;if(n!=%s||a[1]!=%s||a[0]!=13||a[2]!=29)return \
                       0;return 42;}F();"
                      type_name initial type_name expression returned stored
                  in
                  ignore
                    (compare_source ~mode
                       ~label:(type_name ^ " " ^ target ^ " " ^ label)
                       ~expected_type:"I64" ~expected_bits:42L source))
                [
                  ( "wrapping prefix increment",
                    maximum,
                    "++" ^ target,
                    minimum,
                    minimum );
                  ( "wrapping prefix decrement",
                    minimum,
                    "--" ^ target,
                    maximum,
                    maximum );
                  ( "wrapping postfix increment",
                    maximum,
                    target ^ "++",
                    maximum,
                    minimum );
                  ( "wrapping postfix decrement",
                    minimum,
                    target ^ "--",
                    minimum,
                    maximum );
                ])
            [ "a[1]"; "p[-1]" ])
        rows)
    modes

let pointer_faults_and_limits () =
  let recursive =
    "I64 R(I64 n,I64 *p){I64 x=1;if(n)R(n-1,&x);*p+=x;return 0;}I64 F(){I64 \
     x=38;R(3,&x);return x;}F();"
  in
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let _, batch = batch_failure ~mode ~max_steps:10000 source in
          let _, fault, diagnostics =
            native_fault ~mode ~max_steps:10000 source
          in
          let fault = Option.get fault in
          Alcotest.(check string)
            "pointer fault matches VM" batch.code (List.hd diagnostics).code;
          Alcotest.(check int)
            "pointer fault exact work" batch.executed_steps fault.executed_steps)
        [
          "I64 F(){I64 *p;return *p;}F();";
          "I64 F(){I64 x;I64 *p=&x;return *p;}F();";
          "I64 F(){I64 x;I64 *p=&x;return (*p)++;}F();";
          "I64 F(){I64 *p;I64 *q=p;return 42;}F();";
          "I64 F(){I64 x=42;I64 *p=&x;return *p/=0;}F();";
          "I64 F(){I64 x=-9223372036854775808;I64 *p=&x;return *p%=-1;}F();";
        ];
      let image = native_image ~mode recursive in
      let costs = named_physical_costs image in
      let physical =
        Program.entry_stack_bytes image + (4 * List.hd costs) + List.nth costs 1
      in
      let _, batch = batch_success ~mode ~max_steps:10000 recursive in
      let steps = VM.executed_steps batch in
      let _, exact =
        native_success_report ~mode ~max_steps:steps ~max_call_depth:5
          ~max_frame_bytes:104 ~max_active_stack_bytes:physical recursive
      in
      check_native_word "exact reference recursion quotas" "I64" 42L
        exact.execution.final_value;
      List.iter
        (fun (label, get_fault, expected_code) ->
          let _, fault, diagnostics = get_fault () in
          ignore (Option.get fault);
          Alcotest.(check string)
            label expected_code (List.hd diagnostics : Diagnostic.t).code)
        [
          ( "pointer step quota",
            (fun () -> native_fault ~mode ~max_steps:(steps - 1) recursive),
            "HCIRVM0007" );
          ( "pointer frame quota",
            (fun () ->
              native_fault ~mode ~max_steps:steps ~max_frame_bytes:103 recursive),
            "HCIRVM0011" );
          ( "pointer depth quota",
            (fun () ->
              native_fault ~mode ~max_steps:steps ~max_call_depth:4 recursive),
            "HCIRVM0015" );
          ( "pointer physical quota",
            (fun () ->
              native_fault ~mode ~max_steps:steps
                ~max_active_stack_bytes:(physical - 1) recursive),
            "HCNATIVE0006" );
        ];
      for _ = 1 to 3 do
        match Runtime.execute ~max_steps:steps image |> require_ok Fun.id with
        | Program.Completed completed ->
            check_native_word "fresh reference image" "I64" 42L
              completed.final_value
        | Program.Fault _ -> Alcotest.fail "fresh reference image faulted"
      done;
      let persistent =
        native_image ~mode "I64 G=40;I64 Add(I64 *p){return *p+=2;}Add(&G);"
      in
      for _ = 1 to 3 do
        match
          Runtime.execute ~max_steps:1000 persistent |> require_ok Fun.id
        with
        | Program.Completed completed ->
            check_native_word "fresh arena references" "I64" 42L
              completed.final_value
        | Program.Fault _ -> Alcotest.fail "fresh arena reference faulted"
      done)
    modes

let automatic_array_preparation_and_layout () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_, bytes) ->
          ignore
            (compare_source ~mode ~label:("array layout " ^ type_)
               ~expected_type:"I64"
               ~expected_bits:(Int64.of_int ((6 * bytes) + 42))
               (Printf.sprintf
                  "I64 F(){%s a[2][3];I8 marker=42;return \
                   sizeof(a)+marker;}F();"
                  type_)))
        [
          ("I8", 1);
          ("U8", 1);
          ("I16", 2);
          ("U16", 2);
          ("I32", 4);
          ("U32", 4);
          ("I64", 8);
          ("U64", 8);
        ];
      let contents = "I64 F(){I16 a[1+2][7];return sizeof(a);}F();" in
      let run ?max_dimension_work ?max_frame_bytes contents =
        let session, config, source = source_inputs ~mode contents in
        Native_program.evaluate ?max_dimension_work ?max_frame_bytes session
          ~config ~source ~max_steps:1000
      in
      let report = run contents in
      let value =
        Native_program.outcome report |> require_ok diagnostics_text
      in
      check_native_word "array sizeof" "I64" 42L
        value.value.execution.final_value;
      let work = Native_program.dimension_work report in
      Alcotest.(check bool) "nonzero original dimension work" true (work > 1);
      ignore
        (Native_program.outcome (run ~max_dimension_work:work contents)
        |> require_ok diagnostics_text);
      let failed = run ~max_dimension_work:(work - 1) contents in
      Alcotest.(check bool)
        "dimension one below rejects" true
        (Result.is_error (Native_program.outcome failed));
      Alcotest.(check int)
        "dimension failure retains reached work" (work - 1)
        (Native_program.dimension_work failed);
      Alcotest.(check bool)
        "dimension failure prevents entry" true
        (Option.is_none (Native_program.image failed));
      let invalid = run ~max_dimension_work:0 contents in
      Alcotest.(check int)
        "invalid dimension limit prevents parsing" 0
        (Native_program.dimension_work invalid);
      Alcotest.(check bool)
        "invalid dimension limit rejects" true
        (Result.is_error (Native_program.outcome invalid));
      let twice = run (contents ^ "F();") in
      Alcotest.(check int)
        "calls reuse dimensions" work
        (Native_program.dimension_work twice);
      let unused = run "I64 F(){I16 a[1+2][7];return sizeof(a);}42;" in
      Alcotest.(check int)
        "unused function prepares dimensions" work
        (Native_program.dimension_work unused);
      let malformed = run "I64 F(){I16 a[1+2][7;return 42;}F();" in
      Alcotest.(check bool)
        "closing bracket failure" true
        (Result.is_error (Native_program.outcome malformed));
      Alcotest.(check int)
        "lookahead failure retains preparation" work
        (Native_program.dimension_work malformed);
      ignore
        (Native_program.outcome (run ~max_frame_bytes:48 contents)
        |> require_ok diagnostics_text);
      let failed = run ~max_frame_bytes:47 contents in
      (match Native_program.native_outcome failed with
      | Some (Program.Fault { kind = Program.Frame_limit_exceeded; _ }) -> ()
      | _ -> Alcotest.fail "array semantic frame one below did not fault");
      let recursive =
        "I64 F(I64 n){I16 a[3][7];if(n)return F(n-1);return sizeof(a);}F(2);"
      in
      ignore
        (compare_source ~mode ~label:"recursive array layout"
           ~expected_type:"I64" ~expected_bits:42L recursive);
      let image = value.value.image in
      for _ = 1 to 3 do
        match Runtime.execute ~max_steps:1000 image |> require_ok Fun.id with
        | Program.Completed execution ->
            check_native_word "repeated array layout" "I64" 42L
              execution.final_value
        | Program.Fault _ -> Alcotest.fail "repeated array layout faulted"
      done)
    modes

let ordinary_calling_flags_execute () =
  List.iter
    (fun mode ->
      List.iter
        (fun flags ->
          List.iter
            (fun (type_name, literal, expected_type, expected_bits) ->
              ignore
                (compare_source ~mode
                   ~label:(flags ^ " " ^ type_name)
                   ~expected_type ~expected_bits
                   (Printf.sprintf "%s %s Echo(%s n){return n;}Echo(%s);" flags
                      type_name type_name literal)))
            parameter_rows;
          ignore
            (compare_source ~mode
               ~label:(flags ^ " zero arguments")
               ~expected_type:"I64" ~expected_bits:42L
               (flags ^ " I64 Answer(){return 42;}Answer();"));
          ignore
            (compare_source ~mode
               ~label:(flags ^ " saved narrow default")
               ~expected_type:"U64" ~expected_bits:42L
               (flags ^ " U8 Answer(U8 n=554){return n;}Answer();"));
          let _, completed =
            native_success_report ~mode ~max_steps:1000
              (flags ^ " U0 Done(){return;}42;Done();")
          in
          Alcotest.(check bool)
            (flags ^ " U0 completion clears the final word")
            true
            (Option.is_none completed.execution.final_value))
        [
          "argpop";
          "noargpop";
          "argpop noargpop";
          "noargpop argpop";
          "haserrcode";
          "haserrcode argpop";
          "haserrcode noargpop";
          "haserrcode argpop noargpop";
        ];
      List.iter
        (fun flags ->
          ignore
            (compare_source ~mode ~label:(flags ^ " local storage")
               ~expected_type:"I64" ~expected_bits:42L
               ("I64 F(){" ^ flags ^ " I64 n;n=40;return n+2;}F();")))
        [
          "argpop noargpop";
          "interrupt haserrcode public";
          "static argpop";
          "argpop static";
        ];
      ignore
        (compare_source ~mode
           ~label:"mixed cleanup and reverse argument effects"
           ~expected_type:"I64" ~expected_bits:42L
           "argpop I64 Twice(I64 n){return n*2;}\n\
            noargpop I64 Order(I64 a,I64 b){return a*10+b;}\n\
            haserrcode argpop noargpop I64 Outer(){I64 n=0;return \
            Twice(Order(++n,++n));}Outer();"))
    modes

let ordinary_calling_flags_unwind_and_recover () =
  let contents =
    "haserrcode argpop noargpop I64 Walk(I64 n){if(n)return Walk(n-1);return \
     42;}Walk(3);"
  in
  List.iter
    (fun mode ->
      let _, native, _ =
        compare_source ~mode ~label:"ordinary flags recursion"
          ~expected_type:"I64" ~expected_bits:42L contents
      in
      let steps = native.execution.executed_steps in
      let image = native.image in
      let physical =
        match named_physical_costs image with
        | [ cost ] -> Program.entry_stack_bytes image + (4 * cost)
        | _ -> Alcotest.fail "ordinary recursive image has unexpected functions"
      in
      let _, exact =
        native_success_report ~max_frame_bytes:32 ~max_call_depth:4
          ~max_active_stack_bytes:physical ~mode ~max_steps:steps contents
      in
      check_native_word "ordinary flags exact resource limits" "I64" 42L
        exact.execution.final_value;
      List.iter
        (fun (frame, depth, stack, budget, expected_kind) ->
          let fault =
            match
              Runtime.execute ~max_frame_bytes:frame ~max_call_depth:depth
                ~max_active_stack_bytes:stack ~max_steps:budget image
              |> require_ok Fun.id
            with
            | Program.Fault fault -> fault
            | Program.Completed _ ->
                Alcotest.fail "flagged image exceeded a resource limit"
          in
          Alcotest.(check bool)
            "ordinary flagged recursion faults at its selected bound" true
            (fault.kind = expected_kind);
          if expected_kind <> Program.Step_limit_exceeded then
            Alcotest.(check (option string))
              "ordinary flagged recursion retains its fault owner" (Some "Walk")
              fault.function_name;
          match
            Runtime.execute ~max_steps:steps ~max_frame_bytes:32
              ~max_call_depth:4 ~max_active_stack_bytes:physical image
            |> require_ok Fun.id
          with
          | Program.Completed execution ->
              check_native_word "original image recovers after flagged fault"
                "I64" 42L execution.final_value
          | Program.Fault _ ->
              Alcotest.fail "original flagged image did not recover")
        [
          (31, 4, physical, steps, Program.Frame_limit_exceeded);
          (32, 3, physical, steps, Program.Call_depth_exceeded);
          (32, 4, physical - 1, steps, Program.Native_stack_limit_exceeded);
          (32, 4, physical, steps - 1, Program.Step_limit_exceeded);
        ])
    modes

let callback_storage_executes () =
  let cases =
    [
      ( "global scalar survives a call",
        "I64 (*G)(I64 n);I64 Add(I64 n){return n+2;}U0 Set(){G=&Add;}I64 \
         Run(){Set();return G(40);}Run();",
        42L );
      ( "static scalar survives activations",
        "I64 Add(I64 n){return n+2;}I64 Run(I64 save){static I64 (*p)(I64 \
         n);if(save)p=&Add;return p(40);}Run(1);Run(0);",
        42L );
      ( "global arrays copy through parameters",
        "I64 (*G)(I64 n)[2][3];I64 Add(I64 n){return n+2;}I64 Apply(I64 \
         (*p)(I64 n)){return p(40);}I64 \
         Run(){G[1][2]=&Add;G[0][1]=G[1][2];G[1][2]=123;return \
         Apply(G[0][1]);}Run();",
        42L );
      ( "static array survives activations",
        "I64 Add(I64 n){return n+2;}I64 Run(I64 save){static I64 (*p)(I64 \
         n)[2][3];if(save)p[1][2]=&Add;return p[1][2](40);}Run(1);Run(0);",
        42L );
      ( "automatic callback array uses exact element",
        "I64 A(I64 n){return n+1;}I64 B(I64 n){return n+2;}I64 Run(){I64 \
         (*p)(I64 n)[2][3];p[0][0]=&A;p[1][2]=&B;return p[1][2](40);}Run();",
        42L );
      ( "all storage owners transfer through a cycle",
        "I64 (*G)(I64 n)[2];I64 Add(I64 n){return n+2;}I64 Run(){static I64 \
         (*s)(I64 n)[2];I64 (*a)(I64 n)[2],(*p)(I64 \
         n);G[1]=&Add;s[0]=G[1];a[1]=s[0];p=a[1];G[0]=p;s[1]=G[0];G[1]=123;return \
         s[1](40);}Run();",
        42L );
      ( "array snapshot precedes argument overwrite",
        "I64 Add(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 \
         n)[2];p[1]=&Add;return p[1](p[1]=123);}Run();",
        125L );
      ( "global snapshot precedes argument overwrite",
        "I64 (*G)(I64 n);I64 Add(I64 n){return n+2;}I64 Run(){G=&Add;return \
         G(G=123);}Run();",
        125L );
      ( "indexed callee effects precede reverse arguments",
        "I64 Take(I64 a,I64 b){return a*10+b;}I64 Run(){I64 n=0;I64 (*p)(I64 \
         a,I64 b)[2];p[1]=&Take;return p[++n](++n,++n);}Run();",
        32L );
      ( "flat multidimensional callback indexing",
        "I64 Add(){return 42;}I64 Run(){I64 (*p)()[2][3];p[1][2]=&Add;return \
         p[2][-1]();}Run();",
        42L );
      ( "loop overwrites owner for every element",
        "I64 A(){return 20;}I64 B(){return 22;}I64 Run(){I64 (*p)()[2];I64 \
         i;for(i=0;i<2;i++)p[i]=&A;p[1]=&B;return p[0]()+p[1]();}Run();",
        42L );
      ( "numeric array element equality",
        "I64 Run(){I64 (*p)()[2];p[0]=123;p[1]=p[0];return \
         (p[0]==p[1])*42;}Run();",
        42L );
      ( "U0 persistent callback cells",
        "U0 (*G)()[2];U0 Done(){return;}I64 Run(){static U0 (*s)()[2];U0 \
         (*a)()[2];G[1]=&Done;s[0]=G[1];a[1]=s[0];a[1]();return 42;}Run();",
        42L );
      ( "global callback with explicit cleanup",
        "noargpop I64 (*G)(I64 n)[2];noargpop I64 Add(I64 n){return \
         n+2;}G[1]=&Add;G[1](40);",
        42L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, bits) ->
          let _, native, _ =
            compare_source ~mode ~label ~expected_type:"I64" ~expected_bits:bits
              source
          in
          match
            Runtime.execute ~max_steps:10000 native.image |> require_ok Fun.id
          with
          | Program.Completed execution ->
              check_native_word (label ^ " fresh image") "I64" bits
                execution.final_value
          | Program.Fault _ ->
              Alcotest.fail (label ^ " second execution faulted"))
        cases;
      List.iter
        (fun storage ->
          List.iter
            (fun (name, literal, expected_type, bits) ->
              let source =
                Printf.sprintf
                  "%s Echo(%s n){return n;}%s Run(){%s %s (*p)(%s \
                   n)[2];p[1]=&Echo;return p[1](%s);}Run();"
                  name name name storage name name literal
              in
              ignore
                (compare_source ~mode
                   ~label:(storage ^ " " ^ name ^ " callback array")
                   ~expected_type ~expected_bits:bits source))
            parameter_rows)
        [ ""; "static" ])
    modes

let callback_storage_faults () =
  let output =
    "extern U0 PutChars(U64 ch);I64 Arg(){PutChars('A');return 40;}"
  in
  let cases =
    [
      ( "numeric global",
        output ^ "I64 (*G)(I64 n);G=123;G(Arg());",
        "HCIRVM0024",
        "A" );
      ( "numeric static array",
        output
        ^ "I64 Run(){static I64 (*p)(I64 n)[2];p[1]=123;return \
           p[1](Arg());}Run();",
        "HCIRVM0024",
        "A" );
      ( "array numeric overwrite",
        output
        ^ "I64 Add(I64 n){return n;}I64 Run(){I64 (*p)(I64 \
           n)[2];p[1]=&Add;p[1]=123;return p[1](Arg());}Run();",
        "HCIRVM0024",
        "A" );
      ( "array mismatch",
        output
        ^ "U64 Bad(I64 n){return n;}I64 Run(){I64 (*p)(I64 \
           n)[2];p[1]=&Bad;return p[1](Arg());}Run();",
        "HCIRVM0014",
        "A" );
      ( "global mismatch",
        output ^ "I64 (*G)(I64 n);U64 Bad(I64 n){return n;}G=&Bad;G(Arg());",
        "HCIRVM0014",
        "A" );
      ( "array bounds before arguments",
        output ^ "I64 Run(){I64 (*p)(I64 n)[2];return p[2](Arg());}Run();",
        "HCIRVM0019",
        "" );
      ( "uninitialized automatic element",
        output
        ^ "I64 A(I64 n){return n;}I64 Run(){I64 (*p)(I64 n)[2];p[0]=&A;return \
           p[1](Arg());}Run();",
        "HCIRVM0012",
        "" );
    ]
  in
  List.iter
    (fun mode ->
      let unknown_code, unknown_output =
        if mode = Preprocessor.Jit then ("HCIRVM0012", "")
        else ("HCIRVM0024", "A")
      in
      let cases =
        cases
        @ [
            ( "initial global",
              output ^ "I64 (*G)(I64 n);G(Arg());",
              unknown_code,
              unknown_output );
            ( "initial static element",
              output
              ^ "I64 Run(){static I64 (*p)(I64 n)[2];return p[1](Arg());}Run();",
              unknown_code,
              unknown_output );
          ]
      in
      List.iter
        (fun (label, source, code, expected_output) ->
          let _, batch = batch_failure ~mode ~max_steps:10000 source in
          let session, config, input = source_inputs ~mode source in
          let public =
            run_integer_program_report session ~config ~source:input
              ~max_steps:10000
          in
          let public_errors =
            match integer_program_report_outcome public with
            | Error errors -> errors
            | Ok _ -> Alcotest.fail label
          in
          let report, fault, errors =
            native_fault ~mode ~max_steps:10000 source
          in
          Alcotest.(check string) (label ^ " batch code") code batch.code;
          Alcotest.(check string)
            (label ^ " public code") code (List.hd public_errors).code;
          Alcotest.(check string)
            (label ^ " native code: " ^ diagnostics_text errors)
            code (List.hd errors).code;
          Alcotest.(check int)
            (label ^ " reached steps") batch.executed_steps
            (Option.get fault).executed_steps;
          Alcotest.(check string)
            (label ^ " native output") expected_output
            (Native_program.output_bytes report);
          Alcotest.(check string)
            (label ^ " public output") expected_output
            (integer_program_report_output_bytes public))
        cases)
    modes

let callback_storage_limits_and_recovery () =
  let source =
    "I64 (*G)(I64 n)[2];I64 Walk(I64 n){static I64 (*s)(I64 n)[2];I64 (*a)(I64 \
     n)[2];G[1]=&Walk;s[1]=G[1];a[1]=s[1];if(n)return a[1](n-1);return \
     42;}Walk(2);"
  in
  List.iter
    (fun mode ->
      let _, native, _ =
        compare_source ~mode ~label:"recursive callback arrays"
          ~expected_type:"I64" ~expected_bits:42L source
      in
      let image = native.image and steps = native.execution.executed_steps in
      let physical =
        Program.entry_stack_bytes image
        + (3 * List.hd (named_physical_costs image))
      in
      let execute frame depth stack budget =
        Runtime.execute ~max_frame_bytes:frame ~max_call_depth:depth
          ~max_active_stack_bytes:stack ~max_global_bytes:32 ~max_steps:budget
          image
        |> require_ok Fun.id
      in
      (match execute 72 3 physical steps with
      | Program.Completed _ -> ()
      | _ -> Alcotest.fail "callback array exact quotas");
      (match Runtime.execute ~max_global_bytes:31 ~max_steps:steps image with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "callback global quota one below");
      List.iter
        (fun (frame, depth, stack, budget, kind) ->
          (match execute frame depth stack budget with
          | Program.Fault fault ->
              Alcotest.(check bool)
                "callback array quota kind" true (fault.kind = kind)
          | _ -> Alcotest.fail "callback array one-below quota completed");
          match execute 72 3 physical steps with
          | Program.Completed execution ->
              check_native_word "callback array image recovers" "I64" 42L
                execution.final_value
          | _ -> Alcotest.fail "callback array recovery fault")
        [
          (71, 3, physical, steps, Program.Frame_limit_exceeded);
          (72, 2, physical, steps, Program.Call_depth_exceeded);
          (72, 3, physical - 1, steps, Program.Native_stack_limit_exceeded);
          (72, 3, physical, steps - 1, Program.Step_limit_exceeded);
        ])
    modes

let owned_local_callbacks_execute () =
  let cases =
    [
      ( "copied callee survives source reset",
        "I64 Add(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n),(*q)(I64 \
         n);p=&Add;q=p;p=0;return q(40);}Run();",
        42L );
      ( "fixed callback parameter enters original body",
        "I64 Add(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 n),I64 n){return \
         p(n);}Apply(&Add,40);",
        42L );
      ( "forwarded parameter and local copy preserve ownership",
        "I64 Add(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 n),I64 n){I64 \
         (*q)(I64 n);q=p;p=123;return q(n);}I64 Forward(I64 (*p)(I64 n),I64 \
         n){return Apply(p,n);}Forward(&Add,40);",
        42L );
      ( "independent callback parameter lanes",
        "I64 A(I64 n){return n+1;}I64 B(I64 n){return n+2;}I64 Both(I64 \
         (*p)(I64 n),I64 n,I64 (*q)(I64 n)){return p(q(n));}Both(&A,39,&B);",
        42L );
      ( "nested direct calls preserve staged owners",
        "I64 Add(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 n),I64 n){return \
         p(n);}Apply(&Add,Apply(&Add,38));",
        42L );
      ( "numeric callback copies keep word equality",
        "I64 Run(){I64 (*p)(),(*q)();p=123;q=p;p=0;return (q!=0)*42;}Run();",
        42L );
      ( "numeric and owned stores overwrite independent tags",
        "I64 Add(){return 42;}I64 Run(){I64 \
         (*p)();p=&Add;p=123;p=0;p=&Add;return p();}Run();",
        42L );
      ( "full word parameter view retains owned body",
        "I64 Add(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 n),I64 n){return \
         p(n);}Apply((&Add)(U64),40);",
        42L );
      ( "void callback parameter",
        "U0 Done(){return;}I64 Apply(U0 (*p)()){p();return 42;}Apply(&Done);",
        42L );
      ( "callee snapshot precedes argument assignment",
        "I64 Add(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n);p=&Add;return \
         p(p=0);}Run();",
        2L );
      ( "numeric argument store cannot replace captured owner",
        "I64 Add(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n);p=&Add;return \
         p(p=123);}Run();",
        125L );
      ( "parameter addresses compare with local original producers",
        "I64 Add(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 n)){return \
         (p==&Add)*42;}Apply(&Add);",
        42L );
      ( "branch selects original body",
        "I64 A(I64 n){return n+1;}I64 B(I64 n){return n+2;}I64 Run(I64 \
         choose){I64 (*p)(I64 n);if(choose)p=&A;else p=&B;return \
         p(40);}Run(1);Run(0);",
        42L );
      ( "callback copy dependencies cross declarations",
        "I64 Add(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n),(*q)(I64 \
         n),(*r)(I64 n);p=&Add;q=p;r=q;q=r;p=0;return r(40);}Run();",
        42L );
      ( "callback arguments retain reverse effects",
        "I64 Take(I64 a,I64 b){return a*10+b;}I64 Run(){I64 n=0;I64 (*p)(I64 \
         a,I64 b);p=&Take;return p(++n,++n);}Run();",
        21L );
      ( "nested callbacks retain independent stages",
        "I64 Add(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n),(*q)(I64 \
         n);p=&Add;q=&Add;return p(q(38));}Run();",
        42L );
      ( "U0 callback completes before numeric return",
        "U0 Add(I64 n){return;}I64 Run(){U0 (*p)(I64 n);p=&Add;p(40);return \
         42;}Run();",
        42L );
      ( "zero argument callback",
        "I64 A(){return 42;}I64 Run(){I64 (*p)();p=&A;return p();}Run();",
        42L );
      ( "owned code equality and inequality",
        "I64 A(){return 1;}I64 B(){return 2;}I64 Run(){I64 \
         (*p)(),(*q)();p=&A;q=&B;return (p==&A)*40+(p!=q)*2;}Run();",
        42L );
      ( "discarded source address preserves later numeric latch",
        "I64 A(){return 1;}&A;42;",
        42L );
      ( "original source addresses compare independently",
        "I64 A(){return 1;}I64 B(){return 2;}(&A==&A)*40+(&A!=&B)*2;",
        42L );
      ( "null callback cell compares without invocation",
        "I64 Run(){I64 (*p)();p=0;return (p==0)*42;}Run();",
        42L );
      ( "full word view retains original address",
        "I64 A(){return 1;}I64 Run(){I64 (*p)();p=(&A)(U64);return \
         (p==&A)*42;}Run();",
        42L );
      ( "callback result stages coexist with intrinsic stages",
        "public _intern 0xA9 I64 Abs(I64 n);I64 Add(I64 n){return n+2;}I64 \
         Run(){I64 (*p)(I64 n);p=&Add;return p(Abs(-40));}Run();",
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
        cases;
      List.iter
        (fun flags ->
          List.iter
            (fun (type_name, literal, expected_type, expected_bits) ->
              let source =
                Printf.sprintf
                  "%s %s Echo(%s n){return n;}%s Run(){%s (*p)(%s \
                   n);p=&Echo;return p(%s);}Run();"
                  flags type_name type_name type_name type_name type_name
                  literal
              in
              ignore
                (compare_source ~mode
                   ~label:(flags ^ " " ^ type_name ^ " callback width")
                   ~expected_type ~expected_bits source))
            parameter_rows)
        [ ""; "argpop"; "haserrcode"; "haserrcode argpop" ];
      ignore
        (compare_source ~mode ~label:"mixed callback widths"
           ~expected_type:"I64" ~expected_bits:42L
           "I64 Add(I8 a,U16 b,U32 c){return a+b+c;}I64 Run(){I64 (*p)(I8 \
            a,U16 b,U32 c);p=&Add;return p(255,65579,4294967296);}Run();");
      List.iter
        (fun flags ->
          List.iter
            (fun (type_name, literal, expected_type, expected_bits) ->
              let source =
                Printf.sprintf
                  "%s %s Echo(%s n){return n;}%s Apply(%s (*p)(%s n),%s \
                   n){return p(n);}Apply(&Echo,%s);"
                  flags type_name type_name type_name type_name type_name
                  type_name literal
              in
              ignore
                (compare_source ~mode
                   ~label:(flags ^ " " ^ type_name ^ " callback parameter")
                   ~expected_type ~expected_bits source))
            parameter_rows)
        [ ""; "argpop"; "haserrcode"; "haserrcode argpop" ])
    modes

let owned_local_callback_faults () =
  let output =
    "extern U0 PutChars(U64 ch);I64 Arg(){PutChars('A');return 40;}"
  in
  let cases =
    [
      ( "null callee faults after argument effects",
        output ^ "I64 Run(){I64 (*p)(I64 n);p=0;return p(Arg());}Run();",
        "HCIRVM0024",
        "A" );
      ( "numeric callee faults after argument effects",
        output ^ "I64 Run(){I64 (*p)(I64 n);p=123;return p(Arg());}Run();",
        "HCIRVM0024",
        "A" );
      ( "numeric copied callee retains no executable authority",
        output
        ^ "I64 Run(){I64 (*p)(I64 n),(*q)(I64 n);p=123;q=p;return \
           q(Arg());}Run();",
        "HCIRVM0024",
        "A" );
      ( "numeric parameter faults after argument effects",
        output ^ "I64 Run(I64 (*p)(I64 n)){return p(Arg());}Run(123);",
        "HCIRVM0024",
        "A" );
      ( "null parameter faults after argument effects",
        output ^ "I64 Run(I64 (*p)(I64 n)){return p(Arg());}Run(0);",
        "HCIRVM0024",
        "A" );
      ( "owned parameter signature mismatch retains output",
        output
        ^ "U64 Bad(I64 n){return n;}I64 Run(I64 (*p)(I64 n)){return \
           p(Arg());}Run(&Bad);",
        "HCIRVM0014",
        "A" );
      ( "numeric overwrite clears executable ownership",
        output
        ^ "I64 Add(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 \
           n);p=&Add;p=123;return p(Arg());}Run();",
        "HCIRVM0024",
        "A" );
      ( "owned code cannot equal a nonzero numeric callback",
        "I64 A(){return 42;}I64 Run(){I64 (*p)(),(*q)();p=&A;q=123;return \
         p==q;}Run();",
        "HCIRVM0024",
        "" );
      ( "wrong arity faults after argument effects",
        output
        ^ "I64 Bad(I64 a,I64 b){return a+b;}I64 Run(){I64 (*p)(I64 \
           n);p=&Bad;return p(Arg());}Run();",
        "HCIRVM0014",
        "A" );
      ( "wrong return faults after argument effects",
        output
        ^ "U64 Bad(I64 n){return n;}I64 Run(){I64 (*p)(I64 n);p=&Bad;return \
           p(Arg());}Run();",
        "HCIRVM0014",
        "A" );
      ( "wrong cleanup faults after argument effects",
        output
        ^ "noargpop I64 Bad(I64 n){return n;}I64 Run(){I64 (*p)(I64 \
           n);p=&Bad;return p(Arg());}Run();",
        "HCIRVM0014",
        "A" );
      ( "uninitialized callee faults before arguments",
        output ^ "I64 Run(){I64 (*p)(I64 n);return p(Arg());}Run();",
        "HCIRVM0012",
        "" );
      ( "callback body preserves arithmetic fault",
        "I64 Bad(I64 n){return 42/n;}I64 Run(){I64 (*p)(I64 n);p=&Bad;return \
         p(0);}Run();",
        "HCIRVM0009",
        "" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, code, output) ->
          let session, config, input = source_inputs ~mode source in
          let public =
            run_integer_program_report session ~config ~source:input
              ~max_steps:10000
          in
          let public_errors =
            match integer_program_report_outcome public with
            | Error errors -> errors
            | Ok _ -> Alcotest.fail label
          in
          Alcotest.(check string)
            (label ^ " public code") code (List.hd public_errors).code;
          let _, batch = batch_failure ~mode ~max_steps:10000 source in
          let report, fault, errors =
            native_fault ~mode ~max_steps:10000 source
          in
          let fault = Option.get fault in
          Alcotest.(check string)
            (label ^ " checked batch code")
            code batch.code;
          Alcotest.(check string)
            (label ^ " native code: " ^ diagnostics_text errors)
            code (List.hd errors).code;
          Alcotest.(check int)
            (label ^ " exact reached steps")
            batch.executed_steps fault.executed_steps;
          Alcotest.(check string)
            (label ^ " reached native output")
            output
            (Native_program.output_bytes report);
          Alcotest.(check string)
            (label ^ " reached public output")
            output
            (integer_program_report_output_bytes public);
          Alcotest.(check (option string))
            (label ^ " fault owner")
            (Some (if code = "HCIRVM0009" then "Bad" else "Run"))
            fault.function_name)
        cases)
    modes

let owned_local_callback_limits_and_recovery () =
  let contents =
    "I64 Walk(I64 n){I64 (*p)(I64 n);p=&Walk;if(n)return p(n-1);return \
     42;}Walk(2);"
  in
  List.iter
    (fun mode ->
      let _, native, _ =
        compare_source ~mode ~label:"callback recursion" ~expected_type:"I64"
          ~expected_bits:42L contents
      in
      let image = native.image and steps = native.execution.executed_steps in
      let physical =
        Program.entry_stack_bytes image
        + (3 * List.hd (named_physical_costs image))
      in
      let execute frame depth stack budget =
        Runtime.execute ~max_frame_bytes:frame ~max_call_depth:depth
          ~max_active_stack_bytes:stack ~max_steps:budget image
        |> require_ok Fun.id
      in
      (match execute 48 3 physical steps with
      | Program.Completed _ -> ()
      | _ -> Alcotest.fail "callback exact resource limits");
      List.iter
        (fun (frame, depth, stack, budget, kind) ->
          let fault =
            match execute frame depth stack budget with
            | Program.Fault fault -> fault
            | _ -> Alcotest.fail "callback one-below limit did not fault"
          in
          Alcotest.(check bool) "callback quota kind" true (fault.kind = kind);
          if kind <> Program.Step_limit_exceeded then
            Alcotest.(check (option string))
              "callback quota owner" (Some "Walk") fault.function_name;
          match execute 48 3 physical steps with
          | Program.Completed execution ->
              check_native_word "same callback image recovers" "I64" 42L
                execution.final_value
          | _ -> Alcotest.fail "same callback image did not recover")
        [
          (47, 3, physical, steps, Program.Frame_limit_exceeded);
          (48, 2, physical, steps, Program.Call_depth_exceeded);
          (48, 3, physical - 1, steps, Program.Native_stack_limit_exceeded);
          (48, 3, physical, steps - 1, Program.Step_limit_exceeded);
        ])
    modes

let callback_parameter_limits_and_recovery () =
  let contents =
    "I64 Add(I64 n){return n+2;}I64 Walk(I64 (*p)(I64 n),I64 n){if(n)return \
     Walk(p,n-1);return p(40);}Walk(&Add,2);"
  in
  List.iter
    (fun mode ->
      let _, native, _ =
        compare_source ~mode ~label:"recursive callback parameter"
          ~expected_type:"I64" ~expected_bits:42L contents
      in
      let image = native.image and steps = native.execution.executed_steps in
      let costs = named_physical_costs image in
      let physical =
        Program.entry_stack_bytes image + List.hd costs + (3 * List.nth costs 1)
      in
      let execute frame depth stack budget =
        Runtime.execute ~max_frame_bytes:frame ~max_call_depth:depth
          ~max_active_stack_bytes:stack ~max_steps:budget image
        |> require_ok Fun.id
      in
      (match execute 56 4 physical steps with
      | Program.Completed _ -> ()
      | _ -> Alcotest.fail "callback parameter exact activation quotas");
      List.iter
        (fun (frame, depth, stack, budget, kind) ->
          (match execute frame depth stack budget with
          | Program.Fault fault ->
              Alcotest.(check bool)
                "parameter quota fault" true (fault.kind = kind)
          | _ ->
              Alcotest.fail "callback parameter one-below quota did not fault");
          match execute 56 4 physical steps with
          | Program.Completed execution ->
              check_native_word "parameter image recovers" "I64" 42L
                execution.final_value
          | _ -> Alcotest.fail "callback parameter image did not recover")
        [
          (55, 4, physical, steps, Program.Frame_limit_exceeded);
          (56, 3, physical, steps, Program.Call_depth_exceeded);
          (56, 4, physical - 1, steps, Program.Native_stack_limit_exceeded);
          (56, 4, physical, steps - 1, Program.Step_limit_exceeded);
        ])
    modes

let callback_defaults_execute_saved_values () =
  let cases =
    [
      ( "callback default differs from target default",
        "I64 Add(I64 n=17){return n;}I64 (*G)(I64 n=42);I64 \
         Run(){G=&Add;return G();}Run();",
        42L );
      ( "callback parameter keeps its own default",
        "I64 Add(I64 n=17){return n;}I64 Apply(I64 (*p)(I64 n=42)){return \
         p();}Apply(&Add);",
        42L );
      ( "copy invokes the destination header default",
        "I64 (*G)(I64 n=17);I64 Add(I64 n){return n;}I64 Run(){I64 (*p)(I64 \
         n=42);G=&Add;p=G;return p();}Run();",
        42L );
      ( "static default survives calls without re-preparation",
        "I64 Add(I64 n){return n;}I64 Run(I64 seed){static I64 (*p)(I64 \
         n=42);if(seed)p=&Add;return p();}Run(1);Run(0);",
        42L );
      ( "repeated anonymous calls reuse saved bits",
        "I64 Add(I64 n){return n;}I64 Run(){I64 (*p)(I64 n=42);p=&Add;return \
         p()+p();}Run();",
        84L );
      ( "defaults preserve full register bits before narrow entry",
        "U8 Add(U8 n){return n;}I64 Run(){U8 (*p)(U8 n=554);p=&Add;return \
         p();}Run();",
        42L );
      ( "two independent defaults and explicit override",
        "I64 Take(I64 a,I64 b){return a+b;}I64 Run(){I64 (*p)(I64 a=17,I64 \
         b=22);p=&Take;return p(20,);}Run();",
        42L );
      ( "index capture precedes explicit argument with omitted first slot",
        "I64 Take(I64 a,I64 b){return a*10+b;}I64 Run(){I64 n=0;I64 (*p)(I64 \
         a=1,I64 b=2)[2];p[1]=&Take;return p[++n](,++n);}Run();",
        12L );
      ( "saved callee precedes an argument overwrite",
        "I64 Take(I64 a,I64 b){return a*10+b;}I64 Run(){I64 (*p)(I64 a=1,I64 \
         b=2);p=&Take;return p(,p=123);}Run();",
        133L );
      ( "unused anonymous declaration still prepares",
        "I64 (*G)(I64 n=42);42;",
        42L );
      ( "unused function callback default still prepares",
        "I64 Run(){I64 (*p)(I64 n=42);return 17;}42;",
        42L );
      ( "original query default prepares",
        "I64 Take(I64 n){return n;}I64 Run(){I64 (*p)(I64 \
         n=sizeof(I64)+34);p=&Take;return p();}Run();",
        42L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, bits) ->
          let _, native, _ =
            compare_source ~mode ~label ~expected_type:"I64" ~expected_bits:bits
              source
          in
          match
            Runtime.execute ~max_steps:10000 native.image |> require_ok Fun.id
          with
          | Program.Completed execution ->
              check_native_word (label ^ " reused image") "I64" bits
                execution.final_value
          | Program.Fault _ -> Alcotest.fail (label ^ " reused image faulted"))
        cases;
      List.iter
        (fun (name, literal, expected_type, bits, steps) ->
          List.iter
            (fun storage ->
              let declaration, body, entry =
                match storage with
                | "global" ->
                    ( Printf.sprintf "%s (*p)(%s n=%s);" name name literal,
                      "p=&Echo;return p();",
                      "Run();" )
                | "global array" ->
                    ( Printf.sprintf "%s (*p)(%s n=%s)[2][3];" name name literal,
                      "p[1][2]=&Echo;return p[1][2]();",
                      "Run();" )
                | "parameter" -> ("", "return p();", "Run(&Echo);")
                | _ ->
                    ( "",
                      Printf.sprintf
                        "%s %s (*p)(%s n=%s)[2][3];p[1][2]=&Echo;return \
                         p[1][2]();"
                        (if storage = "static array" then "static" else "")
                        name name literal,
                      "Run();" )
              in
              let signature =
                if storage = "parameter" then
                  Printf.sprintf "%s (*p)(%s n=%s)" name name literal
                else ""
              in
              let source =
                Printf.sprintf "%s %s Echo(%s n){return n;}%s Run(%s){%s}%s"
                  declaration name name name signature body entry
              in
              let label = storage ^ " " ^ name ^ " saved default" in
              let report, _, _ =
                compare_source ~mode ~label ~expected_type ~expected_bits:bits
                  source
              in
              Alcotest.(check int)
                (label ^ " original preparation")
                steps
                (Native_program.preparation_steps report);
              Alcotest.(check int)
                (label ^ " one saved word")
                8
                (Native_program.default_bytes report))
            [
              "global";
              "global array";
              "automatic array";
              "static array";
              "parameter";
            ])
        default_rows;
      List.iter
        (fun flags ->
          ignore
            (compare_source ~mode
               ~label:(flags ^ " anonymous default cleanup")
               ~expected_type:"I64" ~expected_bits:42L
               (Printf.sprintf
                  "%s I64 (*G)(I64 n=42);%s I64 Echo(I64 n){return \
                   n;}G=&Echo;G();"
                  flags flags)))
        [ "argpop"; "noargpop"; "argpop noargpop"; "haserrcode" ];
      let source =
        "extern U0 PutChars(U64 ch);U0 Done(I64 n){PutChars(n);}I64 Run(){U0 \
         (*p)(I64 n=65);p=&Done;p();return 42;}Run();"
      in
      let report, _, _ =
        compare_source ~mode ~label:"U0 callback saved default"
          ~expected_type:"I64" ~expected_bits:42L source
      in
      Alcotest.(check string)
        "U0 saved argument output" "A"
        (Native_program.output_bytes report))
    modes

let callback_default_fault_order () =
  let prefix =
    "extern U0 PutChars(U64 ch);I64 Arg(){PutChars('A');return 1;}"
  in
  let cases =
    [
      ( "numeric callback default",
        prefix
        ^ "I64 Run(){I64 (*p)(I64 a=42,I64 b=0);p=123;return p(,Arg());}Run();",
        "HCIRVM0024",
        "A" );
      ( "mismatched callback default",
        prefix
        ^ "U64 Bad(I64 a,I64 b){return a+b;}I64 Run(){I64 (*p)(I64 a=42,I64 \
           b=0);p=&Bad;return p(,Arg());}Run();",
        "HCIRVM0014",
        "A" );
      ( "default array bounds precede arguments",
        prefix
        ^ "I64 Run(){I64 (*p)(I64 a=42,I64 b=0)[2];return p[2](,Arg());}Run();",
        "HCIRVM0019",
        "" );
      ( "default uninitialized element precedes arguments",
        prefix
        ^ "I64 Run(){I64 (*p)(I64 a=42,I64 b=0)[2];return p[1](,Arg());}Run();",
        "HCIRVM0012",
        "" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, code, output) ->
          let _, batch = batch_failure ~mode ~max_steps:10000 source in
          let report, fault, errors =
            native_fault ~mode ~max_steps:10000 source
          in
          Alcotest.(check string) (label ^ " batch diagnostic") code batch.code;
          Alcotest.(check string)
            (label ^ " native diagnostic: " ^ diagnostics_text errors)
            code (List.hd errors).code;
          Alcotest.(check int)
            (label ^ " reached work") batch.executed_steps
            (Option.get fault).executed_steps;
          Alcotest.(check string)
            (label ^ " argument output")
            output
            (Native_program.output_bytes report))
        cases)
    modes

let callback_default_preparation_limits () =
  let source =
    "I64 Echo(I64 n=17){return n;}I64 (*G)(I64 n=42);I64 Run(){G=&Echo;return \
     G();}Run();"
  in
  List.iter
    (fun mode ->
      let report, value =
        native_success_report ~max_initializer_steps:6 ~max_default_bytes:16
          ~mode ~max_steps:10000 source
      in
      check_native_word "exact anonymous and named preparation" "I64" 42L
        value.execution.final_value;
      Alcotest.(check int)
        "combined original work" 6
        (Native_program.preparation_steps report);
      Alcotest.(check int)
        "combined saved words" 16
        (Native_program.default_bytes report);
      List.iter
        (fun (work, bytes, code, reached, saved) ->
          let report, fault, errors =
            native_fault ~max_initializer_steps:work ~max_default_bytes:bytes
              ~mode ~max_steps:10000 source
          in
          Alcotest.(check bool)
            "preparation quota prevents native entry" true
            (Option.is_none fault);
          Alcotest.(check string)
            "preparation quota diagnostic" code (List.hd errors).code;
          Alcotest.(check int)
            "preparation quota reached work" reached
            (Native_program.preparation_steps report);
          Alcotest.(check int)
            "preparation quota preserves completed payloads" saved
            (Native_program.default_bytes report))
        [ (6, 15, "HCIRVM0011", 3, 8); (5, 16, "HCIRVM0007", 5, 8) ];
      List.iter
        (fun rejected ->
          let report, fault, errors =
            native_fault ~mode ~max_steps:10000 rejected
          in
          Alcotest.(check bool)
            "unsupported anonymous preparation prevents native entry" true
            (Option.is_none fault);
          Alcotest.(check bool)
            ("source preparation rejects: " ^ diagnostics_text errors)
            true (errors <> []);
          Alcotest.(check int)
            "unsupported default publishes no saved word" 0
            (Native_program.default_bytes report))
        [
          "I64 (*p)(I64 n=lastclass);42;";
          "I64 (*p)(F64 n=1.0);42;";
          "I64 (*p)(I64 n=\"A\");42;";
          "I64 Value(){return 42;}I64 (*p)(I64 n=Value());42;";
          "42;I64 (*p)(I64 n=42);42;";
        ];
      let malformed, fault, errors =
        native_fault ~mode ~max_steps:10000 "I64 (*p)(I64 n=42;42;"
      in
      Alcotest.(check bool)
        "closing failure prevents native entry" true (Option.is_none fault);
      Alcotest.(check bool)
        "closing failure reports parser errors" true (errors <> []);
      Alcotest.(check int)
        "closing failure retains successful saved default" 8
        (Native_program.default_bytes malformed);
      match
        Runtime.execute ~max_steps:10000 value.image |> require_ok Fun.id
      with
      | Program.Completed execution ->
          check_native_word "image recovers after default quota checks" "I64"
            42L execution.final_value
      | Program.Fault _ -> Alcotest.fail "default image did not recover")
    modes

let word_tail_values_and_storage () =
  let cases =
    [
      ("zero", "I64 F(I64 n,...){return n+argc;}F(42);");
      ("only-tail", "I64 F(...){return argv[0]+argv[1]+argc;}F(20,20);");
      ( "mutable-count",
        "I64 F(I64 n,...){argc=100;return argv[0]+argv[1];}F(0,20,22);" );
      ("shrink-count", "I64 F(I64 n,...){argc=0;return argv[1];}F(0,20,42);");
      ( "array-updates",
        "I64 F(I64 n,...){argv[0]+=2;argv[1]++;return \
         ++argv[0]+argv[1];}F(0,17,21);" );
      ( "pointer-alias",
        "I64 F(I64 n,...){I64 *p=argv;argc=99;*(p+1)+=2;return p[1];}F(0,0,40);"
      );
      ( "one-past",
        "I64 F(I64 n,...){I64 *p=&argv[argc];return *(p-1);}F(0,20,42);" );
      ("zero-address", "I64 F(I64 n,...){I64 *p=argv;return argc+n;}F(42);");
      ( "callback-params",
        "I64 Add(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 n),I64 \
         n,...){return p(argv[argc-1]+n);}Apply(&Add,0,17,40);" );
      ( "multiple-owners",
        "I64 A(I64 n){return n+1;}I64 B(I64 n){return n+2;}I64 Apply(I64 \
         (*a)(I64 n),I64 (*b)(I64 n),...){return \
         a(argv[0])+b(argv[1]);}Apply(&A,&B,17,22);" );
      ( "recursive-owners",
        "I64 Add(I64 n){return n+2;}I64 Walk(I64 (*p)(I64 n),I64 \
         n,...){if(n)return Walk(p,n-1,17,40);return \
         p(argv[1]);}Walk(&Add,2,5,6);" );
      ( "var-signature",
        "I64 F(I64 n,...){return n+argv[0]+argc;}I64 Run(){I64 (*p)(I64 \
         n,...);p=&F;return p(20,21);}Run();" );
      ( "captured-overwrite",
        "I64 (*G)(I64 n,...);I64 F(I64 n,...){return n+argv[0];}I64 Bad(I64 \
         n,...){return 7;}I64 Arg(){G=&Bad;return 20;}I64 Run(){G=&F;return \
         G(Arg(),22);}Run();" );
      ( "default-tail",
        "I64 F(I64 n=17,...){return n+argv[0];}I64 Run(){I64 (*p)(I64 \
         n=20,...);p=&F;return p(,22);}Run();" );
      ( "cleanup",
        "argpop I64 F(I64 n,...){return n+argv[0];}argpop I64 (*G)(I64 \
         n,...);I64 Run(){G=&F;return G(20,22);}Run();" );
      ( "void",
        "extern U0 PutChars(U64 ch);U0 F(I64 \
         n,...){PutChars(argv[0]);}F(0,65);42;" );
      ( "argc-address",
        "I64 F(I64 n,...){I64 *p=&argc;*p=100;return argv[1];}F(0,17,42);" );
      ( "large-recursion",
        "I64 F(I64 n,...){if(n)return F(n-1,40,2,3);return \
         argv[0]+argv[1];}F(2,99);" );
      ( "pointer equality preserves the synthetic origin",
        "I64 F(...){I64 *p=argv;I64 *q=&argv[0];if(p!=q)return 0;return \
         p[0];}F(42);" );
      ( "different tail lengths use their own bounds",
        "I64 F(...){I64 *p=argv;return p[argc-1];}F(17);F(1,2,42);" );
      ( "variadic callback parameter inside a variadic function",
        "I64 Sum(I64 n,...){return n+argv[0];}I64 Apply(I64 (*p)(I64 \
         n,...),...){return p(argv[0],argv[1]);}Apply(&Sum,20,22);" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) ->
          let report, _, _ =
            compare_source ~mode ~label ~expected_type:"I64" ~expected_bits:42L
              source
          in
          Alcotest.(check string)
            (label ^ " output")
            (if label = "void" then "A" else "")
            (Native_program.output_bytes report))
        cases;
      List.iter
        (fun (type_, input, expected_type, expected_bits) ->
          List.iter
            (fun callback ->
              let call =
                if callback then
                  type_ ^ " Run(){" ^ type_ ^ " (*p)(" ^ type_
                  ^ " n,...);p=&Echo;return p(" ^ input ^ "," ^ input
                  ^ ");}Run();"
                else "Echo(" ^ input ^ "," ^ input ^ ");"
              in
              let source =
                type_ ^ " Echo(" ^ type_ ^ " n,...){return argv[0];}" ^ call
              in
              (* A tail occupies a complete word. The declared return class does not
           normalize the literal through the fixed parameter's narrow storage. *)
              let bits =
                if type_ = "I8" || type_ = "U8" then 255L
                else if type_ = "I16" || type_ = "U16" then 65535L
                else if type_ = "I32" || type_ = "U32" then 4294967295L
                else expected_bits
              in
              ignore
                (compare_source ~mode
                   ~label:(type_ ^ " complete tail word")
                   ~expected_type ~expected_bits:bits source))
            [ false; true ])
        parameter_rows;
      List.iter
        (fun flags ->
          let source =
            Printf.sprintf
              "%s I64 Sum(I64 n,...){return n+argv[0];}%s I64 (*G)(I64 \
               n,...);I64 Run(){G=&Sum;return G(20,22);}Run();"
              flags flags
          in
          ignore
            (compare_source ~mode
               ~label:(flags ^ " variadic cleanup")
               ~expected_type:"I64" ~expected_bits:42L source))
        [ ""; "argpop"; "noargpop"; "haserrcode" ];
      List.iter
        (fun storage ->
          let source = "I64 Sum(I64 n,...){return n+argv[0];}" ^ storage in
          ignore
            (compare_source ~mode ~label:"variadic retained storage"
               ~expected_type:"I64" ~expected_bits:42L source))
        [
          "I64 (*G)(I64 n,...);I64 Run(){G=&Sum;return G(20,22);}Run();";
          "I64 Run(I64 seed){static I64 (*p)(I64 n,...);if(seed)p=&Sum;return \
           p(20,22);}Run(1);Run(0);";
          "I64 (*G)(I64 n,...)[2];I64 Run(){G[1]=&Sum;return \
           G[1](20,22);}Run();";
          "I64 Apply(I64 (*p)(I64 n,...)){return p(20,22);}Apply(&Sum);";
          "I64 Run(){I64 (*p)(I64 n,...)[2];p[1]=&Sum;return \
           p[1](20,22);}Run();";
        ])
    modes

let word_tail_faults_and_effects () =
  let prefix =
    "extern U0 PutChars(U64 ch);I64 Arg(U64 ch){PutChars(ch);return 14;}"
  in
  let cases =
    [
      ( "bounds-mutable",
        "I64 F(I64 n,...){argc=100;return argv[2];}F(0,20,22);",
        "HCIRVM0019",
        "" );
      ( "negative",
        "I64 F(I64 n,...){return argv[-1];}F(0,42);",
        "HCIRVM0019",
        "" );
      ("empty-read", "I64 F(I64 n,...){return argv[0];}F(42);", "HCIRVM0019", "");
      ( "one-past-read",
        "I64 F(I64 n,...){I64 *p=&argv[argc];return *p;}F(0,42);",
        "HCIRVM0019",
        "" );
      ( "empty-pointer",
        "I64 F(I64 n,...){I64 *p=argv;return *p;}F(42);",
        "HCIRVM0019",
        "" );
      ( "wrong-fixed",
        "extern U0 PutChars(U64 ch);I64 F(I64 n){return n;}I64 \
         A(){PutChars(65);return 20;}I64 Run(){I64 (*p)(I64 n,...);p=&F;return \
         p(A(),22);}Run();",
        "HCIRVM0014",
        "A" );
      ( "wrong-variadic",
        "extern U0 PutChars(U64 ch);I64 F(I64 n,...){return n;}I64 \
         A(){PutChars(65);return 42;}I64 Run(){I64 (*p)(I64 n);p=&F;return \
         p(A());}Run();",
        "HCIRVM0014",
        "A" );
      ( "numeric",
        "extern U0 PutChars(U64 ch);I64 A(){PutChars(65);return 20;}I64 \
         Run(){I64 (*p)(I64 n,...);p=42;return p(A(),22);}Run();",
        "HCIRVM0024",
        "A" );
      ( "zero tail write",
        "I64 F(...){argv[0]=42;return 0;}F();",
        "HCIRVM0019",
        "" );
      ( "zero tail update",
        "I64 F(...){argv[0]++;return 0;}F();",
        "HCIRVM0019",
        "" );
      ( "short activation stays bounded after longer calls",
        "I64 F(...){return argv[1];}F(0,42);F(42);",
        "HCIRVM0019",
        "" );
      ( "null target evaluates fixed and tail arguments",
        prefix
        ^ "I64 Run(){I64 (*p)(I64 n,...);p=0;return \
           p(Arg('A'),Arg('B'),Arg('C'));}Run();",
        "HCIRVM0024",
        "CBA" );
      ( "uninitialized capture precedes tails",
        prefix
        ^ "I64 Run(){I64 (*p)(I64 n,...)[2];return \
           p[1](Arg('A'),Arg('B'));}Run();",
        "HCIRVM0012",
        "" );
      ( "capture bounds precede tails",
        prefix
        ^ "I64 Run(){I64 (*p)(I64 n,...)[2];return \
           p[2](Arg('A'),Arg('B'));}Run();",
        "HCIRVM0019",
        "" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, code, output) ->
          let _, batch = batch_failure ~mode ~max_steps:10000 source in
          let report, fault, errors =
            native_fault ~mode ~max_steps:10000 source
          in
          Alcotest.(check string) (label ^ " batch diagnostic") code batch.code;
          Alcotest.(check string)
            (label ^ " native diagnostic: " ^ diagnostics_text errors)
            code (List.hd errors).code;
          Alcotest.(check int)
            (label ^ " reached work") batch.executed_steps
            (Option.get fault).executed_steps;
          Alcotest.(check string)
            (label ^ " output") output
            (Native_program.output_bytes report))
        cases;
      let source =
        prefix
        ^ "I64 Sum(I64 n,...){return n+argv[0]+argv[1];}I64 Run(){I64 (*p)(I64 \
           n,...);p=&Sum;return p(Arg('A'),Arg('B'),Arg('C'));}Run();"
      in
      let report, _, _ =
        compare_source ~mode ~label:"reverse fixed/tail argument order"
          ~expected_type:"I64" ~expected_bits:42L source
      in
      Alcotest.(check string)
        "tail effects precede fixed effects" "CBA"
        (Native_program.output_bytes report))
    modes

let word_tail_quotas_and_recovery () =
  let source =
    "I64 Add(I64 n){return n+2;}I64 Walk(I64 (*p)(I64 n),I64 \
     n,...){if(n)return Walk(p,n-1,17,40);return p(argv[1]);}Walk(&Add,2,5,6);"
  in
  List.iter
    (fun mode ->
      let _, native, _ =
        compare_source ~mode ~label:"recursive variadic callback owner"
          ~expected_type:"I64" ~expected_bits:42L source
      in
      let image = native.image and steps = native.execution.executed_steps in
      let costs = named_physical_costs image in
      let physical =
        Program.entry_stack_bytes image + List.hd costs + (3 * List.nth costs 1)
      in
      let execute frame depth stack budget =
        Runtime.execute ~max_frame_bytes:frame ~max_call_depth:depth
          ~max_active_stack_bytes:stack ~max_steps:budget image
        |> require_ok Fun.id
      in
      (match execute 128 4 physical steps with
      | Program.Completed _ -> ()
      | _ -> Alcotest.fail "exact word-tail activation quotas");
      List.iter
        (fun (frame, depth, stack, budget, kind) ->
          (match execute frame depth stack budget with
          | Program.Fault fault ->
              Alcotest.(check bool)
                "word-tail quota fault" true (fault.kind = kind)
          | _ -> Alcotest.fail "one-below word-tail quota did not fault");
          match execute 128 4 physical steps with
          | Program.Completed result ->
              check_native_word "word-tail image recovers" "I64" 42L
                result.final_value
          | _ -> Alcotest.fail "word-tail image did not recover")
        [
          (127, 4, physical, steps, Program.Frame_limit_exceeded);
          (128, 3, physical, steps, Program.Call_depth_exceeded);
          (128, 4, physical - 1, steps, Program.Native_stack_limit_exceeded);
          (128, 4, physical, steps - 1, Program.Step_limit_exceeded);
        ];
      let defaults =
        "I64 Sum(I64 n=17,...){return n+argv[0];}I64 Run(){I64 (*p)(I64 \
         n=20,...);p=&Sum;return p(,22);}Run();"
      in
      let report, native =
        native_success_report ~max_initializer_steps:6 ~max_default_bytes:16
          ~mode ~max_steps:10000 defaults
      in
      check_native_word "variadic original saved default" "I64" 42L
        native.execution.final_value;
      Alcotest.(check int)
        "variadic defaults share preparation work" 6
        (Native_program.preparation_steps report);
      Alcotest.(check int)
        "variadic defaults share saved bytes" 16
        (Native_program.default_bytes report))
    modes

let indirect_callback_arguments_execute () =
  let add = "I64 Add(I64 n){return n+2;}" in
  let apply = "I64 Apply(I64 (*cb)(I64 n)){return cb(40);}" in
  let parent = "I64 (*p)(I64 (*cb)(I64 n));" in
  let cases =
    [
      ( "original nested parameter",
        add ^ apply ^ "I64 Run(){" ^ parent ^ "p=&Apply;return p(&Add);}Run();"
      );
      ( "two protected owner lanes",
        add
        ^ "I64 One(I64 n){return n+1;}I64 Apply(I64 (*a)(I64 n),I64 (*b)(I64 \
           n),I64 n){return a(n)+b(n+1);}I64 Run(){I64 (*p)(I64 (*a)(I64 \
           n),I64 (*b)(I64 n),I64 n);p=&Apply;return p(&One,&Add,19);}Run();" );
      ( "indirect forwarding fixed point",
        add ^ apply ^ "I64 Forward(I64 (*cb)(I64 n)){" ^ parent
        ^ "p=&Apply;return p(cb);}I64 Run(){" ^ parent
        ^ "p=&Forward;return p(&Add);}Run();" );
      ( "destination nested header owns default",
        add
        ^ "I64 Apply(I64 (*cb)(I64 n=40)){return cb();}I64 Run(){I64 (*p)(I64 \
           (*cb)(I64 n=12));I64 (*q)(I64 n=10);p=&Apply;q=&Add;return \
           p(q);}Run();" );
      ( "variadic parent retains callback owner",
        add
        ^ "I64 Apply(I64 (*cb)(I64 n),...){argc=99;return cb(argv[1]);}I64 \
           Run(){I64 (*p)(I64 (*cb)(I64 n),...);p=&Apply;return \
           p(&Add,17,40);}Run();" );
      ( "variadic nested callback",
        "I64 Sum(I64 n,...){return n+argv[0];}I64 Apply(I64 (*cb)(I64 \
         n,...)){return cb(20,22);}I64 Run(){I64 (*p)(I64 (*cb)(I64 \
         n,...));p=&Apply;return p(&Sum);}Run();" );
      ( "numeric owner can remain unused",
        "I64 Ignore(I64 (*cb)(I64 n)){return 42;}I64 Run(){" ^ parent
        ^ "p=&Ignore;return p(17);}Run();" );
      ( "null owner can remain unused",
        "I64 Ignore(I64 (*cb)(I64 n)){return 42;}I64 Run(){" ^ parent
        ^ "p=&Ignore;return p(0);}Run();" );
      ( "reverse assignment copies exact owners",
        "I64 A(I64 n){return n+1;}I64 B(I64 n){return n+2;}I64 Apply(I64 \
         (*a)(I64 n),I64 (*b)(I64 n)){return a(19)+b(19);}I64 Run(){I64 \
         (*p)(I64 (*a)(I64 n),I64 (*b)(I64 n));I64 (*q)(I64 \
         n);p=&Apply;q=&A;return p(q,q=&B);}Run();" );
    ]
  in
  let storage =
    [
      "I64 Run(){I64 (*p)(I64 (*cb)(I64 n))[2];I64 (*q)(I64 \
       n)[2];p[1]=&Apply;q[1]=&Add;return p[1](q[1]);}Run();";
      "I64 (*P)(I64 (*cb)(I64 n))[2];I64 (*Q)(I64 n)[2];I64 \
       Run(){P[1]=&Apply;Q[1]=&Add;return P[1](Q[1]);}Run();";
      "I64 Run(I64 seed){static I64 (*p)(I64 (*cb)(I64 n))[2];static I64 \
       (*q)(I64 n)[2];if(seed){p[1]=&Apply;q[1]=&Add;}return \
       p[1](q[1]);}Run(1);Run(0);";
      "I64 Run(I64 (*p)(I64 (*cb)(I64 n)),I64 (*q)(I64 n)){return \
       p(q);}Run(&Apply,&Add);";
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) ->
          ignore
            (compare_source ~mode ~label ~expected_type:"I64" ~expected_bits:42L
               source))
        cases;
      List.iteri
        (fun index source ->
          ignore
            (compare_source ~mode
               ~label:("nested callback storage " ^ string_of_int index)
               ~expected_type:"I64" ~expected_bits:42L
               (add ^ apply ^ source)))
        storage;
      List.iter
        (fun (type_name, literal, _, expected_bits) ->
          List.iter
            (fun flags ->
              let source =
                Printf.sprintf
                  "%s Echo(%s n){return n;}%s I64 Apply(%s (*cb)(%s n),%s \
                   n){return cb(n);}%s I64 (*P)(%s (*cb)(%s n),%s n);I64 \
                   Run(){P=&Apply;return P(&Echo,%s);}Run();"
                  type_name type_name flags type_name type_name type_name flags
                  type_name type_name type_name literal
              in
              (* Apply returns I64, so U64's full bits cross unchanged through
                 the outer signed word. Narrow parameters normalize on entry. *)
              ignore
                (compare_source ~mode
                   ~label:(flags ^ type_name ^ " nested width")
                   ~expected_type:"I64" ~expected_bits source))
            [ ""; "argpop"; "noargpop"; "haserrcode"; "argpop noargpop" ])
        parameter_rows;
      List.iter
        (fun (type_name, literal, expected_type, expected_bits) ->
          let source =
            Printf.sprintf
              "%s Echo(){return %s;}%s Apply(%s (*cb)()){return cb();}%s \
               Run(){%s (*p)(%s (*cb)());p=&Apply;return p(&Echo);}Run();"
              type_name literal type_name type_name type_name type_name
              type_name
          in
          ignore
            (compare_source ~mode
               ~label:(type_name ^ " nested return")
               ~expected_type ~expected_bits source))
        return_rows;
      let source =
        "extern U0 PutChars(U64 ch);U0 Emit(){PutChars('A');}U0 Apply(U0 \
         (*cb)()){cb();}U0 Run(){U0 (*p)(U0 \
         (*cb)());p=&Apply;p(&Emit);}Run();42;"
      in
      let report, _, _ =
        compare_source ~mode ~label:"nested U0 bodies" ~expected_type:"I64"
          ~expected_bits:42L source
      in
      Alcotest.(check string)
        "nested U0 output" "A"
        (Native_program.output_bytes report);
      let source =
        "extern U0 PutChars(U64 ch);I64 (*P)(I64 (*cb)(I64 n),I64 n);I64 \
         Add(I64 n){return n+2;}I64 Bad(I64 (*cb)(I64 n),I64 n){return 0;}I64 \
         Apply(I64 (*cb)(I64 n),I64 n){return cb(n);}I64 \
         Arg(){PutChars('B');P=&Bad;return 40;}I64 Run(){P=&Apply;return \
         P(&Add,Arg());}Run();"
      in
      let report, _, _ =
        compare_source ~mode ~label:"captured parent precedes argument mutation"
          ~expected_type:"I64" ~expected_bits:42L source
      in
      Alcotest.(check string)
        "parent capture output" "B"
        (Native_program.output_bytes report))
    modes

let indirect_callback_argument_faults () =
  let prefix =
    "extern U0 PutChars(U64 ch);I64 Arg(U64 ch){PutChars(ch);return 40;}"
  in
  let add = "I64 Add(I64 n){return n+2;}" in
  let parent = "I64 (*p)(I64 (*cb)(I64 n),I64 n);" in
  let apply = "I64 Apply(I64 (*cb)(I64 n),I64 n){return cb(Arg('A'));}" in
  let cases =
    [
      ( "numeric nested owner",
        apply ^ "I64 Run(){" ^ parent ^ "p=&Apply;return p(17,Arg('B'));}Run();",
        "HCIRVM0024",
        "BA" );
      ( "null nested owner",
        apply ^ "I64 Run(){" ^ parent ^ "p=&Apply;return p(0,Arg('B'));}Run();",
        "HCIRVM0024",
        "BA" );
      ( "nested signature after inner effects",
        "U64 Bad(I64 n){return n;}" ^ apply ^ "I64 Run(){" ^ parent
        ^ "p=&Apply;return p(&Bad,Arg('B'));}Run();",
        "HCIRVM0014",
        "BA" );
      ( "numeric parent after outer effects",
        add ^ "I64 Run(){" ^ parent ^ "p=17;return p(&Add,Arg('B'));}Run();",
        "HCIRVM0024",
        "B" );
      ( "null parent after outer effects",
        add ^ "I64 Run(){" ^ parent ^ "p=0;return p(&Add,Arg('B'));}Run();",
        "HCIRVM0024",
        "B" );
      ( "uninitialized parent before effects",
        add ^ "I64 Run(){" ^ parent ^ "return p(&Add,Arg('B'));}Run();",
        "HCIRVM0012",
        "" );
      ( "parent bounds before effects",
        add ^ apply
        ^ "I64 Run(){I64 (*p)(I64 (*cb)(I64 n),I64 n)[2];p[1]=&Apply;return \
           p[2](&Add,Arg('B'));}Run();",
        "HCIRVM0019",
        "" );
      ( "argument cell faults after rightmost effects",
        apply ^ "I64 Run(){" ^ parent
        ^ "I64 (*q)(I64 n);p=&Apply;return p(q,Arg('B'));}Run();",
        "HCIRVM0012",
        "B" );
      ( "object and callback physical slots cannot share authority",
        add ^ "I64 Object(I64i *cb,I64 n){PutChars('X');return 42;}I64 Run(){"
        ^ parent ^ "p=&Object;return p(&Add,Arg('B'));}Run();",
        "HCIRVM0014",
        "B" );
      ( "nested target wrong cleanup",
        "noargpop I64 Bad(I64 n){return n;}" ^ apply ^ "I64 Run(){" ^ parent
        ^ "p=&Apply;return p(&Bad,Arg('B'));}Run();",
        "HCIRVM0014",
        "BA" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, body, code, output) ->
          let source = prefix ^ body in
          let session, config, input = source_inputs ~mode source in
          let public =
            run_integer_program_report session ~config ~source:input
              ~max_steps:10000
          in
          let errors =
            match integer_program_report_outcome public with
            | Error errors -> errors
            | Ok _ -> Alcotest.fail label
          in
          Alcotest.(check string)
            (label ^ " public fault") code (List.hd errors).code;
          Alcotest.(check string)
            (label ^ " public output") output
            (integer_program_report_output_bytes public);
          let _, batch = batch_failure ~mode ~max_steps:10000 source in
          let report, fault, errors =
            native_fault ~mode ~max_steps:10000 source
          in
          let fault = Option.get fault in
          Alcotest.(check string)
            (label ^ " native fault") code (List.hd errors).code;
          Alcotest.(check string) (label ^ " batch fault") code batch.code;
          Alcotest.(check int)
            (label ^ " original reached work")
            batch.executed_steps fault.executed_steps;
          Alcotest.(check string)
            (label ^ " native output") output
            (Native_program.output_bytes report))
        cases)
    modes

let indirect_callback_argument_quotas () =
  let source =
    "I64 Add(I64 n){return n+2;}I64 Walk(I64 (*cb)(I64 n),I64 \
     depth){if(depth){I64 (*q)(I64 (*x)(I64 n),I64 d);q=&Walk;return \
     q(cb,depth-1);}return cb(40);}I64 Run(){I64 (*p)(I64 (*x)(I64 n),I64 \
     d);p=&Walk;return p(&Add,2);}Run();"
  in
  List.iter
    (fun mode ->
      let _, native, _ =
        compare_source ~mode ~label:"indirect recursive argument ownership"
          ~expected_type:"I64" ~expected_bits:42L source
      in
      let image = native.image and steps = native.execution.executed_steps in
      let costs = named_physical_costs image in
      let physical =
        Program.entry_stack_bytes image
        + List.hd costs
        + (3 * List.nth costs 1)
        + List.nth costs 2
      in
      let execute frame depth stack budget =
        Runtime.execute ~max_frame_bytes:frame ~max_call_depth:depth
          ~max_active_stack_bytes:stack ~max_steps:budget image
        |> require_ok Fun.id
      in
      let recover () =
        match execute 88 5 physical steps with
        | Program.Completed result ->
            check_native_word "nested callback image recovers" "I64" 42L
              result.final_value
        | _ -> Alcotest.fail "exact indirect callback argument limits"
      in
      recover ();
      List.iter
        (fun (frame, depth, stack, budget, kind) ->
          (match execute frame depth stack budget with
          | Program.Fault fault ->
              Alcotest.(check bool)
                "nested callback quota kind" true (fault.kind = kind)
          | _ -> Alcotest.fail "one-below nested callback quota completed");
          recover ())
        [
          (87, 5, physical, steps, Program.Frame_limit_exceeded);
          (88, 4, physical, steps, Program.Call_depth_exceeded);
          (88, 5, physical - 1, steps, Program.Native_stack_limit_exceeded);
          (88, 5, physical, steps - 1, Program.Step_limit_exceeded);
        ];
      let defaults =
        "I64 Add(I64 n){return n+2;}I64 Apply(I64 (*cb)(I64 n=40)){return \
         cb();}I64 Run(){I64 (*p)(I64 (*cb)(I64 n=12));I64 (*q)(I64 \
         n=10);p=&Apply;q=&Add;return p(q);}Run();"
      in
      let report, result =
        native_success_report ~mode ~max_steps:10000 ~max_initializer_steps:9
          ~max_default_bytes:24 defaults
      in
      check_native_word "nested header defaults" "I64" 42L
        result.execution.final_value;
      Alcotest.(check int)
        "all nested defaults prepare" 9
        (Native_program.preparation_steps report);
      Alcotest.(check int)
        "each original header retains its payload" 24
        (Native_program.default_bytes report);
      List.iter
        (fun (work, bytes, code) ->
          let _, fault, errors =
            native_fault ~mode ~max_steps:10000 ~max_initializer_steps:work
              ~max_default_bytes:bytes defaults
          in
          Alcotest.(check bool)
            "nested default quota prevents entry" true (Option.is_none fault);
          Alcotest.(check string)
            "nested default quota code" code (List.hd errors).code)
        [ (8, 24, "HCIRVM0007"); (9, 23, "HCIRVM0011") ])
    modes

let callback_word_defaults_execute () =
  let cases =
    [
      ( "named numeric default",
        "I64 Ignore(I64 (*cb)(I64 n)=17){if(cb==17)return 42;return \
         0;}Ignore();" );
      ( "anonymous default differs from destination default",
        "I64 Ignore(I64 (*cb)(I64 n)=12){if(cb==17)return 42;return 0;}I64 \
         Run(){I64 (*p)(I64 (*cb)(I64 n)=17);p=&Ignore;return p();}Run();" );
      ( "nontrailing multiple callback defaults",
        "I64 Ignore(I64 (*a)(I64 n)=17,U0 (*b)()=0,I64 n=42){if(a==17 && \
         b==0)return n;return 0;}I64 Run(){I64 (*p)(I64 (*a)(I64 n)=17,U0 \
         (*b)()=0,I64 n=42);p=&Ignore;return p(,,);}Run();" );
      ( "explicit owned override retains unused numeric default",
        "I64 Add(I64 n){return n+2;}I64 Apply(I64 (*cb)(I64 n)=17){return \
         cb(40);}I64 Run(){I64 (*p)(I64 (*cb)(I64 n)=0);p=&Apply;return \
         p(&Add);}Run();" );
      ( "numeric forwarding and stored copies",
        "I64 (*G)(I64 n)[2];I64 Check(I64 (*cb)(I64 n)){I64 (*a)(I64 \
         n)[2];static I64 (*s)(I64 n);G[1]=cb;a[1]=G[1];s=a[1];if(s==17)return \
         42;return 0;}I64 Apply(I64 (*cb)(I64 n)=17){return \
         Check(cb);}Apply();Apply();" );
      ( "callback default before actual word tail",
        "I64 Apply(I64 (*cb)(I64 n)=17,...){if(cb==17)return \
         argv[0]+argv[1];return 0;}I64 Run(){I64 (*p)(I64 (*cb)(I64 \
         n)=17,...);p=&Apply;return p(,20,22);}Run();" );
      ( "effective disabled callback default register",
        "I64 Ignore(reg RAX noreg I64 (*cb)(I64 n)=17){if(cb==17)return \
         42;return 0;}Ignore();" );
      ( "disabled narrow scalar default",
        "U8 Echo(noreg U8 n=554){return n;}Echo();" );
      ( "callback values compare against a computed numeric word",
        "I64 Number(){return 17;}I64 Ignore(I64 (*cb)(I64 \
         n)=17){if(cb==Number())return 42;return 0;}Ignore();" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) ->
          ignore
            (compare_source ~mode ~label
               ~expected_type:
                 (if label = "disabled narrow scalar default" then "U64"
                  else "I64")
               ~expected_bits:42L source))
        cases;
      List.iter
        (fun return_type ->
          List.iter
            (fun literal ->
              let source =
                Printf.sprintf
                  "I64 Ignore(%s (*cb)(I64 n)=%s){if(cb==%s)return 42;return \
                   0;}I64 Run(){I64 (*p)(%s (*cb)(I64 n)=%s);p=&Ignore;return \
                   p();}Run();"
                  return_type literal literal return_type literal
              in
              let report, _, _ =
                compare_source ~mode
                  ~label:(return_type ^ " full callback word " ^ literal)
                  ~expected_type:"I64" ~expected_bits:42L source
              in
              Alcotest.(check int)
                "both callback words retain full payload bytes" 16
                (Native_program.default_bytes report))
            [ "0"; "17"; "0x8000000000000001"; "0xffffffffffffffff" ])
        [
          "I8";
          "U8";
          "I16";
          "U16";
          "I32";
          "U32";
          "I64";
          "U64";
          "F64";
          "U0";
          "I64 *";
          "I64 ****";
        ])
    modes

let callback_word_default_faults () =
  let prefix =
    "extern U0 PutChars(U64 ch);I64 Arg(){PutChars('B');return 40;}"
  in
  let cases =
    [
      ( "named numeric default grants no code",
        "I64 Apply(I64 (*cb)(I64 n)=17){PutChars('A');return \
         cb(Arg());}Apply();",
        "HCIRVM0024",
        "AB" );
      ( "named null default grants no code",
        "I64 Apply(I64 (*cb)(I64 n)=0){PutChars('A');return cb(Arg());}Apply();",
        "HCIRVM0024",
        "AB" );
      ( "indirect numeric default grants no code",
        "I64 Apply(I64 (*cb)(I64 n)=0){PutChars('A');return cb(Arg());}I64 \
         Run(){I64 (*p)(I64 (*cb)(I64 n)=17);p=&Apply;return p();}Run();",
        "HCIRVM0024",
        "AB" );
      ( "owned override compares after numeric argument effects",
        "I64 Add(I64 n){return n+2;}I64 Apply(I64 (*cb)(I64 n)=17){return \
         cb==Arg();}Apply(&Add);",
        "HCIRVM0024",
        "B" );
      ( "numeric default copies retain zero owners",
        "I64 (*G)(I64 n)[2];I64 Apply(I64 (*cb)(I64 n)=17){G[1]=cb;return \
         G[1](Arg());}Apply();",
        "HCIRVM0024",
        "B" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, body, code, output) ->
          let source = prefix ^ body in
          let session, config, input = source_inputs ~mode source in
          let public =
            run_integer_program_report session ~config ~source:input
              ~max_steps:10000
          in
          let errors =
            match integer_program_report_outcome public with
            | Error e -> e
            | Ok _ -> Alcotest.fail label
          in
          Alcotest.(check string)
            (label ^ " public code") code (List.hd errors).code;
          Alcotest.(check string)
            (label ^ " public output") output
            (integer_program_report_output_bytes public);
          let _, batch = batch_failure ~mode ~max_steps:10000 source in
          let report, fault, errors =
            native_fault ~mode ~max_steps:10000 source
          in
          let fault = Option.get fault in
          Alcotest.(check string)
            (label ^ " native code") code (List.hd errors).code;
          Alcotest.(check string) (label ^ " batch code") code batch.code;
          Alcotest.(check int)
            (label ^ " reached work") batch.executed_steps fault.executed_steps;
          Alcotest.(check string)
            (label ^ " native output") output
            (Native_program.output_bytes report))
        cases)
    modes

let callback_word_default_quotas () =
  let source =
    "I64 Check(I64 (*cb)(I64 n)=12){if(cb==17)return 42;return 0;}I64 \
     Run(){I64 (*p)(I64 (*cb)(I64 n)=17);p=&Check;return p();}Run();"
  in
  List.iter
    (fun mode ->
      let report, native, _ =
        compare_source ~mode ~label:"callback word default quotas"
          ~expected_type:"I64" ~expected_bits:42L source
      in
      Alcotest.(check int)
        "two original callback defaults" 6
        (Native_program.preparation_steps report);
      Alcotest.(check int)
        "two full-word saved payloads" 16
        (Native_program.default_bytes report);
      let _, exact =
        native_success_report ~mode ~max_steps:native.execution.executed_steps
          ~max_initializer_steps:6 ~max_default_bytes:16 source
      in
      check_native_word "exact callback word preparation" "I64" 42L
        exact.execution.final_value;
      List.iter
        (fun (work, bytes, code, reached, saved) ->
          let report, fault, errors =
            native_fault ~mode ~max_steps:10000 ~max_initializer_steps:work
              ~max_default_bytes:bytes source
          in
          Alcotest.(check bool)
            "callback word preparation prevents entry" true
            (Option.is_none fault);
          Alcotest.(check string)
            "callback word preparation fault" code (List.hd errors).code;
          Alcotest.(check int)
            "callback word reached preparation" reached
            (Native_program.preparation_steps report);
          Alcotest.(check int)
            "callback word completed payloads" saved
            (Native_program.default_bytes report);
          match
            Runtime.execute ~max_steps:10000 exact.image |> require_ok Fun.id
          with
          | Program.Completed result ->
              check_native_word "callback word image recovers" "I64" 42L
                result.final_value
          | _ -> Alcotest.fail "callback word image did not recover")
        [ (5, 16, "HCIRVM0007", 5, 8); (6, 15, "HCIRVM0011", 3, 8) ];
      let recursive =
        "I64 Walk(I64 (*cb)(I64 n)=17,I64 depth=2){if(depth)return \
         Walk(,depth-1);if(cb==17)return 42;return 0;}Walk();"
      in
      let _, native, _ =
        compare_source ~mode ~label:"recursive callback word defaults"
          ~expected_type:"I64" ~expected_bits:42L recursive
      in
      let image = native.image and steps = native.execution.executed_steps in
      let physical =
        Program.entry_stack_bytes image
        + (3 * List.hd (named_physical_costs image))
      in
      let execute frame depth stack work =
        Runtime.execute ~max_frame_bytes:frame ~max_call_depth:depth
          ~max_active_stack_bytes:stack ~max_steps:work image
        |> require_ok Fun.id
      in
      let recover () =
        match execute 48 3 physical steps with
        | Program.Completed result ->
            check_native_word "recursive callback word image recovers" "I64" 42L
              result.final_value
        | _ -> Alcotest.fail "exact recursive callback word limits"
      in
      recover ();
      List.iter
        (fun (frame, depth, stack, work, kind) ->
          (match execute frame depth stack work with
          | Program.Fault fault ->
              Alcotest.(check bool)
                "callback word quota kind" true (fault.kind = kind)
          | _ -> Alcotest.fail "one-below callback word quota completed");
          recover ())
        [
          (47, 3, physical, steps, Program.Frame_limit_exceeded);
          (48, 2, physical, steps, Program.Call_depth_exceeded);
          (48, 3, physical - 1, steps, Program.Native_stack_limit_exceeded);
          (48, 3, physical, steps - 1, Program.Step_limit_exceeded);
        ])
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
              Alcotest.test_case "callback word defaults retain numeric bits"
                `Quick callback_word_defaults_execute;
              Alcotest.test_case
                "callback word defaults preserve reached faults" `Quick
                callback_word_default_faults;
              Alcotest.test_case
                "callback word default preparation quotas recover" `Quick
                callback_word_default_quotas;
              Alcotest.test_case "indirect arguments retain callback ownership"
                `Quick indirect_callback_arguments_execute;
              Alcotest.test_case "indirect callback arguments preserve faults"
                `Quick indirect_callback_argument_faults;
              Alcotest.test_case "indirect callback argument quotas recover"
                `Quick indirect_callback_argument_quotas;
              Alcotest.test_case "word tails retain values and bounded storage"
                `Quick word_tail_values_and_storage;
              Alcotest.test_case "word-tail faults preserve argument effects"
                `Quick word_tail_faults_and_effects;
              Alcotest.test_case
                "word-tail activation quotas unwind and recover" `Quick
                word_tail_quotas_and_recovery;
              Alcotest.test_case
                "anonymous saved defaults execute original values" `Quick
                callback_defaults_execute_saved_values;
              Alcotest.test_case
                "anonymous default faults preserve argument effects" `Quick
                callback_default_fault_order;
              Alcotest.test_case
                "anonymous preparation shares quotas and recovers" `Quick
                callback_default_preparation_limits;
              Alcotest.test_case "callback storage quotas unwind and recover"
                `Quick callback_storage_limits_and_recovery;
              Alcotest.test_case
                "callback storage survives calls and indexed copies" `Quick
                callback_storage_executes;
              Alcotest.test_case "callback storage faults preserve effect order"
                `Quick callback_storage_faults;
              Alcotest.test_case "callback parameter quotas unwind and recover"
                `Quick callback_parameter_limits_and_recovery;
              Alcotest.test_case "owned local callbacks execute original bodies"
                `Quick owned_local_callbacks_execute;
              Alcotest.test_case "owned local callback faults preserve effects"
                `Quick owned_local_callback_faults;
              Alcotest.test_case
                "owned local callback quotas unwind and recover" `Quick
                owned_local_callback_limits_and_recovery;
              Alcotest.test_case "ordinary flags preserve values and cleanup"
                `Quick ordinary_calling_flags_execute;
              Alcotest.test_case "ordinary flags unwind quotas and recover"
                `Quick ordinary_calling_flags_unwind_and_recover;
              Alcotest.test_case "automatic array preparation and frame bounds"
                `Quick automatic_array_preparation_and_layout;
              Alcotest.test_case
                "typed pointer aliases preserve source semantics" `Quick
                pointer_alias_semantics;
              Alcotest.test_case
                "array updates normalize wrapping results at every width" `Quick
                array_update_wrap_results;
              Alcotest.test_case
                "pointer initialization recursion quotas and recovery" `Quick
                pointer_faults_and_limits;
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
