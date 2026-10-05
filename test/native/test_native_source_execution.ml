open Holyc_lib
module Native = Native_source_execution
module Image = X86_64_program
module VM = Ir_integer_interpreter

type raw_arena

external raw_create_arena : Obj.t -> raw_arena
  = "holyc_native_create_task_arena"

external raw_admit_arena : raw_arena -> Obj.t -> Obj.t -> int
  = "holyc_native_task_arena_admit"

external raw_release_arena : raw_arena -> unit
  = "holyc_native_release_task_arena"

type raw_retained

external raw_retain_task : Obj.t -> raw_retained
  = "holyc_native_retain_task_fragment"

external raw_release_program : raw_retained -> unit
  = "holyc_native_release_program"

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics errors =
  errors
  |> List.map (fun (diagnostic : Diagnostic.t) ->
      diagnostic.code ^ ": " ^ diagnostic.message)
  |> String.concat "; "

let inputs ?(mode = Preprocessor.Jit) text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-source-task.hc" ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  (session, config, source)

let run ?mode ?(max_steps = 100_000) ?max_global_bytes ?max_code_bytes
    ?max_ir_instructions ?max_initializer_steps ?max_active_stack_bytes
    ?status_abi text =
  let session, config, source = inputs ?mode text in
  Native.evaluate ?max_global_bytes ?max_code_bytes ?max_ir_instructions
    ?max_initializer_steps ?max_active_stack_bytes ?status_abi session ~config
    ~source ~max_steps

let expect_value expected report =
  let result =
    Native.outcome report |> Result.map_error diagnostics |> checked
  in
  let word = Option.get result.value.final_value in
  Alcotest.(check int64) "native source final bits" expected word.bits;
  Alcotest.(check string) "quiet native capture" "" (Native.output_bytes report);
  Alcotest.(check int) "quiet native output work" 0 (Native.output_work report);
  result.value

let expect_error code report =
  match Native.outcome report with
  | Ok _ -> Alcotest.failf "expected %s" code
  | Error errors ->
      Alcotest.(check bool)
        ("diagnostic " ^ code ^ " in " ^ diagnostics errors)
        true
        (List.exists (fun (error : Diagnostic.t) -> error.code = code) errors);
      errors

let expect_rejection report =
  match Native.outcome report with
  | Error errors ->
      Alcotest.(check bool) "rejection has a diagnostic" true (errors <> [])
  | Ok _ -> Alcotest.fail "unsupported native task source executed"

let expect_native_fault kind report =
  expect_rejection report;
  match List.rev (Native.fragments report) with
  | { native_outcome = Some (Ok (Image.Fault fault)); _ } :: _ ->
      Alcotest.(check bool) "actual native fault" true (fault.kind = kind);
      Alcotest.(check int)
        "native cumulative fault count" fault.executed_steps
        (Native.executed_steps report);
      fault
  | _ -> Alcotest.fail "failure has no actual native fault outcome"

let source = "I64 A=41; I64 B=A+1; B;"

let original_live_initializers () =
  let report = run source in
  ignore (expect_value 42L report);
  let fragments = Native.fragments report in
  Alcotest.(check int)
    "two initializer leaves and one expression" 3 (List.length fragments);
  Alcotest.(check (list int))
    "append-only logical bytes" [ 8; 16; 16 ]
    (List.map
       (fun (fragment : Native.fragment) -> fragment.image.global_bytes)
       fragments);
  Alcotest.(check (list int))
    "stable data and flags" [ 9; 18; 18 ]
    (List.map
       (fun (fragment : Native.fragment) -> fragment.image.global_arena_bytes)
       fragments);
  List.iteri
    (fun index (fragment : Native.fragment) ->
      Alcotest.(check bool)
        "original source fragment kind" true
        (fragment.kind
        = if index < 2 then Native.Initializer else Native.Command);
      match fragment.native_outcome with
      | Some (Ok (Image.Completed execution)) ->
          Alcotest.(check bool)
            "actual native entry consumed work" true
            (execution.executed_steps > 0)
      | _ -> Alcotest.fail "source fragment has no checked native completion")
    fragments;
  let progress = Option.get (Native.source_progress report) in
  Alcotest.(check int)
    "no interpreted runtime instructions" 0 progress.runtime.executed_steps;
  Alcotest.(check bool)
    "closed preparation recorded separately" true
    (Native.preparation_steps report > 0);
  let session, config, source = inputs source in
  let oracle =
    run_integer_program_report session ~config ~source ~max_steps:100_000
  in
  let oracle =
    integer_program_report_outcome oracle
    |> Result.map_error diagnostics
    |> checked
  in
  Alcotest.(check int64)
    "fresh public IR value oracle" 42L
    (Option.get (VM.final_value oracle.value)).bits

let preserved_writes_and_growth () =
  List.iter
    (fun text -> ignore (expect_value 42L (run text)))
    [
      "I64 A=39; I64 B=A+1; I64 C=B+1; I64 D=C+1; A+B+C+D-120;";
      "I64 A=40; I64 B=++A; A+B-40;";
      "I64 A=41; I64 B=A++; A+B-41;";
      "I64 A=40,B=++A; A+B-40;";
      "I64 A; A=40; I64 B=++A; A+B-40;";
      "I64 A=1; A=40; I64 B=A+2; B;";
      "I64 A=40; I64 B=A; A++; B++; A+B-40;";
      "42; I64 A=1;";
      "42;;";
      "42; if(0)1;";
      "42; while(0)1;";
    ]

let declared_widths () =
  List.iter
    (fun text -> ignore (expect_value 42L (run text)))
    [
      "I8 A=-1; I8 B=A+43; B;";
      "U8 A=255; U8 B=A+43; B;";
      "I16 A=-2; I16 B=A+44; B;";
      "U16 A=65535; U16 B=A+43; B;";
      "I32 A=-3; I32 B=A+45; B;";
      "U32 A=4294967295; U32 B=A+43; B;";
      "I64 A=-4; I64 B=A+46; B;";
      "U64 A=41; U64 B=A+1; B;";
    ];
  let result =
    run "U64 A=0x8000000000000000; I64 One=1; U64 B=A+One; B;"
    |> expect_value (Int64.succ Int64.min_int)
  in
  Alcotest.(check bool)
    "full unsigned result class" true
    ((Option.get result.final_value).type_ = Image.U64)

let reached_faults () =
  let uninitialized = run "I64 A; I64 B=A+1; B;" in
  ignore (expect_error "HCIRVM0012" uninitialized);
  ignore (expect_native_fault Image.Uninitialized_read uninitialized);
  let division = run "I64 Z=0;\nI64 B=41/Z;\nB;" in
  let errors = expect_error "HCIRVM0009" division in
  let fault = expect_native_fault Image.Division_by_zero division in
  Alcotest.(check bool)
    "fault keeps original instruction span" true
    (Option.is_some fault.span);
  Alcotest.(check bool)
    "source diagnostic carries reached work" true
    (List.exists
       (fun (error : Diagnostic.t) ->
         List.exists (String.starts_with ~prefix:"executed_steps=") error.notes)
       errors);
  Alcotest.(check int)
    "later B expression never entered" 2
    (List.length (Native.fragments division));
  let parsed = run "I64 A=41;\nI64 B=A+1;\nI64 Broken=;\nA=99;" in
  expect_rejection parsed;
  Alcotest.(check int)
    "native leaves survive later parse failure" 2
    (List.length (Native.fragments parsed));
  Alcotest.(check bool)
    "earlier native work preserved on parse failure" true
    (Native.executed_steps parsed > 0)

let exact_native_limits () =
  let baseline = run source in
  ignore (expect_value 42L baseline);
  let steps = Native.executed_steps baseline in
  Alcotest.(check bool) "nonempty native work" true (steps > 1);
  ignore (expect_value 42L (run ~max_steps:steps source));
  let bounded = run ~max_steps:(steps - 1) source in
  ignore (expect_error "HCIRVM0007" bounded);
  ignore (expect_native_fault Image.Step_limit_exceeded bounded);
  Alcotest.(check int)
    "cumulative step boundary" (steps - 1)
    (Native.executed_steps bounded);
  let initializers =
    Native.fragments baseline
    |> List.filter (fun (fragment : Native.fragment) ->
        fragment.kind = Native.Initializer)
  in
  let exhausted_at =
    match (List.hd (List.rev initializers)).native_outcome with
    | Some (Ok (Image.Completed execution)) -> execution.executed_steps
    | _ -> Alcotest.fail "missing initializer completion"
  in
  let exhausted = run ~max_steps:exhausted_at source in
  ignore (expect_native_fault Image.Step_limit_exceeded exhausted);
  Alcotest.(check int)
    "zero remaining reaches next native guard" exhausted_at
    (Native.executed_steps exhausted);
  ignore (expect_value 42L (run ~max_global_bytes:16 source));
  expect_rejection (run ~max_global_bytes:15 source);
  let code =
    List.fold_left
      (fun total (fragment : Native.fragment) ->
        total + fragment.image.code_bytes)
      0
      (Native.fragments baseline)
  in
  let ir =
    List.fold_left
      (fun total (fragment : Native.fragment) ->
        total + fragment.image.ir_instructions)
      0
      (Native.fragments baseline)
  in
  ignore
    (expect_value 42L (run ~max_code_bytes:code ~max_ir_instructions:ir source));
  ignore (expect_error "HCBACK0005" (run ~max_code_bytes:(code - 1) source));
  ignore (expect_error "HCBACK0001" (run ~max_ir_instructions:(ir - 1) source));
  let first = List.hd (Native.fragments baseline) in
  let first_steps =
    match first.native_outcome with
    | Some (Ok (Image.Completed execution)) -> execution.executed_steps
    | _ -> Alcotest.fail "first initializer has no native completion"
  in
  let exhausted_code = run ~max_code_bytes:first.image.code_bytes source in
  ignore (expect_error "HCBACK0005" exhausted_code);
  Alcotest.(check int)
    "exhausted code bytes preserve the first native initializer" first_steps
    (Native.executed_steps exhausted_code);
  Alcotest.(check int)
    "exhausted code bytes do not emit the second fragment" 1
    (List.length (Native.fragments exhausted_code));
  let exhausted_ir =
    run ~max_ir_instructions:first.image.ir_instructions source
  in
  ignore (expect_error "HCBACK0001" exhausted_ir);
  Alcotest.(check int)
    "exhausted IR preserves the first native initializer" first_steps
    (Native.executed_steps exhausted_ir);
  Alcotest.(check int)
    "exhausted IR does not emit the second fragment" 1
    (List.length (Native.fragments exhausted_ir));
  let preparation = Native.preparation_steps baseline in
  ignore (expect_value 42L (run ~max_initializer_steps:preparation source));
  let bounded_preparation =
    run ~max_initializer_steps:(preparation - 1) source
  in
  expect_rejection bounded_preparation;
  Alcotest.(check int)
    "failed preparation retains its reached work" (preparation - 1)
    (Native.preparation_steps bounded_preparation);
  Alcotest.(check int)
    "preparation exhaustion precedes native entry" 0
    (Native.executed_steps bounded_preparation);
  let stack =
    List.fold_left
      (fun peak (fragment : Native.fragment) ->
        max peak fragment.image.entry_stack_bytes)
      0
      (Native.fragments baseline)
  in
  ignore (expect_value 42L (run ~max_active_stack_bytes:stack source));
  let bounded_stack = run ~max_active_stack_bytes:(stack - 1) source in
  expect_rejection bounded_stack;
  Alcotest.(check int)
    "entry stack bound rejects before native work" 0
    (Native.executed_steps bounded_stack);
  let invalid = run ~max_steps:0 source in
  expect_rejection invalid;
  Alcotest.(check int)
    "invalid limit precedes native code" 0
    (List.length (Native.fragments invalid))

let unsupported_domains () =
  let aot = run ~mode:Preprocessor.Aot source in
  expect_rejection aot;
  Alcotest.(check int)
    "AOT rejected before native source callbacks" 0
    (List.length (Native.fragments aot));
  let foreign =
    match Native_program_execution.platform () with
    | Windows_x86_64 -> Image.System_v_x64
    | Linux_x86_64 -> Image.Windows_x64
    | Unsupported -> Alcotest.fail "native test requires a supported host"
  in
  let foreign = run ~status_abi:foreign source in
  expect_rejection foreign;
  Alcotest.(check int)
    "foreign ABI rejected before native callbacks" 0
    (List.length (Native.fragments foreign));
  List.iter
    (fun text -> expect_rejection (run text))
    [
      "I64 F(){return 42;} F();";
      "I64 A[2]={41,42}; A[1];";
      "I64 *A; A;";
      "F64 A=42.0; A;";
      "\"unsupported\";";
      "#exe {StreamPrint(\"42;\");}";
    ]

let separate_tasks_and_gc () =
  for _ = 1 to 4 do
    ignore (expect_value 42L (run source));
    Gc.full_major ();
    Gc.compact ();
    let fresh = run "I64 A; A;" in
    ignore (expect_native_fault Image.Uninitialized_read fresh)
  done;
  let worker = Domain.spawn (fun () -> run "I64 A=40; I64 B=++A; A+B-40;") in
  ignore (expect_value 42L (run source));
  ignore (expect_value 42L (Domain.join worker))

let original_arena_owner () =
  let layout =
    Image.create_task_layout ~max_global_bytes:16
    |> Result.map_error (fun errors ->
        errors
        |> List.map (fun (error : Image.error) -> error.message)
        |> String.concat "; ")
    |> checked
  in
  let module Runtime = Native_program_execution in
  Alcotest.(check bool)
    "invalid capacity leaves owner unclaimed" true
    (Result.is_error (Runtime.create_task_arena ~max_arena_bytes:0 layout));
  let arena = Runtime.create_task_arena ~max_arena_bytes:32 layout |> checked in
  Alcotest.(check bool)
    "one original arena per layout" true
    (Result.is_error (Runtime.create_task_arena ~max_arena_bytes:32 layout));
  Runtime.release_task_arena arena |> checked;
  Runtime.release_task_arena arena |> checked;
  Alcotest.(check bool)
    "released arena cannot be replaced with a copy" true
    (Result.is_error (Runtime.create_task_arena ~max_arena_bytes:32 layout))

let detached_report_images () =
  let report = run source in
  ignore (expect_value 42L report);
  let descriptions = Native.fragments report in
  let before =
    List.map (fun (fragment : Native.fragment) -> fragment.image) descriptions
  in
  let steps = Native.executed_steps report in
  Gc.full_major ();
  Gc.compact ();
  ignore (expect_value 42L (run "I64 A=40; I64 B=++A; A+B-40;"));
  Alcotest.(check bool)
    "detached image observations survive source cleanup" true
    (before
    = List.map
        (fun (fragment : Native.fragment) -> fragment.image)
        (Native.fragments report));
  Alcotest.(check int)
    "completed report counters are immutable" steps
    (Native.executed_steps report);
  List.iter
    (fun (fragment : Native.fragment) ->
      Alcotest.(check bool)
        "detached image has generated code" true
        (fragment.image.code_bytes > 0);
      Alcotest.(check bool)
        "detached image has original IR count" true
        (fragment.image.ir_instructions > 0);
      match fragment.native_outcome with
      | Some (Ok (Image.Completed execution)) ->
          Alcotest.(check bool)
            "native outcome remains independent of arena lifetime" true
            (execution.executed_steps > 0 && execution.executed_steps <= steps)
      | _ -> Alcotest.fail "completed native outcome was lost during cleanup")
    descriptions;
  ignore (expect_value 42L report)

let raw_arena_admission () =
  let reject action =
    try
      action ();
      Alcotest.fail "malformed task arena admission was accepted"
    with Invalid_argument _ -> ()
  in
  List.iter
    (fun capacity -> reject (fun () -> ignore (raw_create_arena capacity)))
    [
      Obj.repr 0;
      Obj.repr (-1);
      Obj.repr 1.0;
      Obj.repr "32";
      Obj.repr (Native_program_execution.hard_max_arena_bytes + 1);
    ];
  let arena = raw_create_arena (Obj.repr 32) in
  Fun.protect
    ~finally:(fun () -> raw_release_arena arena)
    (fun () ->
      let admit prefix extent =
        raw_admit_arena arena (Obj.repr prefix) (Obj.repr extent)
      in
      Alcotest.(check int) "initial prefix" 16 (admit 0 16);
      reject (fun () -> ignore (admit 0 24));
      reject (fun () -> ignore (admit 16 8));
      reject (fun () -> ignore (admit 16 33));
      reject (fun () -> ignore (admit 16 (-1)));
      reject (fun () -> ignore (admit 16 max_int));
      reject (fun () ->
          ignore (raw_admit_arena arena (Obj.repr 16.0) (Obj.repr 24)));
      reject (fun () ->
          ignore (raw_admit_arena arena (Obj.repr 16) (Obj.repr "24")));
      reject (fun () ->
          ignore (raw_admit_arena arena (Obj.repr 16) (Obj.repr 24.0)));
      Alcotest.(check int)
        "append requires only a checked extent" 24 (admit 16 24);
      Alcotest.(check int) "unchanged admitted extent" 24 (admit 24 24);
      Gc.full_major ();
      Gc.compact ();
      Alcotest.(check int) "exact capacity after collection" 32 (admit 24 32);
      raw_release_arena arena;
      reject (fun () -> ignore (admit 32 32)));
  let extent = 2 * 1024 * 1024 in
  let arena = raw_create_arena (Obj.repr extent) in
  Fun.protect
    ~finally:(fun () -> raw_release_arena arena)
    (fun () ->
      let before = Gc.allocated_bytes () in
      let admitted = raw_admit_arena arena (Obj.repr 0) (Obj.repr extent) in
      let allocated = Gc.allocated_bytes () -. before in
      Alcotest.(check int)
        "large arena extent is admitted directly" extent admitted;
      Alcotest.(check bool)
        "arena admission allocates no arena-sized OCaml seed" true
        (allocated < 4096.))

let retained_extent_metadata () =
  let session, config, source = inputs "I64 A; A=42; A;" in
  let image =
    Native_program.compile session ~config ~source
    |> Result.map_error diagnostics
    |> checked
    |> fun checked -> checked.value
  in
  let abi =
    match Image.status_abi image with
    | Image.Windows_x64 -> 1
    | Image.System_v_x64 -> 2
  in
  let extent = Native_program_execution.hard_max_arena_bytes in
  let logical = Native_program_execution.hard_max_global_bytes in
  let identity =
    Obj.repr
      ( Image.code image,
        Array.of_list (Image.windows_unwind_functions image),
        abi,
        Image.entry_stack_bytes image,
        (logical, 0, extent - logical, extent) )
  in
  let handles = ref [] in
  Fun.protect
    ~finally:(fun () -> List.iter raw_release_program !handles)
    (fun () ->
      let before = Gc.allocated_bytes () in
      for _ = 1 to 8 do
        handles := raw_retain_task identity :: !handles
      done;
      let allocated = Gc.allocated_bytes () -. before in
      Alcotest.(check bool)
        "retained task handles carry extents without arena-sized OCaml seeds"
        true (allocated < 65_536.);
      Gc.full_major ();
      List.iter raw_release_program !handles;
      Gc.compact ())

let () =
  Alcotest.run "Native source execution"
    [
      ( "shared task",
        [
          Alcotest.test_case "original live initializer entries" `Quick
            original_live_initializers;
          Alcotest.test_case "once-only writes and stable growth" `Quick
            preserved_writes_and_growth;
          Alcotest.test_case "declared scalar widths and unsigned words" `Quick
            declared_widths;
          Alcotest.test_case "reached faults and stopped source" `Quick
            reached_faults;
          Alcotest.test_case "exact cumulative native limits" `Quick
            exact_native_limits;
          Alcotest.test_case "unsupported source and ABI rejection" `Quick
            unsupported_domains;
          Alcotest.test_case "separate arenas and collection" `Quick
            separate_tasks_and_gc;
          Alcotest.test_case "original task arena ownership" `Quick
            original_arena_owner;
          Alcotest.test_case "detached public report images" `Quick
            detached_report_images;
          Alcotest.test_case "raw arena admission and capacity" `Quick
            raw_arena_admission;
          Alcotest.test_case "length-only retained task metadata" `Quick
            retained_extent_metadata;
        ] );
    ]
