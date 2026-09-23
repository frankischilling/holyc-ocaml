open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution

type status = int64 * int64 * int64 * int64 * int64

external execute_output :
  string ->
  (int * int * string) array ->
  int ->
  Obj.t ->
  Obj.t ->
  status * string * int = "holyc_native_execute_program_output"

let abi =
  match Runtime.platform () with
  | Runtime.Windows_x86_64 -> 1
  | Runtime.Linux_x86_64 -> 2
  | Runtime.Unsupported -> 0

let default_limits = Obj.repr (100, 1024, 8, 65536, 32, 1024, 1024, 16, 16)
let empty_storage = Obj.repr (0, 0, 0, "")

let rejects label message ?(limits = default_limits) ?(storage = empty_storage)
    () =
  Alcotest.check_raises label (Invalid_argument message) (fun () ->
      ignore (execute_output "" [||] abi limits storage))

let malformed_tuples () =
  List.iter
    (fun (label, limits) ->
      rejects label "native output program limits tuple is malformed" ~limits ())
    [
      ("immediate limits", Obj.repr 0);
      ("old seven-field limits", Obj.repr (100, 1024, 8, 65536, 32, 1024, 1024));
      ( "missing output-work limit",
        Obj.repr (100, 1024, 8, 65536, 32, 1024, 1024, 16) );
      ( "extra limit field",
        Obj.repr (100, 1024, 8, 65536, 32, 1024, 1024, 16, 16, 0) );
      ( "boxed output byte limit",
        Obj.repr (100, 1024, 8, 65536, 32, 1024, 1024, "16", 16) );
      ( "boxed output work limit",
        Obj.repr (100, 1024, 8, 65536, 32, 1024, 1024, 16, "16") );
    ];
  List.iter
    (fun (label, storage) ->
      rejects label "native output program storage tuple is malformed" ~storage
        ())
    [
      ("immediate storage", Obj.repr 0);
      ("old storage tuple", Obj.repr (0, 0, ""));
      ("extra storage field", Obj.repr (0, 0, 0, "", 0));
      ("boxed global count", Obj.repr (0L, 0, 0, ""));
      ("boxed literal count", Obj.repr (0, "0", 0, ""));
      ("boxed metadata count", Obj.repr (0, 0, "0", ""));
    ];
  rejects "non-string arena" "native output program arena image is not a string"
    ~storage:(Obj.repr (0, 0, 0, [||]))
    ()

let limit_validation () =
  List.iter
    (fun output_bytes ->
      rejects "invalid output byte limit"
        "native program max_output_bytes is outside the host bound"
        ~limits:
          (Obj.repr (100, 1024, 8, 65536, 32, 1024, 1024, output_bytes, 16))
        ())
    [ -1; 0; Runtime.hard_max_output_bytes + 1 ];
  List.iter
    (fun output_work ->
      rejects "invalid output work limit"
        "native program max_output_work must be greater than zero"
        ~limits:
          (Obj.repr (100, 1024, 8, 65536, 32, 1024, 1024, 16, output_work))
        ())
    [ -1; 0 ];
  rejects "zero instruction budget"
    "native program max_steps must be greater than zero"
    ~limits:(Obj.repr (0, 1024, 8, 65536, 32, 1024, 1024, 16, 16))
    ();
  rejects "zero frame budget"
    "native program max_frame_bytes must be greater than zero"
    ~limits:(Obj.repr (100, 0, 8, 65536, 32, 1024, 1024, 16, 16))
    ();
  rejects "zero call-depth budget"
    "native program max_call_depth must be greater than zero"
    ~limits:(Obj.repr (100, 1024, 0, 65536, 32, 1024, 1024, 16, 16))
    ();
  List.iter
    (fun active_stack ->
      rejects "invalid active stack limit"
        "native program max_active_stack_bytes must be between 1 and 65536"
        ~limits:(Obj.repr (100, 1024, 8, active_stack, 32, 1024, 1024, 16, 16))
        ())
    [ 0; 65_537 ];
  List.iter
    (fun global_limit ->
      rejects "invalid global limit"
        "native program max_global_bytes is outside the host bound"
        ~limits:(Obj.repr (100, 1024, 8, 65536, 32, global_limit, 1024, 16, 16))
        ())
    [ 0; Runtime.hard_max_global_bytes + 1 ];
  List.iter
    (fun literal_limit ->
      rejects "invalid literal limit"
        "native program max_literal_bytes is outside the host bound"
        ~limits:
          (Obj.repr (100, 1024, 8, 65536, 32, 1024, literal_limit, 16, 16))
        ())
    [ 0; Runtime.hard_max_literal_bytes + 1 ]

let storage_validation () =
  List.iter
    (fun bytes ->
      rejects "invalid global data count"
        "native program logical global bytes exceed their bound"
        ~storage:(Obj.repr (bytes, 0, 0, ""))
        ())
    [ -1; 1025; Runtime.hard_max_global_bytes + 1 ];
  List.iter
    (fun bytes ->
      rejects "invalid literal data count"
        "native program logical literal bytes exceed their bound"
        ~storage:(Obj.repr (0, bytes, 0, ""))
        ())
    [ -1; 1025; Runtime.hard_max_literal_bytes + 1 ];
  List.iter
    (fun bytes ->
      rejects "invalid private metadata count"
        "native program private metadata bytes exceed their bound"
        ~storage:(Obj.repr (0, 0, bytes, ""))
        ())
    [ -1; Runtime.hard_max_arena_bytes + 1 ];
  List.iter
    (fun storage ->
      rejects "inconsistent arena length"
        "native output program arena image is inconsistent with data and \
         metadata"
        ~storage ())
    [
      Obj.repr (1, 0, 0, "");
      Obj.repr (0, 1, 0, "");
      Obj.repr (0, 0, 0, "\000");
      Obj.repr (1, 1, 0, "\000");
    ];
  rejects "metadata without persistent data"
    "native output program private metadata has no persistent data"
    ~storage:(Obj.repr (0, 0, 1, "\000"))
    ()

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let source_image contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-output-bridge.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
    |> Result.get_ok
  in
  match Native_program.compile session ~config ~source with
  | Ok checked -> checked.value
  | Error diagnostics -> Alcotest.fail (diagnostics_text diagnostics)

let abi_of_image image =
  match Program.status_abi image with
  | Program.Windows_x64 -> 1
  | Program.System_v_x64 -> 2

let limits_for image ~output_bytes ~output_work =
  Obj.repr
    ( 1000,
      1024,
      8,
      65536,
      Program.entry_stack_bytes image,
      1024,
      1024,
      output_bytes,
      output_work )

let storage_for image =
  Obj.repr
    ( Program.global_bytes image,
      Program.literal_bytes image,
      Program.arena_metadata_bytes image,
      Program.global_image image )

let execute_image ?(output_bytes = 16) ?(output_work = 16) image =
  execute_output (Program.code image)
    (Array.of_list (Program.windows_unwind_functions image))
    (abi_of_image image)
    (limits_for image ~output_bytes ~output_work)
    (storage_for image)

let check_success label expected_output expected_work image =
  let (kind, _, _, value_site, bits), output, work = execute_image image in
  Alcotest.(check int64) (label ^ " status") 0L kind;
  Alcotest.(check bool)
    (label ^ " value site") true
    (not (Int64.equal value_site 0L));
  Alcotest.(check int64) (label ^ " final bits") 42L bits;
  Alcotest.(check string) (label ^ " output") expected_output output;
  Alcotest.(check int) (label ^ " work") expected_work work

let valid_empty_and_storage_arenas () =
  let output_only = source_image "extern U0 PutChars(U64 ch);'42';42;" in
  Alcotest.(check bool)
    "output-only fixture is authenticated" true
    (Program.has_output output_only);
  Alcotest.(check int)
    "output-only global bytes" 0
    (Program.global_bytes output_only);
  Alcotest.(check int)
    "output-only literal bytes" 0
    (Program.literal_bytes output_only);
  Alcotest.(check int)
    "output-only metadata bytes" 0
    (Program.arena_metadata_bytes output_only);
  Alcotest.(check string)
    "output-only arena is empty" ""
    (Program.global_image output_only);
  check_success "output-only" "42" 4 output_only;

  let with_storage =
    source_image "U8 G;extern U0 PutChars(U64 ch);G=1;'42';G+41;"
  in
  Alcotest.(check bool)
    "storage fixture is authenticated" true
    (Program.has_output with_storage);
  Alcotest.(check bool)
    "storage fixture has an arena" true
    (String.length (Program.global_image with_storage) > 0);
  check_success "storage" "42" 4 with_storage

let report_fault_prefixes () =
  let image = source_image "extern U0 PutChars(U64 ch);'42';42;" in
  let exact =
    Runtime.execute_report ~max_output_bytes:2 ~max_output_work:4
      ~max_steps:1000 image
  in
  (match Runtime.outcome exact with
  | Ok (Program.Completed execution) -> (
      match execution.final_value with
      | Some value -> Alcotest.(check int64) "report final bits" 42L value.bits
      | None -> Alcotest.fail "exact output report has no final value")
  | Ok (Program.Fault _) | Error _ -> Alcotest.fail "exact output limits failed");
  Alcotest.(check string)
    "exact captured bytes" "42"
    (Runtime.output_bytes exact);
  Alcotest.(check int) "exact output work" 4 (Runtime.output_work exact);

  let byte_fault =
    Runtime.execute_report ~max_output_bytes:1 ~max_output_work:16
      ~max_steps:1000 image
  in
  (match Runtime.outcome byte_fault with
  | Ok (Program.Fault fault) ->
      Alcotest.(check bool)
        "byte fault kind" true
        (fault.kind = Program.Output_limit_exceeded)
  | Ok (Program.Completed _) | Error _ ->
      Alcotest.fail "output byte limit did not fault");
  Alcotest.(check string)
    "byte fault retains prefix" "4"
    (Runtime.output_bytes byte_fault);
  Alcotest.(check int) "byte fault work" 4 (Runtime.output_work byte_fault);

  let work_fault =
    Runtime.execute_report ~max_output_bytes:16 ~max_output_work:3
      ~max_steps:1000 image
  in
  (match Runtime.outcome work_fault with
  | Ok (Program.Fault fault) ->
      Alcotest.(check bool)
        "work fault kind" true
        (fault.kind = Program.Output_work_limit_exceeded)
  | Ok (Program.Completed _) | Error _ ->
      Alcotest.fail "output work limit did not fault");
  Alcotest.(check string)
    "work fault retains prefix" "4"
    (Runtime.output_bytes work_fault);
  Alcotest.(check int)
    "work fault exact charge" 3
    (Runtime.output_work work_fault)

let mixed_arena_minor_gc () =
  let image = source_image "U8 G;extern U0 PutChars(U64 ch);G=1;'42';G+41;" in
  let original = Gc.get () in
  Fun.protect
    ~finally:(fun () -> Gc.set original)
    (fun () ->
      Gc.set { original with minor_heap_size = 4096 };
      Gc.minor ();
      let before = (Gc.quick_stat ()).minor_collections in
      for round = 1 to 128 do
        let (kind, _, _, value_site, bits), output, work =
          execute_image ~output_bytes:1024 image
        in
        Alcotest.(check int64)
          (Printf.sprintf "minor-GC status %d" round)
          0L kind;
        Alcotest.(check bool)
          (Printf.sprintf "minor-GC value site %d" round)
          true
          (not (Int64.equal value_site 0L));
        Alcotest.(check int64)
          (Printf.sprintf "minor-GC final bits %d" round)
          42L bits;
        Alcotest.(check string)
          (Printf.sprintf "minor-GC output %d" round)
          "42" output;
        Alcotest.(check int) (Printf.sprintf "minor-GC work %d" round) 4 work
      done;
      let after = (Gc.quick_stat ()).minor_collections in
      Alcotest.(check bool)
        "capture stress performs a minor collection" true (after > before))

let () =
  if abi = 0 then failwith "native output bridge tests require x86-64";
  Alcotest.run "Native output bridge validation"
    [
      ( "before allocation",
        [
          Alcotest.test_case "tuple shapes and tags" `Quick malformed_tuples;
          Alcotest.test_case "runtime limits" `Quick limit_validation;
          Alcotest.test_case "storage counts and exact arena" `Quick
            storage_validation;
        ] );
      ( "execution",
        [
          Alcotest.test_case "empty and nonempty arenas" `Quick
            valid_empty_and_storage_arenas;
          Alcotest.test_case "report preserves reached fault prefixes" `Quick
            report_fault_prefixes;
          Alcotest.test_case "mixed arena survives capture allocation GC" `Quick
            mixed_arena_minor_gc;
        ] );
    ]
