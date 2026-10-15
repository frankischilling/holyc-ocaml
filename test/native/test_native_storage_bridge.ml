open Holyc_lib
module Runtime = Native_program_execution

external execute_storage :
  string ->
  (int * int * string) array ->
  int ->
  Obj.t ->
  Obj.t ->
  int64 * int64 * int64 * int64 * int64 = "holyc_native_execute_program_storage"

let abi =
  match Runtime.platform () with
  | Runtime.Windows_x86_64 -> 1
  | Runtime.Linux_x86_64 -> 2
  | Runtime.Unsupported -> 0

let limits = Obj.repr (100, 1024, 8, 65536, 32, 1024, 1024)
let storage = Obj.repr (1, 0, 1, "\000\000")

let rejects label message ?(limits = limits) ?(storage = storage) () =
  Alcotest.check_raises label (Invalid_argument message) (fun () ->
      ignore (execute_storage "" [||] abi limits storage))

let malformed_tuples () =
  List.iter
    (fun (label, limits) ->
      rejects label "native storage program limits tuple is malformed" ~limits
        ())
    [
      ("immediate limits", Obj.repr 0);
      ("old six-field limits", Obj.repr (100, 1024, 8, 65536, 32, 1024));
      ("extra limit field", Obj.repr (100, 1024, 8, 65536, 32, 1024, 1024, 0));
      ("boxed literal limit", Obj.repr (100, 1024, 8, 65536, 32, 1024, "1024"));
    ];
  List.iter
    (fun (label, storage) ->
      rejects label "native program storage tuple is malformed" ~storage ())
    [
      ("immediate storage", Obj.repr 0);
      ("old storage tuple", Obj.repr (1, "\000\000"));
      ("missing metadata count", Obj.repr (1, 0, "\000\000"));
      ("boxed global count", Obj.repr (1L, 0, 1, "\000\000"));
      ("boxed literal count", Obj.repr (1, "0", 1, "\000\000"));
      ("boxed metadata count", Obj.repr (1, 0, "1", "\000\000"));
    ];
  rejects "non-string arena" "native program arena image is not a string"
    ~storage:(Obj.repr (1, 0, 1, [| 0; 0 |]))
    ()

let count_limits () =
  List.iter
    (fun literal_limit ->
      rejects "invalid literal limit"
        "native program max_literal_bytes is outside the host bound"
        ~limits:(Obj.repr (100, 1024, 8, 65536, 32, 1024, literal_limit))
        ())
    [ -1; 0; 16_777_217 ];
  List.iter
    (fun bytes ->
      rejects "invalid global data count"
        "native program logical global bytes exceed their bound"
        ~storage:(Obj.repr (bytes, 0, 1, "\000\000"))
        ())
    [ -1; 1025; 16_777_217 ];
  List.iter
    (fun bytes ->
      rejects "invalid literal data count"
        "native program logical literal bytes exceed their bound"
        ~storage:(Obj.repr (0, bytes, 0, "\000"))
        ())
    [ -1; 1025; 16_777_217 ];
  List.iter
    (fun bytes ->
      rejects "invalid private metadata count"
        "native program private metadata bytes exceed their bound"
        ~storage:(Obj.repr (1, 0, bytes, "\000"))
        ())
    [ -1; 33_554_433 ]

let exact_arena_accounting () =
  List.iter
    (fun storage ->
      rejects "inconsistent arena length"
        "native program arena image is inconsistent with data and metadata"
        ~storage ())
    [
      Obj.repr (1, 0, 1, "\000");
      Obj.repr (1, 0, 1, "\000\000\000");
      Obj.repr (0, 1, 32, "\000");
      Obj.repr (1, 1, 0, "\000");
      Obj.repr (1024, 1024, 33_554_432, "\000");
    ];
  rejects "metadata-only arena still requires a checked code image"
    "native program unwind table has an unsupported function count"
    ~storage:(Obj.repr (0, 0, 1, "\000"))
    ()

let metadata_only_execution () =
  let module Program = X86_64_program in
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-empty-storage-bridge.hc"
      ~contents:
        "class Empty{};Empty object;I64 F(){static Empty saved;return \
         sizeof(object)+sizeof(saved)+42;}F();"
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
    |> Result.get_ok
  in
  let image =
    match Native_program.compile session ~config ~source with
    | Ok checked -> checked.value
    | Error diagnostics ->
        Alcotest.fail
          (List.map
             (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
             diagnostics
          |> String.concat "; ")
  in
  Alcotest.(check int) "no data bytes" 0 (Program.global_bytes image);
  Alcotest.(check int) "no literal bytes" 0 (Program.literal_bytes image);
  Alcotest.(check int)
    "two original root owner words" 16
    (Program.arena_metadata_bytes image);
  let kind, _, _, _, bits =
    execute_storage (Program.code image)
      (Array.of_list (Program.windows_unwind_functions image))
      abi
      (Obj.repr
         (100, 1024, 8, 65536, Program.entry_stack_bytes image, 1024, 1024))
      (Obj.repr (0, 0, 16, Program.global_image image))
  in
  Alcotest.(check int64) "successful metadata-only entry" 0L kind;
  Alcotest.(check int64) "independent expected value" 42L bits

let () =
  if abi = 0 then failwith "native storage bridge tests require x86-64";
  Alcotest.run "Native storage bridge validation"
    [
      ( "before allocation",
        [
          Alcotest.test_case "tuple shapes and tags" `Quick malformed_tuples;
          Alcotest.test_case "logical and physical limits" `Quick count_limits;
          Alcotest.test_case "exact arena accounting" `Quick
            exact_arena_accounting;
        ] );
      ( "execution",
        [
          Alcotest.test_case "metadata-only arena" `Quick
            metadata_only_execution;
        ] );
    ]
