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
  rejects "metadata without an object"
    "native storage program has no persistent data"
    ~storage:(Obj.repr (0, 0, 1, "\000"))
    ()

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
    ]
