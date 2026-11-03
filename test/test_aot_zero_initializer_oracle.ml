open Holyc_lib
open Yojson.Safe.Util

let read name =
  let channel = open_in_bin ("oracle/aot-zero-initializers/" ^ name) in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture stem =
  let metadata = Yojson.Safe.from_string (read (stem ^ ".json")) in
  Alcotest.(check string)
    "pinned reference" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    (metadata |> member "reference_commit" |> to_string);
  List.iter
    (fun (name, key) ->
      Alcotest.(check string)
        "retained native evidence"
        (metadata |> member key |> to_string)
        (Digestif.SHA256.digest_string (read name) |> Digestif.SHA256.to_hex))
    [ (stem ^ ".hc", "payload_sha256"); (stem ^ ".capture", "capture_sha256") ];
  let fields =
    metadata |> member "fields" |> to_assoc
    |> List.map (fun (name, value) -> (name, to_int value))
  in
  let one =
    List.map (fun (name, value) -> name ^ "=" ^ string_of_int value) fields
    @ [ "POINTER_DONE" ]
  in
  let actual =
    read (stem ^ ".capture")
    |> String.split_on_char '\n'
    |> List.filter (( <> ) "")
  in
  Alcotest.(check (list string))
    "two complete native observations" (one @ one) actual;
  fields

let run native mode contents =
  let session = Session.create () in
  let source = Session.add_source session ~path:"warning-oracle.hc" ~contents in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  if native then
    let report =
      Native_source_execution.evaluate ~max_code_bytes:524288 ~max_steps:100000
        session ~config ~source
    in
    let output = Native_source_execution.output_bytes report in
    match Native_source_execution.outcome report with
    | Ok result ->
        ( result.diagnostics,
          Ok
            (Option.map
               (fun w -> w.Native_source_execution.bits)
               result.value.final_value),
          output )
    | Error diagnostics -> (diagnostics, Error (), output)
  else
    let report =
      run_integer_program_report session ~config ~source ~max_steps:100000
    in
    let output = integer_program_report_output_bytes report in
    match integer_program_report_outcome report with
    | Ok result ->
        ( result.diagnostics,
          Ok
            (Option.map
               (fun w -> w.Ir_integer_interpreter.bits)
               (Ir_integer_interpreter.final_value result.value)),
          output )
    | Error diagnostics -> (diagnostics, Error (), output)

let controls () =
  let fields = fixture "integer" in
  Alcotest.(check int)
    "original JIT copy width" 42
    (List.assoc "JIT_INTEGER" fields);
  Alcotest.(check int)
    "original AOT assignment width" 7
    (List.assoc "AOT_INTEGER" fields);
  Alcotest.(check int)
    "original static AOT assignment" 7
    (List.assoc "AOT_STATIC" fields);
  List.iter
    (fun native ->
      List.iter
        (fun mode ->
          let diagnostics, result, output =
            run native mode
              "class Box{U0 empty;I64 value;};Box object={7,42};I64 F(){return \
               object.value;}F();"
          in
          Alcotest.(check (option int64))
            "original constant copy remains empty"
            (Some (Int64.of_int (List.assoc "AOT_CONSTANT" fields)))
            (Result.get_ok result);
          Alcotest.(check int)
            "constant has no warnings" 0 (List.length diagnostics);
          Alcotest.(check string) "constant has no effects" "" output)
        [ Preprocessor.Jit; Preprocessor.Aot ])
    [ false; true ]

let original_store_width () =
  let fields = fixture "pattern" in
  List.iter
    (fun name ->
      Alcotest.(check int64)
        "original empty copy retains all bytes" 0x112233445566772aL
        (Int64.of_int (List.assoc name fields)))
    [ "JIT_INTEGER"; "AOT_CONSTANT" ];
  List.iter
    (fun name ->
      Alcotest.(check int64)
        "AOT U0 assignment overwrites low four bytes" 0x1122334400000007L
        (Int64.of_int (List.assoc name fields)))
    [ "AOT_INTEGER"; "AOT_STATIC" ]

let boundaries () =
  List.iter
    (fun native ->
      List.iter
        (fun contents ->
          let _, jit, _ = run native Preprocessor.Jit contents in
          Alcotest.(check (option int64))
            "JIT retains its empty copy" (Some 42L) (Result.get_ok jit);
          let diagnostics, aot, output = run native Preprocessor.Aot contents in
          Alcotest.(check bool)
            "AOT never substitutes JIT empty completion" true
            (Result.is_error aot);
          Alcotest.(check (list string))
            "separate load-assignment boundary" [ "HCRUN0006" ]
            (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics);
          Alcotest.(check string) "unsupported load is not executed" "" output)
        [
          "I64 Get(){return 7;}class Box{U0 empty;I64 value;};Box \
           object={Get(),42};I64 F(){return object.value;}F();";
          "I64 Get(){return 7;}class Box{U0 empty;I64 value;};I64 F(){static \
           Box object={Get(),42};return object.value;}F();";
          "U0 Get(){}class Box{U0 empty;I64 value;};Box object={Get(),42};I64 \
           F(){return object.value;}F();";
          "U0 Get(){}class Box{U0 empty;I64 value;};I64 F(){static Box \
           object={Get(),42};return object.value;}F();";
          "I64 Get(){return 7;}class Empty{};Empty object=Get();42;";
        ])
    [ false; true ]

let () =
  Alcotest.run "AOT zero-byte initializer phases"
    [
      ( "source",
        [
          Alcotest.test_case "repeated native BIN load records and hashes"
            `Quick (fun () ->
              ignore (fixture "void");
              ignore (fixture "integer"));
          Alcotest.test_case
            "patterned native stores retain their observed width" `Quick
            original_store_width;
          Alcotest.test_case "constant copies retain their original width"
            `Quick controls;
          Alcotest.test_case
            "nonconstant assignments need their own storage operation" `Quick
            boundaries;
        ] );
    ]
