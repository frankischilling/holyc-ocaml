open Holyc_lib
open Yojson.Safe.Util

let read name =
  let channel = open_in_bin ("oracle/void-initializers/" ^ name) in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture () =
  let metadata = Yojson.Safe.from_string (read "native.json") in
  Alcotest.(check string)
    "pinned reference" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    (metadata |> member "reference_commit" |> to_string);
  List.iter
    (fun (file, key) ->
      Alcotest.(check string)
        file
        (metadata |> member key |> to_string)
        (Digestif.SHA256.digest_string (read file) |> Digestif.SHA256.to_hex))
    [ ("native.hc", "payload_sha256"); ("native.capture", "capture_sha256") ];
  let expected =
    metadata |> member "fields" |> to_assoc
    |> List.map (fun (name, value) -> (name, to_int value))
  in
  let records =
    read "native.capture" |> String.split_on_char '\n'
    |> List.filter (( <> ) "")
  in
  let one =
    List.map (fun (name, value) -> name ^ "=" ^ string_of_int value) expected
    @ [ "VOID_DONE" ]
  in
  Alcotest.(check (list string))
    "two complete same-boot observations" (one @ one) records;
  expected

let program expression =
  {|I64 count=0;U0 Effect(){count++;}class Box{U0 empty;U8 value;};
Box object={Effect(),42};I64 before=count;
I64 Static(){static Box saved={Effect(),42};return saved.value;}
I64 twice=Static()+Static();|}
  ^ expression ^ ";"

let expressions =
  [
    ("GLOBAL_COUNT", "before");
    ("GLOBAL_VALUE", "object.value");
    ("STATIC_VALUE", "twice");
    ("STATIC_COUNT", "count");
  ]

let execution () =
  let expected = fixture () in
  List.iter
    (fun (name, expression) ->
      let contents = program expression in
      let inputs () =
        let session = Session.create () in
        let source =
          Session.add_source session ~path:"void-oracle.hc" ~contents
        in
        let config =
          Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
          |> Result.get_ok
        in
        (session, source, config)
      in
      let session, source, config = inputs () in
      let report =
        run_integer_program_report session ~config ~source ~max_steps:100000
      in
      let result = integer_program_report_outcome report |> Result.get_ok in
      Alcotest.(check (option int64))
        (name ^ " IR")
        (Some (Int64.of_int (List.assoc name expected)))
        (Option.map
           (fun word -> word.Ir_integer_interpreter.bits)
           (Ir_integer_interpreter.final_value result.value));
      let session, source, config = inputs () in
      let report =
        Native_source_execution.evaluate ~max_code_bytes:524288
          ~max_steps:100000 session ~config ~source
      in
      let result = Native_source_execution.outcome report |> Result.get_ok in
      Alcotest.(check (option int64))
        (name ^ " native")
        (Some (Int64.of_int (List.assoc name expected)))
        (Option.map
           (fun word -> word.Native_source_execution.bits)
           result.value.final_value))
    expressions

let warning_fixture ?(stem = "warnings") () =
  let metadata = Yojson.Safe.from_string (read (stem ^ ".json")) in
  Alcotest.(check string)
    "warning reference" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    (metadata |> member "reference_commit" |> to_string);
  List.iter
    (fun (file, key) ->
      Alcotest.(check string)
        "warning evidence hash"
        (metadata |> member key |> to_string)
        (Digestif.SHA256.digest_string (read file) |> Digestif.SHA256.to_hex))
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
    "complete repeated original warning counts" (one @ one) actual;
  fields

let warning_run native contents =
  let session = Session.create () in
  let source = Session.add_source session ~path:"warning-oracle.hc" ~contents in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
    |> Result.get_ok
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

let warning_execution () =
  let fields = warning_fixture () in
  List.iter
    (fun native ->
      List.iter
        (fun (field, contents, expected) ->
          let diagnostics, result, output = warning_run native contents in
          Alcotest.(check (option int64))
            "native-observed value" (Some expected) (Result.get_ok result);
          Alcotest.(check string) "warning produces no program output" "" output;
          Alcotest.(check int)
            field (List.assoc field fields) (List.length diagnostics);
          List.iter
            (fun (d : Diagnostic.t) ->
              Alcotest.(check string)
                "original warning code" "HCSEMA0079" d.code;
              Alcotest.(check bool)
                "warning severity" true
                (d.severity = Diagnostic.Warning);
              Alcotest.(check string)
                "original warning wording" "U0 Expression" d.message;
              Alcotest.(check string)
                "warning retains its expression span" "Effect()"
                (String.sub contents d.primary.start
                   (d.primary.stop - d.primary.start)))
            diagnostics)
        [
          ("STATEMENT_WARNINGS", "U0 Effect(){}Effect();42;", 42L);
          ( "AGGREGATE_WARNINGS",
            "U0 Effect(){}class Box{U0 empty;U8 value;};Box \
             object={Effect(),42};I64 F(){return object.value;}F();",
            Int64.of_int (List.assoc "AGGREGATE_VALUE" fields) );
          ( "STATIC_WARNINGS",
            "U0 Effect(){}class Box{U0 empty;U8 value;};I64 F(){static Box \
             object={Effect(),42};return object.value;}42;",
            42L );
          ( "WARNINGS_AFTER_CALLS",
            "U0 Effect(){}class Box{U0 empty;U8 value;};I64 F(){static Box \
             object={Effect(),42};return object.value;}F()+F();",
            Int64.of_int (List.assoc "STATIC_SUM" fields) );
        ];
      let diagnostics, result, output =
        warning_run native
          "extern U0 PutChars(U64 ch);I64 z=0;U0 Effect(){PutChars(65);I64 \
           x=1/z;PutChars(x);}class Box{U0 empty;U8 value;};Box \
           object={Effect(),42};"
      in
      Alcotest.(check bool)
        "callee failure stays a failure" true (Result.is_error result);
      Alcotest.(check string) "earlier callee output survives" "A" output;
      Alcotest.(check (list string))
        "warning precedes the execution fault"
        [ "HCSEMA0079"; "HCIRVM0009" ]
        (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
    [ false; true ]

let warning_controls () =
  let fields = warning_fixture ~stem:"warning-controls" () in
  List.iter
    (fun native ->
      List.iter
        (fun (field, contents) ->
          let diagnostics, result, output = warning_run native contents in
          Alcotest.(check int)
            field (List.assoc field fields) (List.length diagnostics);
          Alcotest.(check string) "word selection has no output" "" output;
          Alcotest.(check (option int64))
            "word selection does not call the function" (Some 42L)
            (Result.get_ok result))
        [
          ( "CALLBACK_WORD_WARNINGS",
            "I64 count=0;U0 Effect(){count++;}U0 (*p)()=&Effect;class Box{U0 \
             empty;U8 value;};Box object={p,42};I64 F(){if(count)return \
             -1;return object.value;}F();" );
          ( "INTEGER_LEAF_WARNINGS",
            "class Box{U0 empty;U8 value;};Box object={7,42};I64 F(){return \
             object.value;}F();" );
        ];
      let diagnostics, result, output =
        warning_run native
          "I64 count=0;U0 Effect(){count++;}class Box{U0 empty;U8 value;};Box \
           object={&Effect,42};I64 F(){if(count)return -1;return \
           object.value;}F();"
      in
      Alcotest.(check (list string))
        "a function address retains its physical pointer class" []
        (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics);
      Alcotest.(check (option int64))
        "zero-byte function-address leaf executes without a call" (Some 42L)
        (Result.get_ok result);
      Alcotest.(check string) "function address produces no output" "" output)
    [ false; true ]

let () =
  Alcotest.run "U0 initializer native oracle"
    [
      ( "source",
        [
          Alcotest.test_case "complete repeated capture and source hashes"
            `Quick (fun () -> ignore (fixture ()));
          Alcotest.test_case "IR and native preserve observed effects and bytes"
            `Quick execution;
          Alcotest.test_case "original JIT warning counts and failure order"
            `Quick warning_execution;
          Alcotest.test_case
            "callback words retain their physical expression class" `Quick
            warning_controls;
        ] );
    ]
