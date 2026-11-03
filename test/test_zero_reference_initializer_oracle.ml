open Holyc_lib
open Yojson.Safe.Util

let read name =
  let channel = open_in_bin ("oracle/zero-reference-initializers/" ^ name) in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture () =
  let metadata = Yojson.Safe.from_string (read "native.json") in
  Alcotest.(check string)
    "pinned reference" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    (metadata |> member "reference_commit" |> to_string);
  List.iter
    (fun (name, key) ->
      Alcotest.(check string)
        "retained native evidence"
        (metadata |> member key |> to_string)
        (Digestif.SHA256.digest_string (read name) |> Digestif.SHA256.to_hex))
    [ ("native.hc", "payload_sha256"); ("native.capture", "capture_sha256") ];
  let fields =
    metadata |> member "fields" |> to_assoc
    |> List.map (fun (name, value) -> (name, to_int value))
  in
  let one =
    List.map (fun (name, value) -> name ^ "=" ^ string_of_int value) fields
    @ [ "POINTER_DONE" ]
  in
  let actual =
    read "native.capture" |> String.split_on_char '\n'
    |> List.filter (( <> ) "")
  in
  Alcotest.(check (list string))
    "two complete native observations" (one @ one) actual;
  fields

let prefix =
  {|I64 count=0,target=7;U0 Void(){count+=100;}I64 Fn(){count+=100;return 7;}
I64 (*callback)()=&Fn;I64 *Get(){count++;return &target;}U8 bytes[2]={7,8};
class Box{U0 empty;I64 value;};
Box address={&Void,42},data={&target,42},decay={bytes,42},text={"AB",42},returned={Get(),42},word={callback,42};
I64 before=count;I64 Static(){static Box object={Get(),42};return object.value;}
I64 static_before=count;I64 sum=Static()+Static();I64 after=count;|}

let expressions =
  [
    ("FUNCTION_ADDRESS", "address.value");
    ("DATA_ADDRESS", "data.value");
    ("ARRAY_DECAY", "decay.value");
    ("STRING_ADDRESS", "text.value");
    ("RETURNED_REFERENCE", "returned.value");
    ("CALLBACK_WORD", "word.value");
    ("GLOBAL_CALLS", "before");
    ("STATIC_CALLS_BEFORE", "static_before");
    ("STATIC_SUM", "sum");
    ("STATIC_CALLS_AFTER", "after");
  ]

let execution () =
  let fields = fixture () in
  List.iter
    (fun (field, expression) ->
      let inputs () =
        let session = Session.create () in
        let source =
          Session.add_source session ~path:"zero-reference-oracle.hc"
            ~contents:(prefix ^ expression ^ ";")
        in
        let config =
          Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
          |> Result.get_ok
        in
        (session, source, config)
      in
      let expected = Some (Int64.of_int (List.assoc field fields)) in
      let session, source, config = inputs () in
      let report =
        run_integer_program_report session ~config ~source ~max_steps:100000
      in
      let result = integer_program_report_outcome report |> Result.get_ok in
      Alcotest.(check (option int64))
        (field ^ " IR") expected
        (Option.map
           (fun w -> w.Ir_integer_interpreter.bits)
           (Ir_integer_interpreter.final_value result.value));
      Alcotest.(check int)
        "pointer values have no U0 warning" 0
        (List.length result.diagnostics);
      Alcotest.(check string)
        "IR output" ""
        (integer_program_report_output_bytes report);
      let session, source, config = inputs () in
      let report =
        Native_source_execution.evaluate ~max_code_bytes:524288 session ~config
          ~source ~max_steps:100000
      in
      let result = Native_source_execution.outcome report |> Result.get_ok in
      Alcotest.(check (option int64))
        (field ^ " native") expected
        (Option.map
           (fun w -> w.Native_source_execution.bits)
           result.value.final_value);
      Alcotest.(check int)
        "native pointer values have no U0 warning" 0
        (List.length result.diagnostics);
      Alcotest.(check string)
        "native output" ""
        (Native_source_execution.output_bytes report))
    expressions

let () =
  Alcotest.run "Zero-byte reference initializer oracle"
    [
      ( "source",
        [
          Alcotest.test_case "original repeated records and hashes" `Quick
            (fun () -> ignore (fixture ()));
          Alcotest.test_case
            "IR and native preserve references, bytes and declaration phases"
            `Quick execution;
        ] );
    ]
