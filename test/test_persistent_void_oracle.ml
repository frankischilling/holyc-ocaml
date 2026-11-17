open Holyc_lib
open Yojson.Safe.Util

let read name =
  let channel = open_in_bin ("oracle/persistent-void/" ^ name) in
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
  {|I64 count=0,target=7;U0 Effect(){count++;}I64 Word(){count++;return 42;}
I64 *Ref(){count++;return &target;}U0 empty,from_void=Effect(),from_word=Word(),from_ref=Ref();
I64 before=count;I64 Static(){static U0 empty,value=Effect();return sizeof(empty)+sizeof(value)+21;}
I64 static_before=count;I64 sum=Static()+Static();I64 after=count;|}

let expressions =
  [
    ("UNINITIALIZED_SIZE", "sizeof(empty)");
    ("VOID_SIZE", "sizeof(from_void)");
    ("GLOBAL_CALLS", "before");
    ("STATIC_CALLS_BEFORE", "static_before");
    ("STATIC_SUM", "sum");
    ("STATIC_CALLS_AFTER", "after");
    ("TARGET", "target");
  ]

let execution () =
  let fields = fixture () in
  List.iter
    (fun (field, expression) ->
      let inputs () =
        let session = Session.create () in
        let source =
          Session.add_source session ~path:"persistent-void-oracle.hc"
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
        "only the two U0 calls warn" 2
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
        "only native U0 calls warn" 2
        (List.length result.diagnostics);
      Alcotest.(check string)
        "native output" ""
        (Native_source_execution.output_bytes report))
    expressions

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

let failed_initializers () =
  List.iter
    (fun native ->
      List.iter
        (fun (body, codes) ->
          let diagnostics, result, output =
            warning_run native ("extern U0 PutChars(U64 ch);I64 z=0;" ^ body)
          in
          Alcotest.(check bool)
            "failed leaf cannot complete" true (Result.is_error result);
          Alcotest.(check string) "earlier source output survives" "A" output;
          Alcotest.(check (list string))
            "original warning and execution fault order" codes
            (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
        [
          ( "U0 Effect(){PutChars(65);I64 n=1/z;PutChars(n);}U0 x=Effect();",
            [ "HCSEMA0079"; "HCIRVM0009" ] );
          ( "U0 Effect(){PutChars(65);I64 n=1/z;PutChars(n);}I64 F(){static U0 \
             x=Effect();return sizeof(x);}",
            [ "HCSEMA0079"; "HCIRVM0009" ] );
          ( "I64 Effect(){PutChars(65);return 1/z;}U0 x=Effect();",
            [ "HCIRVM0009" ] );
          ( "I64 value=7;I64 *Effect(){PutChars(65);I64 n=1/z;return \
             &value+n;}U0 x=Effect();",
            [ "HCIRVM0009" ] );
        ])
    [ false; true ]

let zero_storage_shape () =
  let module Shape = Holyc_lib__Ir.Integer_storage_shape in
  let module Type = Holyc_lib__Sema.Type in
  let module Primitive = Holyc_lib__Sema.Primitive_type in
  let type_ =
    Type.make_primitive ~form:Type.Public_spelling ~primitive:Primitive.U0
      ~pointer_depth:0
    |> Result.get_ok
  in
  let shape = Shape.create ~type_ ~dimensions:[] |> Result.get_ok in
  Alcotest.(check int) "no source bytes" 0 (Shape.byte_size shape);
  Alcotest.(check int) "no scalar cells or flags" 0 (Shape.cell_count shape);
  Alcotest.(check int)
    "one original scalar declaration" 1
    (Shape.element_count shape);
  Alcotest.(check bool)
    "U0 retains its own shape" true
    (Shape.is_storage_free shape);
  Alcotest.(check bool)
    "U0 is not a nominal class" false (Shape.is_aggregate shape);
  Alcotest.(check bool)
    "U0 retains byte-counted leaves" true
    (Shape.has_byte_leaves shape);
  List.iter
    (fun dimensions ->
      Alcotest.(check bool)
        "scalar admission does not authorize U0 arrays" true
        (Result.is_error (Shape.create ~type_ ~dimensions)))
    [ [ 0L ]; [ 1L ]; [ 2L ] ];
  Alcotest.(check bool)
    "U0 has no integer storage type" true
    (Option.is_none (Holyc_lib__Ir.Integer_scalar_storage.of_type type_))

let () =
  Alcotest.run "Persistent U0 initializer oracle"
    [
      ( "source",
        [
          Alcotest.test_case "failed leaves preserve output and warning order"
            `Quick failed_initializers;
          Alcotest.test_case "U0 shape retains no scalar cell or class owner"
            `Quick zero_storage_shape;
          Alcotest.test_case "original repeated records and hashes" `Quick
            (fun () -> ignore (fixture ()));
          Alcotest.test_case
            "IR and native preserve zero storage and declaration effects" `Quick
            execution;
        ] );
    ]
