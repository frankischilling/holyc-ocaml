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

let () =
  Alcotest.run "U0 initializer native oracle"
    [
      ( "source",
        [
          Alcotest.test_case "complete repeated capture and source hashes"
            `Quick (fun () -> ignore (fixture ()));
          Alcotest.test_case "IR and native preserve observed effects and bytes"
            `Quick execution;
        ] );
    ]
