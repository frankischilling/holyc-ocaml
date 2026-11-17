open Holyc_lib
open Yojson.Safe.Util

let read name =
  let channel = open_in_bin ("oracle/empty-arrays/" ^ name) in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture stem =
  let metadata = Yojson.Safe.from_string (read (stem ^ ".json")) in
  Alcotest.(check string)
    "pinned reference" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    (metadata |> member "reference_commit" |> to_string);
  List.iter
    (fun (suffix, key) ->
      Alcotest.(check string)
        "retained capture hash"
        (metadata |> member key |> to_string)
        (Digestif.SHA256.digest_string (read (stem ^ suffix))
        |> Digestif.SHA256.to_hex))
    [ (".hc", "payload_sha256"); (".capture", "capture_sha256") ];
  let fields =
    metadata |> member "fields" |> to_assoc
    |> List.map (fun (key, value) -> (key, to_int value))
  in
  let one =
    List.map (fun (key, value) -> key ^ "=" ^ string_of_int value) fields
    @ [ "POINTER_DONE" ]
  in
  let records =
    read (stem ^ ".capture")
    |> String.split_on_char '\n'
    |> List.filter (( <> ) "")
  in
  Alcotest.(check (list string))
    "two complete same-boot observations" (one @ one) records;
  fields

let evaluate mode name expected contents =
  let inputs () =
    let session = Session.create () in
    let source =
      Session.add_source session ~path:"empty-array-oracle.hc" ~contents
    in
    let config =
      Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
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
    (Some (Int64.of_int expected))
    (Option.map
       (fun word -> word.Ir_integer_interpreter.bits)
       (Ir_integer_interpreter.final_value result.value));
  let session, source, config = inputs () in
  let report =
    Native_source_execution.evaluate ~max_code_bytes:524288 session ~config
      ~source ~max_steps:100000
  in
  let result = Native_source_execution.outcome report |> Result.get_ok in
  Alcotest.(check (option int64))
    (name ^ " native")
    (Some (Int64.of_int expected))
    (Option.map
       (fun word -> word.Native_source_execution.bits)
       result.value.final_value)

let execution () =
  let fields = fixture "native" in
  let cases =
    [
      ("GLOBAL_STRING_BYTES", {|U8 text[]="AB";sizeof(text);|});
      ("GLOBAL_WORD_BYTES", {|I64 words[]={};sizeof(words);|});
      ( "GLOBAL_CLASS_BYTES",
        {|class Item{U8 value;};Item items[]={};sizeof(items);|} );
      ( "STATIC_STRING_BYTES",
        {|I64 F(){static U8 text[]="AB";return sizeof(text);}F();|} );
      ( "STATIC_WORD_BYTES",
        {|I64 F(){static I64 words[]={};return sizeof(words);}F();|} );
      ("AUTOMATIC_BYTES", {|I64 F(){U8 text[];return sizeof(text);}F();|});
      ("FIXED_BYTES", {|U8 text[3]="AB";sizeof(text);|});
      ("FIXED_LAST", {|U8 text[3]="AB";I64 F(){return text[2];}F();|});
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (name, contents) ->
          evaluate mode name (List.assoc name fields) contents)
        cases)
    [ Preprocessor.Jit; Preprocessor.Aot ];
  let fields = fixture "controls" in
  let dimensions =
    {|I64 count=0;I64 Size(){count++;return 0;}
I64 values[0][Size()]={};I64 Unused(){static I64 a[Size()]={};return sizeof(a);}
I64 before=count;I64 total=Unused()+Unused();|}
  in
  List.iter
    (fun (name, value) ->
      evaluate Preprocessor.Jit name (List.assoc name fields)
        (dimensions ^ value ^ ";"))
    [
      ("DIMENSION_CALLS", "before");
      ("STATIC_SUM", "total");
      ("CALLS_AFTER", "count");
    ];
  List.iter
    (fun mode ->
      List.iter
        (fun (name, contents) ->
          evaluate mode name (List.assoc name fields) contents)
        [
          ( "FOLLOWING_BYTE",
            {|class Box{U8 empty[];U8 value;};Box object={"AB",42};I64 F(){return object.value;}F();|}
          );
          ("PACKED_BYTES", {|class Box{U8 empty[];U8 value;};sizeof(Box);|});
          ("FRACTIONAL_BYTES", {|I64 values[0.5]={};sizeof(values);|});
          ("NEGATIVE_FRACTIONAL_BYTES", {|I64 values[-0.5]={};sizeof(values);|});
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let failures () =
  let fields = fixture "native" in
  List.iter
    (fun name -> Alcotest.(check int) name 0 (List.assoc name fields))
    [ "NONEMPTY_REACHED"; "NONEMPTY_CALLS"; "NEGATIVE_REACHED" ];
  List.iter
    (fun mode ->
      List.iter
        (fun contents ->
          let inputs () =
            let session = Session.create () in
            let source =
              Session.add_source session ~path:"empty-array-rejection.hc"
                ~contents
            in
            let config =
              Preprocessor.Config.create ~compilation_mode:mode ()
              |> Result.get_ok
            in
            (session, source, config)
          in
          let session, source, config = inputs () in
          let report =
            run_integer_program_report session ~config ~source ~max_steps:100000
          in
          Alcotest.(check bool)
            "invalid declaration does not complete" true
            (Result.is_error (integer_program_report_outcome report));
          Alcotest.(check string)
            "ordinary leaf and later output are not reached" ""
            (integer_program_report_output_bytes report);
          let session, source, config = inputs () in
          let report =
            Native_source_execution.evaluate session ~config ~source
              ~max_steps:100000
          in
          Alcotest.(check bool)
            "native invalid declaration does not complete" true
            (Result.is_error (Native_source_execution.outcome report));
          Alcotest.(check string)
            "native ordinary leaf and later output are not reached" ""
            (Native_source_execution.output_bytes report))
        [
          {|extern U0 Print(U8 *fmt,...);I64 Effect(){Print("LEAF");return 42;}I64 values[]={Effect()};Print("AFTER");|};
          {|extern U0 Print(U8 *fmt,...);I64 values[-1]={};Print("AFTER");|};
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let unqualified_address () =
  let metadata = Yojson.Safe.from_string (read "unqualified-address.json") in
  Alcotest.(check string)
    "unqualified probe uses the pinned reference"
    "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    (metadata |> member "reference_commit" |> to_string);
  Alcotest.(check string)
    "retained original fault observation" "guest-general-protection"
    (metadata |> member "outcome" |> to_string);
  Alcotest.(check bool)
    "faulting probe supplies no completed value capture" false
    (metadata |> member "complete_value_capture" |> to_bool);
  List.iter
    (fun (name, key) ->
      Alcotest.(check string)
        "retained unqualified probe artifact"
        (metadata |> member key |> to_string)
        (Digestif.SHA256.digest_string (read name) |> Digestif.SHA256.to_hex))
    [
      ("unqualified-address.hc", "payload_sha256");
      ("unqualified-address.png", "screenshot_sha256");
    ];
  let definition =
    read "unqualified-address.hc"
    |> String.split_on_char '\n'
    |> List.find (String.starts_with ~prefix:"I64 ECAddresses()")
  in
  List.iter
    (fun mode ->
      let inputs () =
        let session = Session.create () in
        let source =
          Session.add_source session ~path:"unqualified-empty-address.hc"
            ~contents:(definition ^ "ECAddresses();")
        in
        let config =
          Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
        in
        (session, source, config)
      in
      let check = function
        | Ok _ ->
            Alcotest.fail "unqualified automatic empty-array address executed"
        | Error errors ->
            Alcotest.(check bool)
              "explicit lowering boundary" true
              (List.exists
                 (fun (d : Diagnostic.t) -> d.code = "HCRUN0003")
                 errors)
      in
      let session, source, config = inputs () in
      check
        (run_integer_program_report session ~config ~source ~max_steps:100000
        |> integer_program_report_outcome);
      let session, source, config = inputs () in
      check
        (Native_source_execution.evaluate session ~config ~source
           ~max_steps:100000
        |> Native_source_execution.outcome))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let () =
  Alcotest.run "Empty array native oracle"
    [
      ( "source",
        [
          Alcotest.test_case "complete repeated captures and hashes" `Quick
            (fun () ->
              ignore (fixture "native");
              ignore (fixture "controls"));
          Alcotest.test_case
            "zero sizes, following bytes and original dimension effects" `Quick
            execution;
          Alcotest.test_case
            "invalid initializers do not infer or run their leaves" `Quick
            failures;
          Alcotest.test_case "faulting address probe is not a value oracle"
            `Quick unqualified_address;
        ] );
    ]
