open Holyc_lib
open Yojson.Safe.Util

let read name =
  let channel = open_in_bin ("oracle/prefix-updates/" ^ name) in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture ?(stem = "native") () =
  let metadata = Yojson.Safe.from_string (read (stem ^ ".json")) in
  Alcotest.(check string)
    "pinned reference" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    (metadata |> member "reference_commit" |> to_string);
  List.iter
    (fun (file, key) ->
      Alcotest.(check string)
        file
        (metadata |> member key |> to_string)
        (Digestif.SHA256.digest_string (read file) |> Digestif.SHA256.to_hex))
    [ (stem ^ ".hc", "payload_sha256"); (stem ^ ".capture", "capture_sha256") ];
  let expected =
    metadata |> member "fields" |> to_assoc
    |> List.map (fun (name, value) -> (name, to_int value))
  in
  let records =
    read (stem ^ ".capture")
    |> String.split_on_char '\n'
    |> List.filter (( <> ) "")
  in
  let one =
    List.map (fun (name, value) -> name ^ "=" ^ string_of_int value) expected
    @ [ "POINTER_DONE" ]
  in
  Alcotest.(check (list string))
    "two complete same-boot observations" (one @ one) records;
  expected

let expressions =
  [
    ( "LOCAL_POINTER",
      {|I64 PMData[3]={40,41,42};I64 PMLocal(){I64 *p=PMData;I64 a=++*p,b=++*p;if(a!=41 || b!=42)return -1;return PMData[0]*10+(p==&PMData[2]);}PMLocal();|}
    );
    ( "LOCAL_DECREMENT",
      {|I64 PMData[3]={40,41,42};I64 PMDown(){I64 *p=&PMData[2];I64 a=--*p,b=--*p;if(a!=41 || b!=40)return -1;return PMData[0]*10+(p==&PMData[0]);}PMDown();|}
    );
    ( "MINUS",
      {|I64 PMData[3]={40,41,42};I64 PMMinus(){I64 x=4,y=++-x;return x*100+y;}PMMinus();|}
    );
    ( "NOT",
      {|I64 PMData[3]={40,41,42};I64 PMNot(){I64 x=0,y=++!x;return x*100+y;}PMNot();|}
    );
  ]

let execution () =
  let expected = fixture () @ fixture ~stem:"controls" () in
  let fields =
    Yojson.Safe.from_string (read "native.json")
    |> member "implemented_fields"
    |> to_list |> List.map to_string
  in
  Alcotest.(check (list string))
    "implemented capture fields are explicit" fields (List.map fst expressions);
  let payload = read "controls.hc" in
  let contains needle =
    let rec scan index =
      index + String.length needle <= String.length payload
      && (String.sub payload index (String.length needle) = needle
         || scan (index + 1))
    in
    scan 0
  in
  let controls =
    List.mapi
      (fun index (_, contents, _, _) ->
        Alcotest.(check bool)
          "maintained control retains captured source" true
          (String.ends_with ~suffix:"F();" contents
          && contains (String.sub contents 0 (String.length contents - 4)));
        ("CONTROL_" ^ string_of_int index, contents))
      Persistent_aggregate_cases.pending_prefix_values
  in
  let expressions = expressions @ controls in
  List.iter
    (fun mode ->
      List.iter
        (fun (name, expression) ->
          let contents = expression in
          let inputs () =
            let session = Session.create () in
            let source =
              Session.add_source session ~path:"prefix-oracle.hc" ~contents
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
          let result =
            Native_source_execution.outcome report |> Result.get_ok
          in
          Alcotest.(check (option int64))
            (name ^ " native")
            (Some (Int64.of_int (List.assoc name expected)))
            (Option.map
               (fun word -> word.Native_source_execution.bits)
               result.value.final_value))
        expressions)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let numeric_callback_boundary () =
  List.iter
    (fun mode ->
      let inputs () =
        let session = Session.create () in
        let source =
          Session.add_source session ~path:"callback-word-initializer.hc"
            ~contents:
              {|I64 (*P)(I64 n)=34;class Box{I64 *value;};Box object={++*P};0;|}
        in
        let config =
          Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
        in
        (session, source, config)
      in
      let check errors =
        Alcotest.(check bool)
          "numeric callback update grants no data ownership" true
          (List.exists
             (fun (d : Diagnostic.t) ->
               d.code
               = if mode = Preprocessor.Jit then "HCRUN0001" else "HCRUN0003")
             errors)
      in
      let session, source, config = inputs () in
      (match
         integer_program_report_outcome
           (run_integer_program_report session ~config ~source ~max_steps:100000)
       with
      | Error errors -> check errors
      | Ok _ ->
          Alcotest.fail "numeric callback word became an IR data reference");
      let session, source, config = inputs () in
      match
        Native_source_execution.outcome
          (Native_source_execution.evaluate session ~config ~source
             ~max_steps:100000)
      with
      | Error errors -> check errors
      | Ok _ ->
          Alcotest.fail "numeric callback word became a native data reference")
    [ Preprocessor.Jit; Preprocessor.Aot ]

let () =
  Alcotest.run "Pending prefix update native oracle"
    [
      ( "source",
        [
          Alcotest.test_case "complete repeated capture and source hashes"
            `Quick (fun () ->
              ignore (fixture ());
              ignore (fixture ~stem:"controls" ()));
          Alcotest.test_case "IR and native preserve original prefix binding"
            `Quick execution;
          Alcotest.test_case
            "canceled callback update cannot create data ownership" `Quick
            numeric_callback_boundary;
        ] );
    ]
