open Holyc_lib
open Yojson.Safe.Util
module Image = X86_64_program
module Source = Native_source_execution

type target = Source | Isolated

type observation = {
  outcome : ((Image.word_type * int64) option, Diagnostic.t list) result;
  work : int;
  steps : int;
  bytes : int;
  output : string;
}

let diagnostics errors =
  List.map
    (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
    errors
  |> String.concat "; "

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let run ?(target = Source) ?(mode = Preprocessor.Aot) ?(max_steps = 100_000)
    ?max_initializer_steps ?max_default_bytes text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"source-intrinsic-defaults.hc"
      ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  match target with
  | Source ->
      let report =
        Source.evaluate ?max_initializer_steps ?max_default_bytes session
          ~config ~source ~max_steps
      in
      {
        outcome =
          Result.map
            (fun (value : Source.result Source.checked) ->
              Option.map
                (fun (word : Source.word) -> (word.type_, word.bits))
                value.value.final_value)
            (Source.outcome report);
        work = Source.preparation_steps report;
        steps = Source.executed_steps report;
        bytes = Source.default_bytes report;
        output = Source.output_bytes report;
      }
  | Isolated ->
      let report =
        Native_program.evaluate ?max_initializer_steps ?max_default_bytes
          session ~config ~source ~max_steps
      in
      {
        outcome =
          Result.map
            (fun (value : Native_program.result Native_program.checked) ->
              Option.map
                (fun (word : Image.word) -> (word.type_, word.bits))
                value.value.execution.final_value)
            (Native_program.outcome report);
        work = Native_program.preparation_steps report;
        steps = Option.value ~default:0 (Native_program.executed_steps report);
        bytes = Native_program.default_bytes report;
        output = Native_program.output_bytes report;
      }

let value type_ bits report =
  let actual_type, actual_bits =
    report.outcome |> Result.map_error diagnostics |> checked |> Option.get
  in
  Alcotest.(check bool) "original result type" true (actual_type = type_);
  Alcotest.(check int64) "exact saved result word" bits actual_bits;
  Alcotest.(check string) "closed intrinsic produces no output" "" report.output;
  Alcotest.(check bool)
    "physical preparation work is retained" true
    (report.work > 0 && report.steps > report.work)

let rejected report =
  match report.outcome with
  | Error (_ :: _ as errors) -> errors
  | _ -> Alcotest.fail "unsupported source reference unexpectedly completed"

let fault code report =
  let errors = rejected report in
  Alcotest.(check bool)
    (diagnostics errors) true
    (List.exists (fun (error : Diagnostic.t) -> error.code = code) errors)

let declarations =
  "_intern 0x1C F64 Convert(I64 value);_intern 0x1B I64 Truncate(F64 value);"

let modes =
  [
    (Source, Preprocessor.Aot);
    (Isolated, Preprocessor.Aot);
    (Isolated, Preprocessor.Jit);
  ]

let original_numeric_calls () =
  List.iter
    (fun (target, mode) ->
      List.iter
        (fun (text, type_, bits) ->
          let report = run ~target ~mode (declarations ^ text) in
          value type_ bits report;
          Alcotest.(check int) "one original default payload" 8 report.bytes)
        [
          ( "F64 F(F64 value=Convert(42)){return value;}F();",
            Image.F64,
            0x4045_0000_0000_0000L );
          ("I64 F(I64 value=Truncate(42.75)){return value;}F;", Image.I64, 42L);
          ("I64 F(I8 value=Truncate(257.75)){return value;}F();", Image.I64, 1L);
          ( "F64 F(F64 value=Convert(Truncate(42.75))){return value;}F();",
            Image.F64,
            0x4045_0000_0000_0000L );
          ( "F64 F(F64 \
             value=Convert(Truncate(Convert(Truncate(42.75))))){return \
             value;}F();",
            Image.F64,
            0x4045_0000_0000_0000L );
          (* OptPass012.HC:1004-1022 first stores the literal conversion as
             IMM_F64; :939-957 then folds ToI64 from that rounded word. *)
          ( "I64 F(I64 value=Truncate(Convert(9007199254740993))){return \
             value;}F();",
            Image.I64,
            9007199254740992L );
          ( "I64 F(I64 value=Truncate(Convert(9007199254740995))){return \
             value;}F();",
            Image.I64,
            9007199254740996L );
          ( "F64 F(F64 value=Convert(sizeof(I64))){return value;}F();",
            Image.F64,
            0x4020_0000_0000_0000L );
          ("F64 Unused(F64 value=Convert(42)){return value;}42;", Image.I64, 42L);
          ( "F64 F(F64 value=Convert(42)){return value;}F(1.5);",
            Image.F64,
            0x3ff8_0000_0000_0000L );
          ( "F64 F(F64 a=Convert(2),F64 b){return a+b;}F(,3.0);",
            Image.F64,
            0x4014_0000_0000_0000L );
        ])
    modes

let anonymous_and_history () =
  List.iter
    (fun (target, mode) ->
      let callback =
        run ~target ~mode
          (declarations
         ^ "F64 Target(F64 value=Convert(99)){return value;}F64 (*p)(F64 \
            value=Convert(Truncate(42.75)));p=&Target;p();")
      in
      value Image.F64 0x4045_0000_0000_0000L callback;
      Alcotest.(check int)
        "anonymous header retains a distinct saved value" 16 callback.bytes;
      let macro =
        run ~target ~mode
          (declarations
         ^ "\n\
            #define Alias Convert\n\
            F64 F(F64 value=Alias(42)){return value;}F();")
      in
      value Image.F64 0x4045_0000_0000_0000L macro;
      let history =
        run ~target ~mode
          "extern F64 Number(I64 value);_intern 0x1C F64 Number(I64 value);F64 \
           First(F64 value=Number(42)){return value;}_intern 0x1B I64 \
           Number(F64 value);I64 Second(I64 value=Number(42.75)){return \
           value;}First()+Second();"
      in
      value Image.F64 0x4055_0000_0000_0000L history;
      Alcotest.(check int)
        "earlier conversion survives later original header" 16 history.bytes;
      let repeated =
        run ~target ~mode
          "extern F64 Number(I64 first);extern F64 Number(I64 second);_intern \
           0x1C F64 Number(I64 value);F64 F(F64 value=Number(42)){return \
           value;}F();"
      in
      value Image.F64 0x4045_0000_0000_0000L repeated;
      Alcotest.(check int)
        "every original joined extern keeps the one prepared word" 8
        repeated.bytes)
    modes

let cumulative_limits () =
  let text =
    declarations
    ^ "F64 A(F64 value=Convert(40)){return value;}F64 B(F64 \
       value=Convert(Truncate(2.75))){return value;}A()+B();"
  in
  List.iter
    (fun (target, mode) ->
      let baseline = run ~target ~mode text in
      value Image.F64 0x4045_0000_0000_0000L baseline;
      Alcotest.(check int) "two saved words" 16 baseline.bytes;
      value Image.F64 0x4045_0000_0000_0000L
        (run ~target ~mode ~max_steps:baseline.steps
           ~max_initializer_steps:baseline.work ~max_default_bytes:16 text);
      fault "HCIRVM0007"
        (run ~target ~mode ~max_steps:(baseline.steps - 1) text);
      fault "HCIRVM0007"
        (run ~target ~mode ~max_initializer_steps:(baseline.work - 1) text);
      let limited = run ~target ~mode ~max_default_bytes:15 text in
      fault "HCIRVM0011" limited;
      Alcotest.(check int)
        "first completed word survives second payload limit" 8 limited.bytes)
    modes

let failures () =
  List.iter
    (fun (target, mode) ->
      List.iter
        (fun expression ->
          let failed =
            run ~target ~mode
              (declarations
             ^ "F64 Good(F64 value=Convert(1)){return value;}F64 Bad(F64 value="
             ^ expression ^ "){return value;}42;")
          in
          fault "HCIRVM0009" failed;
          Alcotest.(check int)
            "faulting intrinsic argument publishes no word" 8 failed.bytes;
          Alcotest.(check bool)
            "reached failure work remains charged" true
            (failed.work > 0 && failed.steps > 0))
        [ "Convert(7/0)"; "Convert(Truncate(Convert(7/0)))" ];
      let late =
        run ~target ~mode
          (declarations
         ^ "F64 Good(F64 value=Convert(1)){return value;}F64 Broken(F64 \
            value=Convert(2)]")
      in
      ignore (rejected late);
      Alcotest.(check bool)
        "earlier saved value survives later syntax failure" true
        (late.bytes >= 8 && late.work > 0 && late.steps > 0))
    modes

let reference_boundaries () =
  List.iter
    (fun (target, mode) ->
      List.iter
        (fun text ->
          let report = run ~target ~mode text in
          ignore (rejected report);
          Alcotest.(check int)
            "unproved source reference publishes no default" 0 report.bytes)
        [
          "extern F64 ToF64(I64 value);F64 F(F64 value=ToF64(42)){return \
           value;}F();";
          "_intern 0x1C I64 Forged(F64 value);I64 F(I64 \
           value=Forged(42.0)){return value;}F();";
          "_intern 0x1C F64 Convert(I64 value);I64 F(I64 \
           value=&Convert){return value;}F();";
          "I64 Seed(){return 42;}F64 F(F64 value=Seed()){return value;}F();";
          declarations
          ^ "I64 number=42;F64 F(F64 value=Convert(number)){return value;}F();";
          "_intern (0x1B+1) F64 Convert(I64 value);F64 F(F64 \
           value=Convert(42)){return value;}F();";
          "import F64 Convert(I64 value);_intern 0x1C F64 Convert(I64 \
           value);F64 F(F64 value=Convert(42)){return value;}F();";
          "_extern REMOTE F64 Convert(I64 value);_intern 0x1C F64 Convert(I64 \
           value);F64 F(F64 value=Convert(42)){return value;}F();";
        ])
    modes;
  List.iter
    (fun mode ->
      let prefix =
        declarations ^ "F64 F(F64 value=Convert(42)){return value;}"
      in
      let prepared = run ~target:Isolated ~mode (prefix ^ "F();") in
      value Image.F64 0x4045_0000_0000_0000L prepared;
      Alcotest.(check int)
        "original prepared prefix saves one word" 8 prepared.bytes;
      List.iter
        (fun (text, code, expected_bytes, expected_work, expected_steps) ->
          let report = run ~target:Isolated ~mode text in
          let errors = rejected report in
          Alcotest.(check (list string))
            "header fails at its original mode-specific boundary" [ code ]
            (List.map (fun (error : Diagnostic.t) -> error.code) errors);
          Alcotest.(check int)
            "header rejection retains exactly the reached saved words"
            expected_bytes report.bytes;
          Alcotest.(check int)
            "header rejection retains exact completed preparation" expected_work
            report.work;
          Alcotest.(check int)
            "header rejection executes only the native preparation prefix"
            expected_steps report.steps)
        [
          ( "extern F64 Unrelated(I64 value);" ^ prefix ^ "F();",
            "HCRUN0001",
            8,
            prepared.work,
            prepared.work );
          (let text = "import F64 Unrelated(I64 value);" ^ prefix ^ "F();" in
           if mode = Preprocessor.Jit then (text, "HCPARSE0006", 0, 0, 0)
           else (text, "HCRUN0001", 8, prepared.work, prepared.work));
          ( prefix ^ "extern F64 Convert(I64 value);F();",
            "HCRUN0001",
            8,
            prepared.work,
            prepared.work );
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_temp extension action =
  let path = Filename.temp_file "source-intrinsic-defaults-" extension in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () -> action path)

let invoke compiler status args =
  with_temp ".out" (fun stdout ->
      with_temp ".err" (fun stderr ->
          let out_fd =
            Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let err_fd =
            Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let pid =
            Fun.protect
              ~finally:(fun () ->
                Unix.close out_fd;
                Unix.close err_fd)
              (fun () ->
                Unix.create_process compiler
                  (Array.of_list (compiler :: args))
                  Unix.stdin out_fd err_fd)
          in
          let _, result = Unix.waitpid [] pid in
          let error = read_file stderr in
          Alcotest.(check bool)
            ("CLI exit: " ^ error) true
            (result = Unix.WEXITED status);
          Alcotest.(check string) "CLI stderr" "" error;
          read_file stdout |> Yojson.Safe.from_string))

let example compiler path () =
  List.iter
    (fun target ->
      let run status extra =
        invoke compiler status
          ([ "run"; "--mode=aot"; "--target=" ^ target; "--format=json" ]
          @ extra @ [ path ])
      in
      let baseline = run 0 [] in
      Alcotest.(check string)
        "maintained example result" "0x4045000000000000"
        (baseline |> member "final_value" |> member "bits" |> to_string);
      Alcotest.(check int)
        "three original saved defaults" 24
        (baseline |> member "prepared_default_bytes" |> to_int);
      let work = baseline |> member "compiled_initializer_steps" |> to_int in
      ignore
        (run 0
           [
             "--default-byte-limit=24";
             "--initializer-step-limit=" ^ string_of_int work;
           ]);
      let limited = run 1 [ "--default-byte-limit=23" ] in
      Alcotest.(check string)
        "example byte limit rejects" "error"
        (limited |> member "outcome" |> to_string))
    [ "host-jit-task"; "host-jit" ]

let () =
  if Array.length Sys.argv <> 3 then
    failwith
      "usage: test_native_source_intrinsic_defaults.exe <holyc.exe> \
       <aot-intrinsic-defaults.hc>"
  else if
    not
      (Native_execution.x87_supported () && Native_execution.fisttp_supported ())
  then
    print_endline
      "native source intrinsic defaults: x87 or FISTTP unsupported, skipped"
  else
    Alcotest.run
      ~argv:[| Sys.argv.(0) |]
      "Source numeric intrinsic defaults"
      [
        ( "source preparation",
          [
            Alcotest.test_case "original numeric calls and omitted values"
              `Quick original_numeric_calls;
            Alcotest.test_case "anonymous defaults and source header history"
              `Quick anonymous_and_history;
            Alcotest.test_case "cumulative execution and payload limits" `Quick
              cumulative_limits;
            Alcotest.test_case "reached faults and later syntax errors" `Quick
              failures;
            Alcotest.test_case "unproved references and signatures reject"
              `Quick reference_boundaries;
          ] );
        ( "public CLI",
          [
            Alcotest.test_case "maintained AOT example" `Quick
              (example Sys.argv.(1) Sys.argv.(2));
          ] );
      ]
