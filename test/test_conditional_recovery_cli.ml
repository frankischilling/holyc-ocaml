let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc-recovery-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () -> output_string channel contents);
      action path)

let compiler = Sys.argv.(1)

let platform_text text =
  if Sys.win32 then String.concat "\r\n" (String.split_on_char '\n' text)
  else text

let invoke arguments =
  with_file ".stdout" "" (fun stdout ->
      with_file ".stderr" "" (fun stderr ->
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
                  (Array.of_list (compiler :: arguments))
                  Unix.stdin out_fd err_fd)
          in
          let _, status = Unix.waitpid [] pid in
          (status, read stdout, read stderr)))

let contains text fragment =
  let rec find index =
    index + String.length fragment <= String.length text
    && (String.sub text index (String.length fragment) = fragment
       || find (index + 1))
  in
  find 0

let success arguments =
  let status, stdout, stderr = invoke arguments in
  require (status = Unix.WEXITED 0) ("command failed: " ^ stderr ^ stdout);
  require (stderr = "") ("unexpected diagnostics: " ^ stderr);
  stdout

let permissive = "--conditional-recovery=templeos-permissive"

let report source policy format =
  invoke [ "preprocess"; "--dump-preprocessor-report"; policy; format; source ]

let check_reports () =
  let open Yojson.Safe.Util in
  with_file ".hc" "#endif\n42;\n" (fun source ->
      List.iter
        (fun (policy, name, errors, status) ->
          let actual_status, stdout, stderr =
            report source policy "--format=json"
          in
          require (actual_status = Unix.WEXITED status) "JSON report exit";
          let actual = Yojson.Safe.from_string stdout in
          let expected =
            `Assoc
              [
                ("schema", `String "holyc-preprocessor-report-v1");
                ( "templeos_reference",
                  `String Holyc_lib.Version.reference_commit );
                ("conditional_recovery", `String name);
                ("tokens_including_eof", `Int 3);
                ( "diagnostics",
                  `Assoc
                    [
                      ("total", `Int errors);
                      ("errors", `Int errors);
                      ("warnings", `Int 0);
                      ("notes", `Int 0);
                    ] );
              ]
          in
          require (actual = expected) "complete JSON report";
          (if errors = 0 then
             require (stderr = "") "permissive report diagnostics"
           else
             let diagnostics = Yojson.Safe.from_string stderr |> to_list in
             require (List.length diagnostics = 1) "strict diagnostic count";
             require
               (List.hd diagnostics |> member "code" |> to_string = "HCPP0017")
               "strict diagnostic code");
          let actual_status, stdout, _ =
            report source policy "--format=human"
          in
          require (actual_status = Unix.WEXITED status) "human report exit";
          let expected =
            Printf.sprintf
              "holyc-preprocessor-report-v1\n\
               templeos-reference %s\n\
               conditional-recovery %s\n\
               tokens-including-eof 3\n\
               diagnostics %d\n\
               errors %d\n\
               warnings 0\n\
               notes 0\n"
              Holyc_lib.Version.reference_commit name errors errors
          in
          require
            (stdout = platform_text expected)
            (Printf.sprintf "complete human report expected=%S actual=%S"
               expected stdout))
        [
          ("--conditional-recovery=hosted-strict", "hosted-strict", 1, 1);
          (permissive, "templeos-permissive", 0, 0);
          ("--conditional-recovery=templeos", "templeos-permissive", 0, 0);
        ];
      let status, stdout, stderr =
        invoke [ "preprocess"; "--format=json"; source ]
      in
      require (status = Unix.WEXITED 1 && stdout = "") "default remains strict";
      require (contains stderr "HCPP0017") "default diagnostic";
      let status, stdout, _ =
        invoke
          [
            "preprocess";
            "--dump-preprocessor-report";
            "--dump-help-metadata";
            source;
          ]
      in
      require
        (status = Unix.WEXITED 1 && stdout = "")
        "conflicting reports reject";
      let status, stdout, _ =
        invoke [ "preprocess"; "--conditional-recovery=unknown"; source ]
      in
      require
        (status <> Unix.WEXITED 0 && stdout = "")
        "unknown recovery rejects");
  with_file ".hc" "#assert 0\n#endif\n42;" (fun source ->
      let status, stdout, stderr = report source permissive "--format=json" in
      let actual = Yojson.Safe.from_string stdout in
      require (status = Unix.WEXITED 0) "warning remains successful";
      require
        (actual |> member "diagnostics" |> member "warnings" |> to_int = 1)
        "warning retained in report";
      require
        (actual |> member "diagnostics" |> member "errors" |> to_int = 0)
        "warning does not become error";
      require (contains stderr "HCPP0024") "warning retained on stderr")

let check_commands () =
  with_file ".hc" "#endif\n42;" (fun source ->
      List.iter
        (fun command ->
          ignore (success [ command; permissive; source ]);
          let status, _, stderr = invoke [ command; source ] in
          require (status = Unix.WEXITED 1)
            (command ^ " must retain strict default");
          require
            (contains stderr "HCPP0017")
            (command ^ " missing strict diagnostic"))
        [
          "parse"; "dump-ast"; "dump-symbols"; "dump-layout"; "dump-ir"; "eval";
        ];
      require
        (success [ "eval"; permissive; source ] = platform_text "42\n")
        "public expression value");
  List.iter
    (fun contents ->
      with_file ".hc" contents (fun source ->
          require
            (success [ "eval"; permissive; source ] = platform_text "42\n")
            "malformed boundary recovers through eval"))
    [
      "#else discarded #endif 42;";
      "#ifaot discarded #else 42; #else discarded #endif";
      "#ifjit 42;";
    ]

let check_generated () =
  let open Yojson.Safe.Util in
  List.iter
    (fun contents ->
      with_file ".hc" contents (fun source ->
          List.iter
            (fun mode ->
              let json =
                success
                  [
                    "run";
                    "--target=ir";
                    "--format=json";
                    mode;
                    permissive;
                    source;
                  ]
                |> Yojson.Safe.from_string
              in
              require
                (json |> member "final_value" |> member "value" |> to_string
               = "42")
                "generated source retains permissive recovery";
              require
                (json |> member "output_hex" |> to_string = "")
                "discarded or generated source does not become ordinary output";
              let status, stdout, _ =
                invoke [ "run"; "--target=ir"; "--format=json"; mode; source ]
              in
              require
                (status = Unix.WEXITED 1 && contains stdout "HCPP001")
                "generated malformed boundary retains strict default")
            [ "--mode=jit"; "--mode=aot" ]))
    [
      {|#exe {StreamPrint("#endif ");} 42;|};
      {|#exe {StreamPrint("#else discarded ");} caller_discarded #endif 42;|};
      {|#exe { #endif StreamPrint("42;"); }|};
      {|#else #exe {1/0;} #endif 42;|};
    ]

let check_program ~native =
  let open Yojson.Safe.Util in
  let fixture = Sys.argv.(2) in
  let target = if native then "--target=host-jit" else "--target=ir" in
  List.iter
    (fun mode ->
      List.iter
        (fun version ->
          let arguments =
            [ "run"; target; mode; version; permissive; fixture ]
          in
          let json =
            success (arguments @ [ "--format=json" ]) |> Yojson.Safe.from_string
          in
          require
            (json |> member "final_value" |> member "value" |> to_string = "42")
            "recovered program result";
          require
            (json
            |> member "conditional_recovery"
            |> to_string = "templeos-permissive")
            "program policy in JSON";
          require
            (contains (success arguments)
               "conditional-recovery=templeos-permissive")
            "program policy in human report";
          let status, stdout, stderr =
            invoke [ "run"; target; mode; version; "--format=json"; fixture ]
          in
          require (status = Unix.WEXITED 1) "strict program fails";
          require
            (contains (stdout ^ stderr) "HCPP0017")
            "strict program diagnostic")
        (if native then [ "--report-version=2" ]
         else [ "--report-version=1"; "--report-version=2" ]))
    [ "--mode=jit"; "--mode=aot" ];
  with_file ".hc" "42;" (fun source ->
      let json =
        success [ "run"; target; "--format=json"; source ]
        |> Yojson.Safe.from_string
      in
      require
        (json |> member "conditional_recovery" |> to_string = "hosted-strict")
        "default policy named")

let check_native_expression () =
  let open Yojson.Safe.Util in
  with_file ".hc" "#endif\n42;" (fun source ->
      let json =
        success [ "eval-native"; permissive; "--format=json"; source ]
        |> Yojson.Safe.from_string
      in
      require
        (json |> member "final_value" |> member "value" |> to_string = "42")
        "native recovered expression";
      require
        (json
        |> member "conditional_recovery"
        |> to_string = "templeos-permissive")
        "native expression policy";
      let status, stdout, _ =
        invoke [ "eval-native"; "--format=json"; source ]
      in
      require
        (status = Unix.WEXITED 1 && contains stdout "HCPP0017")
        "native expression strict default")

let () =
  let native = Array.length Sys.argv > 3 && Sys.argv.(3) = "--native" in
  if native then check_native_expression ()
  else (
    check_reports ();
    check_commands ();
    check_generated ());
  check_program ~native;
  Printf.printf "Conditional recovery %s CLI checks passed.\n"
    (if native then "native" else "hosted")
