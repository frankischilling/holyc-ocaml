let capture executable arguments =
  let stdout_path = Filename.temp_file "holyc-jit-stdout" ".txt" in
  let stderr_path = Filename.temp_file "holyc-jit-stderr" ".txt" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove stdout_path;
      Sys.remove stderr_path)
    (fun () ->
      let stdout =
        Unix.openfile stdout_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
      in
      let stderr =
        Unix.openfile stderr_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
      in
      let pid =
        Fun.protect
          ~finally:(fun () ->
            Unix.close stdout;
            Unix.close stderr)
          (fun () ->
            Unix.create_process executable
              (Array.of_list (executable :: arguments))
              Unix.stdin stdout stderr)
      in
      let _, status = Unix.waitpid [] pid in
      let read path =
        let channel = open_in_bin path in
        Fun.protect
          ~finally:(fun () -> close_in channel)
          (fun () -> really_input_string channel (in_channel_length channel))
      in
      (status, read stdout_path, read stderr_path))

let with_source contents run =
  let path = Filename.temp_file "holyc-jit-source" ".hc" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      run path)

let require condition message = if not condition then failwith message

let omission_source =
  {|#exe {I64 N=38;I64 Out=0;U0 Print(U8 *s,I64 saved=++N,I64 required,I64 tail=1){Out=saved+required+tail;}N=0;"top",,2,;I64 Top=Out;U0 Saved(){"body",,2,;}Out=0;Saved;StreamPrint("%d;",Top+Out+N-42);}|}

let parenthesized_source =
  {|#exe {I64 N=38;I64 Out=0;U0 Print(U8 *s,I64 saved=++N,I64 required,I64 tail=1){Out=saved+required+tail;}N=0;""("top",,2,);I64 Top=Out;U0 Saved(){""("body",20,21,1);}Out=0;Saved;I64 Body=Out;U0 PutChars(I64 a,I64 b=2){Out=a+b;}''(40);StreamPrint("%d;",Top+Body+Out+N-84);}|}

let absent_source =
  {|#exe {
 I64 N=39; I64 Out=0;
 U0 Print(I64 a=++N,I64 b) { Out=a+b; }
 N=0;
 ""(,2);
 I64 Top=Out;
 U0 Saved() { ""(,2); }
 Out=0; Saved;
 I64 Body=Out;
 U0 Print() { Out+=20; }
 U0 PutChars(I64 a=22) { Out+=a; }
 Out=0; ""(); ''();
 StreamPrint("%d;",Top+Body+Out+N-84);
}
|}

let adjacent_source =
  {|#exe {
 I64 N=39; I64 Out=0;
 U0 PutChars(I64 saved=++N,I64 required) { Out=saved+required; }
 N=0;
 ''2;
 I64 Top=Out;
 U0 Saved() { 'A'-63; }
 Out=0; Saved;
 I64 Body=Out;
 U0 PutChars(I64 a,I64 b=1,I64 c) { Out=a+b+c; }
 ''39 2;
 StreamPrint("%d;",Top+Body+Out+N-84);
}
|}

let variadic_source =
  {|#exe {
 I64 Out=0;
 U0 Print(I64 a=40,...){Out=a+argc+argv[0];}
 ""(,1);
 I64 Top=Out;
 U0 Saved(){""(39,1,7);}
 Out=0;Saved;
 StreamPrint("%d;",Top+Out-42);
}|}

let extern_source =
  {|#exe {
 extern I64 F(I64 n=40,...);
 I64 Use(){return F(,1);}
 I64 F(I64 n=99,...){return n+argc+argv[0];}
 StreamPrint("%d;",Use());
}|}

let pending_header_source =
  {|#exe {
 I64 F(I64 n=40,...){I64 value=n+argc+argv[0];return value;}
 #exe {I64 Saved(){return F(,1);}}
 StreamPrint("%d;",Saved());
}|}

let function_versions_source =
  {|#exe {
 I64 F(I64 n=40){return n+2;}
 #exe {
   I64 SavedBefore(){return F();}
   I64 F(I64 n=99){return 7;}
   I64 SavedInner(){return F();}
 }
 StreamPrint("%d;",SavedBefore()+SavedInner()+F()-108);
}|}

let variadic_termination_source =
  {|#exe {
 I64 F(I64 n=40,... { I64 v=n+argc+argv[0]; return v; }
 #exe { I64 Saved() { return F(,1); } }
 StreamPrint("%d;",Saved());
}|}

let parameter_delimiters_source =
  "I64 F(;;I64 n=40,;;I64 m=2,;;){return n+m;};F();"

let () =
  let executable = Sys.argv.(1) in
  List.iter
    (fun mode ->
      List.iter
        (fun (steps, prep, expected_exit, reached_steps) ->
          let status, output, errors =
            capture executable
              [
                "run";
                "--format=json";
                "--report-version=2";
                "--mode=" ^ mode;
                "--step-limit=" ^ string_of_int steps;
                "--initializer-step-limit=" ^ string_of_int prep;
                Sys.argv.(5);
              ]
          in
          require
            (status = Unix.WEXITED expected_exit && errors = "")
            ("aggregate offsets: " ^ output ^ errors);
          let open Yojson.Basic.Util in
          let report = Yojson.Basic.from_string output in
          require
            (report |> member "executed_steps" |> to_int = reached_steps)
            "offset runtime work";
          require
            (report |> member "compiled_initializer_steps" |> to_int = prep)
            "offset preparation work";
          require
            (report |> member "dimension_preparation_work" |> to_int = 0)
            "offsets are not dimensions";
          require
            (report |> member "output_hex" |> to_string = "")
            "offset output capture";
          if expected_exit = 0 then (
            require
              (report |> member "final_value" |> member "value" |> to_string
             = "42")
              "offset result";
            require
              (report |> member "diagnostics" |> to_list = [])
              "offset diagnostics")
          else
            require
              (report |> member "diagnostics" |> to_list |> List.hd
             |> member "code" |> to_string = "HCIRVM0007")
              "offset bounded failure")
        [ (27, 6, 0, 27); (26, 6, 1, 26); (27, 5, 1, 4) ])
    [ "jit"; "aot" ];
  let phase_fixture = Sys.argv.(4) in
  List.iter
    (fun mode ->
      List.iter
        (fun (steps, prep, exit_code, reached_steps, reached_prep) ->
          let status, output, errors =
            capture executable
              [
                "run";
                "--format=json";
                "--report-version=2";
                "--mode=" ^ mode;
                "--step-limit=" ^ string_of_int steps;
                "--initializer-step-limit=" ^ string_of_int prep;
                phase_fixture;
              ]
          in
          require
            (status = Unix.WEXITED exit_code && errors = "")
            ("aggregate phase fixture: " ^ output ^ errors);
          let open Yojson.Basic.Util in
          let report = Yojson.Basic.from_string output in
          require
            (report |> member "executed_steps" |> to_int = reached_steps)
            "partial aggregate runtime work";
          require
            (report
            |> member "compiled_initializer_steps"
            |> to_int = reached_prep)
            "partial aggregate preparation work";
          require
            (report |> member "dimension_preparation_work" |> to_int = 0)
            "partial metadata does not synthesize dimension work";
          require
            (report |> member "output_hex" |> to_string = "")
            "partial aggregate output";
          if exit_code = 0 then (
            require
              (report |> member "final_value" |> member "value" |> to_string
             = "42")
              "partial aggregate result";
            require
              (report |> member "diagnostics" |> to_list = [])
              "partial aggregate diagnostics")
          else
            require
              (report |> member "diagnostics" |> to_list |> List.hd
             |> member "code" |> to_string = "HCIRVM0007")
              "partial aggregate one-below diagnostic")
        [ (35, 3, 0, 35, 3); (34, 3, 1, 34, 3); (35, 2, 1, 3, 2) ])
    [ "jit"; "aot" ];
  let aggregate_fixture = Sys.argv.(3) in
  List.iter
    (fun mode ->
      List.iter
        (fun (steps, prep, status_code, reached_steps, reached_prep) ->
          let status, output, errors =
            capture executable
              [
                "run";
                "--format=json";
                "--report-version=2";
                "--mode=" ^ mode;
                "--step-limit=" ^ string_of_int steps;
                "--initializer-step-limit=" ^ string_of_int prep;
                aggregate_fixture;
              ]
          in
          require
            (status = Unix.WEXITED status_code && errors = "")
            ("aggregate fixture failed: " ^ output ^ errors);
          let open Yojson.Basic.Util in
          let report = Yojson.Basic.from_string output in
          require
            (report |> member "executed_steps" |> to_int = reached_steps)
            "aggregate runtime work";
          require
            (report
            |> member "compiled_initializer_steps"
            |> to_int = reached_prep)
            "aggregate preparation work";
          require
            (report
            |> member "dimension_preparation_work"
            |> to_int = reached_prep)
            "original aggregate bounds are charged once";
          require
            (report |> member "output_hex" |> to_string = "")
            "aggregate stream bytes are not output";
          if status_code = 0 then (
            require
              (report |> member "final_value" |> member "value" |> to_string
             = "42")
              "aggregate frozen size result";
            require
              (report |> member "diagnostics" |> to_list = [])
              "aggregate successful diagnostics")
          else
            require
              (report |> member "diagnostics" |> to_list |> List.hd
             |> member "code" |> to_string = "HCIRVM0007")
              "aggregate one-below diagnostic")
        [ (31, 3, 0, 31, 3); (30, 3, 1, 30, 3); (31, 2, 1, 4, 2) ])
    [ "jit"; "aot" ];
  let fixture = Sys.argv.(2) in
  List.iter
    (fun mode ->
      let status, output, errors =
        capture executable [ "run"; "--format=json"; "--mode=" ^ mode; fixture ]
      in
      require
        (status = Unix.WEXITED 0 && errors = "")
        ("implicit phase fixture failed: " ^ output ^ errors);
      let open Yojson.Basic.Util in
      let report = Yojson.Basic.from_string output in
      require
        (report |> member "final_value" |> member "value" |> to_string = "42")
        "implicit phase fixture result";
      require
        (report |> member "diagnostics" |> to_list = [])
        "implicit phase fixture diagnostics";
      require
        (report |> member "output_hex" |> to_string = "")
        "implicit replacement must execute its source body";
      List.iter
        (fun (steps, exit_code) ->
          let status, output, errors =
            capture executable
              [
                "run";
                "--format=json";
                "--mode=" ^ mode;
                "--step-limit=" ^ string_of_int steps;
                fixture;
              ]
          in
          require
            (status = Unix.WEXITED exit_code && errors = "")
            ("implicit phase step limit: " ^ output ^ errors);
          let report = Yojson.Basic.from_string output in
          require
            (report |> member "executed_steps" |> to_int = steps)
            "implicit phase exact step charge";
          if exit_code = 0 then
            require
              (report |> member "final_value" |> member "value" |> to_string
             = "42")
              "implicit phase exact-limit result"
          else (
            require
              (report |> member "final_value" = `Null)
              "failed implicit phase input has no successful result";
            require
              (report |> member "diagnostics" |> to_list |> List.hd
             |> member "code" |> to_string = "HCIRVM0007")
              "implicit phase one-below diagnostic"))
        [ (62, 0); (61, 1) ])
    [ "jit"; "aot" ];
  List.iter
    (fun (mode, text) ->
      with_source text (fun path ->
          let status, output, errors =
            capture executable
              [ "run"; "--format=json"; "--mode=" ^ mode; path ]
          in
          require
            (status = Unix.WEXITED 0 && errors = "")
            ("stateful run failed: " ^ output ^ errors);
          let open Yojson.Basic.Util in
          let report = Yojson.Basic.from_string output in
          require
            (report |> member "final_value" |> member "value" |> to_string
           = "42")
            "stateful CLI result";
          require
            (report |> member "diagnostics" |> to_list = [])
            "stateful CLI diagnostics";
          require
            (report |> member "output_hex" |> to_string = "")
            "stream output leaked into ordinary output"))
    [
      ("jit", {|#exe {StreamPrint("42;");}|});
      ("aot", {|#exe {StreamPrint("42;");}|});
      ( "jit",
        {|#exe {I64 Out=0;U0 Print(U8 *s,I64 a=40,I64 b,I64 c=1){Out=a+b+c;}"x",,1,;StreamPrint("%d;",Out);}|}
      );
      ( "aot",
        {|#exe {I64 Out=0;U0 Print(U8 *s,I64 a=40,I64 b,I64 c=1){Out=a+b+c;}"x",,1,;StreamPrint("%d;",Out);}|}
      );
      ("jit", {|I64 N=40;#exe {StreamPrint("%d;",N+2);}|});
      ("jit", {|I64 F(I64 n=42){return n;};F();|});
      ("aot", {|I64 F(I64 n=42){return n;};F();|});
      ( "aot",
        {|extern I64 Unused(I64 n=20+22);I64 Saved(U8 n=sizeof U8+276){return n;};I64 Twice(){return Saved()+Saved();};Twice();|}
      );
      ( "jit",
        {|I64 N=20;I64 Next(){return ++N;};I64 Saved(I64 n=Next()){return n;};N=0;Saved()+Saved();|}
      );
      ( "jit",
        {|I64 N=20;I64 Next(){return ++N;};I64 Saved(I64 n=Next()){return n;};N=0;#exe {StreamPrint("%d;",Saved()+Saved());}|}
      );
    ];
  List.iter
    (fun (source, mode, steps, prep) ->
      with_source source (fun path ->
          List.iter
            (fun (limits, expected_status, expected_steps, preparation) ->
              let status, output, errors =
                capture executable
                  ([
                     "run";
                     "--format=json";
                     "--report-version=2";
                     "--mode=" ^ mode;
                   ]
                  @ limits @ [ path ])
              in
              require
                (status = Unix.WEXITED expected_status && errors = "")
                ("implicit omission limit status: " ^ output ^ errors);
              let open Yojson.Basic.Util in
              let report = Yojson.Basic.from_string output in
              require
                (report |> member "output_hex" |> to_string = "")
                "implicit omission capture";
              require
                (report
                |> member "compiled_initializer_steps"
                |> to_int = preparation)
                "implicit omission preparation accounting";
              Option.iter
                (fun expected ->
                  require
                    (report |> member "executed_steps" |> to_int = expected)
                    "implicit omission runtime accounting")
                expected_steps;
              if expected_status = 0 then (
                require
                  (report |> member "final_value" |> member "value" |> to_string
                 = "42")
                  "implicit omission exact-limit result";
                require
                  (report |> member "diagnostics" |> to_list = [])
                  "implicit omission exact-limit diagnostics")
              else
                require
                  (report |> member "diagnostics" |> to_list |> List.hd
                 |> member "code" |> to_string = "HCIRVM0007")
                  "implicit omission one-below diagnostic")
            [
              ( [
                  "--step-limit=" ^ string_of_int steps;
                  "--initializer-step-limit=" ^ string_of_int prep;
                ],
                0,
                Some steps,
                prep );
              ( [ "--step-limit=" ^ string_of_int (steps - 1) ],
                1,
                Some (steps - 1),
                prep );
              ( [ "--initializer-step-limit=" ^ string_of_int (prep - 1) ],
                1,
                None,
                prep - 1 );
            ]))
    [
      (omission_source, "jit", 114, 9);
      (omission_source, "aot", 114, 9);
      (parenthesized_source, "jit", 147, 12);
      (parenthesized_source, "aot", 147, 12);
      (absent_source, "jit", 145, 9);
      (absent_source, "aot", 145, 9);
      (adjacent_source, "jit", 141, 9);
      (adjacent_source, "aot", 141, 9);
      (variadic_source, "jit", 108, 6);
      (variadic_source, "aot", 108, 6);
      (extern_source, "jit", 52, 6);
      (extern_source, "aot", 52, 6);
      (pending_header_source, "jit", 60, 3);
      (pending_header_source, "aot", 60, 3);
      (function_versions_source, "jit", 73, 6);
      (function_versions_source, "aot", 73, 6);
      (parameter_delimiters_source, "jit", 22, 6);
      (parameter_delimiters_source, "aot", 20, 6);
      (variadic_termination_source, "jit", 60, 3);
      (variadic_termination_source, "aot", 60, 3);
    ];
  List.iter
    (fun source ->
      with_source source (fun path ->
          let status, output, errors =
            capture executable [ "dump-ir"; "--program"; "--mode=jit"; path ]
          in
          require
            (status = Unix.WEXITED 0 && errors = "")
            ("stateful IR inspection failed: " ^ output ^ errors);
          let units =
            String.split_on_char '\n' output
            |> List.filter (String.starts_with ~prefix:"task unit ")
          in
          require
            (List.length units >= 3)
            "stateful IR must identify separate task units"))
    [
      {|#exe {I64 N=40;StreamPrint("%d;",N+2);}|};
      {|I64 N=20;I64 Next(){return ++N;};I64 Saved(I64 n=Next()){return n;};N=0;Saved()+Saved();|};
    ];
  print_endline
    "Stateful JIT/AOT CLI execution and separate JIT IR units passed."
