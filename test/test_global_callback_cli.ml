open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc global callback cli " suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let () =
  require (Array.length Sys.argv = 7) "expected compiler, examples and oracle"

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)
let top_level_example = Sys.argv.(3)
let oracle = Yojson.Safe.from_file Sys.argv.(4)
let update_example = Sys.argv.(5)
let indexed_dereference_example = Sys.argv.(6)

let invoke ~mode ?(target = "ir") ?(options = []) ?(status = 0) path =
  let arguments =
    [
      "run";
      "--report-version=2";
      "--format=json";
      "--target=" ^ target;
      "--mode=" ^ mode;
    ]
    @ options @ [ path ]
  in
  with_file ".stdout" "" (fun stdout ->
      with_file ".stderr" "" (fun stderr ->
          let out_fd =
            Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          and err_fd =
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
          let _, actual = Unix.waitpid [] pid in
          let output = read stdout in
          require
            (actual = Unix.WEXITED status)
            ("unexpected exit: " ^ output ^ read stderr);
          require (read stderr = "") "JSON command wrote diagnostics to stderr";
          Yojson.Safe.from_string output))

let output report text =
  let hex =
    String.to_seq text |> List.of_seq
    |> List.map (fun byte -> Printf.sprintf "%02x" (Char.code byte))
    |> String.concat ""
  in
  require (member "output_hex" report = `String hex) "captured bytes";
  require
    (member "output_byte_length" report = `Int (String.length text))
    "captured length"

let success report value =
  require (member "schema" report = `String "holyc-integer-program-v2") "schema";
  require (member "outcome" report = `String "success") "success outcome";
  require (member "diagnostics" report = `List []) "success diagnostics";
  require
    (member "final_value" report |> member "type" = `String "i64")
    "result class";
  require
    (member "final_value" report |> member "value" = `String value)
    "source result"

let error report code text =
  require (member "outcome" report = `String "error") "fault outcome";
  require (member "final_value" report = `Null) "fault retained a final value";
  (match member "diagnostics" report |> to_list with
  | first :: _ ->
      require (member "code" first = `String code) "fault diagnostic"
  | [] -> failwith "fault has no diagnostic");
  output report text

let () =
  List.iter
    (fun mode ->
      let steps, prep = if mode = "jit" then (79, 8) else (75, 6) in
      let exact =
        [
          "--global-byte-limit=56";
          "--frame-byte-limit=8";
          "--call-depth-limit=2";
          "--step-limit=" ^ string_of_int steps;
        ]
      in
      let report = invoke ~mode ~options:exact example in
      success report "42";
      output report "";
      require (member "executed_steps" report = `Int steps) "runtime work";
      require
        (member "compiled_initializer_steps" report = `Int prep)
        "declaration work";
      require
        (member "dimension_preparation_work" report = `Int 2)
        "original array dimensions";
      List.iter
        (fun (option, code) ->
          error (invoke ~mode ~options:[ option ] ~status:1 example) code "")
        [
          ("--global-byte-limit=55", "HCIRVM0016");
          ("--frame-byte-limit=7", "HCIRVM0011");
          ("--call-depth-limit=1", "HCIRVM0015");
          ("--step-limit=" ^ string_of_int (steps - 1), "HCIRVM0007");
        ];
      let prefix =
        "extern U0 Print(U8 *fmt,...);I64 Side(){Print(\"arg\");return 40;}"
      in
      List.iter
        (fun (source, code, text) ->
          with_file ".HC" (prefix ^ source) (fun path ->
              error (invoke ~mode ~status:1 path) code text))
        [
          ( "I64 (*p)(I64 n);I64 Run(){p=0;return \
             p(Side());}Print(\"before\");Run();",
            "HCIRVM0024",
            "beforearg" );
          ( "I64 (*p)(I64 n);I64 Run(){return \
             p(Side());}Print(\"before\");Run();",
            (if mode = "jit" then "HCIRVM0012" else "HCIRVM0024"),
            if mode = "jit" then "before" else "beforearg" );
          ( "I64 (*p)(I64 n)[2];I64 Run(){return \
             p[2](Side());}Print(\"before\");Run();",
            "HCIRVM0019",
            "before" );
          ( "I64 Add(I64 n){return n+2;}noargpop I64 (*p)(I64 n);I64 \
             Run(){p=&Add;return p(Side());}Print(\"before\");Run();",
            "HCIRVM0014",
            "beforearg" );
        ];
      List.iter
        (fun (source, target, code) ->
          with_file ".HC" source (fun path ->
              error (invoke ~mode ~target ~status:1 path) code ""))
        [ ("I64 (**p)()=0;42;", "ir", "HCRUN0001") ];
      List.iter
        (fun source ->
          with_file ".HC" source (fun path ->
              let report = invoke ~mode ~target:"host-jit" path in
              success report "42";
              output report ""))
        [
          "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n);p=&Add;p(40);";
          "I64 A(){return 42;}I64 (*p)();I64 Run(){p=&A;return p();}Run();";
        ];
      List.iter
        (fun (source, value) ->
          with_file ".HC" source (fun path ->
              let report = invoke ~mode path in
              success report value;
              output report ""))
        [
          ("I64 A(){return 42;}I64 (*p)();p=&A;p();", "42");
          ("I64 (*p)()=0;42;", "42");
          ( "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n);p=&Add;I64 n=p(40);n;",
            "42" );
          ("I64 Add(I64 n){return n+2;}I64 (*p)(I64 n);p=&Add;p(p=0);", "2");
          ( "I64 Take(I64 a,I64 b){return a*10+b;}I64 (*p)(I64 a,I64 b);I64 \
             n;n=0;p=&Take;p(++n,++n);",
            "21" );
          ("I64 Add(I64 n){return n+2;}I64 (*p)(I64 n);p=&Add;p(p(38));", "42");
        ];
      let top_report = invoke ~mode top_level_example in
      success top_report "42";
      output top_report "";
      let top_steps = member "executed_steps" top_report |> to_int in
      success
        (invoke ~mode
           ~options:
             [
               "--global-byte-limit=56";
               "--frame-byte-limit=8";
               "--call-depth-limit=1";
               "--step-limit=" ^ string_of_int top_steps;
             ]
           top_level_example)
        "42";
      error
        (invoke ~mode
           ~options:[ "--step-limit=" ^ string_of_int (top_steps - 1) ]
           ~status:1 top_level_example)
        "HCIRVM0007" "";
      List.iter
        (fun (source, code, text) ->
          with_file ".HC" (prefix ^ source) (fun path ->
              error (invoke ~mode ~status:1 path) code text))
        [
          ( "I64 (*p)(I64 n);p=0;Print(\"before\");p(Side());",
            "HCIRVM0024",
            "beforearg" );
          ( "I64 (*p)(I64 n)[2];Print(\"before\");p[2](Side());",
            "HCIRVM0019",
            "before" );
        ];
      require
        (member "id" oracle = `String "execution/callback-storage-and-calls-001")
        "wrong callback oracle";
      require
        (member "reference" oracle |> member "commit"
       = `String "c26482bb6ad3f80106d28504ec5db3c6a360732c")
        "oracle revision";
      let observed id field =
        member "checks" oracle |> to_list
        |> List.find (fun check -> member "id" check = `String id)
        |> member "observed" |> member field |> to_string
      in
      List.iter
        (fun (field, source) ->
          let bits = observed "values-basic-A" field in
          require
            (bits = observed "values-basic-B" field)
            "native repeat differs";
          with_file ".HC" source (fun path ->
              let report = invoke ~mode path in
              success report "42";
              require
                (member "final_value" report
                |> member "bits"
                = `String ("0x" ^ String.lowercase_ascii bits))
                "native word projection"))
        [
          ( "GLOBAL",
            "I64 CbAdd(I64 n){return n+2;}I64 (*CbGlobal)(I64 \
             n)=&CbAdd;CbGlobal(40);" );
          ( "ARRAY",
            "I64 CbAdd(I64 n){return n+2;}I64 (*CbArray)(I64 \
             n)[2];CbArray[1]=&CbAdd;CbArray[1](40);" );
        ])
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      with_file ".HC"
        "extern U0 Print(U8 *fmt,...);I64 Seed(I64 n){Print(\"A\");return \
         n+2;}I64 (*p)(I64 n)[2]={&Seed,p[0]};I64 N=p[1](40);N;" (fun path ->
          let report = invoke ~mode path in
          success report "42";
          output report "A"))
    [ "jit"; "aot" ];
  with_file ".HC"
    "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n=40);p=&Add;I64 Run(){return \
     p();}Run();I64 Add(I64 n){return n+100;}Run();" (fun path ->
      success (invoke ~mode:"jit" path) "42");
  List.iter
    (fun mode ->
      with_file ".HC"
        "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n);p=&Add;(*p)(40);"
        (fun path ->
          let report = invoke ~mode path in
          success report "42";
          output report "";
          let steps = member "executed_steps" report |> to_int in
          success
            (invoke ~mode
               ~options:[ "--step-limit=" ^ string_of_int steps ]
               path)
            "42";
          error
            (invoke ~mode ~status:1
               ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
               path)
            "HCIRVM0007" ""))
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      List.iter
        (fun target ->
          let report = invoke ~mode ~target update_example in
          success report "42";
          output report "";
          let steps = member "executed_steps" report |> to_int in
          success
            (invoke ~mode ~target
               ~options:[ "--step-limit=" ^ string_of_int steps ]
               update_example)
            "42";
          error
            (invoke ~mode ~target ~status:1
               ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
               update_example)
            "HCIRVM0007" "";
          with_file ".HC"
            "extern U0 Print(U8 *fmt,...);I64 Target(I64 n){return n;}I64 \
             Side(){Print(\"R\");return 1;}I64 F(){I64 (*p)(I64 n);p=&Target; \
             Print(\"B\");p+=Side();return 42;}F();" (fun path ->
              error (invoke ~mode ~target ~status:1 path) "HCIRVM0024" "BR"))
        [ "ir"; "host-jit" ])
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      List.iter
        (fun target ->
          let report = invoke ~mode ~target indexed_dereference_example in
          success report "42";
          output report "I";
          let steps = member "executed_steps" report |> to_int in
          let exact =
            invoke ~mode ~target
              ~options:[ "--step-limit=" ^ string_of_int steps ]
              indexed_dereference_example
          in
          success exact "42";
          output exact "I";
          error
            (invoke ~mode ~target ~status:1
               ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
               indexed_dereference_example)
            "HCIRVM0007" "I")
        [ "ir"; "host-jit" ])
    [ "jit"; "aot" ];
  print_endline "Global callback CLI checks passed."
