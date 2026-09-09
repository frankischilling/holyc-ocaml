let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc-program-test-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let compiler = Sys.argv.(1)

let invoke_raw arguments =
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

let invoke = function
  | "run" :: arguments -> invoke_raw ("run" :: "--report-version=1" :: arguments)
  | arguments -> invoke_raw arguments

let require condition message = if not condition then failwith message

let contains text fragment =
  let size = String.length fragment in
  let rec find index =
    index + size <= String.length text
    && (String.sub text index size = fragment || find (index + 1))
  in
  find 0

let success arguments =
  let status, stdout, stderr = invoke arguments in
  require (status = Unix.WEXITED 0) ("command failed: " ^ stderr);
  require (stderr = "") ("unexpected diagnostics: " ^ stderr);
  stdout

let () =
  let open Yojson.Safe.Util in
  let fixture = Sys.argv.(2) in
  let output =
    success [ "run"; "--target=ir"; "--format=json"; fixture ]
    |> Yojson.Safe.from_string
  in
  let string name = output |> member name |> to_string in
  require (string "schema" = "holyc-integer-program-v1") "result schema";
  require
    (string "implementation_commit" = Holyc_lib.Version.implementation_commit)
    "program report must identify the compiler build";
  require
    (string "reference_commit" = Holyc_lib.Version.reference_commit)
    "program report must identify the pinned reference";
  require (string "mode" = "jit" && string "target" = "ir") "execution mode";
  require (string "arithmetic" = "runtime-ir") "arithmetic boundary";
  require (string "termination" = "stream-end") "stream termination";
  require
    (output |> member "executed_steps" |> to_int = 23)
    "executed path length";
  require (output |> member "step_limit" |> to_int = 100000) "reported budget";
  require
    (success [ "dump-ir"; "--program"; fixture ]
    = success [ "dump-ir"; "--program"; fixture ])
    "deterministic program IR";
  let human = success [ "run"; "--target=ir"; fixture ] in
  require
    (String.starts_with ~prefix:"holyc-integer-program-v1" human)
    "human report";
  require
    (List.mem "step-limit=100000"
       (String.split_on_char '\n' human |> List.map String.trim))
    "human report must retain the execution budget";
  with_file ".hc" "if(1) 1/0;" (fun source ->
      let status, stdout, stderr = invoke [ "run"; "--format=json"; source ] in
      require
        (status = Unix.WEXITED 1 && stdout = "")
        "fault must not expose a result";
      let diagnostics = Yojson.Safe.from_string stderr |> to_list in
      let diagnostic = List.hd diagnostics in
      require
        (diagnostic |> member "code" |> to_string = "HCIRVM0009")
        "fault diagnostic";
      let notes =
        diagnostic |> member "notes" |> to_list |> List.map to_string
      in
      require
        (List.exists (String.starts_with ~prefix:"executed_steps=") notes)
        "fault must retain execution progress");
  let status, stdout, _ = invoke [ "run"; "--step-limit=22"; fixture ] in
  require
    (status = Unix.WEXITED 1 && stdout = "")
    "budget failure exit and stdout";
  ignore (success [ "run"; "--step-limit=23"; fixture ]);
  List.iter
    (fun command ->
      with_file ".hc" "#assert 0\n42;" (fun source ->
          let status, stdout, stderr = invoke (command @ [ source ]) in
          require
            (status = Unix.WEXITED 0 && stdout <> "")
            "warning permits success";
          require (stderr <> "")
            "successful source command must retain parser warnings";
          if List.mem "--format=json" command then (
            let diagnostics = Yojson.Safe.from_string stderr |> to_list in
            require (List.length diagnostics = 1) "one retained warning";
            require
              (List.hd diagnostics |> member "code" |> to_string = "HCPP0024")
              "retained assertion warning")))
    [ [ "run"; "--format=json" ]; [ "dump-ir"; "--program" ] ];
  with_file ".hc" "#assert 0\n1/0;" (fun source ->
      let status, stdout, stderr = invoke [ "run"; "--format=json"; source ] in
      require
        (status = Unix.WEXITED 1 && stdout = "")
        "warning then runtime fault";
      let codes =
        Yojson.Safe.from_string stderr
        |> to_list
        |> List.map (fun item -> item |> member "code" |> to_string)
      in
      require
        (codes = [ "HCPP0024"; "HCIRVM0009" ])
        "warnings and errors must share one diagnostic array");
  let status, stdout, stderr = invoke [ "run"; "--target=host-jit"; fixture ] in
  require (status = Unix.WEXITED 1 && stdout = "") "unsupported native target";
  require
    (String.starts_with ~prefix:"holyc: run: HCRUN0005" stderr)
    "target diagnostic";
  List.iter
    (fun mode ->
      let result =
        success
          [
            "run";
            "--target=ir";
            "--format=json";
            "--mode=" ^ mode;
            Sys.argv.(3);
          ]
        |> Yojson.Safe.from_string
      in
      let value = result |> member "final_value" in
      require
        (value |> member "value" |> to_string = "42")
        "original source function must return 42";
      require
        (value |> member "type" |> to_string = "i64")
        "source function result class";
      require
        (value |> member "bits" |> to_string = "0x000000000000002a")
        "source function result bits";
      require
        (result |> member "termination" |> to_string = "stream-end")
        "caller resumes and completes the source stream")
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let result =
        success
          [
            "run";
            "--target=ir";
            "--format=json";
            "--mode=" ^ mode;
            Sys.argv.(4);
          ]
        |> Yojson.Safe.from_string
      in
      require
        (result |> member "final_value" |> member "value" |> to_string = "42")
        "nested source call expressions must return 42")
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let result =
        success
          [
            "run";
            "--format=json";
            "--mode=" ^ mode;
            "--global-byte-limit=8";
            Sys.argv.(5);
          ]
        |> Yojson.Safe.from_string
      in
      require
        (result |> member "final_value" |> member "value" |> to_string = "42")
        "global accumulator source returns 42";
      require
        (result |> member "global_byte_limit" |> to_int = 8)
        "reported global limit";
      let human =
        success
          [ "run"; "--mode=" ^ mode; "--global-byte-limit=8"; Sys.argv.(5) ]
      in
      require
        (String.split_on_char '\n' human
        |> List.map String.trim
        |> List.mem "global-byte-limit=8")
        "human global limit";
      let status, stdout, stderr =
        invoke
          [
            "run";
            "--format=json";
            "--mode=" ^ mode;
            "--global-byte-limit=7";
            Sys.argv.(5);
          ]
      in
      require
        (status = Unix.WEXITED 1 && stdout = "")
        "global byte limit rejects before effects";
      require
        (Yojson.Safe.from_string stderr
        |> to_list |> List.hd |> member "code" |> to_string = "HCIRVM0016")
        "global limit diagnostic")
    [ "jit"; "aot" ];
  with_file ".hc" "@invalid" (fun source ->
      let status, stdout, stderr =
        invoke [ "run"; "--global-byte-limit=0"; source ]
      in
      require
        (status = Unix.WEXITED 1 && stdout = "")
        "nonpositive global limit rejects before source parsing";
      require
        (String.starts_with ~prefix:"holyc: run: HCIRVM0001" stderr)
        "global limit configuration diagnostic");
  List.iter
    (fun mode ->
      let source = Sys.argv.(6) in
      let result =
        success
          [
            "run";
            "--format=json";
            "--mode=" ^ mode;
            "--initializer-step-limit=3";
            "--global-byte-limit=8";
            source;
          ]
        |> Yojson.Safe.from_string
      in
      require
        (result |> member "final_value" |> member "value" |> to_string = "42")
        "initialized accumulator returns 42";
      require
        (result |> member "initializer_step_limit" |> to_int = 3)
        "reported initializer preparation budget";
      require
        (result |> member "compiled_initializer_steps" |> to_int = 3)
        "constant preparation has a separate count";
      require
        (result |> member "executed_steps" |> to_int = 46)
        "initializer preparation is excluded from source execution steps";
      let human =
        success
          [ "run"; "--mode=" ^ mode; "--initializer-step-limit=3"; source ]
      in
      require
        (String.split_on_char '\n' human
        |> List.map String.trim
        |> List.mem "compiled-initializer-steps=3")
        "human preparation count";
      let command =
        [
          "dump-ir";
          "--program";
          "--mode=" ^ mode;
          "--initializer-step-limit=3";
          source;
        ]
      in
      require
        (success command = success command)
        "deterministic initialized program IR";
      List.iter
        (fun command ->
          let status, stdout, stderr =
            invoke
              (command
              @ [
                  "--format=json";
                  "--mode=" ^ mode;
                  "--initializer-step-limit=2";
                  source;
                ])
          in
          require
            (status = Unix.WEXITED 1 && stdout = "")
            "constant preparation limit fails before output";
          require
            (Yojson.Safe.from_string stderr
            |> to_list |> List.hd |> member "code" |> to_string = "HCIRVM0007")
            "preparation limit diagnostic")
        [ [ "run" ] ];
      with_file ".hc" "I64 Fail(I64 d){return 1/d;}I64 G=Fail(0);"
        (fun source ->
          let status, stdout, stderr =
            invoke [ "run"; "--format=json"; "--mode=" ^ mode; source ]
          in
          require
            (status = Unix.WEXITED 1 && stdout = "")
            "initializer fault has no result";
          let notes =
            Yojson.Safe.from_string stderr
            |> to_list |> List.hd |> member "notes" |> to_list
            |> List.map to_string
          in
          require
            (List.mem "initializer=G" notes && List.mem "function=Fail" notes)
            "fault retains initializer and active function";
          require
            (List.mem
               ("initializer_phase="
               ^
               if mode = "jit" then "compile-initializer"
               else "load-initializer")
               notes)
            "fault phase"))
    [ "jit"; "aot" ];
  with_file ".hc" "@invalid" (fun source ->
      List.iter
        (fun command ->
          let status, stdout, stderr =
            invoke (command @ [ "--initializer-step-limit=0"; source ])
          in
          require
            (status = Unix.WEXITED 1 && stdout = "")
            "invalid initializer limit rejects before parsing";
          require
            (String.starts_with
               ~prefix:("holyc: " ^ List.hd command ^ ": HCIRVM0001")
               stderr)
            "initializer limit configuration diagnostic")
        [ [ "run" ]; [ "dump-ir"; "--program" ] ]);
  List.iter
    (fun mode ->
      let source = Sys.argv.(7) in
      let result =
        success
          [
            "run";
            "--target=ir";
            "--format=json";
            "--mode=" ^ mode;
            "--global-byte-limit=8";
            "--frame-byte-limit=8";
            "--call-depth-limit=1";
            "--initializer-step-limit=3";
            source;
          ]
        |> Yojson.Safe.from_string
      in
      require
        (result |> member "final_value" |> member "value" |> to_string = "42")
        "compound accumulator source returns 42";
      require
        (result |> member "compiled_initializer_steps" |> to_int = 3)
        "compound execution preserves separate preparation count";
      let dump = success [ "dump-ir"; "--program"; "--mode=" ^ mode; source ] in
      require
        (dump = success [ "dump-ir"; "--program"; "--mode=" ^ mode; source ])
        "deterministic compound program dump";
      require
        (contains dump "IC_ADD_EQU")
        "original compound update opcode retained";
      with_file ".hc" "I64 G=41;G++;G;" (fun source ->
          let result =
            success [ "run"; "--format=json"; "--mode=" ^ mode; source ]
            |> Yojson.Safe.from_string
          in
          require
            (result |> member "final_value" |> member "value" |> to_string
           = "42")
            "postfix source executes from CLI"))
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let source = Sys.argv.(8) in
      let result =
        success
          [
            "run";
            "--target=ir";
            "--format=json";
            "--mode=" ^ mode;
            "--global-byte-limit=8";
            "--frame-byte-limit=1";
            "--call-depth-limit=1";
            "--initializer-step-limit=4";
            source;
          ]
        |> Yojson.Safe.from_string
      in
      require
        (result |> member "final_value" |> member "value" |> to_string = "42")
        "persistent static counter returns 42";
      require
        (result |> member "compiled_initializer_steps" |> to_int = 4)
        "static image has separate definition-time preparation count";
      let dump = success [ "dump-ir"; "--program"; "--mode=" ^ mode; source ] in
      require
        (contains dump "holyc-integer-statics-v1 bytes=8"
        && contains dump "IC_PP_")
        "static storage and original prefix opcode are visible";
      require
        (dump = success [ "dump-ir"; "--program"; "--mode=" ^ mode; source ])
        "static dump is deterministic";
      List.iter
        (fun (limit, code) ->
          let status, stdout, stderr =
            invoke [ "run"; "--format=json"; "--mode=" ^ mode; limit; source ]
          in
          require
            (status = Unix.WEXITED 1 && stdout = "")
            "static limit fails without a result";
          require
            (Yojson.Safe.from_string stderr
            |> to_list |> List.hd |> member "code" |> to_string = code)
            "static bound diagnostic")
        [
          ("--global-byte-limit=7", "HCIRVM0016");
          ("--initializer-step-limit=3", "HCIRVM0007");
        ])
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let source = Sys.argv.(9) in
      let command =
        [
          "run";
          "--target=ir";
          "--format=json";
          "--mode=" ^ mode;
          "--global-byte-limit=8";
          "--frame-byte-limit=1";
          "--call-depth-limit=1";
          "--initializer-step-limit=1";
          "--step-limit=32";
          source;
        ]
      in
      let result = success command |> Yojson.Safe.from_string in
      require
        (result |> member "final_value" |> member "value" |> to_string = "42")
        "nonconstant static counter";
      require
        (result |> member "executed_steps" |> to_int = 32)
        "static initialization runtime count";
      require
        (result |> member "compiled_initializer_steps" |> to_int = 0)
        "scheduled static has no constant preparation";
      let command = [ "dump-ir"; "--program"; "--mode=" ^ mode; source ] in
      let dump = success command in
      require
        (dump = success command)
        "static initialization deterministic dump";
      require
        (contains dump "holyc-static-initialization-v1"
        && contains dump
             ("phase="
             ^
             if mode = "jit" then "compile-initializer" else "load-initializer"
             ))
        "static declaration phase";
      List.iter
        (fun (limit, code) ->
          let status, stdout, stderr =
            invoke [ "run"; "--format=json"; "--mode=" ^ mode; limit; source ]
          in
          require
            (status = Unix.WEXITED 1 && stdout = "")
            "scheduled static bound";
          require
            (Yojson.Safe.from_string stderr
            |> to_list |> List.hd |> member "code" |> to_string = code)
            "scheduled static bound code")
        [
          ("--step-limit=31", "HCIRVM0007");
          ("--global-byte-limit=7", "HCIRVM0016");
        ])
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let source = Sys.argv.(10) in
      let result =
        success
          [
            "run";
            "--format=json";
            "--mode=" ^ mode;
            "--frame-byte-limit=16";
            "--call-depth-limit=2";
            "--step-limit=43";
            source;
          ]
        |> Yojson.Safe.from_string
      in
      require
        (result |> member "final_value" |> member "value" |> to_string = "42")
        "callee writes the original caller object";
      require
        (result |> member "executed_steps" |> to_int = 43)
        "pointer instruction count";
      require
        (result |> member "compiled_initializer_steps" |> to_int = 0)
        "pointer preparation count";
      let command = [ "dump-ir"; "--program"; "--mode=" ^ mode; source ] in
      let dump = success command in
      require (dump = success command) "deterministic pointer dump";
      require
        (contains dump "IC_ADDR" && contains dump "IC_ADD_EQU"
        && contains dump "public:I64**")
        "materialized reference, indirect update and pointer parameter slot";
      List.iter
        (fun (limit, code) ->
          let status, stdout, stderr =
            invoke [ "run"; "--format=json"; "--mode=" ^ mode; limit; source ]
          in
          require
            (status = Unix.WEXITED 1 && stdout = "")
            "pointer limit fails without a result";
          require
            (Yojson.Safe.from_string stderr
            |> to_list |> List.hd |> member "code" |> to_string = code)
            "pointer limit diagnostic")
        [
          ("--step-limit=42", "HCIRVM0007");
          ("--frame-byte-limit=15", "HCIRVM0011");
          ("--call-depth-limit=1", "HCIRVM0015");
        ])
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let source = Sys.argv.(11) in
      let result =
        success
          [
            "run";
            "--format=json";
            "--mode=" ^ mode;
            "--frame-byte-limit=24";
            "--call-depth-limit=2";
            "--step-limit=51";
            source;
          ]
        |> Yojson.Safe.from_string
      in
      require
        (result |> member "final_value" |> member "value" |> to_string = "42")
        "caller element writeback";
      require
        (result |> member "executed_steps" |> to_int = 51)
        "array instruction count";
      require
        (result |> member "compiled_initializer_steps" |> to_int = 0)
        "array preparation count";
      let command = [ "dump-ir"; "--program"; "--mode=" ^ mode; source ] in
      let dump = success command in
      require (dump = success command) "deterministic array dump";
      require
        (contains dump "IC_MUL" && contains dump "IC_ADDR"
       && contains dump "IC_ADD_EQU")
        "indexed address and indirect update";
      List.iter
        (fun (limit, code) ->
          let status, stdout, stderr =
            invoke [ "run"; "--format=json"; "--mode=" ^ mode; limit; source ]
          in
          require
            (status = Unix.WEXITED 1 && stdout = "")
            "array limit has no result";
          require
            (Yojson.Safe.from_string stderr
            |> to_list |> List.hd |> member "code" |> to_string = code)
            "array limit diagnostic")
        [
          ("--step-limit=50", "HCIRVM0007");
          ("--frame-byte-limit=23", "HCIRVM0011");
          ("--call-depth-limit=1", "HCIRVM0015");
        ])
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let source = Sys.argv.(12) in
      let result =
        success
          [
            "run";
            "--target=ir";
            "--format=json";
            "--mode=" ^ mode;
            "--frame-byte-limit=16";
            "--call-depth-limit=2";
            "--step-limit=69";
            source;
          ]
        |> Yojson.Safe.from_string
      in
      let value = result |> member "final_value" in
      require
        (value |> member "value" |> to_string = "42")
        "byte Sum fixture returns 42";
      require
        (value |> member "type" |> to_string = "i64")
        "byte Sum fixture retains the declared return class";
      require
        (result |> member "mode" |> to_string = mode)
        "byte report retains execution mode";
      require
        (result |> member "executed_steps" |> to_int = 69)
        "byte fixture instruction count";
      require
        (result |> member "compiled_initializer_steps" |> to_int = 0)
        "automatic bytes add no initializer preparation";
      require
        ( success [ "run"; "--target=ir"; "--mode=" ^ mode; source ]
        |> fun output -> contains output "final-value=42 type=i64" )
        "human byte report returns the checked word";
      let command = [ "dump-ir"; "--program"; "--mode=" ^ mode; source ] in
      let dump = success command in
      require (dump = success command) "deterministic byte program dump";
      require
        (contains dump "public:U8*" && contains dump "IC_MUL"
       && contains dump "IC_DEREF")
        "byte pointer and indexed load evidence survives the dump";
      List.iter
        (fun (limit, code) ->
          let status, stdout, stderr =
            invoke
              [
                "run";
                "--target=ir";
                "--format=json";
                "--mode=" ^ mode;
                limit;
                source;
              ]
          in
          require
            (status = Unix.WEXITED 1 && stdout = "")
            "byte limit fails without a result";
          require
            (Yojson.Safe.from_string stderr
            |> to_list |> List.hd |> member "code" |> to_string = code)
            "byte limit diagnostic")
        [
          ("--step-limit=68", "HCIRVM0007");
          ("--frame-byte-limit=15", "HCIRVM0011");
          ("--call-depth-limit=1", "HCIRVM0015");
        ];
      with_file ".hc" "I64 F(){U8 n;I64 value=(n=298);return value*1000+n;}F();"
        (fun source ->
          let result =
            success [ "run"; "--format=json"; "--mode=" ^ mode; source ]
            |> Yojson.Safe.from_string
          in
          require
            (result |> member "final_value" |> member "value" |> to_string
           = "298042")
            "byte assignment value and stored readback are independent");
      List.iter
        (fun (text, code) ->
          with_file ".hc" text (fun source ->
              let status, stdout, stderr =
                invoke [ "run"; "--format=json"; "--mode=" ^ mode; source ]
              in
              require
                (status = Unix.WEXITED 1 && stdout = "")
                "byte fault has no result";
              let diagnostic =
                Yojson.Safe.from_string stderr |> to_list |> List.hd
              in
              require
                (diagnostic |> member "code" |> to_string = code)
                "byte access diagnostic";
              require
                (diagnostic |> member "notes" |> to_list |> List.map to_string
               |> List.mem "function=F")
                "byte access retains the owning function"))
        [
          ("I64 F(){U8 a[2];a[0]=42;return a[1];}F();", "HCIRVM0012");
          ("I64 F(){U8 a[2];U8 *p=&a[2];return *p;}F();", "HCIRVM0019");
        ])
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let source = Sys.argv.(13) in
      let result =
        success
          [
            "run";
            "--target=ir";
            "--format=json";
            "--mode=" ^ mode;
            "--literal-byte-limit=2";
            "--step-limit=45";
            "--frame-byte-limit=16";
            "--call-depth-limit=2";
            source;
          ]
        |> Yojson.Safe.from_string
      in
      require
        (result |> member "final_value" |> member "value" |> to_string = "42")
        "owned string Read fixture returns 42";
      require
        (result |> member "literal_byte_limit" |> to_int = 2)
        "literal storage has a separately reported byte bound";
      require
        (result |> member "executed_steps" |> to_int = 45)
        "owned-string fixture instruction count";
      require
        (result |> member "compiled_initializer_steps" |> to_int = 0)
        "literal allocation consumes no constant-preparation instructions";
      let human =
        success [ "run"; "--mode=" ^ mode; "--literal-byte-limit=2"; source ]
      in
      require
        (contains human "literal-byte-limit=2"
        && contains human "final-value=42 type=i64")
        "human report retains string bound and result";
      let dump_command = [ "dump-ir"; "--program"; "--mode=" ^ mode; source ] in
      let dump = success dump_command in
      require
        (dump = success dump_command && contains dump "IC_STR_CONST")
        "owned strings reuse deterministic canonical literal IR";
      let status, stdout, stderr =
        invoke
          [
            "run";
            "--format=json";
            "--mode=" ^ mode;
            "--literal-byte-limit=1";
            source;
          ]
      in
      require
        (status = Unix.WEXITED 1 && stdout = "")
        "literal capacity fails before publishing a result";
      let diagnostic = Yojson.Safe.from_string stderr |> to_list |> List.hd in
      require
        (diagnostic |> member "code" |> to_string = "HCIRVM0021")
        "literal image capacity diagnostic";
      let notes =
        diagnostic |> member "notes" |> to_list |> List.map to_string
      in
      require
        (List.mem "stage=preflight" notes
        && List.mem "executed_steps=0" notes
        && List.mem "function=F" notes)
        "capacity reports the literal owner and no executed effects";
      List.iter
        (fun (limit, code) ->
          let status, stdout, stderr =
            invoke [ "run"; "--format=json"; "--mode=" ^ mode; limit; source ]
          in
          require
            (status = Unix.WEXITED 1 && stdout = "")
            "string execution limit has no result";
          require
            (Yojson.Safe.from_string stderr
            |> to_list |> List.hd |> member "code" |> to_string = code)
            "string execution limit diagnostic")
        [
          ("--step-limit=44", "HCIRVM0007");
          ("--frame-byte-limit=15", "HCIRVM0011");
          ("--call-depth-limit=1", "HCIRVM0015");
        ];
      with_file ".hc" "I64 F(){return \"*\"[2];}F();" (fun source ->
          let status, stdout, stderr =
            invoke [ "run"; "--format=json"; "--mode=" ^ mode; source ]
          in
          require
            (status = Unix.WEXITED 1 && stdout = "")
            "literal one-past dereference has no result";
          let diagnostic =
            Yojson.Safe.from_string stderr |> to_list |> List.hd
          in
          require
            (diagnostic |> member "code" |> to_string = "HCIRVM0019")
            "literal access uses declared object bounds"))
    [ "jit"; "aot" ];
  List.iter
    (fun limit ->
      with_file ".hc" "this is not a valid program" (fun source ->
          let status, stdout, stderr =
            invoke [ "run"; "--literal-byte-limit=" ^ limit; source ]
          in
          require
            (status = Unix.WEXITED 1 && stdout = ""
            && contains stderr "HCIRVM0001")
            "nonpositive literal budget rejects before source compilation"))
    [ "0"; "-1" ];
  List.iter
    (fun mode ->
      let source = Sys.argv.(14) in
      let result =
        success
          [
            "run";
            "--target=ir";
            "--format=json";
            "--mode=" ^ mode;
            "--step-limit=19";
            "--frame-byte-limit=8";
            "--global-byte-limit=8";
            "--call-depth-limit=1";
            source;
          ]
        |> Yojson.Safe.from_string
      in
      require
        (result |> member "final_value" |> member "value" |> to_string = "42")
        "U0 call writes the global before caller continuation";
      require
        (result |> member "executed_steps" |> to_int = 19
        && result |> member "compiled_initializer_steps" |> to_int = 0)
        "U0 fixture uses its canonical runtime instructions without preparation";
      require
        (result |> member "termination" |> to_string = "stream-end")
        "U0 callee return does not terminate the caller stream";
      let human = success [ "run"; "--mode=" ^ mode; source ] in
      require
        (contains human "final-value=42 type=i64")
        "human U0 fixture report retains the final word";
      let dump_command = [ "dump-ir"; "--program"; "--mode=" ^ mode; source ] in
      let dump = success dump_command in
      require
        (dump = success dump_command
        && contains dump "public:U0 = IC_CALL_END"
        && contains dump "IC_END_EXP" && contains dump "IC_RET")
        "U0 source retains canonical call-end, discard and return instructions";
      List.iter
        (fun (limit, code) ->
          let status, stdout, stderr =
            invoke [ "run"; "--format=json"; "--mode=" ^ mode; limit; source ]
          in
          require
            (status = Unix.WEXITED 1 && stdout = "")
            "U0 resource failure publishes no result";
          let diagnostic =
            let json = Yojson.Safe.from_string stderr in
            if code = "HCIRVM0001" then (
              require
                (json |> member "schema" |> to_string = "holyc-command-error-v1")
                "nonpositive call depth uses the command configuration report";
              json)
            else json |> to_list |> List.hd
          in
          require
            (diagnostic |> member "code" |> to_string = code)
            "U0 fixture resource diagnostic")
        [
          ("--step-limit=18", "HCIRVM0007");
          ("--frame-byte-limit=7", "HCIRVM0011");
          ("--global-byte-limit=7", "HCIRVM0016");
          ("--call-depth-limit=0", "HCIRVM0001");
        ];
      List.iter
        (fun (text, expected) ->
          with_file ".hc" text (fun source ->
              let result =
                success [ "run"; "--format=json"; "--mode=" ^ mode; source ]
                |> Yojson.Safe.from_string
              in
              require
                (result |> member "final_value" = expected)
                "U0 discard clears a preceding final word";
              if expected = `Null then
                require
                  (contains
                     (success [ "run"; "--mode=" ^ mode; source ])
                     "final-value=none")
                  "human no-value report is explicit"))
        [
          ("U0 F(){}F();", `Null);
          ("U0 F(){}42;F();", `Null);
          ("I64 W(){return 42;}U0 F(){}W();F();", `Null);
        ];
      with_file ".hc" "U0 F(){}F();42;" (fun source ->
          let result =
            success [ "run"; "--format=json"; "--mode=" ^ mode; source ]
            |> Yojson.Safe.from_string
          in
          require
            (result |> member "final_value" |> member "value" |> to_string
           = "42")
            "a later word expression replaces the no-value result");
      List.iter
        (fun (text, options, code, owner) ->
          with_file ".hc" text (fun source ->
              let status, stdout, stderr =
                invoke
                  ([ "run"; "--format=json"; "--mode=" ^ mode ]
                  @ options @ [ source ])
              in
              require
                (status = Unix.WEXITED 1 && stdout = "")
                "failed U0 or word callee supplies no successful report";
              let diagnostic =
                Yojson.Safe.from_string stderr |> to_list |> List.hd
              in
              let notes =
                diagnostic |> member "notes" |> to_list |> List.map to_string
              in
              require
                (diagnostic |> member "code" |> to_string = code
                && List.mem "stage=execution" notes
                && List.mem ("function=" ^ owner) notes
                && diagnostic |> member "primary" <> `Null)
                "call failure retains its phase, owner and source"))
        [
          ( "U0 R(I64 n){if(n)R(n-1);}R(1);",
            [ "--call-depth-limit=1" ],
            "HCIRVM0015",
            "R" );
          ("I64 W(){return 42;}I64 F(){W();return;}F();", [], "HCIRVM0013", "F");
          ("I64 G;U0 F(){G=42;1/0;}F();G;", [], "HCIRVM0009", "F");
        ])
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let run ?(options = []) path =
        let status, stdout, stderr =
          invoke_raw
            ([ "run"; "--format=json"; "--mode=" ^ mode ] @ options @ [ path ])
        in
        require (stderr = "") ("v2 diagnostics belong in the report: " ^ stderr);
        (status, Yojson.Safe.from_string stdout)
      in
      let check_capture report bytes length work =
        require
          (report |> member "schema" |> to_string = "holyc-integer-program-v2")
          "default run report version";
        require
          (report |> member "output_hex" |> to_string = bytes
          && report |> member "output_byte_length" |> to_int = length
          && report |> member "output_work" |> to_int = work)
          "lossless captured bytes and exact work"
      in
      List.iter
        (fun text ->
          with_file ".hc" text (fun source ->
              let status, report = run source in
              require (status = Unix.WEXITED 0) "output form succeeds";
              let work = if contains text "PutChars" then 6 else 7 in
              check_capture report "34320a" 3 work;
              require
                (report |> member "outcome" |> to_string = "success"
                && report |> member "final_value" |> member "value" |> to_string
                   = "42")
                "capture is separate from final expression"))
        [
          "extern U0 Print(U8 *fmt,...);\"42\\n\";42;";
          "extern U0 Print(U8 *fmt,...);Print(\"42\\n\");42;";
          "extern U0 PutChars(U64 ch);'42\\n';42;";
          "extern U0 PutChars(U64 ch);PutChars('42\\n');42;";
        ];
      let status, report =
        run
          ~options:
            [
              "--output-byte-limit=3";
              "--output-work-limit=7";
              "--step-limit=10";
              "--frame-byte-limit=16";
              "--call-depth-limit=1";
              "--literal-byte-limit=4";
            ]
          Sys.argv.(15)
      in
      require (status = Unix.WEXITED 0) "maintained output fixture exact limits";
      check_capture report "34320a" 3 7;
      let status, packed_report =
        run
          ~options:
            [
              "--output-byte-limit=3";
              "--output-work-limit=6";
              "--step-limit=9";
              "--frame-byte-limit=8";
              "--call-depth-limit=1";
            ]
          Sys.argv.(16)
      in
      require (status = Unix.WEXITED 0)
        "maintained PutChars fixture exact limits";
      check_capture packed_report "34320a" 3 6;
      List.iter
        (fun (fixture, steps, work) ->
          let status, failed =
            run ~options:[ "--step-limit=" ^ string_of_int steps ] fixture
          in
          require (status = Unix.WEXITED 1) "one-below IR step limit fails";
          check_capture failed "34320a" 3 work;
          require
            (failed |> member "executed_steps" |> to_int = steps
            && failed |> member "final_value" = `Null
            && failed |> member "diagnostics" |> to_list |> List.hd
               |> member "code" |> to_string = "HCIRVM0007")
            "later step fault retains output but no successful result")
        [ (Sys.argv.(15), 9, 7); (Sys.argv.(16), 8, 6) ];
      let human_status, human, human_errors =
        invoke_raw [ "run"; "--mode=" ^ mode; Sys.argv.(15) ]
      in
      require
        (human_status = Unix.WEXITED 0
        && human_errors = ""
        && String.starts_with ~prefix:"holyc-integer-program-v2" human
        && contains human "output-hex=34320a"
        && contains human "output-byte-length=3")
        "human output renders bytes explicitly";
      List.iter
        (fun (suffix, options, code, bytes, length, work) ->
          with_file ".hc" ("extern U0 Print(U8 *fmt,...);\"A\";" ^ suffix)
            (fun source ->
              let status, report = run ~options source in
              require (status = Unix.WEXITED 1) "output fault exits one";
              check_capture report bytes length work;
              require
                (report |> member "outcome" |> to_string = "error"
                && report |> member "final_value" = `Null
                && report |> member "termination" = `Null)
                "failure cannot fabricate a final result";
              let diagnostic =
                report |> member "diagnostics" |> to_list |> List.hd
              in
              require
                (diagnostic |> member "code" |> to_string = code)
                "v2 retains the actual runtime diagnostic"))
        [
          ("1/0;", [], "HCIRVM0009", "41", 1, 3);
          ("\"B%q\";42;", [], "HCIRVM0024", "41", 1, 7);
          ("\"BC\";42;", [ "--output-byte-limit=2" ], "HCIRVM0022", "41", 1, 7);
          ("\"B\";42;", [ "--output-work-limit=5" ], "HCIRVM0023", "41", 1, 5);
        ];
      with_file ".hc" "extern U0 Print(U8 *fmt,...);\"\\x80\\xff\";42;"
        (fun source ->
          let status, report = run source in
          require (status = Unix.WEXITED 0) "binary output succeeds";
          check_capture report "80ff" 2 5);
      with_file ".hc" "#assert 0\n42;" (fun source ->
          let status, report = run source in
          require (status = Unix.WEXITED 0) "v2 warnings preserve success";
          require
            (report |> member "diagnostics" |> to_list |> List.hd
           |> member "code" |> to_string = "HCPP0024")
            "v2 report includes warnings");
      List.iter
        (fun option ->
          let status, report = run ~options:[ option ] Sys.argv.(15) in
          require (status = Unix.WEXITED 1) "nonpositive output limit fails";
          check_capture report "" 0 0;
          require
            (report |> member "command_error" |> member "code" |> to_string
           = "HCIRVM0001")
            "v2 command failure carries its code")
        [
          "--output-byte-limit=0";
          "--output-work-limit=0";
          "--output-byte-limit=" ^ string_of_int max_int;
        ];
      with_file ".hc"
        "extern U0 Print(U8 *fmt,...);extern U0 Other();\"A\";Other();42;"
        (fun source ->
          let status, report = run source in
          require (status = Unix.WEXITED 1)
            "unsupported provider fails preflight";
          check_capture report "" 0 0;
          let diagnostic =
            report |> member "diagnostics" |> to_list |> List.hd
          in
          require
            (diagnostic |> member "code" |> to_string = "HCIRVM0014"
            && diagnostic |> member "notes" |> to_list
               |> List.mem (`String "stage=preflight"))
            "preflight failure captures no earlier output");
      let legacy =
        success [ "run"; "--format=json"; "--mode=" ^ mode; Sys.argv.(15) ]
        |> Yojson.Safe.from_string
      in
      require
        (legacy |> member "schema" |> to_string = "holyc-integer-program-v1"
        && legacy |> member "output_hex" = `Null
        && legacy |> member "final_value" |> member "value" |> to_string = "42"
        )
        "explicit v1 retains the established outcome projection")
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let run ?(options = []) source =
        let status, output, errors =
          invoke_raw
            ([ "run"; "--format=json"; "--mode=" ^ mode ] @ options @ [ source ])
        in
        require (errors = "") "joined program JSON has one metadata stream";
        (status, Yojson.Safe.from_string output)
      in
      let check_value report capture =
        require
          (report |> member "outcome" |> to_string = "success"
          && report |> member "final_value" |> member "value" |> to_string
             = "42"
          && report |> member "output_hex" |> to_string = capture)
          "joined definition preserves value and capture"
      in
      let source = Sys.argv.(17) in
      let status, report =
        run
          ~options:
            [
              "--step-limit=29"; "--frame-byte-limit=24"; "--call-depth-limit=1";
            ]
          source
      in
      require (status = Unix.WEXITED 0) "maintained joined Add exact limits";
      check_value report "";
      require
        (report |> member "executed_steps" |> to_int = 29
        && report |> member "compiled_initializer_steps" |> to_int = 0
        && report |> member "output_work" |> to_int = 0)
        "prototypes add no runtime or preparation charge";
      List.iter
        (fun (source, capture) ->
          with_file ".hc" source (fun path ->
              let status, report = run path in
              require (status = Unix.WEXITED 0) "joined source gate";
              check_value report capture))
        [
          ("extern I64 Id(I64 value);I64 Id(I64 n){return n;}Id(42);", "");
          ("extern U0 Set(I64 value);I64 G=0;U0 Set(I64 n){G=n;}Set(42);G;", "");
          ( "extern I64 Inc(I64 value);I64 Inc(I64 n){return n+1;}I64 \
             Twice(I64 n){return Inc(Inc(n));}Twice(40);",
            "" );
          ( "I64 G=0;extern U0 PutChars(U64 ch);PutChars('A');U0 PutChars(U64 \
             word){G=42;}PutChars('B');G;",
            "41" );
        ];
      List.iter
        (fun (option, code) ->
          let status, report = run ~options:[ option ] source in
          require
            (status = Unix.WEXITED 1
            && report |> member "final_value" = `Null
            && report |> member "diagnostics" |> to_list |> List.hd
               |> member "code" |> to_string = code)
            "joined fixture one-below resource bound")
        [
          ("--step-limit=28", "HCIRVM0007");
          ("--frame-byte-limit=23", "HCIRVM0011");
        ];
      let legacy =
        success [ "run"; "--format=json"; "--mode=" ^ mode; source ]
        |> Yojson.Safe.from_string
      in
      require
        (legacy |> member "schema" |> to_string = "holyc-integer-program-v1"
        && legacy |> member "final_value" |> member "value" |> to_string = "42"
        )
        "joined execution preserves legacy reporting";
      let dump () =
        success [ "dump-ir"; "--program"; "--mode=" ^ mode; source ]
      in
      let first = dump () in
      require
        (first = dump () && contains first "holyc-ir-function-binding-v1")
        "deterministic joined callable/definition mapping")
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let run ?(options = []) source =
        let status, output, errors =
          invoke_raw
            ([ "run"; "--format=json"; "--mode=" ^ mode ] @ options @ [ source ])
        in
        require (errors = "") "persistent-byte JSON stays on one stream";
        (status, Yojson.Safe.from_string output)
      in
      let check report type_ capture =
        require
          (report |> member "outcome" |> to_string = "success"
          && report |> member "final_value" |> member "value" |> to_string
             = "42"
          && report |> member "final_value" |> member "type" |> to_string
             = type_
          && report |> member "output_hex" |> to_string = capture)
          "persistent byte class, value and capture"
      in
      let source = Sys.argv.(18) in
      let status, report =
        run
          ~options:
            [
              "--step-limit=77";
              "--initializer-step-limit=7";
              "--global-byte-limit=9";
              "--frame-byte-limit=16";
              "--call-depth-limit=2";
            ]
          source
      in
      require (status = Unix.WEXITED 0)
        "maintained persistent bytes at exact limits";
      check report "i64" "";
      require
        (report |> member "executed_steps" |> to_int = 77
        && report |> member "compiled_initializer_steps" |> to_int = 7
        && report |> member "output_work" |> to_int = 0)
        "maintained persistent byte work counts";
      List.iter
        (fun (text, type_, capture) ->
          with_file ".hc" text (fun path ->
              let status, report = run path in
              require (status = Unix.WEXITED 0) "persistent byte source gate";
              check report type_ capture))
        [
          ("U8 G=298;G;", "u64", "");
          ("U8 G;U0 Set(U8 *p){*p=298;}Set(&G);G;", "u64", "");
          ("I64 Next(){static U8 n=40;n=n+1;return n;}Next();Next();", "i64", "");
          ( "extern U0 PutChars(U64 ch);I64 Seed(I64 n){PutChars(65);return \
             n;}U8 G=Seed(298);G;",
            "u64",
            "41" );
          ( "extern U0 PutChars(U64 ch);I64 Seed(I64 n){PutChars(65);return \
             n;}I64 Read(){static U8 n=Seed(298);return n;}Read();",
            "i64",
            "41" );
          ( "U0 Set(U8 *p){*p=298;}I64 Read(){static U8 n=0;Set(&n);return \
             n;}Read();",
            "i64",
            "" );
        ];
      List.iter
        (fun (option, code) ->
          let status, report = run ~options:[ option ] source in
          require
            (status = Unix.WEXITED 1
            && report |> member "final_value" = `Null
            && report |> member "diagnostics" |> to_list |> List.hd
               |> member "code" |> to_string = code)
            "persistent byte one-below limit")
        [
          ("--step-limit=76", "HCIRVM0007");
          ("--initializer-step-limit=6", "HCIRVM0007");
          ("--global-byte-limit=8", "HCIRVM0016");
          ("--frame-byte-limit=15", "HCIRVM0011");
          ("--call-depth-limit=1", "HCIRVM0015");
        ];
      let legacy =
        success [ "run"; "--format=json"; "--mode=" ^ mode; source ]
        |> Yojson.Safe.from_string
      in
      require
        (legacy |> member "schema" |> to_string = "holyc-integer-program-v1"
        && legacy |> member "final_value" |> member "value" |> to_string = "42"
        )
        "persistent bytes preserve v1 reporting";
      let dump () =
        success [ "dump-ir"; "--program"; "--mode=" ^ mode; source ]
      in
      let first = dump () in
      require
        (first = dump ()
        && contains first "holyc-integer-globals-v1 bytes=1"
        && contains first "holyc-integer-statics-v1 bytes=8")
        "deterministic declared and padded persistent bytes")
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let run ?(options = []) source =
        let status, output, errors =
          invoke_raw
            ([ "run"; "--format=json"; "--mode=" ^ mode ] @ options @ [ source ])
        in
        require (errors = "") "persistent arrays use one metadata stream";
        (status, Yojson.Safe.from_string output)
      in
      let check report type_ capture =
        require
          (report |> member "schema" |> to_string = "holyc-integer-program-v2"
          && report |> member "outcome" |> to_string = "success"
          && report |> member "final_value" |> member "value" |> to_string
             = "42"
          && report |> member "final_value" |> member "type" |> to_string
             = type_
          && report |> member "output_hex" |> to_string = capture)
          "persistent array value class and capture"
      in
      let source = Sys.argv.(19) in
      let bounds =
        [
          ("step", 101, "HCIRVM0007", "3432");
          ("initializer-step", 23, "HCIRVM0007", "");
          ("global-byte", 43, "HCIRVM0016", "");
          ("frame-byte", 24, "HCIRVM0011", "");
          ("call-depth", 2, "HCIRVM0015", "3432");
          ("literal-byte", 3, "HCIRVM0021", "");
          ("output-byte", 2, "HCIRVM0022", "");
          ("output-work", 8, "HCIRVM0023", "");
        ]
      in
      let option name value = Printf.sprintf "--%s-limit=%d" name value in
      let status, report =
        run
          ~options:
            (List.map (fun (name, value, _, _) -> option name value) bounds)
          source
      in
      require (status = Unix.WEXITED 0)
        "maintained array fixture at all exact bounds";
      check report "i64" "3432";
      require
        (report |> member "executed_steps" |> to_int = 101
        && report |> member "compiled_initializer_steps" |> to_int = 23
        && report |> member "output_work" |> to_int = 8)
        "maintained array work counts";
      List.iter
        (fun (name, value, code, capture) ->
          let status, report =
            run ~options:[ option name (value - 1) ] source
          in
          require
            (status = Unix.WEXITED 1
            && report |> member "final_value" = `Null
            && report |> member "diagnostics" |> to_list |> List.hd
               |> member "code" |> to_string = code
            && report |> member "output_hex" |> to_string = capture)
            ("persistent array one-below " ^ name))
        bounds;
      List.iter
        (fun (text, type_, capture) ->
          with_file ".hc" text (fun path ->
              let status, report = run path in
              require (status = Unix.WEXITED 0) "persistent array source gate";
              check report type_ capture))
        [
          ("U8 A[2];A[0]=40;A[1]=2;A[0]+A[1];", "u64", "");
          ("I64 A[2];A[0]=40;A[1]=2;A[0]+A[1];", "i64", "");
          ( "I64 F(){static U8 A[2];A[0]=40;A[1]=2;return A[0]+A[1];}F();",
            "i64",
            "" );
          ( "I64 F(){static I64 A[2];A[0]=40;A[1]=2;return A[0]+A[1];}F();",
            "i64",
            "" );
          ("I64 A[2]={40,2};A[0]+A[1];", "i64", "");
          ("I64 F(){static U8 A[2]={40,2};return A[0]+A[1];}F();", "i64", "");
          ( "extern U0 Print(U8 *fmt,...);U8 Msg[3]=\"42\";Print(\"%s\",Msg);42;",
            "i64",
            "3432" );
          ( "extern U0 Print(U8 *fmt,...);I64 F(){static U8 \
             Msg[3]=\"42\";Print(\"%s\",Msg);return 42;}F();",
            "i64",
            "3432" );
        ];
      let legacy =
        success [ "run"; "--format=json"; "--mode=" ^ mode; source ]
        |> Yojson.Safe.from_string
      in
      require
        (legacy |> member "schema" |> to_string = "holyc-integer-program-v1"
        && legacy |> member "final_value" |> member "value" |> to_string = "42"
        )
        "persistent arrays preserve v1 reports";
      let dump () =
        success [ "dump-ir"; "--program"; "--mode=" ^ mode; source ]
      in
      let first = dump () in
      require
        (first = dump ()
        && contains first "holyc-persistent-arrays-v1"
        && contains first "dimensions=[2,2] strides=[16,8] cells=4 bytes=32"
        && contains first "prepared-bytes:343200"
        && contains first "holyc-array-publications-v1" = (mode = "jit"))
        "deterministic array shapes images and publication markers")
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let run ?(options = []) source =
        let status, output, errors =
          invoke_raw
            ([ "run"; "--format=json"; "--mode=" ^ mode ] @ options @ [ source ])
        in
        require (errors = "") "byte updates use one metadata stream";
        (status, Yojson.Safe.from_string output)
      in
      let check report type_ capture =
        require
          (report |> member "schema" |> to_string = "holyc-integer-program-v2"
          && report |> member "outcome" |> to_string = "success"
          && report |> member "final_value" |> member "value" |> to_string
             = "42"
          && report |> member "final_value" |> member "type" |> to_string
             = type_
          && report |> member "output_hex" |> to_string = capture)
          "byte update value class and capture"
      in
      let source = Sys.argv.(20) in
      let bounds =
        [
          ("step", 79, "HCIRVM0007", "3432");
          ("initializer-step", 10, "HCIRVM0007", "");
          ("global-byte", 12, "HCIRVM0016", "");
          ("frame-byte", 40, "HCIRVM0011", "");
          ("call-depth", 2, "HCIRVM0015", "");
          ("literal-byte", 3, "HCIRVM0021", "");
          ("output-byte", 2, "HCIRVM0022", "");
          ("output-work", 8, "HCIRVM0023", "");
        ]
      in
      let option name value = Printf.sprintf "--%s-limit=%d" name value in
      let status, report =
        run
          ~options:
            (List.map (fun (name, value, _, _) -> option name value) bounds)
          source
      in
      require (status = Unix.WEXITED 0)
        "maintained byte updates at all exact limits";
      check report "i64" "3432";
      require
        (report |> member "executed_steps" |> to_int = 79
        && report |> member "compiled_initializer_steps" |> to_int = 10
        && report |> member "output_work" |> to_int = 8)
        "maintained byte update work counts";
      List.iter
        (fun (name, value, code, capture) ->
          let status, report =
            run ~options:[ option name (value - 1) ] source
          in
          require
            (status = Unix.WEXITED 1
            && report |> member "final_value" = `Null
            && report |> member "diagnostics" |> to_list |> List.hd
               |> member "code" |> to_string = code
            && report |> member "output_hex" |> to_string = capture)
            ("byte update one-below " ^ name))
        bounds;
      List.iter
        (fun (text, type_) ->
          with_file ".hc" text (fun path ->
              let status, report = run path in
              require (status = Unix.WEXITED 0) "byte update source gate";
              check report type_ ""))
        [
          ("U8 G=40;G+=2;G;", "u64");
          ("U8 G=41;++G;", "u64");
          ("I64 F(){U8 n=41;return n++;}F()+1;", "i64");
          ("I64 F(){static U8 n=40;n++;return n;}F();F();", "i64");
          ("U8 A[2]={40,0};A[0]+=2;A[0];", "u64");
          ("I64 F(){static U8 A[2]={41,0};return ++A[0];}F();", "i64");
          ("U8 G=41;U0 Bump(U8 *p){(*p)++;}Bump(&G);G;", "u64");
          ("U8 G=255;I64 N=++G;N+42;", "i64");
        ];
      List.iter
        (fun text ->
          with_file ".hc" text (fun path ->
              let status, report = run path in
              require
                (status = Unix.WEXITED 1
                && report |> member "final_value" = `Null
                && report |> member "output_hex" |> to_string = ""
                && report |> member "output_work" |> to_int = 0
                && report |> member "diagnostics" |> to_list |> List.hd
                   |> member "code" |> to_string = "HCRUN0006")
                "native byte update initializer proof boundary"))
        [
          "U8 G=250;I64 N=(G+=10);N;";
          "U8 G=84;I64 F(){U8 d=554;return G/=d;}I64 N=F();N;";
          "U8 A[8]={42,1,0,0,0,0,0,0};I64 F(){(A[0]&=~256)-0;return A[1];}I64 \
           N=F();N;";
        ];
      let legacy =
        success [ "run"; "--format=json"; "--mode=" ^ mode; source ]
        |> Yojson.Safe.from_string
      in
      require
        (legacy |> member "schema" |> to_string = "holyc-integer-program-v1"
        && legacy |> member "final_value" |> member "value" |> to_string = "42"
        )
        "byte updates preserve v1 reporting";
      let dump () =
        success [ "dump-ir"; "--program"; "--mode=" ^ mode; source ]
      in
      let first = dump () in
      require
        (first = dump ()
        && contains first "public:U8 = IC_ADD_EQU"
        && contains first "public:U8 = IC_PP_"
        && contains first "public:U8 = IC__PP"
        && contains first "holyc-initializer-preparation-v1 steps=10")
        "deterministic byte update original opcodes and preparation")
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let run ?(options = []) source =
        let status, stdout, stderr =
          invoke_raw
            ([ "run"; "--format=json"; "--mode=" ^ mode ] @ options @ [ source ])
        in
        require (stderr = "") "byte signature JSON has no stderr";
        (status, Yojson.Safe.from_string stdout)
      in
      let open Yojson.Safe.Util in
      let check report type_ capture =
        require
          (report |> member "schema" |> to_string = "holyc-integer-program-v2"
          && report |> member "outcome" |> to_string = "success"
          && report |> member "final_value" |> member "value" |> to_string
             = "42"
          && report |> member "final_value" |> member "type" |> to_string
             = type_
          && report |> member "output_hex" |> to_string = capture)
          "byte signature value class and capture"
      in
      let source = Sys.argv.(21) in
      let bounds =
        [
          ("step", 110, "HCIRVM0007", "3432");
          ("initializer-step", 7, "HCIRVM0007", "");
          ("global-byte", 12, "HCIRVM0016", "");
          ("frame-byte", 32, "HCIRVM0011", "");
          ("call-depth", 2, "HCIRVM0015", "");
          ("literal-byte", 3, "HCIRVM0021", "");
          ("output-byte", 2, "HCIRVM0022", "");
          ("output-work", 8, "HCIRVM0023", "");
        ]
      in
      let option name value = Printf.sprintf "--%s-limit=%d" name value in
      let status, report =
        run
          ~options:
            (List.map (fun (name, value, _, _) -> option name value) bounds)
          source
      in
      require (status = Unix.WEXITED 0) "byte signatures at all exact limits";
      check report "i64" "3432";
      require
        (report |> member "executed_steps" |> to_int = 110
        && report |> member "compiled_initializer_steps" |> to_int = 7
        && report |> member "output_work" |> to_int = 8)
        "byte signature fixture counts";
      List.iter
        (fun (name, value, code, capture) ->
          let status, report =
            run ~options:[ option name (value - 1) ] source
          in
          require
            (status = Unix.WEXITED 1
            && report |> member "final_value" = `Null
            && report |> member "diagnostics" |> to_list |> List.hd
               |> member "code" |> to_string = code
            && report |> member "output_hex" |> to_string = capture)
            ("byte signature one-below " ^ name))
        bounds;
      List.iter
        (fun (text, type_) ->
          with_file ".hc" text (fun path ->
              let status, report = run path in
              require (status = Unix.WEXITED 0)
                "original byte signature source gate";
              check report type_ ""))
        [
          ("I64 Echo(U8 n){return n;}Echo(42);", "i64");
          ("I64 Add(U8 a,U8 b){return a+b;}Add(20,22);", "i64");
          ("U8 Answer(){return 42;}Answer();", "u64");
          ("U8 Add(U8 a,U8 b){return a+b;}Add(20,22);", "u64");
          ("I64 Bump(U8 n){return ++n;}Bump(41);", "i64");
          ( "U8 Inner(){return 40;}I64 Outer(U8 n){return n+Inner();}Outer(2);",
            "i64" );
          ("U0 Set(U8 *p){*p=42;}I64 F(U8 n){Set(&n);return n;}F(0);", "i64");
          ("I64 Id(U8 n){return n;}U8 G=Id(42);G;", "u64");
        ];
      List.iter
        (fun text ->
          with_file ".hc" text (fun path ->
              let status, report = run path in
              require
                (status = Unix.WEXITED 1
                && report |> member "final_value" = `Null
                && report |> member "diagnostics" |> to_list |> List.hd
                   |> member "code" |> to_string = "HCRUN0006"
                && report |> member "output_hex" |> to_string = ""
                && report |> member "output_work" |> to_int = 0)
                "native parameter and return proof boundary"))
        [
          "I64 F(U8 n){n=554;return n;}I64 N=F(0);N;";
          "I64 F(U8 n){return ++n;}I64 N=F(255);N;";
          "U8 G=84;U8 Wide(){return 554;}I64 F(){U8 n=Wide();return G/=n;}I64 \
           N=F();N;";
        ];
      let legacy =
        success [ "run"; "--format=json"; "--mode=" ^ mode; source ]
        |> Yojson.Safe.from_string
      in
      require
        (legacy |> member "schema" |> to_string = "holyc-integer-program-v1"
        && legacy |> member "final_value" |> member "value" |> to_string = "42"
        )
        "byte signatures preserve v1 reporting";
      let dump () =
        success [ "dump-ir"; "--program"; "--mode=" ^ mode; source ]
      in
      let first = dump () in
      require
        (first = dump ()
        && contains first "public:U8 = IC_CALL_END"
        && String.split_on_char '\n' first
           |> List.exists (fun line ->
               contains line "IC_RETURN_VAL" && contains line "type=public:U8")
        && contains first "holyc-initializer-preparation-v1 steps=7")
        "deterministic original U8 call and return types")
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let run ?(options = []) source =
        let status, output, errors =
          invoke_raw
            ([ "run"; "--format=json"; "--mode=" ^ mode ] @ options @ [ source ])
        in
        require (errors = "") "narrow integers use one metadata stream";
        let report = Yojson.Safe.from_string output in
        require
          (report
           |> member "implementation_commit"
           |> to_string = Holyc_lib.Version.implementation_commit
          && report |> member "reference_commit" |> to_string
             = Holyc_lib.Version.reference_commit)
          "narrow reports identify the exact build and reference";
        (status, report)
      in
      let check report capture =
        require
          (report |> member "schema" |> to_string = "holyc-integer-program-v2"
          && report |> member "outcome" |> to_string = "success"
          && report |> member "final_value" |> member "value" |> to_string
             = "42"
          && report |> member "final_value" |> member "type" |> to_string
             = "i64"
          && report |> member "output_hex" |> to_string = capture)
          "narrow value class and capture"
      in
      let source = Sys.argv.(22) in
      let bounds =
        [
          ("step", 139, "HCIRVM0007", "3432");
          ("initializer-step", 9, "HCIRVM0007", "");
          ("global-byte", 16, "HCIRVM0016", "");
          ("frame-byte", 40, "HCIRVM0011", "");
          ("call-depth", 2, "HCIRVM0015", "");
          ("literal-byte", 3, "HCIRVM0021", "");
          ("output-byte", 2, "HCIRVM0022", "");
          ("output-work", 5, "HCIRVM0023", "");
        ]
      in
      let option name value = Printf.sprintf "--%s-limit=%d" name value in
      let status, report =
        run
          ~options:
            (List.map (fun (name, value, _, _) -> option name value) bounds)
          source
      in
      require (status = Unix.WEXITED 0) "narrow fixture at all exact limits";
      check report "3432";
      require
        (report |> member "executed_steps" |> to_int = 139
        && report |> member "compiled_initializer_steps" |> to_int = 9
        && report |> member "output_work" |> to_int = 5)
        "narrow fixture work counts";
      List.iter
        (fun (name, value, code, capture) ->
          let status, report =
            run ~options:[ option name (value - 1) ] source
          in
          require
            (status = Unix.WEXITED 1
            && report |> member "final_value" = `Null
            && report |> member "diagnostics" |> to_list |> List.hd
               |> member "code" |> to_string = code
            && report |> member "output_hex" |> to_string = capture)
            ("narrow fixture one-below " ^ name))
        bounds;
      List.iter
        (fun text ->
          with_file ".hc" text (fun path ->
              let status, report = run path in
              require (status = Unix.WEXITED 0) "original narrow source gate";
              check report ""))
        [
          "I64 F(){I8 n=42;return n;}F();";
          "I64 F(){I16 n=42;return n;}F();";
          "I64 F(){U16 n=42;return n;}F();";
          "I64 F(){I32 n=42;return n;}F();";
          "I64 F(){U32 n=42;return n;}F();";
          "I16 A[2]={40,2};I64 F(){return A[0]+A[1];}F();";
          "I64 F(I32 n){return n;}F(42);";
          "I8 F(){return 42;}F();";
        ];
      List.iter
        (fun text ->
          with_file ".hc" text (fun path ->
              let status, report = run path in
              require
                (status = Unix.WEXITED 1
                && report |> member "final_value" = `Null
                && report |> member "output_hex" |> to_string = ""
                && report |> member "output_work" |> to_int = 0
                && report |> member "diagnostics" |> to_list |> List.hd
                   |> member "code" |> to_string = "HCRUN0006")
                "signed native initializer proof boundary"))
        [
          "I64 F(){I8 n=255;return n;}I64 N=F();N;";
          "I8 A=-1;I64 N=(A&=255);N;";
          "I8 A=-128;I64 D=-1;I64 F(){I8 n=(A/=D);return n;}I64 N=F();N;";
          "I8 A=-1;U16 N=(A&=255);N;";
        ];
      let legacy =
        success [ "run"; "--format=json"; "--mode=" ^ mode; source ]
        |> Yojson.Safe.from_string
      in
      require
        (legacy |> member "schema" |> to_string = "holyc-integer-program-v1"
        && legacy |> member "final_value" |> member "value" |> to_string = "42"
        )
        "narrow integers preserve v1 reporting";
      let dump () =
        success [ "dump-ir"; "--program"; "--mode=" ^ mode; source ]
      in
      let first = dump () in
      require
        (first = dump ()
        && contains first "public:U32 = IC_CALL_END"
        && contains first "prepared-bytes:ff00"
        && contains first "dimensions=[2] strides=[2] cells=2 bytes=4"
        && contains first "holyc-initializer-preparation-v1 steps=9")
        "deterministic narrow shapes publications and signature types")
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      let run source limit =
        let status, output, errors =
          invoke_raw
            [
              "run";
              "--format=json";
              "--mode=" ^ mode;
              "--dimension-work-limit=" ^ string_of_int limit;
              "--initializer-step-limit=3";
              source;
            ]
        in
        require (errors = "") "dimension reports stay on one stream";
        (status, Yojson.Safe.from_string output)
      in
      with_file ".hc" "U8 A[1+1];I64 N=42;N;" (fun source ->
          let status, report = run source 3 in
          require
            (status = Unix.WEXITED 0
            && report |> member "dimension_work_limit" |> to_int = 3
            && report |> member "dimension_preparation_work" |> to_int = 3
            && report |> member "compiled_initializer_steps" |> to_int = 3)
            "independent exact dimension and initializer allowances";
          let status, report = run source 2 in
          require
            (status = Unix.WEXITED 1
            && report |> member "dimension_preparation_work" |> to_int = 2
            && report |> member "diagnostics" |> to_list |> List.hd
               |> member "code" |> to_string = "HCIRVM0007")
            "dimension limit preserves reached work";
          List.iter
            (fun version ->
              let status, _, _ =
                invoke_raw
                  [
                    "run";
                    "--format=json";
                    "--report-version=" ^ version;
                    "--mode=" ^ mode;
                    "--dimension-work-limit=2";
                    source;
                  ]
              in
              require (status = Unix.WEXITED 1)
                "both report versions enforce dimension limit")
            [ "1"; "2" ];
          ignore
            (success
               [
                 "dump-ir";
                 "--program";
                 "--mode=" ^ mode;
                 "--dimension-work-limit=3";
                 source;
               ]);
          let status, _, _ =
            invoke_raw
              [
                "dump-ir";
                "--program";
                "--mode=" ^ mode;
                "--dimension-work-limit=2";
                source;
              ]
          in
          require (status = Unix.WEXITED 1)
            "program dump forwards dimension allowance";
          let legacy =
            success
              [
                "run";
                "--format=json";
                "--mode=" ^ mode;
                "--dimension-work-limit=3";
                source;
              ]
            |> Yojson.Safe.from_string
          in
          require
            (legacy |> member "dimension_work_limit" = `Null
            && legacy |> member "dimension_preparation_work" = `Null)
            "v1 retains its existing field contract");
      with_file ".hc" "I64 A[1+1]=40,2;A[0]+A[1];" (fun source ->
          let status, output, errors =
            invoke_raw
              [
                "run";
                "--format=json";
                "--report-version=2";
                "--mode=" ^ mode;
                "--dimension-work-limit=3";
                source;
              ]
          in
          require
            (status = Unix.WEXITED 0 && errors = "")
            "CLI checked unbraced bound succeeds";
          let report = Yojson.Safe.from_string output in
          require
            (report |> member "final_value" |> member "value" |> to_string
             = "42"
            && report |> member "dimension_preparation_work" |> to_int = 3)
            "CLI unbraced initializer reuses checked expression count";
          let status, report = run source 2 in
          require
            (status = Unix.WEXITED 1
            && report |> member "dimension_preparation_work" |> to_int = 2
            && report |> member "diagnostics" |> to_list |> List.hd
               |> member "code" |> to_string = "HCIRVM0007")
            "unbraced initializer preserves one-below numeric limit";
          ignore
            (success
               [
                 "dump-ir";
                 "--program";
                 "--mode=" ^ mode;
                 "--dimension-work-limit=3";
                 source;
               ]));
      List.iter
        (fun (contents, limit, work, code) ->
          with_file ".hc" contents (fun source ->
              let status, report = run source limit in
              require
                (status = Unix.WEXITED 1
                && report
                   |> member "dimension_preparation_work"
                   |> to_int = work
                && report |> member "diagnostics" |> to_list |> List.hd
                   |> member "code" |> to_string = code)
                "parse and evaluation failures retain exact numeric work"))
        [ ("U8 A[2;", 1, 1, "HCPARSE0023"); ("U8 A[1/0;", 3, 3, "HCSEMA0004") ];
      List.iter
        (fun contents ->
          with_file ".hc" contents (fun source ->
              let status, report = run source 1 in
              require
                (status = Unix.WEXITED 1
                && report |> member "dimension_preparation_work" |> to_int = 1)
                "later semantic and runtime failures retain dimension work"))
        [ "U8 A[2];Missing;"; "U8 A[2];1/0;" ];
      with_file ".hc" "#exe {42;}" (fun source ->
          let status, report = run source 0 in
          require
            (status = Unix.WEXITED 1
            && report |> member "dimension_preparation_work" |> to_int = 0
            && report |> member "command_error" |> member "code" |> to_string
               = "HCIRVM0001")
            "invalid dimension allowance rejects before parsing"))
    [ "jit"; "aot" ];
  print_endline "Integer program CLI checks passed."
