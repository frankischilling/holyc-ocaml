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
  print_endline "Integer program CLI checks passed."
