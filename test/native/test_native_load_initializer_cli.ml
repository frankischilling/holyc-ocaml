open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc load initializer cli " suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let () =
  require (Array.length Sys.argv = 3) "expected compiler and load example"

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)

let invoke ?(mode = "aot") ?(target = "host-jit") ?(status = 0) ?(options = [])
    source =
  let arguments =
    [
      compiler;
      "run";
      "--report-version=2";
      "--format=json";
      "--mode=" ^ mode;
      "--target=" ^ target;
    ]
    @ options @ [ source ]
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
                Unix.create_process compiler (Array.of_list arguments)
                  Unix.stdin out_fd err_fd)
          in
          let _, actual = Unix.waitpid [] pid in
          let text = read stdout in
          require
            (actual = Unix.WEXITED status)
            ("unexpected exit: " ^ text ^ read stderr);
          require (read stderr = "") "JSON diagnostics escaped to stderr";
          let report = Yojson.Safe.from_string text in
          require
            (member "schema" report = `String "holyc-integer-program-v2"
            && member "mode" report = `String mode
            && member "target" report = `String target)
            "load report identity";
          if target = "host-jit" then
            require
              (member "arithmetic" report = `String "runtime-native")
              "native load execution identity";
          report))

let success report =
  require
    (member "outcome" report = `String "success"
    && member "diagnostics" report = `List [])
    "load success";
  require
    (member "final_value" report |> member "value" = `String "42")
    "load result";
  require (member "output_hex" report = `String "41") "load output"

let error report code output =
  require (member "outcome" report = `String "error") "load error";
  require (member "final_value" report = `Null) "load fault final value";
  let first = member "diagnostics" report |> to_list |> List.hd in
  require
    (member "code" first = `String code)
    ("unexpected load fault: " ^ Yojson.Safe.to_string report);
  require (member "output_hex" report = `String output) "load fault output"

let () =
  let ir = invoke ~target:"ir" example in
  let native = invoke example in
  success ir;
  success native;
  List.iter
    (fun field ->
      require
        (member field ir = member field native)
        ("fresh public IR/native load agreement: " ^ field))
    [
      "final_value";
      "executed_steps";
      "compiled_initializer_steps";
      "output_hex";
      "dimension_preparation_work";
    ];
  require
    (member "compiled_initializer_steps" native = `Int 3)
    "only the saved default consumes closed preparation work";
  require (member "prepared_default_bytes" native = `Int 8) "saved default word";
  let steps = member "executed_steps" native |> to_int in
  let image = member "native" native |> member "image" in
  require (member "global_bytes" image = `Int 24) "full callback global words";
  let quota field option =
    "--" ^ option ^ "=" ^ string_of_int (member field image |> to_int)
  in
  let exact =
    [
      "--initializer-step-limit=3";
      "--default-byte-limit=8";
      "--global-byte-limit=24";
      "--frame-byte-limit=16";
      "--call-depth-limit=2";
      "--step-limit=" ^ string_of_int steps;
      quota "ir_instructions" "ir-instruction-limit";
      quota "code_bytes" "code-byte-limit";
      quota "block_count" "block-limit";
    ]
  in
  for _ = 1 to 3 do
    success (invoke ~options:exact example)
  done;
  List.iter
    (fun (option, code, output) ->
      error (invoke ~status:1 ~options:[ option ] example) code output;
      success (invoke ~options:exact example))
    [
      ("--initializer-step-limit=2", "HCIRVM0007", "");
      ("--default-byte-limit=7", "HCIRVM0011", "");
      ("--global-byte-limit=23", "HCBACK0001", "");
      ("--frame-byte-limit=15", "HCIRVM0011", "");
      ("--call-depth-limit=1", "HCIRVM0015", "");
      ("--step-limit=" ^ string_of_int (steps - 1), "HCIRVM0007", "41");
      ( "--code-byte-limit="
        ^ string_of_int (member "code_bytes" image |> to_int |> pred),
        "HCBACK0005",
        "" );
      ( "--ir-instruction-limit="
        ^ string_of_int (member "ir_instructions" image |> to_int |> pred),
        "HCBACK0001",
        "" );
      ( "--block-limit="
        ^ string_of_int (member "block_count" image |> to_int |> pred),
        "HCBACK0001",
        "" );
    ];
  let jit = invoke ~mode:"jit" ~status:1 example in
  error jit "HCRUN0006" "";
  require
    (member "executed_steps" jit = `Null)
    "JIT rejection precedes execution";
  List.iter
    (fun initial ->
      with_file ".hc"
        ("extern U0 Print(U8 *fmt,...);I64 Side(){Print(\"A\");return 40;}I64 \
          (*P)(I64 n)=" ^ initial ^ ";I64 N=P(Side());N;")
        (fun path ->
          let native = invoke ~status:1 path
          and ir = invoke ~target:"ir" ~status:1 path in
          error native "HCIRVM0024" "41";
          error ir "HCIRVM0024" "41";
          require
            (member "executed_steps" native = member "executed_steps" ir)
            "reached load fault runtime work";
          success (invoke example)))
    [ "0"; "17"; "0xffffffffffffffff" ]
