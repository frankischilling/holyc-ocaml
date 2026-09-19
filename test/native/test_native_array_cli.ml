open Yojson.Safe.Util
module Runtime = Holyc_lib.Native_program_execution

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc native array cli " suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let () =
  require
    (Array.length Sys.argv = 3)
    "usage: test_native_array_cli.exe <holyc.exe> <arrays.hc>"

let compiler = Sys.argv.(1)
let array_fixture = Sys.argv.(2)

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

let status_name = function
  | Unix.WEXITED code -> Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal

let checked_json expected arguments =
  let status, stdout, stderr = invoke arguments in
  require
    (status = Unix.WEXITED expected)
    (Printf.sprintf "%s: expected exit %d, got %s\nstdout: %s\nstderr: %s"
       (String.concat " " arguments)
       expected (status_name status) stdout stderr);
  require (stderr = "") ("JSON report escaped diagnostics to stderr: " ^ stderr);
  Yojson.Safe.from_string stdout

let host_json ?(status = 0) ?(options = []) ~mode source =
  let report =
    checked_json status
      ([ "run"; "--target=host-jit"; "--format=json"; "--mode=" ^ mode ]
      @ options @ [ source ])
  in
  require
    (report |> member "schema" |> to_string = "holyc-integer-program-v2"
    && report |> member "target" |> to_string = "host-jit"
    && report |> member "mode" |> to_string = mode
    && report |> member "arithmetic" |> to_string = "runtime-native")
    "host-jit array report identity";
  let expected_platform =
    match Runtime.platform () with
    | Runtime.Windows_x86_64 -> "windows-x86_64"
    | Runtime.Linux_x86_64 -> "linux-x86_64"
    | Runtime.Unsupported -> failwith "native array CLI tests require x86-64"
  in
  require
    (report |> member "native" |> member "platform" |> to_string
   = expected_platform)
    "host-jit array report platform";
  report

let ir_json ?(status = 0) ?(options = []) ~mode source =
  checked_json status
    ([
       "run";
       "--target=ir";
       "--report-version=2";
       "--format=json";
       "--mode=" ^ mode;
     ]
    @ options @ [ source ])

let diagnostics report = report |> member "diagnostics" |> to_list

let check_success report =
  require
    (report |> member "outcome" |> to_string = "success"
    && report |> member "termination" |> to_string = "stream-end"
    && diagnostics report = []
    && member "command_error" report = `Null)
    "successful array report"

let check_word report type_ value bits =
  let word = member "final_value" report in
  require
    (word |> member "type" |> to_string = type_
    && word |> member "value" |> to_string = value
    && word |> member "bits" |> to_string = bits)
    ("unexpected array final word: " ^ Yojson.Safe.to_string word)

let () =
  List.iter
    (fun mode ->
      let report = host_json ~mode array_fixture in
      check_success report;
      check_word report "i64" "42" "0x000000000000002a";
      check_word (ir_json ~mode array_fixture) "i64" "42" "0x000000000000002a";
      let work = report |> member "dimension_preparation_work" |> to_int in
      require (work = 2) "two original dimension expressions";
      require
        (report |> member "compiled_initializer_steps" |> to_int = 0)
        "dimension work stays separate from initializers";
      check_success
        (host_json ~mode ~options:[ "--dimension-work-limit=2" ] array_fixture);
      let failed =
        host_json ~status:1 ~mode
          ~options:[ "--dimension-work-limit=1" ]
          array_fixture
      in
      require
        (failed |> member "dimension_preparation_work" |> to_int = 1)
        "failed dimension work is retained";
      require
        (member "executed_steps" failed = `Null)
        "dimension exhaustion precedes native entry";
      let status, stdout, stderr =
        invoke [ "run"; "--target=host-jit"; "--mode=" ^ mode; array_fixture ]
      in
      require (status = Unix.WEXITED 0 && stderr = "") "human array report";
      require
        (String.split_on_char '\n' stdout
        |> List.exists (fun line ->
            String.trim line = "dimension-preparation-work=2"))
        "human dimension count";
      with_file ".hc" "I64 F(){I8 a[1+2][7;return 42;}F();" (fun path ->
          let failed = host_json ~status:1 ~mode path in
          require
            (failed |> member "dimension_preparation_work" |> to_int = 4)
            "closing bracket failure retains original preparation";
          require
            (member "executed_steps" failed = `Null)
            "parse failure has no entry"))
    [ "jit"; "aot" ]
