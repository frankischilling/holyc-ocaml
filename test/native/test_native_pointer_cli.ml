open Yojson.Safe.Util
module Runtime = Holyc_lib.Native_program_execution

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc native pointer cli " suffix in
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
    "usage: test_native_pointer_cli.exe <holyc.exe> <pointers.hc>"

let compiler = Sys.argv.(1)
let pointer_fixture = Sys.argv.(2)

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
    "host-jit pointer report identity";
  let expected_platform =
    match Runtime.platform () with
    | Runtime.Windows_x86_64 -> "windows-x86_64"
    | Runtime.Linux_x86_64 -> "linux-x86_64"
    | Runtime.Unsupported -> failwith "native pointer CLI tests require x86-64"
  in
  require
    (report |> member "native" |> member "platform" |> to_string
   = expected_platform)
    "host-jit pointer report platform";
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

let first_diagnostic report =
  match diagnostics report with
  | first :: _ -> first
  | [] -> failwith "expected a pointer diagnostic"

let check_success report =
  require
    (report |> member "outcome" |> to_string = "success"
    && report |> member "termination" |> to_string = "stream-end"
    && diagnostics report = []
    && member "command_error" report = `Null)
    "successful pointer report"

let check_word report type_ value bits =
  let word = member "final_value" report in
  require
    (word |> member "type" |> to_string = type_
    && word |> member "value" |> to_string = value
    && word |> member "bits" |> to_string = bits)
    ("unexpected pointer final word: " ^ Yojson.Safe.to_string word)

let () =
  List.iter
    (fun mode ->
      let report = host_json ~mode pointer_fixture in
      check_success report;
      check_word report "i64" "42" "0x000000000000002a";
      check_word (ir_json ~mode pointer_fixture) "i64" "42" "0x000000000000002a";
      require
        (report |> member "compiled_initializer_steps" |> to_int = 9)
        "shared pointer preparation budget";
      let failed =
        host_json ~status:1 ~mode
          ~options:[ "--initializer-step-limit=8" ]
          pointer_fixture
      in
      require
        (member "executed_steps" failed = `Null)
        "pointer fixture preparation must precede entry";
      let globals =
        report |> member "native" |> member "image" |> member "global_bytes"
        |> to_int
      in
      check_success
        (host_json ~mode
           ~options:[ "--global-byte-limit=" ^ string_of_int globals ]
           pointer_fixture);
      let failed =
        host_json ~status:1 ~mode
          ~options:[ "--global-byte-limit=" ^ string_of_int (globals - 1) ]
          pointer_fixture
      in
      require
        (member "executed_steps" failed = `Null)
        "references do not weaken global quota";
      List.iter
        (fun source ->
          with_file ".hc" source (fun path ->
              let native = host_json ~status:1 ~mode path in
              let interpreted = ir_json ~status:1 ~mode path in
              require
                (first_diagnostic native |> member "code"
                = (first_diagnostic interpreted |> member "code"))
                "pointer fault code";
              require
                (member "executed_steps" native
                = member "executed_steps" interpreted)
                "pointer fault steps"))
        [
          "I64 F(){I64 *p;return *p;}F();";
          "I64 F(){I64 x;I64 *p=&x;return *p;}F();";
        ];
      List.iter
        (fun source ->
          with_file ".hc" source (fun path ->
              let report = host_json ~status:1 ~mode path in
              require
                (member "executed_steps" report = `Null)
                "reference fabrication reached entry"))
        [
          "I64 F(I64 *p){return *p;}F(42);";
          "I64 F(){I64 x=42;I64 *p=&x;return p;}F();";
          "I64 *F(){I64 x=42;return &x;}42;";
          "I64 F(){I64 x=42;I64 *p=&x;return *(p+1);}F();";
        ];
      with_file ".hc" "I64 G=42;40;&G;" (fun path ->
          let report = host_json ~mode path in
          check_success report;
          require
            (member "final_value" report = `Null)
            "reference discard exposed native bits"))
    [ "jit"; "aot" ]
