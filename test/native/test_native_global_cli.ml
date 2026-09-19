open Yojson.Safe.Util
module Runtime = Holyc_lib.Native_program_execution

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc native global cli " suffix in
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
    (Array.length Sys.argv = 5)
    "usage: test_native_global_cli.exe <holyc.exe> <globals.hc> \
     <initializers.hc>"

let compiler = Sys.argv.(1)
let global_fixture = Sys.argv.(2)

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
    "host-jit global report identity";
  let expected_platform =
    match Runtime.platform () with
    | Runtime.Windows_x86_64 -> "windows-x86_64"
    | Runtime.Linux_x86_64 -> "linux-x86_64"
    | Runtime.Unsupported -> failwith "native global CLI tests require x86-64"
  in
  require
    (report |> member "native" |> member "platform" |> to_string
   = expected_platform)
    "host-jit global report platform";
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
  | [] -> failwith "expected a global diagnostic"

let check_success report =
  require
    (report |> member "outcome" |> to_string = "success"
    && report |> member "termination" |> to_string = "stream-end"
    && diagnostics report = []
    && member "command_error" report = `Null)
    "successful global report"

let check_word report type_ value bits =
  let word = member "final_value" report in
  require
    (word |> member "type" |> to_string = type_
    && word |> member "value" |> to_string = value
    && word |> member "bits" |> to_string = bits)
    ("unexpected global final word: " ^ Yojson.Safe.to_string word)

let () =
  List.iter
    (fun mode ->
      let statics = host_json ~mode Sys.argv.(4) in
      check_success statics;
      check_word statics "i64" "42" "0x000000000000002a";
      check_word (ir_json ~mode Sys.argv.(4)) "i64" "42" "0x000000000000002a";
      let static_image = statics |> member "native" |> member "image" in
      require
        (static_image |> member "global_bytes" |> to_int = 9)
        "static padded bytes";
      require
        (static_image |> member "global_arena_bytes" |> to_int = 11)
        "static arena flags";
      check_success
        (host_json ~mode ~options:[ "--global-byte-limit=9" ] Sys.argv.(4));
      let below_static =
        host_json ~status:1 ~mode
          ~options:[ "--global-byte-limit=8" ]
          Sys.argv.(4)
      in
      require
        (member "executed_steps" below_static = `Null)
        "static quota before entry";
      let prepared = host_json ~mode Sys.argv.(3) in
      check_success prepared;
      check_word prepared "i64" "42" "0x000000000000002a";
      check_word (ir_json ~mode Sys.argv.(3)) "i64" "42" "0x000000000000002a";
      require
        (prepared |> member "compiled_initializer_steps" |> to_int = 13)
        "combined initializer/default work";
      require
        (prepared |> member "prepared_default_bytes" |> to_int = 8)
        "only defaults count toward saved payloads";
      check_success
        (host_json ~mode
           ~options:
             [
               "--initializer-step-limit=13";
               "--default-byte-limit=8";
               "--global-byte-limit=9";
             ]
           Sys.argv.(3));
      let limited =
        host_json ~status:1 ~mode
          ~options:[ "--initializer-step-limit=12" ]
          Sys.argv.(3)
      in
      require
        (member "executed_steps" limited = `Null)
        "preparation failure reached native entry";
      require
        (first_diagnostic limited |> member "code" |> to_string = "HCIRVM0007")
        "shared preparation quota diagnostic";
      let native = host_json ~mode global_fixture in
      check_success native;
      check_word native "i64" "42" "0x000000000000002a";
      let interpreted = ir_json ~mode global_fixture in
      check_success interpreted;
      check_word interpreted "i64" "42" "0x000000000000002a";
      let image = native |> member "native" |> member "image" in
      require (image |> member "global_bytes" |> to_int = 9) "declared widths";
      require
        (image |> member "global_arena_bytes" |> to_int = 11)
        "flags charged separately";
      ignore
        (host_json ~mode ~options:[ "--global-byte-limit=9" ] global_fixture);
      let below =
        host_json ~status:1 ~mode
          ~options:[ "--global-byte-limit=8" ]
          global_fixture
      in
      require (member "executed_steps" below = `Null) "quota must precede entry";
      with_file ".hc" "I8 G;G;" (fun source ->
          let status = if mode = "jit" then 1 else 0 in
          let report = host_json ~status ~mode source in
          if mode = "aot" then check_word report "i64" "0" "0x0000000000000000"
          else
            require
              (first_diagnostic report |> member "code" |> to_string
             = "HCIRVM0012")
              "unknown read code");
      List.iter
        (fun contents ->
          with_file ".hc" contents (fun source ->
              let report = host_json ~status:1 ~mode source in
              require
                (member "executed_steps" report = `Null)
                "unsupported storage reached entry";
              require
                (diagnostics report <> [])
                "unsupported storage has no diagnostics"))
        [
          "I64 F(){return 42;}I64 G=F();G;";
          "I64 G[1];42;";
          "I64 *G;42;";
          "extern I64 G;42;";
          "I64 F(){static I64 G=1;return 42;}F();";
          "F64 G;42;";
          "I64 G;I64 F(I64 x=G){return x;}F();";
        ])
    [ "jit"; "aot" ]
