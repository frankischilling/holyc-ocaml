open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc native output cli " suffix in
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
    "usage: test_native_output_cli.exe <holyc.exe> <integer-putchars.hc>"

let compiler = Sys.argv.(1)
let maintained_fixture = Sys.argv.(2)

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

let run_file ?(status = 0) ?(mode = "jit") ?(format = "json") ?(options = [])
    contents =
  with_file ".hc" contents (fun source ->
      let arguments =
        [
          "run";
          "--target=host-jit";
          "--report-version=2";
          "--format=" ^ format;
          "--mode=" ^ mode;
        ]
        @ options @ [ source ]
      in
      let actual, stdout, stderr = invoke arguments in
      require
        (actual = Unix.WEXITED status)
        (Printf.sprintf "%s: expected exit %d, got %s\nstdout: %s\nstderr: %s"
           (String.concat " " arguments)
           status (status_name actual) stdout stderr);
      (stdout, stderr))

let json ?status ?mode ?options contents =
  let stdout, stderr = run_file ?status ?mode ?options contents in
  require (stderr = "") ("JSON report wrote stderr: " ^ stderr);
  Yojson.Safe.from_string stdout

let json_path ?(status = 0) ?(mode = "jit") ?(options = []) source =
  let arguments =
    [
      "run";
      "--target=host-jit";
      "--report-version=2";
      "--format=json";
      "--mode=" ^ mode;
    ]
    @ options @ [ source ]
  in
  let actual, stdout, stderr = invoke arguments in
  require
    (actual = Unix.WEXITED status)
    (Printf.sprintf "%s: expected exit %d, got %s\nstdout: %s\nstderr: %s"
       (String.concat " " arguments)
       status (status_name actual) stdout stderr);
  require (stderr = "") ("JSON report wrote stderr: " ^ stderr);
  Yojson.Safe.from_string stdout

let diagnostics report = report |> member "diagnostics" |> to_list

let error_code report =
  match diagnostics report with
  | first :: _ -> first |> member "code" |> to_string
  | [] -> (
      match member "command_error" report with
      | `Assoc _ as error -> error |> member "code" |> to_string
      | _ -> failwith "expected a diagnostic or command error")

let final_bits report =
  report |> member "final_value" |> member "bits" |> to_string

let check_output label report ~hex ~bytes ~work =
  require
    (report |> member "output_hex" |> to_string = hex)
    (label ^ " output hex");
  require
    (report |> member "output_byte_length" |> to_int = bytes)
    (label ^ " output byte length");
  require
    (report |> member "output_work" |> to_int = work)
    (label ^ " output work")

let maintained_baseline () =
  let fixture_contents = read maintained_fixture in
  require
    (String.length fixture_contents > 0)
    "maintained PutChars fixture must not be empty";
  let implicit = "extern U0 PutChars(U64 ch);'42\\n';42;" in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, report) ->
          require
            (report |> member "outcome" |> to_string = "success")
            (label ^ " outcome");
          require
            (report |> member "arithmetic" |> to_string = "runtime-native")
            (label ^ " native arithmetic marker");
          require
            (report |> member "executed_steps" |> to_int = 9)
            (label ^ " exact nine IR steps");
          require
            (final_bits report = "0x000000000000002a")
            (label ^ " final value");
          check_output label report ~hex:"34320a" ~bytes:3 ~work:6)
        [
          ("maintained explicit fixture", json_path ~mode maintained_fixture);
          ("implicit", json ~mode implicit);
        ])
    [ "jit"; "aot" ]

let independent_limits () =
  let source = "extern U0 PutChars(U64 ch);PutChars('42\\n');42;" in
  List.iter
    (fun mode ->
      let exact =
        json ~mode
          ~options:
            [
              "--step-limit=9"; "--output-byte-limit=3"; "--output-work-limit=6";
            ]
          source
      in
      check_output "exact limits" exact ~hex:"34320a" ~bytes:3 ~work:6;
      let bytes =
        json ~status:1 ~mode
          ~options:[ "--output-byte-limit=2"; "--output-work-limit=6" ]
          source
      in
      require (error_code bytes = "HCIRVM0022") "byte one-below diagnostic";
      check_output "byte one-below" bytes ~hex:"3432" ~bytes:2 ~work:6;
      let work =
        json ~status:1 ~mode
          ~options:[ "--output-byte-limit=3"; "--output-work-limit=5" ]
          source
      in
      require (error_code work = "HCIRVM0023") "work one-below diagnostic";
      check_output "work one-below" work ~hex:"3432" ~bytes:2 ~work:5;
      let steps = json ~status:1 ~mode ~options:[ "--step-limit=8" ] source in
      require (error_code steps = "HCIRVM0007") "step one-below diagnostic";
      check_output "step one-below" steps ~hex:"34320a" ~bytes:3 ~work:6)
    [ "jit"; "aot" ]

let native_fault_prefix () =
  let source = "extern U0 PutChars(U64 ch);PutChars('A');PutChars(1/0);42;" in
  List.iter
    (fun mode ->
      let report = json ~status:1 ~mode source in
      require (error_code report = "HCIRVM0009") "argument fault diagnostic";
      check_output "argument fault" report ~hex:"41" ~bytes:1 ~work:2)
    [ "jit"; "aot" ]

let source_defined_putchars () =
  let source = "I64 G=0;U0 PutChars(U64 ch){G=ch;}PutChars(42);G;" in
  List.iter
    (fun mode ->
      let report = json ~mode source in
      require
        (report |> member "outcome" |> to_string = "success")
        "source-defined PutChars outcome";
      check_output "source-defined PutChars" report ~hex:"" ~bytes:0 ~work:0;
      require
        (final_bits report = "0x000000000000002a")
        "source-defined PutChars final value")
    [ "jit"; "aot" ]

let invalid_limits_precede_source () =
  let source = "I64 G=1/0;42;" in
  List.iter
    (fun mode ->
      List.iter
        (fun option ->
          let report = json ~status:1 ~mode ~options:[ option ] source in
          require
            (error_code report = "HCIRVM0001")
            (option ^ " configuration diagnostic");
          require
            (report |> member "compiled_initializer_steps" |> to_int = 0)
            (option ^ " precedes initializer preparation");
          require
            (member "executed_steps" report = `Null)
            (option ^ " precedes native execution");
          check_output option report ~hex:"" ~bytes:0 ~work:0)
        [
          "--output-byte-limit=0";
          "--output-work-limit=0";
          "--output-byte-limit="
          ^ string_of_int
              (Holyc_lib.Native_program_execution.hard_max_output_bytes + 1);
        ])
    [ "jit"; "aot" ]

let human_output () =
  let source = "extern U0 PutChars(U64 ch);PutChars('42\\n');42;" in
  let stdout, stderr = run_file ~format:"human" source in
  require (stderr = "") ("human report wrote stderr: " ^ stderr);
  let lines = stdout |> String.split_on_char '\n' |> List.map String.trim in
  List.iter
    (fun expected ->
      require (List.mem expected lines)
        ("missing human report line: " ^ expected))
    [ "output-byte-length=3"; "output-work=6"; "output-hex=34320a" ]

let () =
  maintained_baseline ();
  independent_limits ();
  native_fault_prefix ();
  source_defined_putchars ();
  invalid_limits_precede_source ();
  human_output ()
