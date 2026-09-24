open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc internal strlen cli " suffix in
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
    "usage: test_internal_strlen_cli.exe <holyc.exe> <internal-strlen.hc>"

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

let json_path ?(status = 0) ?(options = []) ~target ~mode path =
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
  let actual, stdout, stderr = invoke arguments in
  require
    (actual = Unix.WEXITED status)
    (Printf.sprintf "%s: expected exit %d\nstdout: %s\nstderr: %s"
       (String.concat " " arguments)
       status stdout stderr);
  require (stderr = "") ("JSON report wrote stderr: " ^ stderr);
  Yojson.Safe.from_string stdout

let check_error expected report =
  require (member "outcome" report = `String "error") "expected error outcome";
  require (member "final_value" report = `Null) "fault retained a final value";
  match report |> member "diagnostics" |> to_list with
  | first :: _ ->
      require
        (first |> member "code" |> to_string = expected)
        ("expected diagnostic " ^ expected)
  | [] -> failwith "error report has no diagnostic"

let check_output report hex bytes work =
  require (report |> member "output_hex" |> to_string = hex) "output bytes";
  require
    (report |> member "output_byte_length" |> to_int = bytes)
    "output length";
  require (report |> member "output_work" |> to_int = work) "output work"

let maintained_example ~target ~mode =
  let exact =
    json_path ~target ~mode
      ~options:
        [ "--step-limit=32"; "--output-byte-limit=1"; "--output-work-limit=1" ]
      maintained_fixture
  in
  require (member "outcome" exact = `String "success") "maintained outcome";
  require (member "executed_steps" exact = `Int 32) "maintained runtime work";
  require
    (exact |> member "final_value" |> member "bits" |> to_string
   = "0x000000000000002a")
    "maintained I64 42 result";
  require
    (member "conditional_recovery" exact = `String "hosted-strict")
    "ordinary source policy";
  require
    (member "arithmetic" exact
    = `String (if target = "ir" then "runtime-ir" else "runtime-native"))
    "execution engine marker";
  check_output exact "" 0 0;
  let below =
    json_path ~status:1 ~target ~mode ~options:[ "--step-limit=31" ]
      maintained_fixture
  in
  check_error "HCIRVM0007" below;
  require (member "executed_steps" below = `Int 31) "one-below runtime work";
  check_output below "" 0 0

let fault_prefix ~target ~mode =
  let declarations =
    "_intern 0x84 I64 StrLen(U8 *s);extern U0 Print(U8 *fmt,...);U8 G[1]={65};"
  in
  let prefix = declarations ^ "Print(\"kept\");" in
  with_file ".hc" (prefix ^ "42;") (fun control_path ->
      let control = json_path ~target ~mode control_path in
      let work = control |> member "output_work" |> to_int in
      with_file ".hc" (prefix ^ "StrLen(G);") (fun fault_path ->
          let bounds = json_path ~status:1 ~target ~mode fault_path in
          check_error "HCIRVM0019" bounds;
          check_output bounds "6b657074" 4 work;
          let steps = bounds |> member "executed_steps" |> to_int in
          let budget =
            json_path ~status:1 ~target ~mode
              ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
              fault_path
          in
          check_error "HCIRVM0007" budget;
          require
            (member "executed_steps" budget = `Int (steps - 1))
            "scan budget precedes an unreached bounds probe";
          check_output budget "6b657074" 4 work))

let () =
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          maintained_example ~target ~mode;
          fault_prefix ~target ~mode)
        [ "jit"; "aot" ])
    [ "ir"; "host-jit" ]
