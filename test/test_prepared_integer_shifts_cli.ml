open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let with_file suffix contents action =
  let path = Filename.temp_file "holyc prepared shift " suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let invoke ?(target = "ir") ?(steps = 100_000) ?(preparation = 100_000) ~status
    compiler mode source =
  with_file ".hc" source (fun path ->
      with_file ".stdout" "" (fun stdout ->
          with_file ".stderr" "" (fun stderr ->
              let out_fd =
                Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              and err_fd =
                Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let arguments =
                [|
                  compiler;
                  "run";
                  "--format=json";
                  "--report-version=2";
                  "--mode=" ^ mode;
                  "--target=" ^ target;
                  "--step-limit=" ^ string_of_int steps;
                  "--initializer-step-limit=" ^ string_of_int preparation;
                  path;
                |]
              in
              let pid =
                Fun.protect
                  ~finally:(fun () ->
                    Unix.close out_fd;
                    Unix.close err_fd)
                  (fun () ->
                    Unix.create_process compiler arguments Unix.stdin out_fd
                      err_fd)
              in
              let _, result = Unix.waitpid [] pid in
              let read path =
                let channel = open_in_bin path in
                Fun.protect
                  ~finally:(fun () -> close_in channel)
                  (fun () ->
                    really_input_string channel (in_channel_length channel))
              in
              let text = read stdout in
              require
                (result = Unix.WEXITED status)
                ("unexpected exit: " ^ text ^ read stderr);
              require (read stderr = "") "JSON CLI wrote to stderr";
              Yojson.Safe.from_string text)))

let error report code =
  require (member "outcome" report = `String "error") "expected error outcome";
  require (member "final_value" report = `Null) "fault exposes a final value";
  match member "diagnostics" report |> to_list with
  | first :: _ -> require (member "code" first = `String code) "fault code"
  | [] -> failwith "fault has no diagnostic"

let () =
  require (Array.length Sys.argv = 4) "expected compiler, fixture and example";
  let compiler = Sys.argv.(1)
  and fixture = Yojson.Safe.from_file Sys.argv.(2) in
  require
    (member "id" fixture = `String "arithmetic/prepared-integer-shifts-001")
    "wrong native fixture";
  let projections = member "hosted_value_projections" fixture |> to_list in
  require (List.length projections = 19) "all native fields required";
  let observed projection field =
    member "checks" fixture |> to_list
    |> List.find (fun check -> member "id" check = member field projection)
    |> member "observed_fields"
    |> member (member "field" projection |> to_string)
    |> to_string
  in
  let channel = open_in_bin Sys.argv.(3) in
  let example =
    Fun.protect
      ~finally:(fun () -> close_in channel)
      (fun () -> really_input_string channel (in_channel_length channel))
  in
  List.iter
    (fun mode ->
      List.iter
        (fun projection ->
          let bits = observed projection "case_id" in
          require
            (bits = observed projection "repeat_case_id")
            "native repeat differs";
          let report =
            invoke ~status:0 compiler mode
              (member "holy_c_source" projection |> to_string)
          in
          require
            (member "outcome" report = `String "success")
            "preparation outcome";
          require
            (member "final_value" report
            |> member "bits"
            = `String ("0x" ^ String.lowercase_ascii bits))
            "saved native bits";
          require
            (member "output_byte_length" report = `Int 0)
            "unexpected replay output")
        projections;
      let report =
        invoke ~steps:199 ~preparation:7 ~status:0 compiler mode example
      in
      require
        (member "final_value" report |> member "value" = `String "42")
        "maintained example value";
      require (member "executed_steps" report = `Int 199) "runtime work";
      require
        (member "compiled_initializer_steps" report = `Int 7)
        "preparation work";
      require
        (member "output_hex" report = `String "")
        "once-only source assertions";
      require (member "output_work" report = `Int 7) "formatting work";
      error (invoke ~steps:198 ~status:1 compiler mode example) "HCIRVM0007";
      error (invoke ~preparation:6 ~status:1 compiler mode example) "HCIRVM0007";
      error
        (invoke ~target:"host-jit" ~status:1 compiler mode example)
        "HCPP0008";
      let fault = member "hosted_fault_projection" fixture in
      error
        (invoke ~status:1 compiler mode
           (member "holy_c_source" fault |> to_string))
        (member "hosted_code" fault |> to_string))
    [ "jit"; "aot" ];
  print_endline
    "38 prepared source projections and retained CLI work/fault boundaries \
     passed."
