open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc data defaults " suffix in
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
    (Array.length Sys.argv = 3 || Array.length Sys.argv = 4)
    "expected compiler and data-default example, with optional --native"

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)
let native = Array.length Sys.argv = 4 && Sys.argv.(3) = "--native"

let invoke ?(status = 0) ?(options = []) target mode path =
  let args =
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
                  (Array.of_list (compiler :: args))
                  Unix.stdin out_fd err_fd)
          in
          let _, actual = Unix.waitpid [] pid in
          let output = read stdout in
          require
            (actual = Unix.WEXITED status)
            ("unexpected CLI exit: " ^ output ^ read stderr);
          require (read stderr = "") "JSON diagnostics escaped to stderr";
          Yojson.Safe.from_string output))

let word bits report =
  require
    (member "outcome" report = `String "success")
    "successful data-default report";
  require
    (member "diagnostics" report = `List [])
    "successful data-default diagnostics";
  require
    (member "command_error" report = `Null)
    "successful data-default command";
  require
    (report |> member "final_value" |> member "value" = `String bits)
    ("unexpected final data-default word: " ^ Yojson.Safe.to_string report)

let error code report =
  require
    (member "outcome" report = `String "error")
    "failed data-default report";
  let first = report |> member "diagnostics" |> to_list |> List.hd in
  require
    (member "code" first = `String code)
    ("unexpected data-default error: " ^ Yojson.Safe.to_string report)

let () =
  List.iter
    (fun target ->
      let baseline = invoke target "jit" example in
      word "42" baseline;
      require
        (member "output_hex" baseline = `String "4142")
        "saved string output";
      let steps = member "executed_steps" baseline |> to_int in
      let preparation =
        member "compiled_initializer_steps" baseline |> to_int
      in
      word "42"
        (invoke
           ~options:
             [
               Printf.sprintf "--step-limit=%d" steps;
               Printf.sprintf "--initializer-step-limit=%d" preparation;
               "--output-byte-limit=2";
               "--output-work-limit=8";
             ]
           target "jit" example);
      error "HCIRVM0007"
        (invoke ~status:1
           ~options:[ Printf.sprintf "--step-limit=%d" (steps - 1) ]
           target "jit" example);
      error "HCIRVM0007"
        (invoke ~status:1
           ~options:
             [ Printf.sprintf "--initializer-step-limit=%d" (preparation - 1) ]
           target "jit" example);
      if target = "host-jit-task" then (
        require
          (member "prepared_default_bytes" baseline = `Int 16)
          "two logical saved pointers";
        word "42"
          (invoke ~options:[ "--default-byte-limit=16" ] target "jit" example);
        error "HCIRVM0011"
          (invoke ~status:1
             ~options:[ "--default-byte-limit=15" ]
             target "jit" example);
        let fragments =
          baseline |> member "native" |> member "fragments" |> to_list
        in
        let defaults =
          List.filter
            (fun fragment -> member "kind" fragment = `String "default")
            fragments
        in
        require
          (List.length defaults = 2)
          "two original native default fragments";
        List.iter
          (fun fragment ->
            require
              (member "outcome" fragment = `String "success")
              "default executes natively")
          defaults))
    (if native then [ "ir"; "host-jit-task" ] else [ "ir" ]);
  error "HCRUN0006" (invoke ~status:1 "ir" "aot" example)
