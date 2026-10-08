open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-stream-generation-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      output_string channel contents;
      close_out channel;
      action path)

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)
let declarations_example = Sys.argv.(3)
let native_only = Array.exists (( = ) "--native") Sys.argv
let targets = if native_only then [ "host-jit-task" ] else [ "ir" ]
let count = ref 0

let invoke ?(status = 0) ?(mode = "jit") ?(options = []) target path =
  temporary ".out" "" (fun output ->
      temporary ".err" "" (fun error ->
          let out_fd =
            Unix.openfile output [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let err_fd =
            Unix.openfile error [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let args =
            [
              compiler;
              "run";
              "--format=json";
              "--mode=" ^ mode;
              "--target=" ^ target;
              "--code-byte-limit=524288";
            ]
            @ options @ [ path ]
          in
          let pid =
            Fun.protect
              ~finally:(fun () ->
                Unix.close out_fd;
                Unix.close err_fd)
              (fun () ->
                Unix.create_process compiler (Array.of_list args) Unix.stdin
                  out_fd err_fd)
          in
          let _, reached = Unix.waitpid [] pid in
          incr count;
          let text = read output in
          require (reached = Unix.WEXITED status) (text ^ read error);
          require (read error = "") "JSON CLI wrote stderr";
          Yojson.Safe.from_string text))

let error code report =
  require
    (report |> member "diagnostics" |> to_list
    |> List.exists (fun d -> d |> member "code" |> to_string = code))
    ("missing stream generation diagnostic " ^ code)

let value report =
  require
    (report |> member "outcome" |> to_string = "success")
    "stream example failed";
  require
    (report |> member "final_value" |> member "value" |> to_string = "42")
    "stream example returned another value";
  require
    (report |> member "output_hex" |> to_string = "6d616465")
    "ordinary output changed or contains generated source"

let () =
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          let declarations = invoke ~mode target declarations_example in
          require
            (declarations |> member "final_value" |> member "value" |> to_string
           = "42")
            "synchronous child declaration did not reach the outer parser";
          require
            (declarations |> member "output_hex" |> to_string = "6368696c64303b")
            "child declaration completion did not return zero";
          if native_only then
            require
              (declarations |> member "arithmetic" |> to_string
               = "runtime-native"
              && declarations |> member "native" |> member "fragments"
                 |> to_list
                 |> List.for_all (fun fragment ->
                     fragment |> member "outcome" |> to_string = "success"))
              "native declaration source did not complete its machine fragments")
        [ "jit"; "aot" ];
      let baseline = invoke target example in
      value baseline;
      let steps = baseline |> member "executed_steps" |> to_int in
      let work = baseline |> member "output_work" |> to_int in
      value
        (invoke target example
           ~options:
             [
               "--step-limit=" ^ string_of_int steps;
               "--output-work-limit=" ^ string_of_int work;
               "--generated-byte-limit=14";
               "--output-byte-limit=4";
             ]);
      List.iter
        (fun (option, code) ->
          let report = invoke ~status:1 ~options:[ option ] target example in
          error code report)
        [
          ("--step-limit=" ^ string_of_int (steps - 1), "HCIRVM0007");
          ("--output-work-limit=" ^ string_of_int (work - 1), "HCIRVM0023");
          ("--generated-byte-limit=13", "HCIRVM0028");
          ("--output-byte-limit=3", "HCIRVM0022");
        ];
      temporary ".hc" {|#exe {StreamPrint("#exe {StreamPrint(\"42;\");}");}|}
        (fun path ->
          let report = invoke target path in
          require
            (report |> member "final_value" |> member "value" |> to_string
           = "42")
            "nested native source did not resume its original parser");
      temporary ".hc"
        {|extern U0 StreamPrint(U8 *fmt,...);StreamPrint("hello");|}
        (fun path ->
          let report = invoke ~status:1 target path in
          error "HCIRVM0027" report;
          require
            (report |> member "output_work" |> to_int = 11)
            "inactive check ran before native formatting";
          require
            (report |> member "output_hex" |> to_string = "")
            "inactive bytes escaped");
      temporary ".hc"
        {|extern I64 StreamExePrint(U8 *fmt,...);StreamExePrint("%f",42);|}
        (fun path ->
          let report = invoke ~status:1 target path in
          error "HCIRVM0024" report;
          require
            (report |> member "output_work" |> to_int = 2)
            "JIT context check ran before formatting");
      if target = "host-jit-task" then (
        let report = invoke ~mode:"aot" target example in
        value report;
        let fragments =
          report |> member "native" |> member "fragments" |> to_list
        in
        require
          (List.hd (List.rev fragments)
          |> member "kind" |> to_string = "aot-module")
          "AOT module follows original native directives"))
    targets;
  Printf.printf "%d stream generation CLI executions passed\n%!" !count
