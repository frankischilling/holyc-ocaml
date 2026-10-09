open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-provider-callback-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      output_string channel contents;
      close_out channel;
      action path)

let compiler = Sys.argv.(1)
let fixture = Sys.argv.(2)
let print_fixture = Sys.argv.(3)
let count = ref 0

let run expected target options path =
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
              "--mode=jit";
              "--target=" ^ target;
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
          let _, status = Unix.waitpid [] pid in
          incr count;
          let text = read output in
          require
            (status = Unix.WEXITED expected)
            ("provider CLI exit: " ^ text ^ read error);
          require (read error = "") "provider JSON CLI wrote stderr";
          Yojson.Safe.from_string text))

let has code report =
  report |> member "diagnostics" |> to_list
  |> List.exists (fun d -> d |> member "code" |> to_string = code)

let () =
  List.iter
    (fun target ->
      let report = run 0 target [] fixture in
      require
        (report |> member "final_value" |> member "bits" |> to_string
       = "0x000000000000002a")
        "provider CLI word";
      require
        (report |> member "output_hex" |> to_string = "4142")
        "provider CLI captured bytes";
      let steps = report |> member "executed_steps" |> to_int in
      ignore
        (run 0 target
           [
             "--step-limit=" ^ string_of_int steps;
             "--output-byte-limit=2";
             "--output-work-limit=4";
           ]
           fixture);
      List.iter
        (fun (option, code) ->
          let fault = run 1 target [ option ] fixture in
          require (has code fault) ("provider CLI fault " ^ code);
          require
            (fault |> member "output_hex" |> to_string = "41")
            "provider CLI prefix")
        [
          ("--output-byte-limit=1", "HCIRVM0022");
          ("--output-work-limit=3", "HCIRVM0023");
        ];
      temporary ".hc"
        "extern U0 PutChars(U64 ch);I64 (*p)(U64 ch);p=&PutChars;p('A');42;"
        (fun path ->
          require
            (has "HCIRVM0014" (run 1 target [] path))
            "provider CLI signature"))
    [ "ir"; "host-jit-task" ];
  temporary ".hc"
    "extern U0 PutChars(U64 ch);U0 (*p)(U64 ch);p=&PutChars;p('A');42;"
    (fun path ->
      require
        (has "HCBACK0002" (run 1 "host-jit" [] path))
        "isolated provider slot boundary");
  List.iter
    (fun target ->
      let options = [ "--code-byte-limit=262144" ] in
      let report = run 0 target options print_fixture in
      require
        (report |> member "final_value" |> member "bits" |> to_string
       = "0x000000000000002a")
        "Print provider CLI word";
      require
        (report |> member "output_hex" |> to_string = "4142")
        "Print provider CLI saved entry bytes";
      require
        (report |> member "reference_commit" |> to_string
       = "c26482bb6ad3f80106d28504ec5db3c6a360732c")
        "Print provider CLI reference";
      require
        (String.length (report |> member "implementation_commit" |> to_string)
        = 40)
        "Print provider CLI implementation identity";
      let steps = report |> member "executed_steps" |> to_int in
      let work = report |> member "output_work" |> to_int in
      ignore
        (run 0 target
           (options
           @ [
               "--step-limit=" ^ string_of_int steps;
               "--output-byte-limit=2";
               "--output-work-limit=" ^ string_of_int work;
             ])
           print_fixture);
      List.iter
        (fun (option, code) ->
          let fault = run 1 target (options @ [ option ]) print_fixture in
          require (has code fault) ("Print provider CLI fault " ^ code);
          require
            (fault |> member "output_hex" |> to_string = "")
            "Print provider CLI atomic draft")
        [
          ("--output-byte-limit=1", "HCIRVM0022");
          ("--output-work-limit=" ^ string_of_int (work - 1), "HCIRVM0023");
        ];
      temporary ".hc"
        "extern U0 Print(U8 *fmt,...);I64 (*p)(U8 *fmt,...)=&Print;p(\"A\");42;"
        (fun path ->
          require
            (has "HCIRVM0014" (run 1 target options path))
            "Print provider CLI signature");
      temporary ".hc"
        "extern U0 Print(U8 *fmt,...);U0 (*p)(U8 \
         *fmt,...)=&Print;p(\"%s\",42);42;" (fun path ->
          require
            (has "HCIRVM0025" (run 1 target options path))
            "Print provider CLI original argument kind"))
    [ "ir"; "host-jit-task" ];
  temporary ".hc"
    "extern U0 Print(U8 *fmt,...);U0 (*p)(U8 *fmt,...);p=&Print;p(\"A\");42;"
    (fun path ->
      require
        (has "HCBACK0002" (run 1 "host-jit" [] path))
        "isolated Print provider slot boundary");
  Printf.printf "Verified %d provider callback CLI executions.\n" !count
