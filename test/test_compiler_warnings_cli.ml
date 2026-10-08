open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-compiler-warnings-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      output_string channel contents;
      close_out channel;
      action path)

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)
let native_only = Array.exists (( = ) "--native") Sys.argv
let targets = if native_only then [ "host-jit-task" ] else [ "ir" ]
let count = ref 0

let invoke ?(status = 0) ?(options = []) mode target path =
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
              "--code-byte-limit=1048576";
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
          require (read error = "") "warning JSON wrote stderr";
          Yojson.Safe.from_string text))

let warnings report =
  report |> member "diagnostics" |> to_list
  |> List.filter (fun diagnostic ->
      diagnostic |> member "severity" |> to_string = "warning")
  |> List.map (fun diagnostic ->
      ( diagnostic |> member "code" |> to_string,
        diagnostic |> member "message" |> to_string ))

let expected =
  [
    ("HCSEMA0034", "unused variable \"unused\" in function \"Loud\"");
    ("HCSEMA0035", "unneeded no_warn for \"used\" in function \"Suppression\"");
    ("HCSEMA0034", "unused variable \"unused\" in function \"Child\"");
  ]

let () =
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          let report = invoke mode target example in
          require
            (report |> member "outcome" |> to_string = "success")
            "warning example failed";
          require
            (report |> member "final_value" |> member "value" |> to_string
           = "42")
            "warning example result changed";
          require
            (report |> member "output_hex" |> to_string
           = "34323b34323b34323b34323b")
            "warnings changed program output";
          require
            (warnings report = expected)
            "reached warning order or option mask changed";
          (if native_only then
             let fragments =
               report |> member "native" |> member "fragments" |> to_list
             in
             require
               (List.length fragments > 8
               && List.for_all
                    (fun fragment ->
                      fragment |> member "outcome" |> to_string = "success")
                    fragments
               && List.exists
                    (fun fragment ->
                      fragment |> member "function_count" |> to_int > 0)
                    fragments)
               "warning fixture lacks completed native function execution");
          let steps = report |> member "executed_steps" |> to_int in
          let exact =
            invoke
              ~options:[ "--step-limit=" ^ string_of_int steps ]
              mode target example
          in
          require (warnings exact = expected) "exact budget changed warnings";
          let below =
            invoke ~status:1
              ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
              mode target example
          in
          require
            (warnings below = expected)
            "later budget fault lost reached warnings";
          require
            (below |> member "executed_steps" |> to_int = steps - 1
            && below |> member "diagnostics" |> to_list
               |> List.exists (fun diagnostic ->
                   diagnostic |> member "code" |> to_string = "HCIRVM0007"))
            "one-below run did not exhaust the actual instruction budget";
          temporary ".hc" "#exe {I64 Broken(I64 unused){return missing;}}42;"
            (fun path ->
              require
                (warnings (invoke ~status:1 mode target path) = [])
                "failed body emitted unused warning");
          temporary ".hc"
            "#exe {I64 F(I64 used){used;return 42;}Print(\"%d;\",F(0));}"
            (fun ordinary ->
              temporary ".hc"
                "#exe {I64 F(I64 used){no_warn used;used;return \
                 42;}Print(\"%d;\",F(0));}" (fun suppressed ->
                  let ordinary = invoke mode target ordinary
                  and suppressed = invoke mode target suppressed in
                  require
                    (ordinary |> member "executed_steps"
                     = (suppressed |> member "executed_steps")
                    && ordinary
                       |> member "compiled_initializer_steps"
                       = (suppressed |> member "compiled_initializer_steps"))
                    "no_warn emitted runtime work";
                  require
                    (warnings ordinary = [])
                    "used local warned without no_warn";
                  require
                    (warnings suppressed
                    = [
                        ( "HCSEMA0035",
                          "unneeded no_warn for \"used\" in function \"F\"" );
                      ])
                    "no_warn did not update local warning state")))
        [ "jit"; "aot" ])
    targets;
  Printf.printf "%d compiler warning CLI executions passed\n%!" !count
