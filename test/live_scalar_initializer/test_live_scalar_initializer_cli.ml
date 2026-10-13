open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let invoke compiler example target mode options status =
  let stdout = Filename.temp_file "holyc scalar copy " ".stdout"
  and stderr = Filename.temp_file "holyc scalar copy " ".stderr" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove stdout;
      Sys.remove stderr)
    (fun () ->
      let out_fd = Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
      and err_fd = Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
      let args =
        [
          compiler;
          "run";
          "--report-version=2";
          "--format=json";
          "--target=" ^ target;
          "--mode=" ^ mode;
          "--code-byte-limit=524288";
        ]
        @ options @ [ example ]
      in
      let pid =
        Fun.protect
          ~finally:(fun () ->
            Unix.close out_fd;
            Unix.close err_fd)
          (fun () ->
            Unix.create_process compiler (Array.of_list args) Unix.stdin out_fd
              err_fd)
      in
      let _, actual = Unix.waitpid [] pid in
      let output = read stdout in
      require
        (actual = Unix.WEXITED status)
        ("unexpected scalar copy exit: " ^ output ^ read stderr);
      require
        (read stderr = "")
        "JSON scalar copy diagnostics escaped to stderr";
      Yojson.Safe.from_string output)

let success expected report =
  require
    (member "outcome" report = `String "success")
    (Yojson.Safe.to_string report);
  require
    (report |> member "final_value" |> member "value" = `String expected)
    "original scalar cursor and saved table result";
  require
    (member "output_hex" report = `String "41")
    "callee output occurred exactly once";
  require
    (member "diagnostics" report = `List [])
    "successful scalar copy diagnostics"

let failure code report =
  require
    (member "outcome" report = `String "error")
    "failed scalar copy outcome";
  require
    (report |> member "diagnostics" |> to_list
    |> List.exists (fun d -> member "code" d = `String code))
    ("missing scalar copy diagnostic " ^ code ^ ": "
    ^ Yojson.Safe.to_string report)

let () =
  require
    (Array.length Sys.argv = 3 || Array.length Sys.argv = 4)
    "expected compiler, live scalar example and optional --native";
  let compiler = Sys.argv.(1) and example = Sys.argv.(2) in
  let targets =
    if Array.length Sys.argv = 4 then [ "ir"; "host-jit-task" ] else [ "ir" ]
  in
  List.iter
    (fun target ->
      List.iter
        (fun (mode, expected) ->
          let run options status =
            invoke compiler example target mode options status
          in
          let baseline = run [] 0 in
          success expected baseline;
          let work = member "compiled_initializer_steps" baseline |> to_int
          and steps = member "executed_steps" baseline |> to_int in
          success expected
            (run
               [
                 Printf.sprintf "--initializer-step-limit=%d" work;
                 Printf.sprintf "--step-limit=%d" steps;
               ]
               0);
          let failed =
            run [ Printf.sprintf "--initializer-step-limit=%d" (work - 1) ] 1
          in
          failure "HCIRVM0007" failed;
          require
            (member "output_hex" failed = `String "41")
            "output survives deferred copy quota";
          failure "HCIRVM0007"
            (run [ Printf.sprintf "--step-limit=%d" (steps - 1) ] 1))
        [ ("jit", "42"); ("aot", "7") ])
    targets;
  Printf.printf "Live scalar initializer CLI values and exact quotas passed\n"
