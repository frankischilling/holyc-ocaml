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

let success ?(output_hex = "41") expected report =
  require
    (member "outcome" report = `String "success")
    (Yojson.Safe.to_string report);
  require
    (report |> member "final_value" |> member "value" = `String expected)
    "original scalar cursor and saved table result";
  require
    (member "output_hex" report = `String output_hex)
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
  let examples =
    [
      example;
      Filename.concat (Filename.dirname example)
        "live-static-scalar-initializers.hc";
    ]
  in
  let targets =
    if Array.length Sys.argv = 4 then [ "ir"; "host-jit-task" ] else [ "ir" ]
  in
  List.iter
    (fun example ->
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
                run
                  [ Printf.sprintf "--initializer-step-limit=%d" (work - 1) ]
                  1
              in
              failure "HCIRVM0007" failed;
              require
                (member "output_hex" failed = `String "41")
                "output survives deferred copy quota";
              failure "HCIRVM0007"
                (run [ Printf.sprintf "--step-limit=%d" (steps - 1) ] 1))
            [ ("jit", "42"); ("aot", "7") ])
        targets)
    examples;
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          let source_example =
            Filename.concat (Filename.dirname example)
              "source-aggregate-initializers.hc"
          in
          let run options status =
            invoke compiler source_example target mode options status
          in
          let baseline = run [] 0 in
          success "42" baseline;
          let work = member "compiled_initializer_steps" baseline |> to_int
          and steps = member "executed_steps" baseline |> to_int in
          success "42"
            (run
               [
                 Printf.sprintf "--initializer-step-limit=%d" work;
                 Printf.sprintf "--step-limit=%d" steps;
               ]
               0);
          failure "HCIRVM0007"
            (run [ Printf.sprintf "--initializer-step-limit=%d" (work - 1) ] 1);
          failure "HCIRVM0007"
            (run [ Printf.sprintf "--step-limit=%d" (steps - 1) ] 1))
        [ "jit"; "aot" ])
    targets;
  let probe = Filename.temp_file "holyc static grammar " ".hc" in
  Fun.protect
    ~finally:(fun () -> Sys.remove probe)
    (fun () ->
      let write source =
        let channel = open_out_bin probe in
        Fun.protect
          ~finally:(fun () -> close_out channel)
          (fun () -> output_string channel (Cases.headers ^ source))
      in
      List.iter
        (fun target ->
          List.iter
            (fun (_, source, code) ->
              write source;
              let report = invoke compiler probe target "jit" [] 1 in
              failure code report;
              require
                (member "output_hex" report = `String "41")
                "source initializer output survives later failure")
            Cases.source_activation_faults;
          List.iter
            (fun mode ->
              List.iter
                (fun (_, source, output) ->
                  write source;
                  success
                    ~output_hex:(if output = "A" then "41" else "")
                    "42"
                    (invoke compiler probe target mode [] 0))
                Cases.ordinary_source_values;
              List.iter
                (fun (_, source, _) ->
                  write source;
                  success "42" (invoke compiler probe target mode [] 0))
                Cases.backed_values;
              List.iter
                (fun filename ->
                  let forward_example =
                    Filename.concat (Filename.dirname example) filename
                  in
                  let baseline =
                    invoke compiler forward_example target mode [] 0
                  in
                  success "42" baseline;
                  let steps = member "executed_steps" baseline |> to_int in
                  success "42"
                    (invoke compiler forward_example target mode
                       [ Printf.sprintf "--step-limit=%d" steps ]
                       0);
                  failure "HCIRVM0007"
                    (invoke compiler forward_example target mode
                       [ Printf.sprintf "--step-limit=%d" (steps - 1) ]
                       1);
                  if filename = "retained-aot-initializer-layouts.hc" then (
                    let work =
                      member "compiled_initializer_steps" baseline |> to_int
                    in
                    success "42"
                      (invoke compiler forward_example target mode
                         [ Printf.sprintf "--initializer-step-limit=%d" work ]
                         0);
                    failure "HCIRVM0007"
                      (invoke compiler forward_example target mode
                         [
                           Printf.sprintf "--initializer-step-limit=%d"
                             (work - 1);
                         ]
                         1)))
                [
                  "backed-forward-initializers.hc";
                  "class-body-initializers.hc";
                  "class-body-member-reads.hc";
                  "class-body-top-level-members.hc";
                  "class-body-callback-fields.hc";
                  "retained-aot-initializer-layouts.hc";
                ])
            [ "jit"; "aot" ];
          write Cases.static_before_nested_brace;
          success "42" (invoke compiler probe target "jit" [] 0);
          List.iter
            (fun (_, source, output) ->
              write source;
              let report = invoke compiler probe target "jit" [] 1 in
              failure "HCRUN0006" report;
              require
                (member "output_hex" report
                = `String (if output = "A" then "41" else ""))
                "static grammar preserves only reached earlier output")
            Cases.grammar_faults)
        targets);
  Printf.printf
    "Live scalar initializer CLI values, grammar timing and exact quotas passed\n"
