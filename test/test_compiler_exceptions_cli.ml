open Yojson.Safe.Util
module Cases = Compiler_exception_cases

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-compiler-exceptions-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      output_string channel contents;
      close_out channel;
      action path)

let hex text =
  String.to_seq text |> List.of_seq
  |> List.map (fun c -> Printf.sprintf "%02x" (Char.code c))
  |> String.concat ""

let invoke compiler target mode ?(with_headers = true) ?(options = []) ~status
    label contents =
  temporary ".HC"
    ((if with_headers && mode = "jit" then Cases.headers else "") ^ contents)
    (fun source ->
      temporary ".out" "" (fun stdout ->
          temporary ".err" "" (fun stderr ->
              let out_fd =
                Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let err_fd =
                Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let args =
                Array.of_list
                  ([
                     compiler;
                     "run";
                     "--format=json";
                     "--mode=" ^ mode;
                     "--target=" ^ target;
                     "--code-byte-limit=1048576";
                   ]
                  @ options @ [ source ])
              in
              let pid =
                Fun.protect
                  ~finally:(fun () ->
                    Unix.close out_fd;
                    Unix.close err_fd)
                  (fun () ->
                    Unix.create_process compiler args Unix.stdin out_fd err_fd)
              in
              let _, actual = Unix.waitpid [] pid in
              let text = read stdout in
              if actual <> Unix.WEXITED status || read stderr <> "" then
                failwith (label ^ text ^ read stderr);
              Yojson.Safe.from_string text)))

let errors report =
  report |> member "diagnostics" |> to_list
  |> List.filter (fun d -> d |> member "severity" |> to_string = "error")

let caught_value target label output report =
  if
    report |> member "outcome" |> to_string <> "success"
    || report |> member "final_value" |> member "value" |> to_string <> "42"
    || report |> member "output_hex" |> to_string <> hex output
    || errors report <> []
  then failwith (label ^ ": " ^ Yojson.Safe.to_string report);
  if
    target = "host-jit-task"
    && (report |> member "arithmetic" |> to_string <> "runtime-native"
       || not
            (List.for_all
               (fun fragment ->
                 fragment |> member "outcome" |> to_string = "success")
               (report |> member "native" |> member "fragments" |> to_list)))
  then failwith (label ^ " did not complete its original native fragments")

let () =
  let compiler = Sys.argv.(1) in
  let target =
    if Array.exists (( = ) "--native") Sys.argv then "host-jit-task" else "ir"
  in
  let uncaught = ref 0 and caught = ref 0 and quota_edges = ref 0 in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text, output) ->
          let report = invoke compiler target mode ~status:1 label text in
          if
            List.filter_map
              (fun d ->
                let code = d |> member "code" |> to_string in
                if code = "HCRUN0004" then None else Some code)
              (errors report)
            <> [ "HCPARSE0168" ]
          then
            failwith
              (label ^ " original return error changed: "
              ^ Yojson.Safe.to_string report);
          if report |> member "output_hex" |> to_string <> hex output then
            failwith (label ^ " reached output changed");
          incr uncaught)
        Cases.failures;
      List.iter
        (fun (label, text, output, _) ->
          let report = invoke compiler target mode ~status:0 label text in
          caught_value target label output report;
          incr caught;
          if label = "parent initializer" then (
            let steps = report |> member "executed_steps" |> to_int in
            let work = report |> member "output_work" |> to_int in
            let bytes = report |> member "output_byte_length" |> to_int in
            let exact =
              invoke compiler target mode ~status:0 label text
                ~options:
                  [
                    "--step-limit=" ^ string_of_int steps;
                    "--output-work-limit=" ^ string_of_int work;
                    "--output-byte-limit=" ^ string_of_int bytes;
                  ]
            in
            caught_value target label output exact;
            if
              member "executed_steps" exact <> member "executed_steps" report
              || member "output_work" exact <> member "output_work" report
            then
              failwith
                "caught child changed its cumulative charges at exact limits";
            incr quota_edges;
            List.iter
              (fun (option, code) ->
                let rejected =
                  invoke compiler target mode ~status:1 label text
                    ~options:[ option ]
                in
                if
                  not
                    (List.exists
                       (fun d -> d |> member "code" |> to_string = code)
                       (errors rejected))
                then
                  failwith
                    ("caught child quota disappeared: "
                    ^ Yojson.Safe.to_string rejected);
                incr quota_edges)
              [
                ("--step-limit=" ^ string_of_int (steps - 1), "HCIRVM0007");
                ("--output-work-limit=" ^ string_of_int (work - 1), "HCIRVM0023");
                ( "--output-byte-limit=" ^ string_of_int (bytes - 1),
                  "HCIRVM0022" );
              ]))
        Cases.caught_children)
    [ "jit"; "aot" ];
  Printf.printf "%d uncaught Compiler return CLI cases passed (%s).\n" !uncaught
    target;
  Printf.printf "%d caught Compiler child CLI cases passed (%s).\n" !caught
    target;
  Printf.printf "%d caught child quota edges passed (%s).\n" !quota_edges target;
  let statement_cases = ref 0 in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text, code, _, output) ->
          let report = invoke compiler target mode ~status:1 label text in
          let codes =
            List.filter_map
              (fun diagnostic ->
                let code = diagnostic |> member "code" |> to_string in
                if code = "HCRUN0004" then None else Some code)
              (errors report)
          in
          if
            codes <> [ code ]
            || report |> member "output_hex" |> to_string <> hex output
          then failwith (label ^ ": " ^ Yojson.Safe.to_string report);
          incr statement_cases)
        Cases.statement_failures;
      List.iter
        (fun (label, text, _, _, output) ->
          let report = invoke compiler target mode ~status:0 label text in
          caught_value target label output report;
          incr statement_cases)
        Cases.statement_caught_children)
    [ "jit"; "aot" ];
  Printf.printf "%d original statement Compiler CLI cases passed (%s).\n"
    !statement_cases target;
  let call_cases = ref 0 in
  List.iter
    (fun (label, text, code, _) ->
      let report =
        invoke compiler target "jit" ~with_headers:false ~status:1 label text
      in
      let codes =
        List.filter_map
          (fun diagnostic ->
            let code = diagnostic |> member "code" |> to_string in
            if code = "HCRUN0004" then None else Some code)
          (errors report)
      in
      if codes <> [ code ] then
        failwith (label ^ ": " ^ Yojson.Safe.to_string report);
      incr call_cases)
    Cases.call_failures;
  List.iter
    (fun mode ->
      List.iter
        (fun (label, with_headers, text, _, _, output) ->
          let report =
            invoke compiler target mode ~with_headers ~status:0 label text
          in
          caught_value target label output report;
          incr call_cases)
        Cases.call_caught_children)
    [ "jit"; "aot" ];
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text, output) ->
          let report =
            invoke compiler target mode ~with_headers:false ~status:0 label text
          in
          caught_value target label output report;
          incr call_cases)
        Cases.call_successes)
    [ "jit"; "aot" ];
  Printf.printf "%d original call Compiler CLI cases passed (%s).\n" !call_cases
    target
