open Yojson.Safe.Util
module Cases = Exception_execution_cases

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-exception-execution-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      output_string channel contents;
      close_out channel;
      action path)

let hex text =
  String.to_seq text |> List.of_seq
  |> List.map (fun byte -> Printf.sprintf "%02x" (Char.code byte))
  |> String.concat ""

let invoke compiler ?(target = "ir") ?(mode = "jit") ?(options = []) ~status
    label program =
  temporary ".hc" (Cases.headers ^ program) (fun source ->
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
                     "--target=" ^ target;
                     "--mode=" ^ mode;
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
              let output = read stdout and error_output = read stderr in
              if actual <> Unix.WEXITED status || error_output <> "" then
                failwith
                  (label ^ ": unexpected process status or stderr; stdout="
                 ^ output ^ "; stderr=" ^ error_output);
              Yojson.Safe.from_string output)))

let errors report =
  report |> member "diagnostics" |> to_list
  |> List.filter (fun d -> d |> member "severity" |> to_string = "error")

let successful (case : Cases.success) report =
  if
    report |> member "outcome" |> to_string <> "success"
    || report |> member "final_value" |> member "value" |> to_string
       <> Int64.to_string case.value
    || report |> member "output_hex" |> to_string <> hex case.output
    || errors report <> []
  then failwith (case.name ^ ": " ^ Yojson.Safe.to_string report)

let failure (case : Cases.failure) report =
  let diagnostics = errors report in
  if
    report |> member "outcome" |> to_string = "success"
    || diagnostics = []
    || Option.fold ~none:false
         ~some:(fun code ->
           not
             (List.exists
                (fun error -> error |> member "code" |> to_string = code)
                diagnostics))
         case.code
  then failwith (case.name ^ ": " ^ Yojson.Safe.to_string report)

let representative =
  [
    "top-level normal try skips catch";
    "top-level throw enters catch";
    "normal path skips catch";
    "throw transfers past protected tail";
    "default throw call raises zero";
    "explicit zero throw raises zero";
    "unmarked inner catch propagates to outer";
    "conditional throw from catch reenters handler";
    "throw propagates across function calls";
    "argument throw skips target invocation";
    "nested return unregisters both handlers";
    "return operand runs after current try is unregistered";
    "subsequent calls register fresh exception frames";
    "named UndefExt source exception";
    "divide by zero becomes DivZero";
    "signed division overflow becomes DivZero";
    "throw retains earlier output and follows catch";
  ]

let () =
  let compiler = Sys.argv.(1) in
  let successes = ref 0 and failures = ref 0 and budgets = ref 0 in
  List.iter
    (fun mode ->
      List.iter
        (fun (case : Cases.success) ->
          if List.mem case.Cases.name representative then (
            invoke compiler ~mode ~status:0 case.name case.program
            |> successful case;
            incr successes))
        Cases.success_cases;
      List.iter
        (fun case ->
          let report =
            invoke compiler ~mode ~status:1 case.Cases.name case.program
          in
          failure case report;
          incr failures)
        (Cases.invalid_transfers @ Cases.uncaught);
      let baseline =
        invoke compiler ~mode ~status:0 "exception quota baseline"
          Cases.quota_source
      in
      let canonical =
        Cases.successful "exception quota baseline" Cases.quota_source 42L
      in
      successful canonical baseline;
      let steps = baseline |> member "executed_steps" |> to_int in
      if steps < 2 then failwith "exception program executed too few steps";
      let exact =
        invoke compiler ~mode ~status:0
          ~options:[ "--step-limit=" ^ string_of_int steps ]
          "exact exception runtime work" Cases.quota_source
      in
      successful canonical exact;
      if exact |> member "executed_steps" |> to_int <> steps then
        failwith "exact exception runtime budget changed work accounting";
      let short =
        invoke compiler ~mode ~status:1
          ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
          "one-below exception runtime work" Cases.quota_source
      in
      failure
        (Cases.failure ~code:"HCIRVM0007" "unhandled step quota"
           Cases.quota_source)
        short;
      incr budgets;
      let fresh =
        invoke compiler ~mode ~status:0 "fresh exception runtime"
          Cases.quota_source
      in
      successful canonical fresh)
    [ "jit"; "aot" ];
  (* Issue #706 reserves native exception unwinding. A normal-path try must
     still be refused if it would require compiling native handler markers. *)
  let native =
    List.find
      (fun (case : Cases.success) ->
        case.Cases.name = "normal path skips catch")
      Cases.success_cases
  in
  let rejected =
    invoke compiler ~target:"host-jit-task" ~mode:"jit" ~status:1
      "native try still requires issue 706" native.program
  in
  failure
    (Cases.failure "native try requires a native exception implementation"
       native.program)
    rejected;
  Printf.printf
    "%d public IR exception CLI results passed in JIT and AOT modes.\n"
    !successes;
  Printf.printf "%d invalid transfer or unhandled exception cases rejected.\n"
    !failures;
  Printf.printf "%d exact/one-below exception budget pairs passed.\n" !budgets;
  Printf.printf "Native exception path rejected pending issue #706.\n"
