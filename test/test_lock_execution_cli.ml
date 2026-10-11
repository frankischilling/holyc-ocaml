open Yojson.Safe.Util
module Cases = Lock_execution_cases

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-lock-execution-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      output_string channel contents;
      close_out channel;
      action path)

let hex text =
  text |> String.to_seq |> List.of_seq
  |> List.map (fun byte -> Printf.sprintf "%02x" (Char.code byte))
  |> String.concat ""

let invoke compiler ~target ~mode ?(options = []) ~status label program =
  temporary ".hc" program (fun source ->
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
              let output = read stdout and error = read stderr in
              if actual <> Unix.WEXITED status || error <> "" then
                failwith
                  (label ^ ": unexpected process status or stderr; stdout="
                 ^ output ^ "; stderr=" ^ error);
              Yojson.Safe.from_string output)))

let errors report =
  report |> member "diagnostics" |> to_list
  |> List.filter (fun d -> d |> member "severity" |> to_string = "error")

let expect (case : Cases.success) report =
  if
    report |> member "outcome" |> to_string <> "success"
    || report |> member "final_value" |> member "value" |> to_string
       <> Int64.to_string case.value
    || report |> member "output_hex" |> to_string <> hex case.output
    || errors report <> []
  then failwith (case.name ^ ": " ^ Yojson.Safe.to_string report)

let expect_failure (case : Cases.failure) report =
  if
    report |> member "outcome" |> to_string = "success"
    || (not
          (List.exists
             (fun d -> d |> member "code" |> to_string = case.code)
             (errors report)))
    || report |> member "output_hex" |> to_string <> hex case.output
  then failwith (case.name ^ ": " ^ Yojson.Safe.to_string report)

let representative_names =
  [
    "Bool locked prefix increment";
    "U8 locked postfix increment";
    "I64 locked prefix decrement";
    "U64 locked postfix decrement";
    "one statement ends lock before following multiply";
    "nested lock regions retain the outer scope";
    "lock does not extend into called function body";
    "rhs call completes before the atomic destination read";
    "captured pointer destination survives rhs rebinding";
    "lock switch retains atomic updates in selected arms";
    "goto can enter and leave a lexical lock body";
    "rhs output is reached exactly once";
    "lock Bts returns prior bit 0";
    "LBtr returns prior bit 1";
    "LBtc returns prior bit 0";
    "locked bit selects a byte across a word boundary";
    "ordinary locked narrow multiply returns its full computation";
    "ordinary locked narrow shift returns its full computation";
    "locked power-of-two modulus uses the original AND rewrite";
    "locked non-power-of-two modulus remains signed remainder";
    "locked commuted addition uses its safe immediate form";
    "locked scalar subtraction uses its safe immediate form";
    "locked immediate addition promotes a signed Bool byte";
    "locked AND loop tests two runtime operands";
  ]

let representative =
  List.filter
    (fun (case : Cases.success) ->
      String.ends_with ~suffix:"locked += reloads its stored width" case.name
      || List.mem case.name representative_names)
    Cases.success_cases

let () =
  let compiler = Sys.argv.(1) and example = Sys.argv.(2) in
  let native = Array.exists (( = ) "--native") Sys.argv in
  let targets = if native then [ "host-jit"; "host-jit-task" ] else [ "ir" ] in
  let successes = ref 0 and failures = ref 0 and budgets = ref 0 in
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          List.iter
            (fun (case : Cases.success) ->
              invoke compiler ~target ~mode ~status:0 case.name case.program
              |> expect case;
              incr successes)
            representative;
          let public_example =
            Cases.successful "public lock-execution example" (read example) 42L
          in
          invoke compiler ~target ~mode ~status:0 public_example.name
            public_example.program
          |> expect public_example;
          incr successes;
          List.iter
            (fun (case : Cases.failure) ->
              invoke compiler ~target ~mode ~status:1 case.name case.program
              |> expect_failure case;
              incr failures)
            (Cases.invalid_cases @ Cases.fault_cases);
          let quota =
            Cases.successful "lock CLI runtime quota" Cases.quota_source 42L
          in
          let baseline =
            invoke compiler ~target ~mode ~status:0 quota.name quota.program
          in
          expect quota baseline;
          let steps = baseline |> member "executed_steps" |> to_int in
          if steps < 2 then failwith "locked program consumed no runtime work";
          let exact =
            invoke compiler ~target ~mode ~status:0
              ~options:[ "--step-limit=" ^ string_of_int steps ]
              quota.name quota.program
          in
          expect quota exact;
          if exact |> member "executed_steps" |> to_int <> steps then
            failwith "exact locked runtime allowance changed the reached work";
          invoke compiler ~target ~mode ~status:1
            ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
            "one-below lock CLI runtime quota" quota.program
          |> expect_failure
               (Cases.failure "one-below lock CLI runtime quota" quota.program
                  "HCIRVM0007");
          incr budgets)
        [ "jit"; "aot" ];
      if target <> "host-jit" then
        List.iter
          (fun (case : Cases.success) ->
            invoke compiler ~target ~mode:"jit" ~status:0 case.name case.program
            |> expect case;
            incr successes)
          Cases.retained_cases)
    targets;
  Printf.printf "%d public lock CLI results passed.\n" !successes;
  Printf.printf "%d invalid or faulting lock CLI cases passed.\n" !failures;
  Printf.printf "%d exact and one-below lock CLI budget pairs passed.\n"
    !budgets
