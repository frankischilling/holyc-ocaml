open Holyc_lib
open Yojson.Safe.Util
module Cases = Persistent_f64_execution_cases

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-persistent-f64-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let () =
  require
    (Array.length Sys.argv = 3 || Array.length Sys.argv = 4)
    "expected compiler and storage example, optionally --native"

let compiler = Sys.argv.(1)
let example_path = Sys.argv.(2)
let native = Array.length Sys.argv = 4 && Sys.argv.(3) = "--native"
let modes = [ "jit"; "aot" ]
let targets = [ "host-jit"; "host-jit-task" ]
let reports = ref 0

let hex bytes =
  String.to_seq bytes
  |> Seq.map (fun byte -> Printf.sprintf "%02x" (Char.code byte))
  |> List.of_seq |> String.concat ""

let invoke ?(status = 0) ?(options = []) ~target ~mode name source =
  temporary ".hc" source (fun path ->
      temporary ".stdout" "" (fun stdout ->
          temporary ".stderr" "" (fun stderr ->
              let output =
                Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              and errors =
                Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let args =
                Array.of_list
                  ([
                     compiler;
                     "run";
                     "--format=json";
                     "--report-version=2";
                     "--target=" ^ target;
                     "--mode=" ^ mode;
                     "--code-byte-limit=1048576";
                   ]
                  @ options @ [ path ])
              in
              let pid =
                Fun.protect
                  ~finally:(fun () ->
                    Unix.close output;
                    Unix.close errors)
                  (fun () ->
                    Unix.create_process compiler args Unix.stdin output errors)
              in
              let _, actual = Unix.waitpid [] pid in
              let output = read stdout and errors = read stderr in
              require
                (actual = Unix.WEXITED status && errors = "")
                (name ^ " " ^ target ^ " " ^ mode ^ ": " ^ output ^ "; stderr="
               ^ errors);
              let report = Yojson.Safe.from_string output in
              require
                (member "schema" report = `String "holyc-integer-program-v2"
                && member "target" report = `String target
                && member "mode" report = `String mode)
                (name ^ ": report identity");
              if target <> "ir" then
                require
                  (member "arithmetic" report = `String "runtime-native")
                  (name ^ ": native execution evidence");
              incr reports;
              report)))

let success (case : Cases.value_case) report =
  let word = member "final_value" report in
  require
    (member "outcome" report = `String "success"
    && member "type" word
       = `String
           (match case.kind with
           | Cases.I64 -> "i64"
           | F64 -> "f64")
    && member "bits" word = `String (Printf.sprintf "0x%016Lx" case.bits)
    && member "output_hex" report = `String (hex case.output)
    && report |> member "executed_steps" |> to_int > 0)
    (case.name ^ ": " ^ Yojson.Safe.to_string report);
  if case.kind = Cases.F64 then
    require
      (match member "value" word with
      | `String _ -> true
      | _ -> false)
      (case.name ^ ": F64 display remains valid JSON")

let rejection name report =
  require
    (member "outcome" report = `String "error"
    && report |> member "diagnostics" |> to_list
       |> List.exists (fun diagnostic ->
           match member "code" diagnostic with
           | `String code -> code <> ""
           | _ -> false))
    (name ^ ": " ^ Yojson.Safe.to_string report)

let has_code report code =
  report |> member "diagnostics" |> to_list
  |> List.exists (fun diagnostic -> member "code" diagnostic = `String code)

let value ~target ~mode (case : Cases.value_case) =
  if case.fisttp && not (Native_execution.fisttp_supported ()) then
    invoke ~status:1 ~target ~mode case.name case.source
    |> rejection (case.name ^ " requires FISTTP")
  else invoke ~target ~mode case.name case.source |> success case

let fault ~target ~mode (case : Cases.fault_case) =
  let report = invoke ~status:1 ~target ~mode case.name case.source in
  rejection case.name report;
  require
    (has_code report case.code
    && member "output_hex" report = `String (hex case.output))
    (case.name ^ ": reached fault/output changed")

let quotas ~target ~mode =
  let case =
    Cases.integer "persistent execution quota" Cases.quota_source 42L
  in
  let baseline = invoke ~target ~mode case.name case.source in
  success case baseline;
  let steps = baseline |> member "executed_steps" |> to_int in
  invoke ~target ~mode case.name case.source
    ~options:[ "--step-limit=" ^ string_of_int steps ]
  |> success case;
  let limited =
    invoke ~status:1 ~target ~mode case.name case.source
      ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
  in
  rejection "one-below runtime allowance" limited;
  require
    (member "executed_steps" limited = `Int (steps - 1))
    "one-below allowance retains every reached charge";
  invoke ~target ~mode case.name case.source
    ~options:[ "--global-byte-limit=8" ]
  |> success case;
  invoke ~status:1 ~target ~mode case.name case.source
    ~options:[ "--global-byte-limit=7" ]
  |> rejection "one-below original global extent";
  let effect_case =
    List.find
      (fun (case : Cases.value_case) ->
        case.name = "argument calls retain reverse effects and stored reads")
      Cases.effect_values
  in
  invoke ~target ~mode effect_case.name effect_case.source
    ~options:[ "--output-byte-limit=2" ]
  |> success effect_case;
  let limited =
    invoke ~status:1 ~target ~mode effect_case.name effect_case.source
      ~options:[ "--output-byte-limit=1" ]
  in
  rejection "one-below output allowance" limited;
  require
    (has_code limited "HCIRVM0022" && member "output_hex" limited = `String "41")
    "output allowance stops at the second original provider"

let failures = ref []

let check name action =
  try action ()
  with error ->
    failures := (name ^ ": " ^ Printexc.to_string error) :: !failures

let () =
  List.iter
    (fun mode ->
      check ("IR storage boundary " ^ mode) (fun () ->
          let report =
            invoke ~status:1 ~target:"ir" ~mode "IR F64 execution boundary"
              Cases.quota_source
          in
          rejection "IR F64 execution boundary" report;
          require
            (member "output_hex" report = `String "")
            "IR rejection precedes effects"))
    modes;
  if native then
    if not (Native_execution.x87_supported ()) then
      print_endline "persistent F64 native tests: x87 unsupported, skipped"
    else (
      List.iter
        (fun mode ->
          List.iter
            (fun target ->
              List.iter
                (fun (case : Cases.value_case) ->
                  check
                    (target ^ " " ^ mode ^ " " ^ case.name)
                    (fun () -> value ~target ~mode case))
                Cases.values;
              List.iter
                (fun (case : Cases.fault_case) ->
                  check
                    (target ^ " " ^ mode ^ " " ^ case.name)
                    (fun () -> fault ~target ~mode case))
                (Cases.faults @ if mode = "jit" then Cases.jit_faults else []);
              List.iter
                (fun (name, source) ->
                  check
                    (target ^ " " ^ mode ^ " " ^ name)
                    (fun () ->
                      invoke ~status:1 ~target ~mode name source
                      |> rejection name))
                Cases.unsupported;
              check
                (target ^ " " ^ mode ^ " quotas")
                (fun () -> quotas ~target ~mode);
              check
                (target ^ " " ^ mode ^ " maintained example")
                (fun () ->
                  value ~target ~mode
                    { Cases.maintained with source = read example_path }))
            targets)
        modes;
      List.iter
        (fun (case : Cases.value_case) ->
          check ("retained JIT source " ^ case.name) (fun () ->
              value ~target:"host-jit-task" ~mode:"jit" case))
        Cases.jit_task_values);
  List.iter prerr_endline (List.rev !failures);
  if !failures <> [] then
    failwith
      (Printf.sprintf "%d persistent F64 CLI checks failed"
         (List.length !failures));
  Printf.printf "persistent F64 execution CLI: %d reports passed\n" !reports
