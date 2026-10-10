open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix action =
  let path = Filename.temp_file "holyc member sizeof " suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () -> action path)

let () =
  require
    (Array.length Sys.argv = 3
    || (Array.length Sys.argv = 4 && Sys.argv.(3) = "--native"))
    "expected compiler and member sizeof example, with optional --native"

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)
let native = Array.length Sys.argv = 4

let invoke target mode =
  let arguments =
    [
      "run";
      "--report-version=2";
      "--format=json";
      "--target=" ^ target;
      "--mode=" ^ mode;
    ]
    @ (if target = "host-jit-task" then [ "--code-byte-limit=524288" ] else [])
    @ [ example ]
  in
  with_file ".stdout" (fun stdout ->
      with_file ".stderr" (fun stderr ->
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
                  (Array.of_list (compiler :: arguments))
                  Unix.stdin out_fd err_fd)
          in
          let _, status = Unix.waitpid [] pid in
          let output = read stdout and errors = read stderr in
          require (status = Unix.WEXITED 0)
            (Printf.sprintf "member sizeof CLI failed for %s/%s: %s%s" target
               mode output errors);
          require (errors = "")
            "member sizeof JSON diagnostics escaped to stderr";
          Yojson.Safe.from_string output))

let check target mode =
  let report = invoke target mode in
  let evidence = Yojson.Safe.to_string report in
  require
    (member "schema" report = `String "holyc-integer-program-v2")
    "member sizeof report schema";
  require (member "target" report = `String target) "requested execution target";
  require
    (member "outcome" report = `String "success")
    ("member sizeof execution failed: " ^ evidence);
  require (member "diagnostics" report = `List []) "unexpected diagnostics";
  require (member "command_error" report = `Null) "unexpected command error";
  require
    (member "termination" report = `String "stream-end")
    "member sizeof execution reaches stream end";
  let final = member "final_value" report in
  require (member "type" final = `String "i64") "I64 function result";
  require
    (member "value" final = `String "33"
    && member "bits" final = `String "0x0000000000000021")
    ("member sizeof final value: " ^ evidence);
  require
    (member "output_hex" report = `String "312031342036203420380a"
    && member "output_byte_length" report = `Int 11)
    ("member sizeof output must be 1 14 6 4 8 followed by a newline: "
   ^ evidence);
  require
    (member "arithmetic" report
    = `String (if target = "ir" then "runtime-ir" else "runtime-native"))
    "requested arithmetic backend";
  if target <> "ir" then (
    require
      (report |> member "executed_steps" |> to_int > 0)
      "native instructions executed";
    let execution = member "native" report in
    if target = "host-jit-task" then (
      let fragments = execution |> member "fragments" |> to_list in
      require (fragments <> []) "native fragments exist";
      List.iter
        (fun fragment ->
          require
            (member "outcome" fragment = `String "success")
            "native fragment completes")
        fragments)
    else require (member "image" execution <> `Null) "native image exists")

let () =
  let cases =
    if native then [ ("host-jit", "jit"); ("host-jit-task", "jit") ]
    else [ ("ir", "jit"); ("ir", "aot") ]
  in
  List.iter (fun (target, mode) -> check target mode) cases;
  Printf.printf "Aggregate member sizeof CLI passed: %s\n"
    (String.concat ", "
       (List.map (fun (target, mode) -> target ^ "/" ^ mode) cases))
