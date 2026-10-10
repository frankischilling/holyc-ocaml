open Yojson.Safe.Util
module Cases = Aggregate_member_cases
module Arrays = Aggregate_array_cases
module Pointers = Aggregate_pointer_cases
module Inherited = Inherited_aggregate_cases
module Backed = Backed_aggregate_cases

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc automatic aggregate " suffix in
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
    (Array.length Sys.argv = 7 || Array.length Sys.argv = 8)
    "expected compiler and five aggregate examples, optionally --native"

let compiler = Sys.argv.(1)
let native = Array.length Sys.argv = 8 && Sys.argv.(7) = "--native"
let reports = ref 0

let invoke ?(status = 0) ?(options = []) target mode source =
  with_file ".hc" source (fun path ->
      with_file ".stdout" "" (fun stdout ->
          with_file ".stderr" "" (fun stderr ->
              let out_fd =
                Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              and err_fd =
                Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let args =
                [
                  "run";
                  "--format=json";
                  "--report-version=2";
                  "--target=" ^ target;
                  "--mode=" ^ mode;
                ]
                @ options @ [ path ]
              in
              let pid =
                Fun.protect
                  ~finally:(fun () ->
                    Unix.close out_fd;
                    Unix.close err_fd)
                  (fun () ->
                    Unix.create_process compiler
                      (Array.of_list (compiler :: args))
                      Unix.stdin out_fd err_fd)
              in
              let _, actual = Unix.waitpid [] pid in
              let output = read stdout in
              require
                (actual = Unix.WEXITED status)
                ("unexpected aggregate exit: " ^ output ^ read stderr);
              require
                (read stderr = "")
                "JSON aggregate diagnostics escaped to stderr";
              incr reports;
              Yojson.Safe.from_string output)))

let hex bytes =
  String.to_seq bytes
  |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
  |> List.of_seq |> String.concat ""

let value ?(unused_array = false) expected output report =
  require
    (member "outcome" report = `String "success")
    (Yojson.Safe.to_string report);
  if unused_array then (
    let diagnostics = member "diagnostics" report |> to_list in
    require (List.length diagnostics = 1) "unused array warning count";
    let warning = List.hd diagnostics in
    require
      (member "code" warning = `String "HCSEMA0034"
      && member "severity" warning = `String "warning"
      && member "message" warning
         = `String "unused variable \"objects\" in function \"F\"")
      "unused array keeps its original warning")
  else
    require
      (member "diagnostics" report = `List [])
      ("unexpected aggregate diagnostics: " ^ Yojson.Safe.to_string report);
  require
    (report |> member "final_value" |> member "value"
    = `String (Int64.to_string expected))
    "independent aggregate word";
  require
    (member "output_hex" report = `String (hex output))
    "aggregate output bytes"

let error ?code output report =
  require
    (member "outcome" report = `String "error")
    "expected rejected aggregate report";
  Option.iter
    (fun code ->
      let first = report |> member "diagnostics" |> to_list |> List.hd in
      require
        (member "code" first = `String code)
        (Yojson.Safe.to_string report))
    code;
  require
    (member "output_hex" report = `String (hex output))
    "aggregate fault preserves reached output"

let () =
  List.iter
    (fun mode ->
      List.iter
        (fun target ->
          List.iter
            (fun (name, source, word, output) ->
              value
                ~unused_array:(name = "unused automatic aggregate array")
                word output
                (invoke target mode source))
            (Cases.values @ Cases.view_matrix @ Arrays.values
           @ Arrays.view_matrix @ Pointers.values @ Pointers.view_matrix
           @ Inherited.values @ Inherited.view_matrix @ Backed.values);
          value 42L "AB" (invoke target mode (read Sys.argv.(2)));
          value 42L "AB" (invoke target mode (read Sys.argv.(3)));
          value 42L "AB" (invoke target mode (read Sys.argv.(4)));
          value 42L "AB" (invoke target mode (read Sys.argv.(5)));
          value 42L "" (invoke target mode (read Sys.argv.(6)));
          if target = "host-jit" then
            error ~code:"HCPP0008" ""
              (invoke ~status:1 target mode Inherited.lookahead_source)
          else if mode = "jit" then
            error ~code:"HCSEMA0046" ""
              (invoke ~status:1 target mode Inherited.lookahead_source)
          else value 42L "" (invoke target mode Inherited.lookahead_source);
          List.iter
            (fun (_, source, code, output) ->
              error ~code output (invoke ~status:1 target mode source))
            (Cases.faults @ Arrays.faults @ Pointers.faults @ Inherited.faults
           @ Backed.faults);
          List.iter
            (fun (definition, bytes) ->
              value 42L ""
                (invoke target mode
                   (Cases.extent_source definition (bytes - 1)));
              error ~code:"HCIRVM0019" ""
                (invoke ~status:1 target mode
                   (Cases.extent_source definition bytes)))
            Cases.extents;
          List.iter
            (fun ((_, _, bytes) as extent) ->
              value 42L ""
                (invoke target mode (Arrays.extent_source extent (bytes - 1)));
              error ~code:"HCIRVM0019" ""
                (invoke ~status:1 target mode
                   (Arrays.extent_source extent bytes)))
            Arrays.extents;
          List.iter
            (fun (quota_source, frame_bytes) ->
              let baseline = invoke target mode quota_source in
              value 42L "" baseline;
              let steps = baseline |> member "executed_steps" |> to_int in
              value 42L ""
                (invoke
                   ~options:
                     [
                       "--frame-byte-limit=" ^ string_of_int frame_bytes;
                       "--step-limit=" ^ string_of_int steps;
                     ]
                   target mode quota_source);
              error ~code:"HCIRVM0011" ""
                (invoke ~status:1
                   ~options:
                     [ "--frame-byte-limit=" ^ string_of_int (frame_bytes - 1) ]
                   target mode quota_source);
              error ~code:"HCIRVM0007" ""
                (invoke ~status:1
                   ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
                   target mode quota_source))
            [
              (Cases.quota_source, 24);
              (Arrays.quota_source, 24);
              (Pointers.quota_source, Pointers.quota_frame_bytes);
              (Inherited.quota_source, Inherited.quota_frame_bytes);
              (Backed.quota_source, Backed.quota_frame_bytes);
            ];
          List.iter
            (fun (_, source) -> error "" (invoke ~status:1 target mode source))
            (Cases.unsupported @ Arrays.unsupported @ Pointers.unsupported
           @ Inherited.unsupported @ Backed.unsupported))
        (if native then [ "ir"; "host-jit" ] else [ "ir" ]))
    [ "jit"; "aot" ];
  if native then
    error "" (invoke ~status:1 "host-jit-task" "jit" Cases.quota_source);
  Printf.printf "Aggregate member CLI checks passed (%d reports).\n" !reports
