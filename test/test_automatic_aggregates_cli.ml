open Yojson.Safe.Util
module Cases = Automatic_aggregate_cases

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
    (Array.length Sys.argv = 3 || Array.length Sys.argv = 4)
    "expected compiler and example, optionally --native"

let compiler = Sys.argv.(1)
let native = Array.length Sys.argv = 4 && Sys.argv.(3) = "--native"
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

let value expected output report =
  require
    (member "outcome" report = `String "success")
    (Yojson.Safe.to_string report);
  require
    (member "diagnostics" report = `List [])
    "unexpected aggregate diagnostics";
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
            (fun (_, source, word, output) ->
              value word output (invoke target mode source))
            (Cases.values @ Cases.view_matrix);
          value 42L "AB" (invoke target mode (read Sys.argv.(2)));
          List.iter
            (fun (_, source, code, output) ->
              error ~code output (invoke ~status:1 target mode source))
            Cases.faults;
          List.iter
            (fun (definition, bytes) ->
              value 42L ""
                (invoke target mode
                   (Cases.extent_source definition (bytes - 1)));
              error ~code:"HCIRVM0019" ""
                (invoke ~status:1 target mode
                   (Cases.extent_source definition bytes)))
            Cases.extents;
          let baseline = invoke target mode Cases.quota_source in
          value 42L "" baseline;
          let steps = baseline |> member "executed_steps" |> to_int in
          value 42L ""
            (invoke
               ~options:
                 [
                   "--frame-byte-limit=24";
                   "--step-limit=" ^ string_of_int steps;
                 ]
               target mode Cases.quota_source);
          error ~code:"HCIRVM0011" ""
            (invoke ~status:1
               ~options:[ "--frame-byte-limit=23" ]
               target mode Cases.quota_source);
          error ~code:"HCIRVM0007" ""
            (invoke ~status:1
               ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
               target mode Cases.quota_source);
          List.iter
            (fun (_, source) -> error "" (invoke ~status:1 target mode source))
            Cases.unsupported)
        (if native then [ "ir"; "host-jit" ] else [ "ir" ]))
    [ "jit"; "aot" ];
  if native then
    error "" (invoke ~status:1 "host-jit-task" "jit" Cases.quota_source);
  Printf.printf "Automatic aggregate CLI checks passed (%d reports).\n" !reports
