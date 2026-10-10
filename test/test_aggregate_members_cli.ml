open Yojson.Safe.Util
module Cases = Aggregate_member_cases
module Arrays = Aggregate_array_cases
module Pointers = Aggregate_pointer_cases
module Inherited = Inherited_aggregate_cases
module Backed = Backed_aggregate_cases
module Default = Default_aggregate_cases
module Parameters = Class_parameter_cases
module Returns = Class_return_cases
module Defaults = Class_default_cases

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
    (Array.length Sys.argv = 11 || Array.length Sys.argv = 12)
    "expected compiler and nine aggregate examples, optionally --native"

let compiler = Sys.argv.(1)
let native = Array.length Sys.argv = 12 && Sys.argv.(11) = "--native"
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

let value ?(warnings = []) ?(unused_array = false) ?(return_warning = false)
    expected output report =
  require
    (member "outcome" report = `String "success")
    (Yojson.Safe.to_string report);
  if warnings <> [] then (
    let actual = member "diagnostics" report |> to_list in
    require
      (List.length actual = List.length warnings)
      "prototype warning count";
    List.iter2
      (fun (code, message) warning ->
        require
          (member "code" warning = `String code
          && member "severity" warning = `String "warning"
          && member "message" warning = `String message)
          "prototype keeps its original header warning")
      warnings actual)
  else if return_warning then (
    let diagnostics = member "diagnostics" report |> to_list in
    require (List.length diagnostics = 1) "empty class return warning count";
    let warning = List.hd diagnostics in
    require
      (member "code" warning = `String "HCSEMA0078"
      && member "severity" warning = `String "warning"
      && member "message" warning = `String "Function should NOT return val")
      "empty class return keeps its original warning")
  else if unused_array then (
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
    = `String
        (if report |> member "final_value" |> member "type" = `String "u64" then
           Printf.sprintf "%Lu" expected
         else Int64.to_string expected))
    "independent aggregate word";
  require
    (report |> member "final_value" |> member "bits"
    = `String (Printf.sprintf "0x%016Lx" expected))
    "independent aggregate word bits";
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
                ~return_warning:
                  (List.exists
                     (fun (warning_name, _, _, _) -> warning_name = name)
                     Returns.warning_values)
                word output
                (invoke target mode source))
            (Cases.values @ Cases.view_matrix @ Arrays.values
           @ Arrays.view_matrix @ Pointers.values @ Pointers.view_matrix
           @ Inherited.values @ Inherited.view_matrix @ Backed.values
           @ Default.values @ Parameters.values @ Parameters.view_matrix
           @ Returns.values @ Returns.view_matrix @ Returns.warning_values);
          value 42L "AB" (invoke target mode (read Sys.argv.(2)));
          value 42L "AB" (invoke target mode (read Sys.argv.(3)));
          value 42L "AB" (invoke target mode (read Sys.argv.(4)));
          value 42L "AB" (invoke target mode (read Sys.argv.(5)));
          value 42L "" (invoke target mode (read Sys.argv.(6)));
          value 42L "" (invoke target mode (read Sys.argv.(7)));
          value 42L "" (invoke target mode (read Sys.argv.(8)));
          value 42L "" (invoke target mode (read Sys.argv.(9)));
          value 42L "" (invoke target mode (read Sys.argv.(10)));
          List.iter
            (fun (_, source, expected, output) ->
              value expected output (invoke target mode source))
            Defaults.native_values;
          List.iter
            (fun (_, source) -> error "" (invoke ~status:1 target mode source))
            (if target = "host-jit" then Defaults.native_unsupported
             else if mode = "jit" then Defaults.jit_unsupported
             else Defaults.unsupported);
          if target = "ir" then
            List.iteri
              (fun index (_, source, expected, output) ->
                let warnings =
                  if mode <> "jit" then []
                  else
                    (if index = 0 then
                       [ ("HCSEMA0075", "Unused extern 'Take'") ]
                     else [])
                    @ [
                        ( "HCSEMA0038",
                          "function \"Take\" argument list does not match the \
                           replaced header" );
                      ]
                in
                value ~warnings expected output (invoke target mode source))
              Defaults.prototype_values
          else
            List.iter
              (fun (_, source, _, _) ->
                error "" (invoke ~status:1 target mode source))
              Defaults.prototype_values;
          error ~code:"HCIRVM0019" ""
            (invoke ~status:1 target mode Defaults.extent_source);
          value 42L ""
            (invoke
               ~options:[ "--initializer-step-limit=5" ]
               target mode Defaults.quota_source);
          error ~code:"HCIRVM0007" ""
            (invoke ~status:1
               ~options:[ "--initializer-step-limit=4" ]
               target mode Defaults.quota_source);
          if target = "host-jit" then
            error ~code:"HCPP0008" ""
              (invoke ~status:1 target mode Inherited.lookahead_source)
          else value 42L "" (invoke target mode Inherited.lookahead_source);
          if target = "ir" && mode = "jit" then
            List.iter
              (fun (_, source, expected, output) ->
                value expected output (invoke target mode source))
              (Defaults.jit_values @ Inherited.retained_values);
          List.iter
            (fun (_, source, code, output) ->
              error ~code output (invoke ~status:1 target mode source))
            (Cases.faults @ Arrays.faults @ Pointers.faults @ Inherited.faults
           @ Backed.faults @ Default.faults @ Parameters.faults @ Returns.faults
            );
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
              (Default.quota_source, Default.quota_frame_bytes);
              (Parameters.quota_source, Parameters.quota_frame_bytes);
              (Returns.quota_source, Returns.quota_frame_bytes);
            ];
          List.iter
            (fun (_, source) -> error "" (invoke ~status:1 target mode source))
            (Cases.unsupported @ Arrays.unsupported @ Pointers.unsupported
            @ (if target = "ir" && mode = "jit" then Inherited.unsupported
               else Inherited.source_unsupported)
            @ Backed.unsupported @ Default.unsupported @ Parameters.unsupported
            @ Returns.unsupported))
        (if native then [ "ir"; "host-jit" ] else [ "ir" ]))
    [ "jit"; "aot" ];
  if native then (
    let invoke_task ?(status = 0) ?(options = []) source =
      invoke ~status
        ~options:("--code-byte-limit=524288" :: options)
        "host-jit-task" "jit" source
    in
    let task_values =
      Cases.values @ Cases.view_matrix @ Arrays.values @ Arrays.view_matrix
      @ Pointers.values @ Pointers.view_matrix @ Inherited.values
      @ Inherited.view_matrix @ Inherited.retained_values @ Backed.values
      @ Default.values @ Parameters.values @ Parameters.view_matrix
      @ Returns.values @ Returns.view_matrix @ Returns.warning_values
      @ Defaults.native_values @ Defaults.jit_values
    in
    List.iter
      (fun (name, source, expected, output) ->
        if List.mem name Cases.retained_nested_class_boundaries then
          error ~code:"HCRUN0001" "" (invoke_task ~status:1 source)
        else
          value
            ~unused_array:(name = "unused automatic aggregate array")
            ~return_warning:
              (List.exists
                 (fun (n, _, _, _) -> n = name)
                 Returns.warning_values)
            expected output (invoke_task source))
      task_values;
    let _, prototype, expected, output = List.hd Defaults.prototype_values in
    value
      ~warnings:
        [
          ("HCSEMA0075", "Unused extern 'Take'");
          ( "HCSEMA0038",
            "function \"Take\" argument list does not match the replaced header"
          );
        ]
      expected output (invoke_task prototype);
    let _, earlier, _, _ = List.nth Defaults.prototype_values 1 in
    error ~code:"HCBACK0002" "" (invoke_task ~status:1 earlier);
    value 42L "" (invoke_task Cases.quota_source);
    error ~code:"HCIRVM0019" "" (invoke_task ~status:1 Defaults.extent_source);
    value 42L ""
      (invoke_task
         ~options:[ "--initializer-step-limit=5" ]
         Defaults.quota_source);
    error ~code:"HCIRVM0007" ""
      (invoke_task ~status:1
         ~options:[ "--initializer-step-limit=4" ]
         Defaults.quota_source));
  Printf.printf "Aggregate member CLI checks passed (%d reports).\n" !reports
