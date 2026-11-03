open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let c = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in c)
    (fun () -> really_input_string c (in_channel_length c))

let with_file suffix f =
  let path = Filename.temp_file "holyc source root shift " suffix in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)

let compiler = Sys.argv.(1)
let native = Array.length Sys.argv = 3 && Sys.argv.(2) = "--native"
let reports = ref 0

let invoke contents target mode options =
  with_file ".hc" (fun source ->
      let c = open_out_bin source in
      output_string c contents;
      close_out c;
      with_file ".json" (fun output ->
          with_file ".err" (fun errors ->
              let out_fd =
                Unix.openfile output [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              and err_fd =
                Unix.openfile errors [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let args =
                [
                  compiler;
                  "run";
                  "--format=json";
                  "--target=" ^ target;
                  "--mode=" ^ mode;
                  "--code-byte-limit=524288";
                ]
                @ options @ [ source ]
              in
              let pid =
                Fun.protect
                  ~finally:(fun () ->
                    Unix.close out_fd;
                    Unix.close err_fd)
                  (fun () ->
                    Unix.create_process compiler (Array.of_list args) Unix.stdin
                      out_fd err_fd)
              in
              let _, status = Unix.waitpid [] pid in
              require
                (read errors = "")
                "root shift JSON diagnostics escaped to stderr";
              incr reports;
              (status, Yojson.Safe.from_string (read output)))))

let success source expected target mode options =
  let status, report = invoke source target mode options in
  require
    (status = Unix.WEXITED 0 && member "outcome" report = `String "success")
    (Yojson.Safe.to_string report);
  require
    (report |> member "final_value" |> member "value" = `String expected)
    "once-only source update and retained value";
  require
    (member "diagnostics" report = `List [])
    "root shift emitted no diagnostics";
  require
    (member "output_hex" report = `String "")
    "root shift emitted no output";
  report

let failure source target mode options codes =
  let status, report = invoke source target mode options in
  require
    (status = Unix.WEXITED 1 && member "outcome" report = `String "error")
    (Yojson.Safe.to_string report);
  require
    (report |> member "diagnostics" |> to_list
    |> List.exists (fun d -> List.mem (member "code" d |> to_string) codes))
    (Yojson.Safe.to_string report)

let () =
  let cases =
    if native then
      [
        ("host-jit-task", "jit"); ("host-jit-task", "aot"); ("host-jit", "aot");
      ]
    else [ ("ir", "jit"); ("ir", "aot") ]
  in
  let sources =
    [
      ( "I64 N=7;I64 O=(++N)<<1;I64 F(){if(N!=8)return -1;N=20;return O;}F();",
        "16",
        16 );
      ( "I64 N=7;I64 F(){static I64 O=(++N)<<1;return O;}I64 Before=N;I64 \
         First=F();N=20;First*100+F()+Before;",
        "1624",
        32 );
      ( "I64 N=7,C=65;I64 F(){static I64 O=N<<C;return O;}I64 A=F();C=1;A+F();",
        "28",
        32 );
    ]
  in
  List.iter
    (fun (source, expected, bytes) ->
      List.iter
        (fun (target, mode) ->
          let baseline = success source expected target mode [] in
          let steps = baseline |> member "executed_steps" |> to_int
          and preparation =
            baseline |> member "compiled_initializer_steps" |> to_int
          in
          require
            (steps > 1 && preparation > 1)
            "root shift uses actual execution and preparation work";
          ignore
            (success source expected target mode
               [
                 "--step-limit=" ^ string_of_int steps;
                 "--initializer-step-limit=" ^ string_of_int preparation;
                 "--global-byte-limit=" ^ string_of_int bytes;
               ]);
          failure source target mode
            [ "--step-limit=" ^ string_of_int (steps - 1) ]
            [ "HCIRVM0007" ];
          failure source target mode
            [ "--initializer-step-limit=" ^ string_of_int (preparation - 1) ]
            [ "HCIRVM0007" ];
          failure source target mode
            [ "--global-byte-limit=" ^ string_of_int (bytes - 1) ]
            [ "HCIRVM0016"; "HCBACK0001" ])
        cases)
    sources;
  Printf.printf "Root initializer shift CLI passed: %d reports\n" !reports
