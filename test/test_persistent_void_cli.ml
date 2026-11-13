open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let c = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in c)
    (fun () -> really_input_string c (in_channel_length c))

let with_file suffix f =
  let p = Filename.temp_file "holyc persistent void " suffix in
  Fun.protect ~finally:(fun () -> Sys.remove p) (fun () -> f p)

let compiler = Sys.argv.(1)
let native = Array.length Sys.argv = 3 && Sys.argv.(2) = "--native"
let reports = ref 0

let invoke target mode source options =
  with_file ".hc" (fun input ->
      let c = open_out_bin input in
      output_string c source;
      close_out c;
      with_file ".out" (fun output ->
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
                @ options @ [ input ]
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
              require (read errors = "") "JSON diagnostics escaped to stderr";
              let report = Yojson.Safe.from_string (read output) in
              incr reports;
              require
                (member "target" report = `String target)
                "requested executor";
              require
                (member "mode" report = `String mode)
                "requested outer mode";
              require
                (member "command_error" report = `Null)
                (Yojson.Safe.to_string report);
              require
                (member "output_hex" report = `String "")
                (Yojson.Safe.to_string report);
              (status, report))))

let success ?(warnings = 0) target mode source options =
  let status, report = invoke target mode source options in
  require
    (status = Unix.WEXITED 0 && member "outcome" report = `String "success")
    (Yojson.Safe.to_string report);
  require
    (report |> member "final_value" |> member "value" = `String "42")
    (Yojson.Safe.to_string report);
  require
    (let diagnostics = report |> member "diagnostics" |> to_list in
     List.length diagnostics = warnings
     && List.for_all
          (fun d ->
            member "code" d = `String "HCSEMA0079"
            && member "severity" d = `String "warning")
          diagnostics)
    (Yojson.Safe.to_string report);
  report

let rejected ?code target mode source options =
  let status, report = invoke target mode source options in
  require
    (status = Unix.WEXITED 1 && member "outcome" report = `String "error")
    (Yojson.Safe.to_string report);
  require
    (member "diagnostics" report <> `List [])
    "rejection requires diagnostics";
  List.iter
    (fun diagnostic ->
      let code = diagnostic |> member "code" |> to_string in
      require
        (not
           (String.starts_with ~prefix:"HCPARSE" code
           || String.starts_with ~prefix:"HCLEX" code))
        "unsupported U0 use must reach its semantic/execution boundary")
    (report |> member "diagnostics" |> to_list);
  Option.iter
    (fun expected ->
      require
        (report |> member "diagnostics" |> to_list
        |> List.exists (fun diagnostic ->
            member "code" diagnostic = `String expected))
        (Yojson.Safe.to_string report))
    code

let uninitialized =
  [
    "U0 first,second;I64 value=42;value+sizeof(first)+sizeof(second);";
    "I64 F(){static U0 first,second;return \
     21+sizeof(first)+sizeof(second);}F()+F();";
    "I64 a=40;U0 x;I64 b=2;a+b+sizeof(x);";
  ]

let initialized =
  [
    ( 1,
      "I64 n=0;U0 Get(){n++;}I64 Unused(){static U0 x=Get();return \
       sizeof(x);}n+41;" );
    (0, "U0 x=42;42;");
    (0, "I64 F(){static U0 x=42;return 21+sizeof(x);}F()+F();");
    (0, "I64 value=42;U0 x=&value;value;");
    (0, "I64 value=42;I64 F(){static U0 x=&value;return value+sizeof(x);}F();");
    (0, "U0 Fn(){}U0 x=&Fn;42;");
    (0, "U0 x=\"AB\";42;");
    (0, "I64 n=0;I64 Get(){n++;return 7;}U0 x=Get(),y=Get();n+40;");
    ( 0,
      "I64 n=0;I64 Get(){n++;return 7;}I64 F(){static U0 \
       x=Get(),y=Get();return n+sizeof(x)+sizeof(y)+19;}F()+F();" );
    (1, "I64 n=0;U0 Get(){n++;}U0 x=Get();n+41;");
    ( 1,
      "I64 n=0;U0 Get(){n++;}I64 F(){static U0 x=Get();return \
       n+sizeof(x)+20;}F()+F();" );
  ]

let unsupported =
  [
    "U0 x;x;";
    "U0 x;&x;";
    "U0 x[2];42;";
    "I64 F(){static U0 x[2];return 42;}F();";
    "I64 F(){U0 x=42;return 42;}F();";
  ]

let () =
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          List.iter
            (fun source -> ignore (success target mode source []))
            uninitialized;
          List.iter (fun source -> rejected target mode source []) unsupported;
          if mode = "aot" then
            List.iter
              (fun (_, source) ->
                if
                  source
                  = "I64 F(){static U0 x=42;return 21+sizeof(x);}F()+F();"
                then ignore (success target mode source [])
                else rejected ~code:"HCRUN0006" target mode source [])
              initialized)
        [ "jit"; "aot" ];
      if target <> "host-jit" then (
        List.iter
          (fun (warnings, source) ->
            ignore (success ~warnings target "jit" source []))
          initialized;
        rejected ~code:"HCIRVM0012" target "jit" "U0 x=42;I64 value;value;" [];
        let source =
          "U0 x=42;I64 answer=42;I64 F(){static U0 y=42;return \
           answer+sizeof(x)+sizeof(y);}F();"
        in
        let report = success target "jit" source [ "--global-byte-limit=8" ] in
        let steps = report |> member "executed_steps" |> to_int in
        let preparation =
          report |> member "compiled_initializer_steps" |> to_int
        in
        require
          (steps > 1 && preparation > 1)
          "real initializer execution and preparation";
        ignore
          (success target "jit" source
             [
               "--global-byte-limit=8";
               "--step-limit=" ^ string_of_int steps;
               "--initializer-step-limit=" ^ string_of_int preparation;
             ]);
        rejected ~code:"HCIRVM0016" target "jit" source
          [ "--global-byte-limit=7" ];
        rejected ~code:"HCIRVM0007" target "jit" source
          [ "--step-limit=" ^ string_of_int (steps - 1) ];
        rejected ~code:"HCIRVM0007" target "jit" source
          [ "--initializer-step-limit=" ^ string_of_int (preparation - 1) ]))
    (if native then [ "host-jit-task"; "host-jit" ] else [ "ir" ]);
  Printf.printf "Persistent U0 CLI passed: %d reports\n" !reports
