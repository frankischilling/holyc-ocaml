open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc pointer difference cli " suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let () =
  require
    (Array.length Sys.argv = 3 || Array.length Sys.argv = 4)
    "expected compiler and maintained wide example"

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)
let native = Array.length Sys.argv = 4 && Sys.argv.(3) = "--native"

let invoke ~target ~mode ?(steps = 100_000) ?(preparation = 100_000)
    ?(status = 0) path =
  let arguments =
    [
      "run";
      "--report-version=2";
      "--format=json";
      "--target=" ^ target;
      "--mode=" ^ mode;
      "--step-limit=" ^ string_of_int steps;
      "--initializer-step-limit=" ^ string_of_int preparation;
      path;
    ]
  in
  with_file ".stdout" "" (fun stdout ->
      with_file ".stderr" "" (fun stderr ->
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
          let _, actual = Unix.waitpid [] pid in
          let output = read stdout in
          require
            (actual = Unix.WEXITED status)
            ("unexpected exit: " ^ output ^ read stderr);
          require (read stderr = "") "JSON command wrote diagnostics to stderr";
          Yojson.Safe.from_string output))

let output report hex bytes work =
  require (member "output_hex" report = `String hex) "exact captured bytes";
  require (member "output_byte_length" report = `Int bytes) "captured length";
  require (member "output_work" report = `Int work) "format work"

let error report code =
  require (member "outcome" report = `String "error") "expected fault outcome";
  require (member "final_value" report = `Null) "fault retained a final value";
  match member "diagnostics" report |> to_list with
  | first :: _ ->
      require (member "code" first = `String code) "fault diagnostic"
  | [] -> failwith "fault has no diagnostic"

let validate report ~steps ~prep ~hex ~bytes ~work =
  require (member "outcome" report = `String "success") "success outcome";
  require (member "executed_steps" report = `Int steps) "runtime work";
  require
    (member "compiled_initializer_steps" report = `Int prep)
    "preparation work";
  require
    (member "final_value" report |> member "value" = `String "42")
    "actual source value";
  require
    (member "final_value" report |> member "type" = `String "i64")
    "actual source result class";
  output report hex bytes work

let () =
  List.iter
    (fun mode ->
      validate
        (invoke ~target:"ir" ~mode ~steps:124 ~preparation:7 example)
        ~steps:124 ~prep:7 ~hex:"" ~bytes:0 ~work:7;
      error
        (invoke ~target:"ir" ~mode ~preparation:6 ~status:1 example)
        "HCIRVM0007";
      error
        (invoke ~target:"ir" ~mode ~steps:123 ~status:1 example)
        "HCIRVM0007";
      with_file ".hc"
        "#exe {I64 Q[4],N=0;I64 Init(){N++;return 45+(Q-(Q+3));}I64 Saved(I64 \
         x=Init()){return \
         x;}if(N!=1||Saved()!=42)Print(\"bad\");N=0;if(Saved()!=42||N)Print(\"bad\");StreamPrint(\"%d;\",Saved());}"
        (fun path ->
          let report = invoke ~target:"ir" ~mode path in
          require
            (member "outcome" report = `String "success")
            "negative wider difference prepares";
          require
            (member "final_value" report |> member "value" = `String "42")
            "saved signed element count";
          output report "" 0 7);
      List.iter
        (fun source ->
          with_file ".hc" source (fun path ->
              let report = invoke ~target:"ir" ~mode path in
              require
                (member "outcome" report = `String "success")
                "original outer division shift prepares";
              require
                (member "final_value" report |> member "value" = `String "42")
                "saved original element-count shift";
              output report "" 0 7))
        [
          "#exe {I64 Q[4];I64 Init(){return 41+((Q+3)-Q)/2;}I64 Saved(I64 \
           x=Init()){return x;}StreamPrint(\"%d;\",Saved());}";
          "#exe {U8 Q[4];I64 Init(){return 43+(Q-(Q+1))/2;}I64 Saved(I64 \
           x=Init()){return x;}StreamPrint(\"%d;\",Saved());}";
        ];
      List.iter
        (fun (source, code, capture) ->
          with_file ".hc" source (fun path ->
              let report = invoke ~target:"ir" ~mode ~status:1 path in
              error report code;
              require
                (member "output_hex" report = `String capture)
                "reached preparation bytes"))
        [
          ( "#exe {I64 Q[4];I64 Init(){Print(\"kept\");I64 *p;return p-Q;}I64 \
             Saved(I64 x=Init()){return x;}Saved();}",
            "HCIRVM0012",
            "6b657074" );
          ( "#exe {I64 Q[4];I64 Init(){Print(\"kept\");I64 r[4];return \
             Q-r;}I64 Saved(I64 x=Init()){return x;}Saved();}",
            "HCIRVM0018",
            "6b657074" );
        ];
      if native then begin
        error (invoke ~target:"host-jit" ~mode ~status:1 example) "HCPP0008";
        with_file ".hc" "I64 Q[4];I64 Saved(I64 x=(Q-Q)){return x;}Saved();"
          (fun path ->
            error (invoke ~target:"host-jit" ~mode ~status:1 path) "HCRUN0006")
      end)
    [ "jit"; "aot" ];
  print_endline "Prepared pointer difference CLI checks passed."
