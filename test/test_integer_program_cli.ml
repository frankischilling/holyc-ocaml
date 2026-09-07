let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc-program-test-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let compiler = Sys.argv.(1)

let invoke arguments =
  with_file ".stdout" "" (fun stdout ->
      with_file ".stderr" "" (fun stderr ->
          let out_fd =
            Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let err_fd =
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
          (status, read stdout, read stderr)))

let require condition message = if not condition then failwith message

let success arguments =
  let status, stdout, stderr = invoke arguments in
  require (status = Unix.WEXITED 0) ("command failed: " ^ stderr);
  require (stderr = "") ("unexpected diagnostics: " ^ stderr);
  stdout

let () =
  let open Yojson.Safe.Util in
  let fixture = Sys.argv.(2) in
  let output =
    success [ "run"; "--target=ir"; "--format=json"; fixture ]
    |> Yojson.Safe.from_string
  in
  let string name = output |> member name |> to_string in
  require (string "schema" = "holyc-integer-program-v1") "result schema";
  require
    (string "implementation_commit" = Holyc_lib.Version.implementation_commit)
    "program report must identify the compiler build";
  require
    (string "reference_commit" = Holyc_lib.Version.reference_commit)
    "program report must identify the pinned reference";
  require (string "mode" = "jit" && string "target" = "ir") "execution mode";
  require (string "arithmetic" = "runtime-ir") "arithmetic boundary";
  require (string "termination" = "stream-end") "stream termination";
  require
    (output |> member "executed_steps" |> to_int = 23)
    "executed path length";
  require (output |> member "step_limit" |> to_int = 100000) "reported budget";
  require
    (success [ "dump-ir"; "--program"; fixture ]
    = success [ "dump-ir"; "--program"; fixture ])
    "deterministic program IR";
  let human = success [ "run"; "--target=ir"; fixture ] in
  require
    (String.starts_with ~prefix:"holyc-integer-program-v1" human)
    "human report";
  require
    (List.mem "step-limit=100000"
       (String.split_on_char '\n' human |> List.map String.trim))
    "human report must retain the execution budget";
  with_file ".hc" "if(1) 1/0;" (fun source ->
      let status, stdout, stderr = invoke [ "run"; "--format=json"; source ] in
      require
        (status = Unix.WEXITED 1 && stdout = "")
        "fault must not expose a result";
      let diagnostics = Yojson.Safe.from_string stderr |> to_list in
      let diagnostic = List.hd diagnostics in
      require
        (diagnostic |> member "code" |> to_string = "HCIRVM0009")
        "fault diagnostic";
      let notes =
        diagnostic |> member "notes" |> to_list |> List.map to_string
      in
      require
        (List.exists (String.starts_with ~prefix:"executed_steps=") notes)
        "fault must retain execution progress");
  let status, stdout, _ = invoke [ "run"; "--step-limit=22"; fixture ] in
  require
    (status = Unix.WEXITED 1 && stdout = "")
    "budget failure exit and stdout";
  ignore (success [ "run"; "--step-limit=23"; fixture ]);
  List.iter
    (fun command ->
      with_file ".hc" "#assert 0\n42;" (fun source ->
          let status, stdout, stderr = invoke (command @ [ source ]) in
          require
            (status = Unix.WEXITED 0 && stdout <> "")
            "warning permits success";
          require (stderr <> "")
            "successful source command must retain parser warnings";
          if List.mem "--format=json" command then (
            let diagnostics = Yojson.Safe.from_string stderr |> to_list in
            require (List.length diagnostics = 1) "one retained warning";
            require
              (List.hd diagnostics |> member "code" |> to_string = "HCPP0024")
              "retained assertion warning")))
    [ [ "run"; "--format=json" ]; [ "dump-ir"; "--program" ] ];
  with_file ".hc" "#assert 0\n1/0;" (fun source ->
      let status, stdout, stderr = invoke [ "run"; "--format=json"; source ] in
      require
        (status = Unix.WEXITED 1 && stdout = "")
        "warning then runtime fault";
      let codes =
        Yojson.Safe.from_string stderr
        |> to_list
        |> List.map (fun item -> item |> member "code" |> to_string)
      in
      require
        (codes = [ "HCPP0024"; "HCIRVM0009" ])
        "warnings and errors must share one diagnostic array");
  let status, stdout, stderr = invoke [ "run"; "--target=host-jit"; fixture ] in
  require (status = Unix.WEXITED 1 && stdout = "") "unsupported native target";
  require
    (String.starts_with ~prefix:"holyc: run: HCRUN0005" stderr)
    "target diagnostic";
  List.iter
    (fun mode ->
      let result =
        success
          [
            "run";
            "--target=ir";
            "--format=json";
            "--mode=" ^ mode;
            Sys.argv.(3);
          ]
        |> Yojson.Safe.from_string
      in
      let value = result |> member "final_value" in
      require
        (value |> member "value" |> to_string = "42")
        "original source function must return 42";
      require
        (value |> member "type" |> to_string = "i64")
        "source function result class";
      require
        (value |> member "bits" |> to_string = "0x000000000000002a")
        "source function result bits";
      require
        (result |> member "termination" |> to_string = "stream-end")
        "caller resumes and completes the source stream")
    [ "jit"; "aot" ];
  print_endline "Integer program CLI checks passed."
