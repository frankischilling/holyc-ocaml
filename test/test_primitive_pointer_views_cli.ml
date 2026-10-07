open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc pointer views " suffix in
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
    "expected compiler and view example, with optional --native"

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)
let native = Array.length Sys.argv = 4 && Sys.argv.(3) = "--native"

let invoke ?(status = 0) ?(options = []) target mode path =
  let args =
    [
      "run";
      "--report-version=2";
      "--format=json";
      "--target=" ^ target;
      "--mode=" ^ mode;
    ]
    @ options @ [ path ]
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
                  (Array.of_list (compiler :: args))
                  Unix.stdin out_fd err_fd)
          in
          let _, actual = Unix.waitpid [] pid in
          let output = read stdout in
          require
            (actual = Unix.WEXITED status)
            ("unexpected CLI exit: " ^ output ^ read stderr);
          require (read stderr = "") "JSON diagnostics escaped to stderr";
          Yojson.Safe.from_string output))

let word bits report =
  require (member "outcome" report = `String "success") "successful view report";
  require (member "diagnostics" report = `List []) "successful view diagnostics";
  require (member "command_error" report = `Null) "successful view command";
  require
    (report |> member "final_value" |> member "value" = `String bits)
    ("unexpected final view word: " ^ Yojson.Safe.to_string report)

let error code report =
  require (member "outcome" report = `String "error") "failed view report";
  let first = report |> member "diagnostics" |> to_list |> List.hd in
  require
    (member "code" first = `String code)
    ("unexpected view error: " ^ Yojson.Safe.to_string report)

let () =
  List.iter
    (fun mode ->
      let success = invoke "ir" mode example in
      word "42" success;
      require (member "executed_steps" success = `Int 233) "view example steps";
      require
        (member "compiled_initializer_steps" success = `Int 0)
        "cast example has no constant preparation";
      require
        (member "output_hex" success = `String "4142"
        && member "output_byte_length" success = `Int 2
        && member "output_work" success = `Int 8)
        "exact view output";
      word "42"
        (invoke
           ~options:
             [
               "--step-limit=233";
               "--output-byte-limit=2";
               "--output-work-limit=8";
             ]
           "ir" mode example);
      error "HCIRVM0007"
        (invoke ~status:1 ~options:[ "--step-limit=232" ] "ir" mode example);
      let limited =
        invoke ~status:1 ~options:[ "--output-byte-limit=1" ] "ir" mode example
      in
      error "HCIRVM0022" limited;
      require
        (member "output_hex" limited = `String "")
        "existing string-field output admission";
      error "HCIRVM0023"
        (invoke ~status:1
           ~options:[ "--output-work-limit=7" ]
           "ir" mode example);
      word "42"
        (invoke
           ~options:[ "--frame-byte-limit=56"; "--call-depth-limit=2" ]
           "ir" mode example);
      error "HCIRVM0011"
        (invoke ~status:1
           ~options:[ "--frame-byte-limit=55" ]
           "ir" mode example);
      error "HCIRVM0015"
        (invoke ~status:1 ~options:[ "--call-depth-limit=1" ] "ir" mode example);
      List.iter
        (fun target ->
          if target = "host-jit-task" && mode = "aot" then (
            let report = invoke ~status:1 target mode example in
            error "HCRUN0001" report;
            require
              (member "executed_steps" report = `Int 0)
              "native source tasks require JIT")
          else
            let report = invoke target mode example in
            word "42" report;
            require
              (member "output_hex" report = `String "4142"
              && member "output_work" report = `Int 8)
              "native view byte scans retain exact output")
        (if native then [ "host-jit"; "host-jit-task" ] else []);
      with_file ".hc"
        "extern U0 Print(U8 *fmt,...);I64 F(){U64 n;U8 \
         *p=(&n)(U8*);p[0]=42;Print(\"kept\");return n;}F();" (fun path ->
          let report = invoke ~status:1 "ir" mode path in
          error "HCIRVM0012" report;
          require
            (member "output_hex" report = `String "6b657074")
            "reached initialization fault retains prior output");
      with_file ".hc"
        "I64 F(){U64 a[2];a[0]=0;a[1]=0;U8 *p=a(U8*);U64 \
         *q=(&p[7])(U64*);*q=0x0807060504030201;return \
         a[0]==0x0100000000000000&&a[1]==0x0008070605040302;}F();" (fun path ->
          word "1" (invoke "ir" mode path)))
    [ "jit"; "aot" ];
  Printf.printf "Primitive pointer view CLI checks passed (%d reports).\n"
    (if native then 24 else 20)
