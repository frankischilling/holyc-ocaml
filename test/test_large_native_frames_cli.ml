open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix f =
  let path = Filename.temp_file "holyc register execution " suffix in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)

let compiler = Sys.argv.(1)
let reports = ref 0

let run ?(fault = false) ?(options = []) source expected target mode =
  incr reports;
  with_file ".hc" (fun path ->
      let channel = open_out_bin path in
      output_string channel source;
      close_out channel;
      with_file ".json" (fun output ->
          with_file ".err" (fun errors ->
              let out_fd =
                Unix.openfile output [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              and err_fd =
                Unix.openfile errors [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let args =
                [|
                  compiler;
                  "run";
                  "--format=json";
                  "--target=" ^ target;
                  "--mode=" ^ mode;
                  path;
                |]
                |> fun args -> Array.append args (Array.of_list options)
              in
              let pid =
                Fun.protect
                  ~finally:(fun () ->
                    Unix.close out_fd;
                    Unix.close err_fd)
                  (fun () ->
                    Unix.create_process compiler args Unix.stdin out_fd err_fd)
              in
              let _, status = Unix.waitpid [] pid in
              require (read errors = "") "unexpected stderr";
              let result = Yojson.Safe.from_string (read output) in
              require
                (status = Unix.WEXITED (if fault then 1 else 0)
                && member "outcome" result
                   = `String (if fault then "error" else "success"))
                (Yojson.Safe.to_string result);
              if fault then
                require
                  (result |> member "diagnostics" |> to_list
                   |> List.exists (fun diagnostic ->
                       member "code" diagnostic = `String expected)
                  ||
                  match member "command_error" result with
                  | `Assoc fields ->
                      List.assoc_opt "code" fields = Some (`String expected)
                  | _ -> false)
                  (target ^ " " ^ mode ^ ": " ^ Yojson.Safe.to_string result)
              else
                require
                  (result |> member "final_value" |> member "value"
                 = `String expected)
                  (target ^ " " ^ mode ^ ": " ^ Yojson.Safe.to_string result))))

let () =
  let ordinary =
    "I64 F(){I64 a[1024];a[0]=20;a[1023]=22;return a[0]+a[1023];}F();"
  in
  let options = [ "--stack-byte-limit=32768" ] in
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          let large =
            "I64 F(){I64 a[5000];a[0]=20;a[4999]=22;return a[0]+a[4999];}F();"
          in
          let large_options =
            [
              "--stack-byte-limit=131072";
              "--code-byte-limit=262144";
              "--active-stack-byte-limit=262144";
            ]
          in
          run ~options:large_options large "42" target mode;
          let compile_options =
            [ "--stack-byte-limit=131072"; "--code-byte-limit=262144" ]
          in
          run ~fault:true ~options:compile_options large "HCNATIVE0006" target
            mode;
          run
            ~options:(compile_options @ [ "--active-stack-byte-limit=80320" ])
            large "42" target mode;
          run ~fault:true
            ~options:(compile_options @ [ "--active-stack-byte-limit=80319" ])
            large "HCNATIVE0006" target mode;
          run
            ~options:
              [
                "--stack-byte-limit=80144";
                "--code-byte-limit=262144";
                "--active-stack-byte-limit=262144";
              ]
            large "42" target mode;
          run ~fault:true
            ~options:
              [
                "--stack-byte-limit=80143";
                "--code-byte-limit=262144";
                "--active-stack-byte-limit=262144";
              ]
            large "HCBACK0004" target mode;
          run ~fault:true
            ~options:
              [
                "--stack-byte-limit=131072";
                "--code-byte-limit=4096";
                "--active-stack-byte-limit=262144";
              ]
            large "HCBACK0005" target mode;
          run ~fault:true
            ~options:(compile_options @ [ "--active-stack-byte-limit=524289" ])
            large "HCIRVM0001" target mode;
          run ~options:large_options
            (read "../examples/compact-native-frame.hc")
            "42" target mode;
          run ~fault:true large "HCBACK0004" target mode;
          run
            ~options:(large_options @ [ "--frame-byte-limit=40000" ])
            large "42" target mode;
          run ~fault:true
            ~options:(large_options @ [ "--frame-byte-limit=39999" ])
            large "HCIRVM0011" target mode;
          run ~options:large_options
            "I64 F(I64 n){I64 a[5000];a[0]=n;a[4999]=21;if(n)return \
             F(n-1)+a[0];return a[4999];}F(1);"
            "22" target mode;
          run ~options:large_options
            "I64 F(){I64 a[5000];a[0]=20;a[4999]=22;return a[0]+a[4999];}I64 \
             sum=0,i;for(i=0;i<5;i++)sum+=F();sum;"
            "210" target mode;
          run ~fault:true ~options:large_options
            "I64 F(I64 n){I64 a[5000];a[0]=n;if(n)return F(n-1);return \
             a[0]/n;}F(1);"
            "HCIRVM0009" target mode;
          run ~fault:true ~options:large_options
            "I64 F(){I64 a[5000];return a[4999];}F();" "HCIRVM0012" target mode;
          run ~options ordinary "42" target mode;
          run
            ~options:[ "--stack-byte-limit=65528"; "--code-byte-limit=262144" ]
            "I64 F(){I64 a[4000];a[0]=20;a[3999]=22;return a[0]+a[3999];}F();"
            "42" target mode;
          run ~options
            (read "../examples/large-native-frame.hc")
            "42" target mode;
          run ~options:[ "--stack-byte-limit=16528" ] ordinary "42" target mode;
          run ~fault:true
            ~options:[ "--stack-byte-limit=16527" ]
            ordinary "HCBACK0004" target mode;
          run ~fault:true ordinary "HCBACK0004" target mode;
          run ~options
            "I64 F(I64 n){I64 a[512];a[0]=n;a[511]=21;if(n)return \
             F(n-1)+a[0];return a[511];}F(2);"
            "24" target mode;
          run ~options
            "I64 F(){I64 a[1024];a[0]=20;a[1023]=22;return a[0]+a[1023];}I64 \
             sum=0,i;for(i=0;i<50;i++)sum+=F();sum;"
            "2100" target mode;
          run ~fault:true ~options
            "I64 F(I64 n){I64 a[512];a[0]=n;if(n)return F(n-1);return \
             a[0]/n;}F(2);"
            "HCIRVM0009" target mode;
          run ~fault:true ~options "I64 F(){I64 a[1024];return a[1023];}F();"
            "HCIRVM0012" target mode;
          run
            ~options:(options @ [ "--frame-byte-limit=8192" ])
            ordinary "42" target mode;
          run ~fault:true
            ~options:(options @ [ "--frame-byte-limit=8191" ])
            ordinary "HCIRVM0011" target mode;
          if target = "host-jit" then (
            run
              ~options:(options @ [ "--active-stack-byte-limit=16704" ])
              ordinary "42" target mode;
            run ~fault:true
              ~options:(options @ [ "--active-stack-byte-limit=16703" ])
              ordinary "HCNATIVE0006" target mode))
        [ "jit"; "aot" ])
    [ "host-jit"; "host-jit-task" ];
  Printf.printf "Large native frame CLI checks passed (%d reports).\n" !reports
