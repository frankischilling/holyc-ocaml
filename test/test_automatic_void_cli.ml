open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let c = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in c)
    (fun () -> really_input_string c (in_channel_length c))

let with_file suffix f =
  let p = Filename.temp_file "holyc void local " suffix in
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

let success target mode source options =
  let status, report = invoke target mode source options in
  require
    (status = Unix.WEXITED 0 && member "outcome" report = `String "success")
    (Yojson.Safe.to_string report);
  require
    (report |> member "final_value" |> member "value" = `String "42")
    (Yojson.Safe.to_string report);
  require
    (member "diagnostics" report = `List [])
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
  Option.iter
    (fun expected ->
      require
        (report |> member "diagnostics" |> to_list
        |> List.exists (fun diagnostic ->
            member "code" diagnostic = `String expected))
        (Yojson.Safe.to_string report))
    code

let values =
  [
    "I64 F(){U0 x;return sizeof(x)+42;}F();";
    "I64 F(){U0 x,y,z;return sizeof(x)+sizeof(y)+sizeof(z)+42;}F();";
    "I64 F(){I64 before=40;U0 x;I64 after=2;return before+after+sizeof(x);}F();";
    "I64 F(){U0 x;I64 after=42;return after+sizeof(x);}F();";
    "I64 F(){U0 x;{U0 y;return sizeof(x)+sizeof(y)+42;}}F();";
    "I64 F(I64 n){U0 x;if(n)return F(n-1)+sizeof(x);return 42;}F(4);";
    "I64 F(){U0 x;return sizeof(x)+21;}F()+F();";
  ]

let unsupported =
  [
    "I64 Next(){PutChars(65);return 42;}I64 F(){U0 x=Next();return 42;}F();";
    "U0 Next(){PutChars(65);}I64 F(){U0 x=Next();return 42;}F();";
    "I64 F(){U0 x;return x;}F();";
    "I64 F(){U0 x;U0 *p=&x;return *p;}F();";
    "I64 F(){U0 x[1];return sizeof(x)+42;}F();";
    "I64 Next(){PutChars(65);return 42;}I64 F(){U0 x;x=Next();return 42;}F();";
  ]

let () =
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          List.iter
            (fun source -> ignore (success target mode source []))
            values;
          List.iter (fun source -> rejected target mode source []) unsupported;
          let source = List.hd values in
          let report = success target mode source [ "--frame-byte-limit=1" ] in
          let steps = report |> member "executed_steps" |> to_int in
          require (steps > 1) "real declaration/function execution";
          ignore
            (success target mode source
               [ "--step-limit=" ^ string_of_int steps ]);
          rejected ~code:"HCIRVM0007" target mode source
            [ "--step-limit=" ^ string_of_int (steps - 1) ])
        [ "jit"; "aot" ])
    (if native then [ "host-jit-task"; "host-jit" ] else [ "ir" ]);
  Printf.printf "Automatic U0 declaration CLI passed: %d reports\n" !reports
