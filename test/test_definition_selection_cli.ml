open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-definition-selection-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      output_string channel contents;
      close_out channel;
      action path)

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)

let targets =
  if Array.exists (( = ) "--native") Sys.argv then [ "host-jit-task" ]
  else [ "ir" ]

let count = ref 0

let invoke mode target path =
  temporary ".out" "" (fun output ->
      temporary ".err" "" (fun error ->
          let out_fd =
            Unix.openfile output [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let err_fd =
            Unix.openfile error [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let args =
            [
              compiler;
              "run";
              "--format=json";
              "--mode=" ^ mode;
              "--target=" ^ target;
              "--code-byte-limit=1048576";
              path;
            ]
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
          let text = read output in
          require (status = Unix.WEXITED 0) (text ^ read error);
          require
            (read error = "")
            "definition selection wrote JSON diagnostics to stderr";
          let report = Yojson.Safe.from_string text in
          require
            (report |> member "final_value" |> member "value" |> to_string
           = "42")
            "definition expanded despite selected original member";
          require
            (report |> member "diagnostics" |> to_list = [])
            "unexpected definition-selection diagnostics";
          incr count;
          report))

let () =
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          let report = invoke mode target example in
          require
            (report |> member "output_hex" |> to_string = "34323b34323b")
            "directive local/parameter selection changed")
        [ "jit"; "aot" ];
      List.iter
        (fun text ->
          temporary ".hc" text (fun path -> ignore (invoke "jit" target path)))
        [
          "I64 F(){I64 n=42;\n#define n 7\nreturn n;}F();";
          "I64 F(I64 n){\n#define n 7\nreturn n;}F(42);";
        ])
    targets;
  Printf.printf "%d definition selection CLI executions passed\n%!" !count
