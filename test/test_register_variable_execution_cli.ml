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
let native = Array.length Sys.argv = 3 && Sys.argv.(2) = "--native"

let run source expected target mode =
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
                (status = Unix.WEXITED 0
                && member "outcome" result = `String "success")
                (Yojson.Safe.to_string result);
              require
                (result |> member "final_value" |> member "value"
               = `String expected)
                (target ^ " " ^ mode ^ ": " ^ Yojson.Safe.to_string result))))

let () =
  let targets = if native then [ "host-jit"; "host-jit-task" ] else [ "ir" ] in
  let cases =
    [
      ("I64 F(I8 x,I8 c){x<<=c;return x;}F(127,1);", "254");
      ("I64 F(I8 noreg x,I8 c){x<<=c;return x;}F(127,1);", "-2");
      ("I64 F(I8 x,I8 c){I8 *p=&x;*p<<=c;return x;}F(127,1);", "-2");
      ("I64 F(U8 x,I8 c){x<<=c;return x;}F(255,1);", "510");
      ("I64 F(U8 noreg x,I8 c){x<<=c;return x;}F(255,1);", "254");
      ("I64 F(I8 x,I8 c){x<<=c;return x;}F(255,0);", "-1");
      ("I64 F(U8 x,I8 c){x<<=c;return x;}F(256,0);", "0");
      ( "I64 F(I8 x,I64 c,I64 a,I64 b,I64 d,I64 e,I64 f,I64 g,I64 h,I64 \
         i){x<<=c;return \
         x+a+a+a+b+b+b+d+d+d+e+e+e+f+f+f+g+g+g+h+h+h+i+i+i;}F(127,1,0,0,0,0,0,0,0,0);",
        "-2" );
    ]
  in
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          List.iter
            (fun (source, expected) -> run source expected target mode)
            cases)
        [ "jit"; "aot" ])
    targets;
  if native then
    List.iter
      (fun mode ->
        List.iter
          (fun (source, expected) -> run source expected "host-jit-task" mode)
          [
            ( "#exe {I64 F(I8 x,I8 c){x<<=c;return x;}I64 I(){return \
               F(127,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
              "254" );
            ( "#exe {I64 F(U8 x,I8 c){x<<=c;return x;}I64 I(){return \
               F(255,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
              "510" );
          ])
      [ "jit"; "aot" ];
  Printf.printf "Register execution checks passed (%d reports).\n"
    ((List.length targets * 2 * List.length cases) + if native then 4 else 0)
