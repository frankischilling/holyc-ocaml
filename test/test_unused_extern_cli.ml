open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-unused-extern-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      output_string channel contents;
      close_out channel;
      action path)

let compiler = Sys.argv.(1)

let targets =
  if Array.exists (( = ) "--native") Sys.argv then [ "host-jit-task" ]
  else [ "ir" ]

let count = ref 0

let invoke ?(status = 0) mode target path =
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
          let _, reached = Unix.waitpid [] pid in
          incr count;
          let text = read output in
          require (reached = Unix.WEXITED status) (text ^ read error);
          require (read error = "") "unused-extern JSON wrote stderr";
          Yojson.Safe.from_string text))

let warnings report =
  report |> member "diagnostics" |> to_list
  |> List.filter (fun d -> d |> member "severity" |> to_string = "warning")
  |> List.map (fun d ->
      (d |> member "code" |> to_string, d |> member "message" |> to_string))

let unused = ("HCSEMA0075", "Unused extern 'OwnedExternUnused'")

let success expected report =
  require
    (report |> member "final_value" |> member "value" |> to_string = "42")
    "unused-extern warning changed execution";
  require
    (warnings report = expected)
    (Printf.sprintf "unused-extern execution %d: expected [%s], received [%s]"
       !count
       (String.concat "; " (List.map snd expected))
       (String.concat "; " (List.map snd (warnings report))))

let () =
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          let report = invoke mode target Sys.argv.(2) in
          success [ unused ] report;
          require
            (report |> member "output_hex" |> to_string = "34323b")
            "warning entered program output";
          List.iter
            (fun (source, expected) ->
              temporary ".hc" source (fun path ->
                  success expected (invoke mode target path)))
            [
              ( "#exe {extern I64 OwnedExternUnused(); extern I64 \
                 OwnedExternUnused(); extern I64 OwnedExternUnused();}42;",
                [ unused; unused ] );
              ( "#exe {extern I64 OwnedExternUnused();\n\
                 #if defined(OwnedExternUnused)\n\
                 #endif\n\
                 extern I64 OwnedExternUnused();}42;",
                [] );
              ( "#exe {extern I64 OwnedExternUnused();\n\
                 #define DEFERRED OwnedExternUnused\n\
                 extern I64 OwnedExternUnused();}42;",
                [ unused ] );
              ( "#exe {extern I64 OwnedExternUnused();\n\
                 #if 0\n\
                 0; OwnedExternUnused\n\
                 #endif\n\
                 extern I64 OwnedExternUnused();}42;",
                [ unused ] );
              ( "#exe {extern I64 OwnedExternUnused();\n\
                 #if 0\n\
                 OwnedExternUnused\n\
                 #endif\n\
                 extern I64 OwnedExternUnused();}42;",
                [] );
              ("#exe {U0 OwnedExternUnused(); U0 OwnedExternUnused();}42;", []);
              ( "#exe {extern I64 AC(); extern I64 BA();\n\
                 #if defined(AC)\n\
                 #endif\n\
                 extern I64 AC(); extern I64 BA();}42;",
                [ ("HCSEMA0075", "Unused extern 'BA'") ] );
            ];
          temporary ".hc"
            "#exe {extern I64 OwnedExternUnused(); extern I64 \
             OwnedExternUnused(NoSuchType arg);}42;" (fun path ->
              let report = invoke ~status:1 mode target path in
              require
                (warnings report = [ unused ])
                "parameter fault lost its earlier join warning");
          temporary ".hc"
            "#exe {extern I64 OwnedExternUnused(); extern I64 \
             OwnedExternUnused();}missing;" (fun path ->
              let report = invoke ~status:1 mode target path in
              require
                (warnings report = [ unused ])
                "later outer fault lost its earlier join warning"))
        [ "jit"; "aot" ])
    targets;
  Printf.printf "%d unused extern CLI executions passed\n%!" !count
