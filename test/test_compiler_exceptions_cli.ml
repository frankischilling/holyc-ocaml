open Yojson.Safe.Util
module Cases = Compiler_exception_cases

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-compiler-exceptions-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      output_string channel contents;
      close_out channel;
      action path)

let hex text =
  String.to_seq text |> List.of_seq
  |> List.map (fun c -> Printf.sprintf "%02x" (Char.code c))
  |> String.concat ""

let () =
  let compiler = Sys.argv.(1) in
  let target =
    if Array.exists (( = ) "--native") Sys.argv then "host-jit-task" else "ir"
  in
  let count = ref 0 in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text, output) ->
          temporary ".HC"
            ((if mode = "jit" then Cases.headers else "") ^ text)
            (fun source ->
              temporary ".out" "" (fun stdout ->
                  temporary ".err" "" (fun stderr ->
                      let out_fd =
                        Unix.openfile stdout
                          [ Unix.O_WRONLY; Unix.O_TRUNC ]
                          0o600
                      in
                      let err_fd =
                        Unix.openfile stderr
                          [ Unix.O_WRONLY; Unix.O_TRUNC ]
                          0o600
                      in
                      let args =
                        [|
                          compiler;
                          "run";
                          "--format=json";
                          "--mode=" ^ mode;
                          "--target=" ^ target;
                          "--code-byte-limit=1048576";
                          source;
                        |]
                      in
                      let pid =
                        Fun.protect
                          ~finally:(fun () ->
                            Unix.close out_fd;
                            Unix.close err_fd)
                          (fun () ->
                            Unix.create_process compiler args Unix.stdin out_fd
                              err_fd)
                      in
                      let _, status = Unix.waitpid [] pid in
                      let text = read stdout in
                      if status <> Unix.WEXITED 1 || read stderr <> "" then
                        failwith (label ^ text ^ read stderr);
                      let report = Yojson.Safe.from_string text in
                      let errors =
                        report |> member "diagnostics" |> to_list
                        |> List.filter (fun d ->
                            d |> member "severity" |> to_string = "error")
                      in
                      if
                        List.filter_map
                          (fun d ->
                            let code = d |> member "code" |> to_string in
                            if code = "HCRUN0004" then None else Some code)
                          errors
                        <> [ "HCPARSE0168" ]
                      then
                        failwith
                          (label ^ " original return error changed: " ^ text);
                      if
                        report |> member "output_hex" |> to_string <> hex output
                      then failwith (label ^ " reached output changed: " ^ text);
                      incr count))))
        Cases.failures)
    [ "jit"; "aot" ];
  Printf.printf "%d original Compiler return CLI cases passed (%s).\n" !count
    target
