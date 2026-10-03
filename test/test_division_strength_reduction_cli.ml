open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc division cli " suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let invoke compiler target mode source expected_status =
  with_file ".hc" source (fun path ->
      with_file ".stdout" "" (fun stdout ->
          with_file ".stderr" "" (fun stderr ->
              let out_fd =
                Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              and err_fd =
                Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let args =
                [|
                  compiler;
                  "run";
                  "--report-version=2";
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
              let output = read stdout in
              require
                (status = Unix.WEXITED expected_status)
                ("unexpected CLI status: " ^ output ^ read stderr);
              require (read stderr = "") "JSON CLI wrote to stderr";
              Yojson.Safe.from_string output)))

let () =
  require
    (Array.length Sys.argv = 3 || Array.length Sys.argv = 4)
    "expected compiler, fixture and optional --native";
  let target =
    if Array.length Sys.argv = 4 && Sys.argv.(3) = "--native" then "host-jit"
    else "ir"
  in
  let fixture = Yojson.Safe.from_file Sys.argv.(2) in
  require
    (fixture |> member "id"
   = `String "arithmetic/division-strength-reductions-001")
    "wrong native fixture";
  let projections = fixture |> member "hosted_value_projections" |> to_list in
  require
    (List.length projections = 38)
    "all primary native fields are required";
  let observed projection case_field =
    let check =
      fixture |> member "checks" |> to_list
      |> List.find (fun check ->
          check |> member "id" = (projection |> member case_field))
    in
    check |> member "observed_fields"
    |> member (projection |> member "field" |> to_string)
    |> to_string
    |> fun bits -> Int64.of_string ("0x" ^ bits)
  in
  List.iter
    (fun mode ->
      List.iter
        (fun projection ->
          let bits = observed projection "case_id" in
          require
            (bits = observed projection "repeat_case_id")
            "native repeat differs";
          let report =
            invoke Sys.argv.(1) target mode
              (projection |> member "holy_c_source" |> to_string)
              0
          in
          require
            (report |> member "outcome" = `String "success")
            "source outcome";
          require
            (report |> member "final_value" |> member "bits"
            = `String (Printf.sprintf "0x%016Lx" bits))
            "source bits differ from native";
          require
            (report |> member "output_byte_length" = `Int 0)
            "unexpected source output";
          require
            (report |> member "output_work" = `Int 0)
            "unexpected formatting work")
        projections)
    [ "jit"; "aot" ];
  Printf.printf "%d %s source controls match repeated native fields.\n" 76
    target
