open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc constant shift cli " suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let invoke compiler target mode source =
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
              require (status = Unix.WEXITED 0)
                ("source CLI failed: " ^ output ^ read stderr);
              require (read stderr = "") "JSON CLI wrote to stderr";
              Yojson.Safe.from_string output)))

let () =
  require
    (Array.length Sys.argv = 3 || Array.length Sys.argv = 4)
    "expected compiler, fixture and optional --native";
  let native = Array.length Sys.argv = 4 && Sys.argv.(3) = "--native" in
  let fixture = Yojson.Safe.from_file Sys.argv.(2) in
  let projections = fixture |> member "hosted_value_projections" |> to_list in
  let native_bits projection =
    let case_id = projection |> member "case_id" |> to_string in
    let check =
      fixture |> member "checks" |> to_list
      |> List.find (fun check -> check |> member "id" = `String case_id)
    in
    let prefix = (projection |> member "field" |> to_string) ^ "=" in
    let token =
      check |> member "observed_output" |> to_list |> List.map to_string
      |> List.concat_map (String.split_on_char ' ')
      |> List.filter (String.starts_with ~prefix)
    in
    match token with
    | [ token ] ->
        let bits =
          String.sub token (String.length prefix)
            (String.length token - String.length prefix)
        in
        `String (Printf.sprintf "0x%016Lx" (Int64.of_string ("0x" ^ bits)))
    | _ -> failwith "expected exactly one captured native field"
  in
  require (List.length projections = 49) "49 source controls required";
  let target = if native then "host-jit" else "ir" in
  List.iter
    (fun mode ->
      List.iter
        (fun projection ->
          let label = projection |> member "field" |> to_string in
          let report =
            invoke Sys.argv.(1) target mode
              (projection |> member "holy_c_source" |> to_string)
          in
          require
            (report |> member "outcome" = `String "success")
            (label ^ " source outcome");
          require
            (report |> member "final_value" |> member "bits"
           = native_bits projection)
            (label ^ " source bits differ from native capture");
          require
            (report |> member "output_byte_length" = `Int 0)
            (label ^ " unexpected source output");
          require
            (report |> member "output_work" = `Int 0)
            (label ^ " unexpected formatting work"))
        projections)
    [ "jit"; "aot" ];
  Printf.printf
    "98 %s source controls match captured native bits in both modes.\n" target
