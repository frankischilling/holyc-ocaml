open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let with_file suffix contents action =
  let path = Filename.temp_file "holyc conditional chain " suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let invoke ?(steps = 100_000) ?(preparation = 100_000) ~status compiler target
    mode source =
  with_file ".hc" source (fun path ->
      with_file ".stdout" "" (fun stdout ->
          with_file ".stderr" "" (fun stderr ->
              let out_fd =
                Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              and err_fd =
                Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let arguments =
                [|
                  compiler;
                  "run";
                  "--format=json";
                  "--report-version=2";
                  "--mode=" ^ mode;
                  "--target=" ^ target;
                  "--step-limit=" ^ string_of_int steps;
                  "--initializer-step-limit=" ^ string_of_int preparation;
                  path;
                |]
              in
              let pid =
                Fun.protect
                  ~finally:(fun () ->
                    Unix.close out_fd;
                    Unix.close err_fd)
                  (fun () ->
                    Unix.create_process compiler arguments Unix.stdin out_fd
                      err_fd)
              in
              let _, result = Unix.waitpid [] pid in
              let text = read stdout in
              require
                (result = Unix.WEXITED status)
                ("unexpected exit: " ^ text ^ read stderr);
              require (read stderr = "") "JSON CLI wrote to stderr";
              let report = Yojson.Safe.from_string text in
              require
                (member "target" report = `String target)
                "wrong execution target";
              report)))

let word ?(output = "") target bits report =
  require
    (member "outcome" report = `String "success")
    "expected successful execution";
  require
    (member "final_value" report |> member "type" = `String "i64")
    "wrong result type";
  require
    (member "final_value" report |> member "bits" = `String bits)
    "wrong result bits";
  require (member "output_hex" report = `String output) "unexpected output";
  if target = "host-jit" then (
    require
      (member "arithmetic" report = `String "runtime-native")
      "expected native execution";
    require
      (member "native" report |> member "image" <> `Null)
      "missing native image")

let error code report =
  require (member "outcome" report = `String "error") "expected rejected source";
  require (member "final_value" report = `Null) "failure exposes a final value";
  match member "diagnostics" report |> to_list with
  | first :: _ ->
      require (member "code" first = `String code) "wrong diagnostic"
  | [] -> failwith "failure has no diagnostic"

let () =
  require
    (Array.length Sys.argv = 4 || Array.length Sys.argv = 5)
    "expected compiler, oracle and example";
  let compiler = Sys.argv.(1)
  and fixture = Yojson.Safe.from_file Sys.argv.(2)
  and example = read Sys.argv.(3) in
  let target = if Array.length Sys.argv = 5 then "host-jit" else "ir" in
  require
    (member "id" fixture = `String "execution/conditional-comparison-chains-001")
    "wrong oracle";
  let projections = member "hosted_value_projections" fixture |> to_list in
  require
    (List.length projections = 50)
    "all fifty independent native fields required";
  let observed id field =
    member "checks" fixture |> to_list
    |> List.find (fun check -> member "id" check = id)
    |> member "observed_fields" |> member field |> to_string
  in
  List.iter
    (fun mode ->
      List.iter
        (fun projection ->
          let field = member "field" projection |> to_string in
          let bits = observed (member "case_id" projection) field in
          require
            (bits = observed (member "repeat_case_id" projection) field)
            "native repeat differs";
          invoke ~status:0 compiler target mode
            (member "holy_c_source" projection |> to_string)
          |> word target ("0x" ^ String.lowercase_ascii bits))
        projections;
      List.iter
        (fun regression ->
          let source = member "holy_c_source" regression |> to_string in
          let bits = member "expected_bits" regression |> to_string in
          let report = invoke ~status:0 compiler target mode source in
          word target bits report;
          let steps = member "executed_steps" report |> to_int in
          invoke ~steps ~status:0 compiler target mode source
          |> word target bits;
          invoke ~steps:(steps - 1) ~status:1 compiler target mode source
          |> error "HCIRVM0007")
        (member "hosted_regressions" fixture |> to_list);
      invoke ~status:0 compiler target mode example
      |> word target "0x000000000000002a";
      let output_source link =
        "extern U0 Print(U8 *fmt,...);I64 Bad(I64 z){Print(\"right\");return \
         1/z;}" ^ "I64 F(I64 z){Print(\"kept\");if(" ^ link
        ^ "<Bad(z))return 7;return 42;}F(0);"
      in
      invoke ~status:0 compiler target mode (output_source "3<2")
      |> word ~output:"6b657074" target "0x000000000000002a";
      let reached =
        invoke ~status:1 compiler target mode (output_source "1<2")
      in
      error "HCIRVM0009" reached;
      require
        (member "output_hex" reached = `String "6b6570747269676874")
        "reached output before a later-link fault must survive";
      List.iter
        (fun source ->
          invoke ~status:1 compiler target mode source |> error "HCRUN0003")
        [
          "I64 F(){if(1==2<3==1)return 7;return 9;}F();";
          "I64 F(){if(1==2<3<4==1)return 7;return 9;}F();";
          "I64 F(){if(1!=2>=3!=1)return 7;return 9;}F();";
          "I64 F(){if(1.0<2<3)return 7;return 9;}F();";
          "I64 F(){if(1<2.0<3)return 7;return 9;}F();";
        ];
      List.iter
        (fun source ->
          invoke ~status:1 compiler target mode source |> error "HCIRVM0009")
        [
          "I64 F(I64 z){if(1<2<1/z)return 7;return 9;}F(0);";
          "I64 F(I64 z){if((3<2<1/z)+1)return 7;return 9;}F(0);";
          "I64 F(I64 z){return 3<2<1/z;}F(0);";
        ])
    [ "jit"; "aot" ];
  Printf.printf
    "conditional comparison chains CLI (%s): 50 native fields, execution and \
     rejection limits passed\n"
    target
