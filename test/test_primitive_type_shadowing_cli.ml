open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let with_file suffix contents action =
  let path = Filename.temp_file "holyc primitive shadow " suffix in
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

let word target bits report =
  require
    (member "outcome" report = `String "success")
    "expected successful execution";
  require
    (member "final_value" report |> member "type" = `String "i64")
    "wrong result type";
  require
    (member "final_value" report |> member "bits" = `String bits)
    "wrong result bits";
  require (member "output_hex" report = `String "") "unexpected output";
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
    (member "id" fixture = `String "parser/primitive-type-shadowing-001")
    "wrong oracle";
  let projections = member "hosted_value_projections" fixture |> to_list in
  require (List.length projections = 7) "all independent native fields required";
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
        (fun name ->
          List.iter
            (fun source ->
              invoke ~status:0 compiler target mode source
              |> word target "0x000000000000002a")
            [
              Printf.sprintf "I64 %s(){return 42;}%s();" name name;
              Printf.sprintf "I64 %s=41;%s++;(%s);" name name name;
              Printf.sprintf "I64 F(){I64 %s=41;%s++;return (%s);}F();" name
                name name;
              Printf.sprintf "I64 F(I64 %s){%s++;return (%s);}F(41);" name name
                name;
            ])
        [
          "I0";
          "I8";
          "I16";
          "I32";
          "I64";
          "U0";
          "U8";
          "U16";
          "U32";
          "U64";
          "F64";
          "Bool";
        ];
      let bits = observed (`String "boot3-example-A") "EX" in
      require
        (bits = observed (`String "boot3-example-B") "EX")
        "example native repeat differs";
      let report =
        invoke ~steps:42 ~preparation:3 ~status:0 compiler target mode example
      in
      word target ("0x" ^ String.lowercase_ascii bits) report;
      require (member "executed_steps" report = `Int 42) "example runtime work";
      require
        (member "compiled_initializer_steps" report = `Int 3)
        "example preparation work";
      error "HCIRVM0007"
        (invoke ~steps:41 ~status:1 compiler target mode example);
      error "HCIRVM0007"
        (invoke ~preparation:2 ~status:1 compiler target mode example);
      List.iter
        (fun (source, code) ->
          error code (invoke ~status:1 compiler target mode source))
        [
          ("I64 U64=42;U64 value;", "HCPARSE0047");
          ("I64 F(){I64 U64=42;return (U64);}U64 value;F();", "HCPARSE0001");
          ("I64 F(I64 F64){return (F64);}F64 value;", "HCPARSE0001");
        ])
    [ "jit"; "aot" ];
  print_endline
    (target
   ^ " primitive shadow projections, spelling matrix and exact limits passed.")
