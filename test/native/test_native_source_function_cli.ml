open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc source function " suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let () =
  require
    (Array.length Sys.argv = 3 || Array.length Sys.argv = 4)
    "usage: test_native_source_function_cli.exe <holyc.exe> \
     <native-source-functions.hc> [native-source-output.hc]"

let compiler = Sys.argv.(1)
let fixture = Sys.argv.(2)
let executions = ref 0

let invoke expected arguments =
  with_file ".out" "" (fun stdout ->
      with_file ".err" "" (fun stderr ->
          let out_fd =
            Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let err_fd =
            Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let pid =
            Fun.protect
              ~finally:(fun () ->
                Unix.close out_fd;
                Unix.close err_fd)
              (fun () ->
                Unix.create_process compiler
                  (Array.of_list (compiler :: arguments))
                  Unix.stdin out_fd err_fd)
          in
          let _, status = Unix.waitpid [] pid in
          incr executions;
          let stdout = read stdout and stderr = read stderr in
          require
            (status = Unix.WEXITED expected)
            (Printf.sprintf "unexpected CLI exit for %s\n%s\n%s"
               (String.concat " " arguments)
               stdout stderr);
          (stdout, stderr)))

let json_path ?(status = 0) ?(target = "host-jit-task") ?(options = []) path =
  let stdout, stderr =
    invoke status
      ([ "run"; "--target=" ^ target; "--mode=jit"; "--format=json" ]
      @ options @ [ path ])
  in
  require (stderr = "") ("JSON CLI wrote stderr: " ^ stderr);
  Yojson.Safe.from_string stdout

let final_bits json = json |> member "final_value" |> member "bits" |> to_string
let fragments json = json |> member "native" |> member "fragments" |> to_list
let last_fragment json = List.hd (List.rev (fragments json))

let has_diagnostic code json =
  json |> member "diagnostics" |> to_list
  |> List.exists (fun diagnostic ->
      diagnostic |> member "code" |> to_string = code)

let () =
  let baseline = json_path fixture in
  require (final_bits baseline = "0x000000000000002a") "native function result";
  require
    (baseline |> member "arithmetic" |> to_string = "runtime-native")
    "native source arithmetic";
  let images = fragments baseline in
  require (List.length images = 3) "initializer, definition and resumed call";
  require
    (List.map (fun fragment -> fragment |> member "kind" |> to_string) images
    = [ "initializer"; "command"; "command" ])
    "original source fragment order";
  require
    (List.map
       (fun fragment -> fragment |> member "global_arena_bytes" |> to_int)
       images
    = [ 9; 9; 9 ])
    "definition and call share original storage";
  require
    (List.map
       (fun fragment -> fragment |> member "function_count" |> to_int)
       images
    = [ 0; 1; 1 ])
    "each caller reports its compiled function closure";
  List.iter
    (fun image ->
      require
        (image |> member "outcome" |> to_string = "success")
        "actual native fragment completion")
    images;
  let steps = baseline |> member "executed_steps" |> to_int in
  require (steps > 5) "native call contributes reached work";
  let exact =
    json_path ~options:[ "--step-limit=" ^ string_of_int steps ] fixture
  in
  require
    (final_bits exact = final_bits baseline)
    "exact cumulative native limit";
  let stopped =
    json_path ~status:1
      ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
      fixture
  in
  require
    (has_diagnostic "HCIRVM0007" stopped)
    "native function quota diagnostic";
  require
    (stopped |> member "executed_steps" |> to_int = steps - 1)
    "function quota preserves reached work";
  require
    (last_fragment stopped |> member "outcome" |> to_string = "fault")
    "quota reaches native entry";
  require
    (final_bits (json_path ~target:"ir" fixture) = final_bits baseline)
    "independent interpreted source result";
  List.iter
    (fun text ->
      with_file ".hc" text (fun path ->
          require
            (final_bits (json_path path) = "0x000000000000002a")
            ("native original function source: " ^ text)))
    [
      "I64 A=41; I64 F(){return A+1;} I64 B=F(); B;";
      "I64 Base(){return 33;} I64 Wrap(){return Base()+9;} I64 Base(){return \
       100;} Wrap();";
      "I64 A=40; I64 Next(){return ++A;} Next(); I64 A=100; Next();";
      "I64 A[2]={41,1}; I64 F(){return A[0]+A[1];} F();";
      "I64 N=0; I64 Next(){return ++N;} I64 Pair(I64 a,I64 b){return a*10+b;} \
       Pair(Next(),Next())+N+19;";
      "I64 Recur(I64 n){if(n)return 1+Recur(n-1);return 40;} Recur(2);";
      "I64 A=40; U0 F(){A+=2;} F(); A;";
    ];
  with_file ".hc" "U64 F(U64 n){return n+1;} F(0x8000000000000000);"
    (fun path ->
      let report = json_path path in
      require
        (final_bits report = "0x8000000000000001")
        "full native return word";
      require
        (report |> member "final_value" |> member "type" |> to_string = "u64")
        "unsigned native return class");
  List.iter
    (fun (code, text) ->
      with_file ".hc" text (fun path ->
          let report = json_path ~status:1 path in
          require
            (has_diagnostic code report)
            ("reached function diagnostic " ^ code);
          require
            (last_fragment report |> member "outcome" |> to_string = "fault")
            "original function fault reaches the CLI"))
    [
      ("HCIRVM0012", "I64 A; I64 F(){return A;} F();");
      ("HCIRVM0009", "I64 A=0; I64 Broken(){A=41;return 1/0;} Broken(); A=99;");
    ];
  let stdout, _ =
    invoke 0 [ "run"; "--target=host-jit-task"; "--format=human"; fixture ]
  in
  require
    (String.split_on_char '\n' stdout
    |> List.exists (String.starts_with ~prefix:"native-fragments=3"))
    "human report retains separate native function fragments";
  let output_fixture =
    "extern U0 PutChars(U64 ch);extern U0 Print(U8 *fmt,...);U8 \
     Format[4]={37,100,59,0};I64 Emit(){PutChars('A');Print(Format,42);return \
     42;}Emit();"
  in
  let output_checks path =
    let report = json_path path in
    require (final_bits report = "0x000000000000002a") "provider result";
    require
      (report |> member "output_hex" |> to_string = "4134323b")
      "retained provider captures original bytes";
    let ir = json_path ~target:"ir" path in
    require
      (ir |> member "output_hex" |> to_string = "4134323b")
      "independent source output";
    let limited =
      json_path ~status:1 ~options:[ "--output-byte-limit=3" ] path
    in
    require (has_diagnostic "HCIRVM0022" limited) "retained output limit";
    require
      (limited |> member "output_hex" |> to_string = "41")
      "faulting Print preserves prior PutChars only"
  in
  if Array.length Sys.argv = 4 then output_checks Sys.argv.(3)
  else with_file ".hc" output_fixture output_checks;
  with_file ".hc"
    "extern U0 PutChars(U64 ch);I64 F(){PutChars('A');return \
     1/0;}PutChars('P');F();PutChars('Z');" (fun path ->
      let report = json_path ~status:1 path in
      require (has_diagnostic "HCIRVM0009" report) "late provider body fault";
      require
        (report |> member "output_hex" |> to_string = "5041")
        "earlier output survives native function fault");
  Printf.printf "Native source function CLI: %d executions passed.\n"
    !executions
