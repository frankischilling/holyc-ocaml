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
    (Array.length Sys.argv >= 3 && Array.length Sys.argv <= 9)
    "usage: test_native_source_function_cli.exe <holyc.exe> \
     <native-source-functions.hc> [native-source-output.hc] \
     [native-source-literals.hc] [native-source-static-copies.hc] \
     [native-source-defaults.hc] [native-source-extern-slots.hc] \
     [native-source-callback-words.hc]"

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
  if Array.length Sys.argv >= 4 then output_checks Sys.argv.(3)
  else with_file ".hc" output_fixture output_checks;
  with_file ".hc"
    "extern U0 PutChars(U64 ch);I64 F(){PutChars('A');return \
     1/0;}PutChars('P');F();PutChars('Z');" (fun path ->
      let report = json_path ~status:1 path in
      require (has_diagnostic "HCIRVM0009" report) "late provider body fault";
      require
        (report |> member "output_hex" |> to_string = "5041")
        "earlier output survives native function fault");
  let literal_checks path =
    let report = json_path ~options:[ "--literal-byte-limit=4" ] path in
    require (final_bits report = "0x000000000000002a") "retained literal result";
    require
      (report |> member "output_hex" |> to_string = "34323b")
      "retained literal output";
    require
      (List.map
         (fun fragment -> fragment |> member "literal_bytes" |> to_int)
         (fragments report)
      = [ 4; 4 ])
      "original literal bytes are admitted once";
    require
      (List.map
         (fun fragment -> fragment |> member "arena_metadata_bytes" |> to_int)
         (fragments report)
      = [ 160; 160 ])
      "canonical reference metadata is admitted once";
    let ir = json_path ~target:"ir" path in
    require
      (ir |> member "output_hex" |> to_string = "34323b")
      "independent IR literal output";
    let limited =
      json_path ~status:1 ~options:[ "--literal-byte-limit=3" ] path
    in
    require (has_diagnostic "HCBACK0004" limited) "one-byte-below literal quota";
    require
      (limited |> member "output_hex" |> to_string = "")
      "unadmitted literal prints nothing"
  in
  if Array.length Sys.argv >= 5 then literal_checks Sys.argv.(4)
  else
    with_file ".hc"
      "extern U0 Print(U8 *fmt,...);I64 Answer(){Print(\"%d;\",42);return \
       42;}Answer();"
      literal_checks;
  with_file ".hc"
    "I64 F(){U8 *p=\"A\";p[0]++;return p[0];}F();I64 A[2]={20,22};F();"
    (fun path ->
      let report = json_path ~options:[ "--literal-byte-limit=2" ] path in
      require
        (final_bits report = "0x0000000000000043")
        "literal mutation survives later array fragments");
  with_file ".hc" "I64 F(){static I64 A=41;return ++A;}F();F();" (fun path ->
      let report = json_path path in
      require (final_bits report = "0x000000000000002b") "native static counter";
      require
        (report |> member "compiled_initializer_steps" |> to_int = 0)
        "static values execute without closed preparation";
      let steps = report |> member "executed_steps" |> to_int in
      require
        (final_bits
           (json_path ~options:[ "--step-limit=" ^ string_of_int steps ] path)
        = final_bits report)
        "exact native static runtime allowance";
      let limited =
        json_path ~status:1
          ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
          path
      in
      require
        (has_diagnostic "HCIRVM0007" limited)
        "native static runtime limit";
      require
        (limited |> member "executed_steps" |> to_int = steps - 1)
        "static runtime allowance remains cumulative";
      require
        (final_bits (json_path ~target:"ir" path) = final_bits report)
        "independent static counter result");
  with_file ".hc"
    "extern U0 PutChars(U64 ch);I64 N=40;I64 Next(){PutChars('I');return \
     ++N;}I64 F(){static I64 A=Next(),B=A+1;return B;}F();F();" (fun path ->
      let report = json_path path in
      require (final_bits report = "0x000000000000002a") "static leaf order";
      require
        (report |> member "output_hex" |> to_string = "49")
        "static initializer effect occurs once";
      let ir = json_path ~target:"ir" path in
      require
        (final_bits ir = final_bits report
        && ir |> member "output_hex" |> to_string = "49")
        "independent static initializer effects");
  with_file ".hc" "I64 F(){static U8 A[3]=\"AB\";return A[1];}F();" (fun path ->
      let report = json_path path in
      require
        (final_bits report = "0x0000000000000042")
        "native static string copy";
      let copies =
        report |> member "native" |> member "static_copies" |> to_list
      in
      require (List.length copies = 1) "one original direct native byte copy";
      require
        (List.hd copies |> member "byte_count" |> to_int = 3)
        "original fixed count includes terminating zero";
      require
        (List.length (fragments report) = 2)
        "direct copy fabricates no expression image";
      require
        (report |> member "compiled_initializer_steps" |> to_int = 4)
        "one dimension and three copied bytes";
      List.iter
        (fun target ->
          require
            (final_bits (json_path ~target path) = final_bits report)
            "independent existing consumers agree")
        [ "ir"; "host-jit" ];
      let exact = json_path ~options:[ "--initializer-step-limit=4" ] path in
      require
        (final_bits exact = final_bits report)
        "exact original initializer allowance";
      let limited =
        json_path ~status:1 ~options:[ "--initializer-step-limit=3" ] path
      in
      require
        (has_diagnostic "HCIRVM0007" limited)
        "one-below direct copy allowance";
      require
        (limited |> member "native" |> member "static_copies" |> to_list
       |> List.hd |> member "outcome" |> to_string = "error")
        "unentered direct copy is reported independently");
  let retained_copy_checks path =
    let report = json_path path in
    require
      (final_bits report = "0x0000000000000045")
      "retained original byte-copy mutation";
    require
      (report |> member "native" |> member "static_copies" |> to_list
     |> List.length = 2)
      "nested direct copies occur once before later storage growth"
  in
  if Array.length Sys.argv >= 6 then retained_copy_checks Sys.argv.(5)
  else
    with_file ".hc"
      "I64 NextByte(){static U8 Bytes[2][3]={\"AB\",\"CD\"};return \
       ++Bytes[1][0];}NextByte();U8 Later[2]={20,22};NextByte();"
      retained_copy_checks;
  let default_checks path =
    let report = json_path path in
    require
      (final_bits report = "0x000000000000002a")
      "native saved live default result";
    require
      (report |> member "prepared_default_bytes" |> to_int = 8)
      "original saved word payload";
    require
      (final_bits (json_path ~options:[ "--default-byte-limit=8" ] path)
      = final_bits report)
      "exact saved word allowance";
    let payload_limited =
      json_path ~status:1 ~options:[ "--default-byte-limit=7" ] path
    in
    require
      (has_diagnostic "HCIRVM0011" payload_limited)
      "one-below saved word allowance";
    require
      (payload_limited |> member "prepared_default_bytes" |> to_int = 0)
      "unexecuted default retains no word";
    let defaults =
      List.filter
        (fun image -> image |> member "kind" |> to_string = "default")
        (fragments report)
    in
    require
      (List.length defaults = 1)
      "one original default expression execution";
    require
      (List.hd defaults |> member "outcome" |> to_string = "success")
      "actual native default completion";
    require
      (final_bits (json_path ~target:"ir" path) = final_bits report)
      "independent IR saved default";
    let isolated = json_path ~status:1 ~target:"host-jit" path in
    require
      (has_diagnostic "HCRUN0006" isolated)
      "existing isolated live default boundary";
    let work = report |> member "compiled_initializer_steps" |> to_int in
    require
      (final_bits
         (json_path
            ~options:[ "--initializer-step-limit=" ^ string_of_int work ]
            path)
      = final_bits report)
      "exact native default preparation allowance";
    let limited =
      json_path ~status:1
        ~options:[ "--initializer-step-limit=" ^ string_of_int (work - 1) ]
        path
    in
    require
      (has_diagnostic "HCIRVM0007" limited)
      "native default preparation quota";
    require
      (last_fragment limited |> member "kind" |> to_string = "default")
      "quota occurs in original native default";
    require
      (last_fragment limited |> member "outcome" |> to_string = "fault")
      "quota reaches native default code"
  in
  if Array.length Sys.argv >= 7 then default_checks Sys.argv.(6)
  else
    with_file ".hc"
      "I64 Counter=40;I64 Seed(){return ++Counter;}I64 Answer(I64 \
       value=Seed()){return value+1;}Answer();Counter=100;Answer();"
      default_checks;
  let extern_checks path =
    let report = json_path path in
    require
      (final_bits report = "0x000000000000002a")
      "original joined slot and saved default";
    require
      (final_bits (json_path ~target:"ir" path) = final_bits report)
      "independent original slot history";
    require
      (has_diagnostic "HCRUN0001" (json_path ~status:1 ~target:"host-jit" path))
      "existing isolated declaration boundary";
    let steps = report |> member "executed_steps" |> to_int in
    require
      (final_bits
         (json_path ~options:[ "--step-limit=" ^ string_of_int steps ] path)
      = final_bits report)
      "exact native slot allowance";
    require
      (has_diagnostic "HCIRVM0007"
         (json_path ~status:1
            ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
            path))
      "one-below native slot allowance"
  in
  if Array.length Sys.argv >= 8 then extern_checks Sys.argv.(7)
  else
    with_file ".hc"
      "extern I64 Answer(I64 n=41);I64 Old(){return Answer();}I64 Answer(I64 \
       n){return n+1;}I64 Answer(I64 n){return 100;}Old();"
      extern_checks;
  List.iter
    (fun (code, text) ->
      with_file ".hc" text (fun path ->
          let report = json_path ~status:1 path in
          require (has_diagnostic code report) "reached native slot fault";
          require
            (report |> member "output_hex" |> to_string = "4241")
            "right-to-left output before slot fault";
          require
            (last_fragment report |> member "outcome" |> to_string = "fault")
            "original slot failure executes native instructions";
          let oracle = json_path ~status:1 ~target:"ir" path in
          require
            (has_diagnostic code oracle
            && oracle |> member "output_hex" |> to_string = "4241")
            "independent IR slot fault effects"))
    [
      ( "HCIRVM0030",
        "extern I64 Answer(I64 a,I64 b);extern U0 PutChars(U64 ch);I64 \
         A(){PutChars('A');return 1;}I64 B(){PutChars('B');return 2;}I64 \
         Old(){return Answer(A(),B());}Old();I64 Answer(I64 a,I64 b){return \
         42;}" );
      ( "HCIRVM0014",
        "extern I64 Answer(I64 a,I64 b);extern U0 PutChars(U64 ch);I64 \
         A(){PutChars('A');return 1;}I64 B(){PutChars('B');return 2;}I64 \
         Old(){return Answer(A(),B());}I64 Answer(U8 a,I64 b){return \
         42;}Old();" );
    ];
  let callback_word_checks path =
    let report = json_path path in
    require
      (final_bits report = "0x000000000000002a")
      "original numeric callback storage and forwarding";
    require
      (final_bits (json_path ~target:"ir" path) = final_bits report)
      "independent original callback words";
    require
      (List.map
         (fun fragment -> fragment |> member "global_arena_bytes" |> to_int)
         (fragments report)
      = [ 96; 96; 96; 96; 113; 113; 113; 113; 113 ])
      "persistent callback data, flags and owner lanes";
    let steps = report |> member "executed_steps" |> to_int in
    require
      (final_bits
         (json_path ~options:[ "--step-limit=" ^ string_of_int steps ] path)
      = final_bits report)
      "exact native callback word work";
    require
      (has_diagnostic "HCIRVM0007"
         (json_path ~status:1
            ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
            path))
      "one-below native callback word work";
    require
      (final_bits (json_path ~options:[ "--global-byte-limit=40" ] path)
      = final_bits report)
      "exact callback logical byte allowance";
    require
      (has_diagnostic "HCIRVM0016"
         (json_path ~status:1 ~options:[ "--global-byte-limit=39" ] path))
      "private owners do not replace the logical byte quota";
    require
      (has_diagnostic "HCRUN0006" (json_path ~status:1 ~target:"host-jit" path))
      "isolated callback initializer source boundary"
  in
  if Array.length Sys.argv >= 9 then callback_word_checks Sys.argv.(8)
  else
    with_file ".hc"
      "I64 (*words)()[2][2]={{0,34},{50,0}};I64 \
       (*saved)()=words[0][1];++saved;words[1][0]--;I64 Read(I64 \
       (*value)()){return value;}Read(saved);"
      callback_word_checks;
  List.iter
    (fun (output, text) ->
      with_file ".hc" text (fun path ->
          List.iter
            (fun target ->
              let report = json_path ~status:1 ~target path in
              require
                (has_diagnostic "HCIRVM0024" report)
                "numeric bits grant no executable target";
              require
                (report |> member "output_hex" |> to_string = output)
                "original callback argument effects precede owning fault";
              if target = "host-jit-task" then
                require
                  (last_fragment report |> member "outcome" |> to_string
                 = "fault")
                  "numeric callback rejection executes original native code")
            [ "ir"; "host-jit-task" ]))
    [
      ("", "I64 (*p)()=42;p();");
      ("", "I64 (*p)()[2]={0xffffffffffffffff,0};p[0]();");
      ( "4241",
        "extern U0 PutChars(U64 ch);I64 (*p)(I64 a,I64 b)=0;I64 Mark(I64 \
         n){PutChars(n);p=42;return n;}p(Mark(65),Mark(66));" );
    ];
  List.iter
    (fun text ->
      with_file ".hc" text (fun path ->
          let report = json_path path in
          require
            (final_bits report = "0x000000000000002a")
            "persistent original native callback executes";
          require
            (final_bits (json_path ~target:"ir" path) = final_bits report)
            "original IR callback value"))
    [
      "I64 F(){return 42;}I64 (*p)()=&F;p();";
      "I64 F(){return 42;}I64 (*p)()=&F;I64 F(){return 17;}p();";
      "I64 F(){return 42;}I64 (*p)()[2]={0,&F};I64 Call(I64 (*q)()){return \
       q();}Call(p[1]);";
    ];
  with_file ".hc"
    "I64 F(){return 42;}I64 Call(I64 (*q)()=&F){return q();}Call();"
    (fun path ->
      let native = json_path path in
      require
        (final_bits native = "0x000000000000002a")
        "owned callback default selects its original native body";
      require
        (final_bits (json_path ~target:"ir" path) = final_bits native)
        "original IR and native owned default agree";
      require
        (has_diagnostic "HCRUN0006"
           (json_path ~target:"host-jit" ~status:1 path))
        "isolated JIT retains its reference-bearing default restriction");
  List.iter
    (fun text ->
      with_file ".hc" text (fun path ->
          let native = json_path path in
          require
            (final_bits native = "0x000000000000002a")
            "original saved callback default history or effects changed";
          require
            (final_bits (json_path ~target:"ir" path) = final_bits native)
            "saved callback default IR comparison changed"))
    [
      "I64 F(){return 42;}I64 G(){return 17;}I64 (*p)()=&F;I64 Call(I64 \
       (*q)()=p){return q();}p=&G;Call();";
      "I64 F(){return 42;}I64 G(){return 17;}I64 (*p)()=&G;I64 Unused(I64 \
       (*q)()=(p=&F)){return q();}p();";
      "I64 F(){return 42;}I64 G(){return 17;}I64 Call(I64 (*q)()=&F){return \
       q();}I64 Old(){return Call();}I64 Call(I64 (*q)()=&G){return \
       q();}Old();";
    ];
  Printf.printf "Native source function CLI: %d executions passed.\n"
    !executions
