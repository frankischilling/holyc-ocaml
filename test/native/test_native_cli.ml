open Yojson.Safe.Util
module Native = Holyc_lib.Native_execution

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  (* Spaces exercise argument handling on both Windows and Linux. *)
  let path = Filename.temp_file "holyc native cli " suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let compiler =
  require
    (Array.length Sys.argv = 6)
    "usage: test_native_cli.exe <holyc.exe> <native-integer-expression.hc> \
     <native-integer-predicates.hc> <native-integer-logical.hc> \
     <native-integer-spills.hc>";
  Sys.argv.(1)

let invoke arguments =
  with_file ".stdout" "" (fun stdout ->
      with_file ".stderr" "" (fun stderr ->
          let out_fd =
            Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let pid =
            Fun.protect
              ~finally:(fun () -> Unix.close out_fd)
              (fun () ->
                let err_fd =
                  Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
                in
                Fun.protect
                  ~finally:(fun () -> Unix.close err_fd)
                  (fun () ->
                    Unix.create_process compiler
                      (Array.of_list (compiler :: arguments))
                      Unix.stdin out_fd err_fd))
          in
          let _, status = Unix.waitpid [] pid in
          (status, read stdout, read stderr)))

let status_name = function
  | Unix.WEXITED code -> Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

let checked_invoke expected arguments =
  let status, stdout, stderr = invoke arguments in
  require
    (status = Unix.WEXITED expected)
    (Printf.sprintf "%s: expected exit %d, got %s\nstdout: %s\nstderr: %s"
       (String.concat " " arguments)
       expected (status_name status) stdout stderr);
  (stdout, stderr)

let contains text fragment =
  let size = String.length fragment in
  let rec find index =
    index + size <= String.length text
    && (String.sub text index size = fragment || find (index + 1))
  in
  find 0

let normalize_whitespace text =
  text
  |> String.map (function
    | '\t' | '\r' | '\n' -> ' '
    | character -> character)
  |> String.split_on_char ' '
  |> List.filter (fun word -> word <> "")
  |> String.concat " "

let string name json = json |> member name |> to_string
let integer name json = json |> member name |> to_int

let check_keys label expected json =
  let actual = json |> to_assoc |> List.map fst |> List.sort String.compare in
  require
    (actual = List.sort String.compare expected)
    (label ^ " fields: " ^ String.concat ", " actual)

let reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"

let platform =
  match Native.platform () with
  | Native.Windows_x86_64 -> "windows-x86_64"
  | Native.Linux_x86_64 -> "linux-x86_64"
  | Native.Unsupported ->
      failwith
        "native-tests explicitly requires Windows x86-64 or Linux x86-64; this \
         platform is unsupported"

let native_json ?(options = []) ~mode status source =
  let stdout, stderr =
    checked_invoke status
      ([ "eval-native"; "--format=json"; "--mode=" ^ mode ]
      @ options @ [ source ])
  in
  require (stderr = "") ("native JSON diagnostics escaped the report: " ^ stderr);
  let report = Yojson.Safe.from_string stdout in
  check_keys "native report"
    [
      "schema";
      "implementation_commit";
      "reference_commit";
      "command";
      "execution_target";
      "platform";
      "mode";
      "outcome";
      "limits";
      "image";
      "final_value";
      "diagnostics";
      "command_error";
    ]
    report;
  require
    (string "schema" report = "holyc-native-expression-v1"
    && string "implementation_commit" report
       = Holyc_lib.Version.implementation_commit
    && string "reference_commit" report = reference_commit)
    "native schema and compiler/reference provenance";
  require
    (string "command" report = "eval-native"
    && string "execution_target" report = "x86-64-native"
    && string "platform" report = platform
    && string "mode" report = mode)
    "native command, platform, target and preprocessing mode";
  require
    (string "outcome" report = if status = 0 then "success" else "error")
    "native outcome must agree with the process exit status";
  check_keys "native limits"
    [ "ir_instructions"; "code_bytes"; "stack_bytes" ]
    (member "limits" report);
  if status <> 0 then
    require
      (member "image" report = `Null && member "final_value" report = `Null)
      "native failure must not publish an image or a result";
  report

let check_limits ?(stack = 4088) report ir bytes =
  let limits = member "limits" report in
  require
    (integer "ir_instructions" limits = ir
    && integer "code_bytes" limits = bytes
    && integer "stack_bytes" limits = stack)
    "native report must retain the requested IR, code and stack budgets"

let check_success ?(frame_bytes = 0) report =
  require
    (member "diagnostics" report = `List []
    && member "command_error" report = `Null)
    "successful fixture has no diagnostics or command error";
  let image = member "image" report in
  check_keys "native image"
    [
      "bytes_hex";
      "byte_count";
      "ir_instructions";
      "machine_instructions";
      "register_peak";
      "frame_bytes";
    ]
    image;
  let hex = string "bytes_hex" image in
  let bytes = integer "byte_count" image in
  require
    (bytes > 0
    && String.length hex = 2 * bytes
    && String.for_all
         (function
           | '0' .. '9' | 'a' .. 'f' -> true
           | _ -> false)
         hex
    && String.ends_with ~suffix:"c3" hex)
    "native image must report complete hexadecimal bytes ending in RET";
  require
    (integer "ir_instructions" image >= 3
    && integer "machine_instructions" image >= 2
    && integer "machine_instructions" image <= bytes
    && integer "register_peak" image >= 1
    && integer "register_peak" image <= 7
    && integer "frame_bytes" image = frame_bytes)
    "native image has bounded instruction/register counts and the expected \
     stack frame"

let check_word report type_ decimal bits =
  let value = member "final_value" report in
  check_keys "native word" [ "type"; "value"; "bits" ] value;
  require
    (string "type" value = type_
    && string "value" value = decimal
    && string "bits" value = bits)
    (Printf.sprintf "expected %s %s (%s), got %s" type_ decimal bits
       (Yojson.Safe.to_string value))

(* Two MOV r64,imm64 instructions, IMUL RAX,RCX and RET. A literal 42
   trampoline cannot satisfy this image or the independent instruction counts. *)
let multiply_hex = "48b8060000000000000048b90700000000000000480fafc1c3"

let check_multiply report =
  check_success report;
  check_word report "I64" "42" "0x000000000000002a";
  let image = member "image" report in
  require
    (string "bytes_hex" image = multiply_hex
    && integer "byte_count" image = 25
    && integer "ir_instructions" image = 5
    && integer "machine_instructions" image = 4
    && integer "register_peak" image = 2)
    "6*7 must emit and report two full-width inputs, IMUL and RET"

let multiplication_and_modes () =
  List.iter
    (fun mode ->
      let report = native_json ~mode 0 Sys.argv.(2) in
      check_multiply report;
      check_limits report 4096 65536)
    [ "jit"; "aot" ];
  with_file ".hc" "6*7;" (fun source ->
      List.iter
        (fun mode ->
          let report = native_json ~mode 0 source in
          check_multiply report;
          check_limits report 4096 65536;
          let stdout, stderr =
            checked_invoke 0 [ "eval-native"; "--mode=" ^ mode; source ]
          in
          require
            (String.trim stdout = "42" && stderr = "")
            "default human output is the result, and exit status is zero")
        [ "jit"; "aot" ]);
  with_file ".hc" "#ifjit\n6*7;\n#else\n7*7;\n#endif\n" (fun source ->
      List.iter
        (fun (mode, decimal, bits) ->
          let report = native_json ~mode 0 source in
          check_success report;
          check_word report "I64" decimal bits)
        [
          ("jit", "42", "0x000000000000002a");
          ("aot", "49", "0x0000000000000031");
        ])

(* Literal expectations deliberately avoid using the backend or interpreter as
   the oracle for decimal formatting, signedness or the 64 returned bits. *)
let full_width_values () =
  let cases =
    [
      ("0;", "I64", "0", "0x0000000000000000");
      ("-1;", "I64", "-1", "0xffffffffffffffff");
      ("4294967297;", "I64", "4294967297", "0x0000000100000001");
      ("9007199254740993;", "I64", "9007199254740993", "0x0020000000000001");
      ( "9223372036854775807;",
        "I64",
        "9223372036854775807",
        "0x7fffffffffffffff" );
      ("0x8000000000000000;", "U64", "9223372036854775808", "0x8000000000000000");
      ( "0xFFFFFFFFFFFFFFFF;",
        "U64",
        "18446744073709551615",
        "0xffffffffffffffff" );
      ( "0x7FFFFFFFFFFFFFFF+1;",
        "I64",
        "-9223372036854775808",
        "0x8000000000000000" );
      ("0xFFFFFFFFFFFFFFFF+1;", "U64", "0", "0x0000000000000000");
      ( "(-9223372036854775807-1)-1;",
        "I64",
        "9223372036854775807",
        "0x7fffffffffffffff" );
      ("0x8000000000000000*2;", "U64", "0", "0x0000000000000000");
      ( "0xFFFFFFFFFFFFFFFF*0xFFFFFFFFFFFFFFFF;",
        "U64",
        "1",
        "0x0000000000000001" );
      ( "-0x8000000000000000;",
        "I64",
        "-9223372036854775808",
        "0x8000000000000000" );
      ( "~0x8000000000000000;",
        "I64",
        "9223372036854775807",
        "0x7fffffffffffffff" );
      ( "(~0x8000000000000000)+2;",
        "U64",
        "9223372036854775809",
        "0x8000000000000001" );
      ("-(6*7);", "I64", "-42", "0xffffffffffffffd6");
      ("(15&6)|(8^3);", "I64", "15", "0x000000000000000f");
      ("(100-7)-(7-100);", "I64", "186", "0x00000000000000ba");
    ]
  in
  List.iter
    (fun (text, type_, decimal, bits) ->
      with_file ".hc" text (fun source ->
          List.iter
            (fun mode ->
              let report = native_json ~mode 0 source in
              check_success report;
              check_word report type_ decimal bits;
              check_limits report 4096 65536)
            [ "jit"; "aot" ]))
    cases;
  List.iter
    (fun (text, decimal) ->
      with_file ".hc" text (fun source ->
          List.iter
            (fun mode ->
              let stdout, stderr =
                checked_invoke 0
                  [ "eval-native"; "--format=human"; "--mode=" ^ mode; source ]
              in
              require
                (String.trim stdout = decimal && stderr = "")
                ("human output must preserve the full word: " ^ text))
            [ "jit"; "aot" ]))
    [
      ("0;", "0");
      ("-(6*7);", "-42");
      ("0x7FFFFFFFFFFFFFFF+1;", "-9223372036854775808");
      ("0xFFFFFFFFFFFFFFFF;", "18446744073709551615");
    ]

let predicate_values () =
  List.iter
    (fun mode ->
      let report = native_json ~mode 0 Sys.argv.(3) in
      check_success report;
      check_word report "I64" "42" "0x000000000000002a";
      let image = member "image" report in
      require
        (integer "ir_instructions" image = 38
        && integer "byte_count" image = 272
        && integer "machine_instructions" image = 51
        && integer "register_peak" image = 3)
        "predicate fixture must execute its arithmetic, six relations and NOT")
    [ "jit"; "aot" ];
  List.iter
    (fun (text, type_, decimal, bits) ->
      with_file ".hc" text (fun source ->
          List.iter
            (fun mode ->
              let report = native_json ~mode 0 source in
              check_success report;
              check_word report type_ decimal bits)
            [ "jit"; "aot" ]))
    [
      ("-1<0;", "I64", "1", "0x0000000000000001");
      ("-1>=0;", "I64", "0", "0x0000000000000000");
      ("-1>0;", "I64", "0", "0x0000000000000000");
      ("-1<=0;", "I64", "1", "0x0000000000000001");
      ("0xFFFFFFFFFFFFFFFF==-1;", "I64", "1", "0x0000000000000001");
      ("0xFFFFFFFFFFFFFFFF!=-1;", "I64", "0", "0x0000000000000000");
      ("(~0x8000000000000000)<-1;", "I64", "1", "0x0000000000000001");
      ("(~0x8000000000000000)>-1;", "I64", "0", "0x0000000000000000");
      ("(0xFFFFFFFFFFFFFFFF>1)<-1;", "I64", "0", "0x0000000000000000");
      ("(0x7FFFFFFFFFFFFFFF+1)<0;", "I64", "1", "0x0000000000000001");
      ("!0x100000000;", "I64", "0", "0x0000000000000000");
      ("!0x8000000000000000;", "U64", "0", "0x0000000000000000");
      ("!~0xFFFFFFFFFFFFFFFF;", "U64", "1", "0x0000000000000001");
      ("!!0x8000000000000000;", "U64", "1", "0x0000000000000001");
      ( "(!0xFFFFFFFFFFFFFFFF)+0xFFFFFFFFFFFFFFFF;",
        "U64",
        "18446744073709551615",
        "0xffffffffffffffff" );
      ( "(0xFFFFFFFFFFFFFFFF==0xFFFFFFFFFFFFFFFF)*0x8000000000000000;",
        "U64",
        "9223372036854775808",
        "0x8000000000000000" );
      ( "(0xFFFFFFFFFFFFFFFF!=0xFFFFFFFFFFFFFFFF)*0x8000000000000000;",
        "U64",
        "0",
        "0x0000000000000000" );
    ]

let logical_values () =
  List.iter
    (fun mode ->
      let report = native_json ~mode 0 Sys.argv.(4) in
      check_success report;
      check_word report "I64" "42" "0x000000000000002a";
      let image = member "image" report in
      require
        (integer "ir_instructions" image = 61
        && integer "byte_count" image = 562
        && integer "machine_instructions" image = 120
        && integer "register_peak" image = 4)
        "logical fixture must execute its operands, normalization and chain \
         links";
      let stdout, stderr =
        checked_invoke 0 [ "eval-native"; "--mode=" ^ mode; Sys.argv.(4) ]
      in
      require
        (String.trim stdout = "42" && stderr = "")
        "logical fixture returns 42 through the public native command")
    [ "jit"; "aot" ];
  List.iter
    (fun (text, expected) ->
      with_file ".hc" text (fun source ->
          List.iter
            (fun mode ->
              let report = native_json ~mode 0 source in
              check_success report;
              check_word report "I64" (string_of_int expected)
                (Printf.sprintf "0x%016x" expected))
            [ "jit"; "aot" ]))
    [
      ("2&&4;", 1);
      ("2^^4;", 0);
      ("0||256;", 1);
      ("0^^0x100000000;", 1);
      ("0x8000000000000000&&0xFFFFFFFFFFFFFFFF;", 1);
      ("0x8000000000000000^^0xFFFFFFFFFFFFFFFF;", 0);
      ("!((0x8000000000000000&&1)-1);", 1);
      ("((0x8000000000000000&&1)-2)<0;", 1);
      ("1<2<3;", 1);
      ("3<2<1;", 0);
      ("(3<2)<1;", 1);
      ("(~0x8000000000000000)>0>-1;", 0);
      ("(~0x8000000000000000)>0<-1;", 1);
      ("~0x8000000000000000 < -1 < 0;", 0);
      ("0<1<(~0x8000000000000000)>0>-1;", 0);
      ("((~0x8000000000000000)>0)>-1;", 1);
    ]

let check_diagnostic ~source ~code report =
  require
    (member "command_error" report = `Null)
    "source failure must retain its compilation diagnostic";
  let diagnostics = report |> member "diagnostics" |> to_list in
  require (diagnostics <> []) "source failure must include a diagnostic";
  let diagnostic = List.hd diagnostics in
  require
    (string "code" diagnostic = code
    && string "severity" diagnostic = "error"
    && string "message" diagnostic <> "")
    ("expected diagnostic " ^ code ^ ", got " ^ Yojson.Safe.to_string diagnostic);
  let primary = member "primary" diagnostic in
  require
    (Filename.basename (string "path" primary) = Filename.basename source
    && integer "line" primary >= 1
    && integer "column" primary >= 1
    && integer "start" primary >= 0
    && integer "stop" primary >= integer "start" primary)
    "source failure must retain the input path and valid source location";
  diagnostic

let spill_frames () =
  List.iter
    (fun mode ->
      let fixture = native_json ~mode 0 Sys.argv.(5) in
      check_success ~frame_bytes:8 fixture;
      check_word fixture "I64" "42" "0x000000000000002a";
      check_limits fixture 4096 65536;
      let image = member "image" fixture in
      require
        (integer "ir_instructions" image = 17
        && integer "byte_count" image = 132
        && integer "machine_instructions" image = 20
        && integer "register_peak" image = 7)
        "spill fixture must account for its prologue, one store/reload pair, \
         arithmetic and epilogue";
      let exact =
        native_json ~mode
          ~options:[ "--code-byte-limit=132"; "--stack-byte-limit=8" ]
          0 Sys.argv.(5)
      in
      check_success ~frame_bytes:8 exact;
      check_word exact "I64" "42" "0x000000000000002a";
      check_limits ~stack:8 exact 4096 132;
      let too_small_code =
        native_json ~mode
          ~options:[ "--code-byte-limit=131"; "--stack-byte-limit=8" ]
          1 Sys.argv.(5)
      in
      check_limits ~stack:8 too_small_code 4096 131;
      ignore
        (check_diagnostic ~source:Sys.argv.(5) ~code:"HCBACK0005" too_small_code);
      List.iter
        (fun stack ->
          let report =
            native_json ~mode
              ~options:[ "--stack-byte-limit=" ^ string_of_int stack ]
              1 Sys.argv.(5)
          in
          check_limits ~stack report 4096 65536;
          let diagnostic =
            check_diagnostic ~source:Sys.argv.(5) ~code:"HCBACK0004" report
          in
          require
            (contains (string "message" diagnostic) "max_stack_bytes")
            "spill-frame exhaustion must identify the stack byte budget")
        [ 7; 0 ];
      let register_only =
        native_json ~mode ~options:[ "--stack-byte-limit=0" ] 0 Sys.argv.(2)
      in
      check_multiply register_only;
      check_limits ~stack:0 register_only 4096 65536;
      let stdout, stderr =
        checked_invoke 0 [ "eval-native"; "--mode=" ^ mode; Sys.argv.(5) ]
      in
      require
        (String.trim stdout = "42" && stderr = "")
        "default native CLI executes the bounded spill fixture")
    [ "jit"; "aot" ]

let unsupported_sources () =
  List.iter
    (fun (text, code) ->
      with_file ".hc" text (fun source ->
          List.iter
            (fun mode ->
              let report = native_json ~mode 1 source in
              check_limits report 4096 65536;
              ignore (check_diagnostic ~source ~code report))
            [ "jit"; "aot" ]))
    [
      ("", "HCEVAL0001");
      ("1;2;", "HCEVAL0001");
      ("I64 x=42;", "HCEVAL0001");
      ("if(1)42;", "HCEVAL0001");
      ("I64 F(){return 42;}F();", "HCEVAL0001");
      ("\"hello\";", "HCEVAL0001");
      ("~2.0;", "HCEVAL0002");
      ("1.0;", "HCBACK0002");
      ("6.0*7.0;", "HCBACK0002");
      ("6/2;", "HCBACK0002");
      ("1/0;", "HCBACK0002");
      ("7%2;", "HCBACK0002");
      ("1<<2;", "HCBACK0002");
      ("8>>1;", "HCBACK0002");
      ("0&&(1/0);", "HCBACK0002");
      ("1||(1/0);", "HCBACK0002");
      ("1^^(1<<2);", "HCBACK0002");
      ("1==2<3==1;", "HCEVAL0002");
      ("1!=2>=3!=1;", "HCEVAL0002");
      ("!1.0;", "HCBACK0002");
      ("(6*);", "HCPARSE0018");
      ("6*7", "HCPARSE0047");
    ];
  with_file ".hc" "6/2;" (fun source ->
      let stdout, stderr = checked_invoke 1 [ "eval-native"; source ] in
      require
        (stdout = ""
        && contains stderr "HCBACK0002"
        && contains stderr (Filename.basename source))
        "human source rejection uses stderr and retains its source")

let budgets () =
  with_file ".hc" "6*7;" (fun source ->
      List.iter
        (fun mode ->
          let exact =
            native_json ~mode
              ~options:[ "--ir-instruction-limit=5"; "--code-byte-limit=25" ]
              0 source
          in
          check_multiply exact;
          check_limits exact 5 25;
          List.iter
            (fun (ir, bytes, code, message) ->
              let report =
                native_json ~mode
                  ~options:
                    [
                      "--ir-instruction-limit=" ^ string_of_int ir;
                      "--code-byte-limit=" ^ string_of_int bytes;
                    ]
                  1 source
              in
              check_limits report ir bytes;
              let diagnostic = check_diagnostic ~source ~code report in
              require
                (contains (string "message" diagnostic) message)
                "exhausted budget must identify the bounded resource")
            [
              (4, 25, "HCBACK0001", "IR instruction count");
              (5, 24, "HCBACK0005", "max_code_bytes");
            ];
          let maximum =
            native_json ~mode
              ~options:
                [
                  "--ir-instruction-limit=100000"; "--code-byte-limit=16777216";
                ]
              0 source
          in
          check_multiply maximum;
          check_limits maximum 100000 16777216)
        [ "jit"; "aot" ])

let exact_image_budgets () =
  List.iter
    (fun (text, hex, ir, bytes, machine, peak) ->
      with_file ".hc" text (fun source ->
          List.iter
            (fun mode ->
              let options ir bytes =
                [
                  "--ir-instruction-limit=" ^ string_of_int ir;
                  "--code-byte-limit=" ^ string_of_int bytes;
                ]
              in
              let report =
                native_json ~mode ~options:(options ir bytes) 0 source
              in
              check_success report;
              check_limits report ir bytes;
              check_word report "I64" "1" "0x0000000000000001";
              let image = member "image" report in
              require
                (string "bytes_hex" image = hex
                && integer "byte_count" image = bytes
                && integer "ir_instructions" image = ir
                && integer "machine_instructions" image = machine
                && integer "register_peak" image = peak)
                "Boolean image must retain its flag producers and full-word \
                 results";
              List.iter
                (fun (ir, bytes, code) ->
                  let report =
                    native_json ~mode ~options:(options ir bytes) 1 source
                  in
                  check_limits report ir bytes;
                  ignore (check_diagnostic ~source ~code report))
                [ (ir - 1, bytes, "HCBACK0001"); (ir, bytes - 1, "HCBACK0005") ])
            [ "jit"; "aot" ]))
    [
      ( "1<2;",
        "48b8010000000000000048b902000000000000004839c80f9cc0480fb6c0c3",
        5,
        31,
        6,
        2 );
      ("!0;", "48b800000000000000004885c00f94c0480fb6c0c3", 4, 21, 5, 1);
      ( "2&&4;",
        "48b8020000000000000048b904000000000000004885c00f95c0480fb6c04885c90f95c1480fb6c94821c8c3",
        5,
        44,
        10,
        2 );
      ( "2||4;",
        "48b8020000000000000048b904000000000000004885c00f95c0480fb6c04885c90f95c1480fb6c94809c8c3",
        5,
        44,
        10,
        2 );
      ( "0^^4;",
        "48b8000000000000000048b904000000000000004885c00f95c0480fb6c04885c90f95c1480fb6c94831c8c3",
        5,
        44,
        10,
        2 );
    ]

let check_command_error report code fragment =
  require
    (member "diagnostics" report = `List [])
    "command failure must not fabricate source diagnostics";
  let error = member "command_error" report in
  check_keys "native command error" [ "code"; "message" ] error;
  require
    (string "code" error = code && contains (string "message" error) fragment)
    ("expected command error " ^ code ^ ", got " ^ Yojson.Safe.to_string error)

let invalid_configuration () =
  with_file ".hc" "(6*);" (fun source ->
      List.iter
        (fun (ir, bytes, resource) ->
          let report =
            native_json ~mode:"jit"
              ~options:
                [
                  "--ir-instruction-limit=" ^ string_of_int ir;
                  "--code-byte-limit=" ^ string_of_int bytes;
                ]
              1 source
          in
          check_limits report ir bytes;
          check_command_error report "HCBACK0001" resource)
        [
          (0, 65536, "max_ir_instructions");
          (-1, 65536, "max_ir_instructions");
          (100001, 65536, "max_ir_instructions");
          (max_int, 65536, "max_ir_instructions");
          (4096, 0, "max_code_bytes");
          (4096, -1, "max_code_bytes");
          (4096, 16777217, "max_code_bytes");
          (4096, max_int, "max_code_bytes");
        ];
      List.iter
        (fun stack ->
          let report =
            native_json ~mode:"jit"
              ~options:[ "--stack-byte-limit=" ^ string_of_int stack ]
              1 source
          in
          check_limits ~stack report 4096 65536;
          check_command_error report "HCBACK0001" "max_stack_bytes")
        [ -1; 4089 ];
      let report =
        native_json ~mode:"jit" ~options:[ "--include-depth-limit=-1" ] 1 source
      in
      check_command_error report "HCNATIVE0003"
        "invalid preprocessor configuration";
      List.iter
        (fun option ->
          let stdout, stderr =
            checked_invoke 1 [ "eval-native"; option; source ]
          in
          require
            (stdout = ""
            && String.starts_with ~prefix:"holyc: eval-native: HCBACK0001:"
                 stderr
            && not (contains stderr "HCPARSE"))
            ("human invalid limit is rejected before source parsing: " ^ option))
        [
          "--ir-instruction-limit=0";
          "--stack-byte-limit=-1";
          "--stack-byte-limit=4089";
        ])

let invalid_options () =
  with_file ".hc" "6*7;" (fun source ->
      List.iter
        (fun (option, name) ->
          let stdout, stderr =
            checked_invoke 124
              [ "eval-native"; "--format=json"; option; source ]
          in
          require
            (stdout = "" && contains stderr name)
            ("invalid CLI option must fail argument parsing: " ^ option))
        [
          ("--ir-instruction-limit=invalid", "ir-instruction-limit");
          ("--code-byte-limit=invalid", "code-byte-limit");
          ("--stack-byte-limit=invalid", "stack-byte-limit");
          ("--mode=invalid", "mode");
          ("--target=ir", "target");
          ("--step-limit=5", "step-limit");
          ("--unknown-native-option", "unknown-native-option");
        ]);
  let stdout, stderr = checked_invoke 124 [ "eval-native"; "--format=json" ] in
  require
    (stdout = "" && stderr <> "")
    "a missing source argument must fail argument parsing";
  (* The shared Arg.file converter rejects missing paths before the native
     command body runs, including when JSON output was requested. *)
  with_file ".missing.hc" "" (fun source ->
      Sys.remove source;
      List.iter
        (fun mode ->
          List.iter
            (fun format ->
              let stdout, stderr =
                checked_invoke 124
                  [
                    "eval-native";
                    "--format=" ^ format;
                    "--mode=" ^ mode;
                    source;
                  ]
              in
              (* Cmdliner may wrap at spaces within the filename itself. *)
              let filename = Filename.basename source in
              require
                (stdout = ""
                && contains
                     (normalize_whitespace stderr)
                     (normalize_whitespace filename))
                (Printf.sprintf
                   "missing-file argument diagnostic must retain %S on stderr\n\
                    stdout: %s\n\
                    stderr: %s"
                   filename stdout stderr))
            [ "human"; "json" ])
        [ "jit"; "aot" ])

let existing_eval_contract () =
  List.iter
    (fun (text, type_, decimal, steps) ->
      with_file ".hc" text (fun source ->
          List.iter
            (fun mode ->
              let stdout, stderr =
                checked_invoke 0
                  [ "eval"; "--format=json"; "--mode=" ^ mode; source ]
              in
              require (stderr = "") "existing eval success diagnostics";
              let report = Yojson.Safe.from_string stdout in
              check_keys "existing eval v1"
                [
                  "schema";
                  "reference_commit";
                  "word_type";
                  "word";
                  "executed_steps";
                ]
                report;
              require
                (string "schema" report = "holyc-integer-expression-v1"
                && string "reference_commit" report = reference_commit
                && string "word_type" report = type_
                && string "word" report = decimal
                && integer "executed_steps" report = steps)
                "eval must retain its existing IR result schema and semantics")
            [ "jit"; "aot" ]))
    [
      ("6*7;", "I64", "42", 5);
      ("0xFFFFFFFFFFFFFFFF;", "U64", "18446744073709551615", 3);
      ("6/2;", "I64", "3", 5);
      ("1<<2;", "I64", "4", 5);
      ("2&&4;", "I64", "1", 5);
      ("2^^4;", "I64", "0", 5);
      ("(~0x8000000000000000)>0>-1;", "I64", "0", 11);
    ];
  List.iter
    (fun (text, code) ->
      with_file ".hc" text (fun source ->
          let stdout, stderr =
            checked_invoke 1 [ "eval"; "--format=json"; source ]
          in
          require (stdout = "") "existing eval errors must leave stdout empty";
          let diagnostics = Yojson.Safe.from_string stderr |> to_list in
          require
            (diagnostics <> [] && string "code" (List.hd diagnostics) = code)
            "existing eval must retain its JSON diagnostic array on stderr"))
    [ ("1/0;", "HCIRVM0009"); ("(6*);", "HCPARSE0018") ]

let () =
  multiplication_and_modes ();
  full_width_values ();
  predicate_values ();
  logical_values ();
  spill_frames ();
  unsupported_sources ();
  budgets ();
  exact_image_budgets ();
  invalid_configuration ();
  invalid_options ();
  existing_eval_contract ()
