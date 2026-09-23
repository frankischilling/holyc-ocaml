open Yojson.Safe.Util
module Runtime = Holyc_lib.Native_program_execution

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc native Print cli " suffix in
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
    (Array.length Sys.argv = 5)
    "usage: test_native_print_cli.exe <holyc.exe> \
     <integer-persistent-arrays.hc> <integer-formatting.hc> \
     <quoted-formatting.hc>"

let compiler = Sys.argv.(1)
let maintained_fixture = Sys.argv.(2)
let formatting_fixture = Sys.argv.(3)
let quoted_fixture = Sys.argv.(4)

let invoke arguments =
  with_file ".stdout" "" (fun stdout ->
      with_file ".stderr" "" (fun stderr ->
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
          (status, read stdout, read stderr)))

let status_name = function
  | Unix.WEXITED code -> Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal

let checked_invoke expected arguments =
  let status, stdout, stderr = invoke arguments in
  require
    (status = Unix.WEXITED expected)
    (Printf.sprintf "%s: expected exit %d, got %s\nstdout: %s\nstderr: %s"
       (String.concat " " arguments)
       expected (status_name status) stdout stderr);
  (stdout, stderr)

let host_path ?(status = 0) ?(format = "json") ?(options = []) ~mode source =
  checked_invoke status
    ([
       "run";
       "--target=host-jit";
       "--report-version=2";
       "--format=" ^ format;
       "--mode=" ^ mode;
     ]
    @ options @ [ source ])

let host_file ?status ?(format = "json") ?options ~mode contents =
  with_file ".hc" contents (fun source ->
      host_path ?status ~format ?options ~mode source)

let ir_path ?(status = 0) ?(options = []) ~mode source =
  checked_invoke status
    ([
       "run";
       "--target=ir";
       "--report-version=2";
       "--format=json";
       "--mode=" ^ mode;
     ]
    @ options @ [ source ])

let parse_json label (stdout, stderr) =
  require (stderr = "") (label ^ " wrote stderr: " ^ stderr);
  Yojson.Safe.from_string stdout

let host_json_path ?status ?options ~mode source =
  host_path ?status ?options ~mode source |> parse_json "host JSON report"

let host_json ?status ?options ~mode contents =
  host_file ?status ?options ~mode contents |> parse_json "host JSON report"

let ir_json_path ?status ?options ~mode source =
  ir_path ?status ?options ~mode source |> parse_json "IR JSON report"

let diagnostics report = report |> member "diagnostics" |> to_list

let first_code report =
  match diagnostics report with
  | first :: _ -> first |> member "code" |> to_string
  | [] -> (
      match member "command_error" report with
      | `Assoc _ as error -> error |> member "code" |> to_string
      | _ -> failwith "expected a diagnostic or command error")

let check_success label report =
  require
    (report |> member "outcome" |> to_string = "success"
    && report |> member "termination" |> to_string = "stream-end"
    && diagnostics report = []
    && member "command_error" report = `Null)
    (label ^ " success report")

let check_word label report =
  let value = member "final_value" report in
  require
    (value |> member "type" |> to_string = "i64"
    && value |> member "value" |> to_string = "42"
    && value |> member "bits" |> to_string = "0x000000000000002a")
    (label ^ " final value")

let check_output label report ~hex ~bytes ~work =
  require
    (report |> member "output_hex" |> to_string = hex)
    (label ^ " output hex");
  require
    (report |> member "output_byte_length" |> to_int = bytes)
    (label ^ " output byte length");
  require
    (report |> member "output_work" |> to_int = work)
    (label ^ " output work")

let exact_options =
  [
    "--step-limit=101";
    "--initializer-step-limit=21";
    "--dimension-work-limit=4";
    "--global-byte-limit=43";
    "--literal-byte-limit=3";
    "--frame-byte-limit=24";
    "--call-depth-limit=2";
    "--output-byte-limit=2";
    "--output-work-limit=8";
  ]

let maintained_baseline () =
  let fixture = read maintained_fixture in
  require
    (String.length fixture > 0)
    "maintained Print fixture must not be empty";
  List.iter
    (fun mode ->
      let native = host_json_path ~mode maintained_fixture in
      let interpreted = ir_json_path ~mode maintained_fixture in
      check_success (mode ^ " native maintained fixture") native;
      check_success (mode ^ " IR maintained fixture") interpreted;
      check_word (mode ^ " native maintained fixture") native;
      check_word (mode ^ " IR maintained fixture") interpreted;
      check_output
        (mode ^ " native maintained fixture")
        native ~hex:"3432" ~bytes:2 ~work:8;
      check_output
        (mode ^ " IR maintained fixture")
        interpreted ~hex:"3432" ~bytes:2 ~work:8;
      require
        (native |> member "executed_steps" |> to_int = 101
        && interpreted |> member "executed_steps" |> to_int = 101)
        (mode ^ " maintained runtime steps");
      require
        (native |> member "compiled_initializer_steps" |> to_int = 21)
        (mode ^ " native closed initializer preparation");
      require
        (interpreted |> member "compiled_initializer_steps" |> to_int = 23)
        (mode ^ " interpreter initializer preparation");
      require
        (native |> member "dimension_preparation_work" |> to_int = 4
        && interpreted |> member "dimension_preparation_work" |> to_int = 4)
        (mode ^ " maintained dimension work");
      require
        (native |> member "target" |> to_string = "host-jit"
        && native |> member "arithmetic" |> to_string = "runtime-native")
        (mode ^ " native report identity"))
    [ "jit"; "aot" ]

let replace_option prefix replacement options =
  replacement
  :: List.filter
       (fun option ->
         not (String.starts_with ~prefix:("--" ^ prefix ^ "=") option))
       options

let exact_and_one_below_limits () =
  List.iter
    (fun mode ->
      let exact =
        host_json_path ~mode ~options:exact_options maintained_fixture
      in
      check_success (mode ^ " exact maintained limits") exact;
      check_word (mode ^ " exact maintained limits") exact;
      check_output
        (mode ^ " exact maintained limits")
        exact ~hex:"3432" ~bytes:2 ~work:8;
      let pre_entry =
        [
          ("initializer-step-limit", 20, "HCIRVM0007");
          ("dimension-work-limit", 3, "HCIRVM0007");
          ("global-byte-limit", 42, "HCBACK0001");
          ("literal-byte-limit", 2, "HCBACK0004");
        ]
      in
      List.iter
        (fun (name, value, code) ->
          let options =
            replace_option name
              ("--" ^ name ^ "=" ^ string_of_int value)
              exact_options
          in
          let failed =
            host_json_path ~status:1 ~mode ~options maintained_fixture
          in
          require (first_code failed = code) (name ^ " one-below diagnostic");
          require
            (member "executed_steps" failed = `Null)
            (name ^ " one-below precedes native entry");
          check_output (name ^ " one below") failed ~hex:"" ~bytes:0 ~work:0)
        pre_entry;
      let bytes =
        host_json_path ~status:1 ~mode
          ~options:
            (replace_option "output-byte-limit" "--output-byte-limit=1"
               exact_options)
          maintained_fixture
      in
      require
        (first_code bytes = "HCIRVM0022")
        "output byte one-below diagnostic";
      check_output "output byte one below" bytes ~hex:"" ~bytes:0 ~work:6;
      let work =
        host_json_path ~status:1 ~mode
          ~options:
            (replace_option "output-work-limit" "--output-work-limit=7"
               exact_options)
          maintained_fixture
      in
      require
        (first_code work = "HCIRVM0023")
        "output work one-below diagnostic";
      check_output "output work one below" work ~hex:"" ~bytes:0 ~work:7;
      let steps =
        host_json_path ~status:1 ~mode
          ~options:
            (replace_option "step-limit" "--step-limit=100" exact_options)
          maintained_fixture
      in
      require
        (first_code steps = "HCIRVM0007")
        "runtime step one-below diagnostic";
      require
        (steps |> member "executed_steps" |> to_int = 100)
        "runtime step one-below count";
      check_output "runtime step one below" steps ~hex:"3432" ~bytes:2 ~work:8;
      let frame =
        host_json_path ~status:1 ~mode
          ~options:
            (replace_option "frame-byte-limit" "--frame-byte-limit=23"
               exact_options)
          maintained_fixture
      in
      require
        (first_code frame = "HCIRVM0011")
        "Print frame one-below diagnostic";
      check_output "Print frame one below" frame ~hex:"" ~bytes:0 ~work:0;
      let depth =
        host_json_path ~status:1 ~mode
          ~options:
            (replace_option "call-depth-limit" "--call-depth-limit=1"
               exact_options)
          maintained_fixture
      in
      require
        (first_code depth = "HCIRVM0015")
        "nested call depth one-below diagnostic";
      check_output "nested call depth one below" depth ~hex:"3432" ~bytes:2
        ~work:8)
    [ "jit"; "aot" ]

let atomic_failure_prefixes () =
  let invalid =
    "extern U0 Print(U8 *fmt,...);extern U0 PutChars(U64 \
     ch);Print(\"A\");PutChars('B');Print(\"C%j\");42;"
  in
  let capacity =
    "extern U0 Print(U8 *fmt,...);Print(\"A\");Print(\"BC\");42;"
  in
  List.iter
    (fun mode ->
      let invalid_report = host_json ~status:1 ~mode invalid in
      require
        (first_code invalid_report = "HCIRVM0024")
        (mode ^ " invalid format diagnostic");
      check_output
        (mode ^ " invalid format prefix")
        invalid_report ~hex:"4142" ~bytes:2 ~work:9;
      let capacity_report =
        host_json ~status:1 ~mode ~options:[ "--output-byte-limit=2" ] capacity
      in
      require
        (first_code capacity_report = "HCIRVM0022")
        (mode ^ " atomic capacity diagnostic");
      check_output
        (mode ^ " atomic capacity prefix")
        capacity_report ~hex:"41" ~bytes:1 ~work:7)
    [ "jit"; "aot" ]

let implicit_and_source_defined_paths () =
  List.iter
    (fun mode ->
      let implicit = host_json ~mode "extern U0 Print(U8 *fmt,...);42;\"x\";" in
      check_success (mode ^ " implicit Print") implicit;
      check_word (mode ^ " implicit Print") implicit;
      check_output (mode ^ " implicit Print") implicit ~hex:"78" ~bytes:1
        ~work:3;
      let source =
        host_json ~mode "I64 Print(U8 *fmt){return 42;}Print(\"x\");"
      in
      check_success (mode ^ " source-defined Print") source;
      check_word (mode ^ " source-defined Print") source;
      check_output
        (mode ^ " source-defined Print")
        source ~hex:"" ~bytes:0 ~work:0)
    [ "jit"; "aot" ]

let binary_capture () =
  List.iter
    (fun mode ->
      let report =
        host_json ~mode
          "extern U0 Print(U8 \
           *fmt,...);Print(\"\\x80%s%c%%\",\"\\xff\",0x81fe);42;"
      in
      check_success (mode ^ " binary Print") report;
      check_word (mode ^ " binary Print") report;
      check_output (mode ^ " binary Print") report ~hex:"80fffe8125" ~bytes:5
        ~work:18)
    [ "jit"; "aot" ]

let hex bytes =
  bytes |> String.to_seq
  |> Seq.map (fun byte -> Printf.sprintf "%02x" (Char.code byte))
  |> List.of_seq |> String.concat ""

let expanded_format_reports () =
  List.iter
    (fun mode ->
      List.iter
        (fun (case : Integer_format_fixture.t) ->
          with_file ".hc" (Integer_format_fixture.source case) (fun source ->
              List.iter
                (fun report ->
                  check_success case.label report;
                  check_word case.label report;
                  check_output case.label report ~hex:(hex case.bytes)
                    ~bytes:(String.length case.bytes) ~work:case.work)
                [ host_json_path ~mode source; ir_json_path ~mode source ]))
        (List.filter
           (fun (case : Integer_format_fixture.t) ->
             List.mem case.label
               [
                 "unsigned maximum";
                 "grouped zero padding starts with comma";
                 "dynamic zero-padded hexadecimal";
                 "star overrides literal precision";
                 "binary format and string padding";
                 "full packed word truncation";
                 "quoted dollar and percent modifiers";
                 "decoded invalid hex is left for the next token";
                 "decoded NUL still scans later input";
                 "decoded NUL padding preserves the next argument";
                 "decoded high byte";
                 "quoted truncation splits an escape";
                 "uppercase full packed word";
                 "uppercase packed high bytes stay unchanged";
                 "quoted high bytes stay unchanged";
               ])
           (Integer_format_fixture.all @ Quoted_format_fixture.all));
      let expected = "0000002A|OK   |-0042|18446744073709551615\n" in
      List.iter
        (fun report ->
          check_success "maintained formatter fixture" report;
          check_word "maintained formatter fixture" report;
          check_output "maintained formatter fixture" report ~hex:(hex expected)
            ~bytes:42 ~work:68)
        [
          host_json_path ~mode formatting_fixture;
          ir_json_path ~mode formatting_fixture;
        ];
      let bounded =
        host_json_path ~mode
          ~options:[ "--output-byte-limit=42"; "--output-work-limit=68" ]
          formatting_fixture
      in
      check_success "maintained formatter exact limits" bounded;
      let below =
        host_json_path ~status:1 ~mode
          ~options:[ "--output-byte-limit=41" ]
          formatting_fixture
      in
      require
        (first_code below = "HCIRVM0022")
        "maintained formatter byte limit one below";
      check_output "maintained formatter atomic second call" below
        ~hex:(hex "0000002A") ~bytes:8 ~work:67;
      List.iter
        (fun (label, body, work, code) ->
          let report =
            host_json ~status:1 ~mode
              ("extern U0 Print(U8 *fmt,...);" ^ body ^ "42;")
          in
          require (first_code report = code) (label ^ " diagnostic");
          check_output label report ~hex:"" ~bytes:0 ~work)
        (Integer_format_fixture.invalid_fields
       @ Quoted_format_fixture.invalid_fields))
    [ "jit"; "aot" ]

let quoted_fixture_reports () =
  let expected = "a\\\"\\d\\n|42|OK\n" in
  List.iter
    (fun mode ->
      List.iter
        (fun report ->
          check_success "maintained quoted fixture" report;
          check_word "maintained quoted fixture" report;
          check_output "maintained quoted fixture" report ~hex:(hex expected)
            ~bytes:14 ~work:54)
        [
          host_json_path ~mode quoted_fixture; ir_json_path ~mode quoted_fixture;
        ];
      let exact =
        host_json_path ~mode
          ~options:[ "--output-byte-limit=14"; "--output-work-limit=54" ]
          quoted_fixture
      in
      check_success "quoted fixture exact bounds" exact;
      List.iter
        (fun (options, code) ->
          let report = host_json_path ~status:1 ~mode ~options quoted_fixture in
          require
            (first_code report = code)
            "quoted fixture one-below diagnostic";
          check_output "quoted fixture one-below atomic draft" report ~hex:""
            ~bytes:0 ~work:53)
        [
          ([ "--output-byte-limit=13" ], "HCIRVM0022");
          ([ "--output-work-limit=53" ], "HCIRVM0023");
        ];
      let stdout, stderr = host_path ~format:"human" ~mode quoted_fixture in
      require (stderr = "") "quoted human report stderr";
      let lines = stdout |> String.split_on_char '\n' |> List.map String.trim in
      List.iter
        (fun expected ->
          require (List.mem expected lines)
            ("quoted human report missing: " ^ expected))
        [
          "output-byte-length=14";
          "output-work=54";
          "output-hex=" ^ hex expected;
        ];
      List.iter
        (fun (label, body, code, work) ->
          let report =
            host_json ~status:1 ~mode ("extern U0 Print(U8 *fmt,...);" ^ body)
          in
          require (first_code report = code) (label ^ " CLI diagnostic");
          check_output label report ~hex:"7c" ~bytes:1 ~work:(work + 3))
        Quoted_format_fixture.memory_failures)
    [ "jit"; "aot" ]

let human_fixture_reports () =
  List.iter
    (fun mode ->
      let stdout, stderr = host_path ~format:"human" ~mode maintained_fixture in
      require (stderr = "") (mode ^ " human report wrote stderr: " ^ stderr);
      let lines = stdout |> String.split_on_char '\n' |> List.map String.trim in
      List.iter
        (fun expected ->
          require (List.mem expected lines)
            (mode ^ " human report missing: " ^ expected))
        [
          "steps=101";
          "compiled-initializer-steps=21";
          "dimension-preparation-work=4";
          "final-value=42 type=i64 bits=0x000000000000002a";
          "output-byte-length=2";
          "output-work=8";
          "output-hex=3432";
        ])
    [ "jit"; "aot" ]

let () =
  (match Runtime.platform () with
  | Runtime.Unsupported -> failwith "native Print CLI tests require x86-64"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 -> ());
  maintained_baseline ();
  exact_and_one_below_limits ();
  atomic_failure_prefixes ();
  implicit_and_source_defined_paths ();
  binary_capture ();
  expanded_format_reports ();
  quoted_fixture_reports ();
  human_fixture_reports ()
