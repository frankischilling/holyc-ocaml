open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-stream-generation-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      output_string channel contents;
      close_out channel;
      action path)

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)
let declarations_example = Sys.argv.(3)

let saved_inputs_example =
  Filename.concat (Filename.dirname example) "native-stream-saved-inputs.hc"

let options_example =
  if Array.length Sys.argv > 4 && Sys.argv.(4) <> "--native" then
    Some Sys.argv.(4)
  else None

let native_only = Array.exists (( = ) "--native") Sys.argv
let targets = if native_only then [ "host-jit-task" ] else [ "ir" ]
let count = ref 0
let saved_input_count = ref 0

let invoke ?(status = 0) ?(mode = "jit") ?(options = []) target path =
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
              "--code-byte-limit=524288";
            ]
            @ options @ [ path ]
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
          require (read error = "") "JSON CLI wrote stderr";
          Yojson.Safe.from_string text))

let error code report =
  require
    (report |> member "diagnostics" |> to_list
    |> List.exists (fun d -> d |> member "code" |> to_string = code))
    ("missing stream generation diagnostic " ^ code)

let value report =
  require
    (report |> member "outcome" |> to_string = "success")
    "stream example failed";
  require
    (report |> member "final_value" |> member "value" |> to_string = "42")
    "stream example returned another value";
  require
    (report |> member "output_hex" |> to_string = "6d616465")
    "ordinary output changed or contains generated source"

let () =
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          let saved = invoke ~mode target saved_inputs_example in
          incr saved_input_count;
          require
            (saved |> member "outcome" |> to_string = "success"
            && saved |> member "final_value" |> member "value" |> to_string
               = "42"
            && saved |> member "output_hex" |> to_string
               = "6368696c643b34323b34323b34323b34323b696e6e65723b6772616e643b34323b34323b"
            )
            "nested saved compiler inputs retain original namespaces and \
             ordered output";
          if native_only then
            require
              (saved |> member "arithmetic" |> to_string = "runtime-native"
              && saved |> member "native" |> member "fragments" |> to_list
                 |> List.for_all (fun fragment ->
                     fragment |> member "outcome" |> to_string = "success"))
              "nested saved compiler inputs completed in actual native \
               fragments";
          let saved_steps = saved |> member "executed_steps" |> to_int in
          let saved_work = saved |> member "output_work" |> to_int in
          let saved_bytes = saved |> member "output_byte_length" |> to_int in
          let exact =
            invoke ~mode target saved_inputs_example
              ~options:
                [
                  "--step-limit=" ^ string_of_int saved_steps;
                  "--output-work-limit=" ^ string_of_int saved_work;
                  "--output-byte-limit=" ^ string_of_int saved_bytes;
                ]
          in
          require
            (member "final_value" exact = member "final_value" saved
            && member "output_hex" exact = member "output_hex" saved)
            "nested saved input exact cumulative allowances";
          List.iter
            (fun (option, code) ->
              error code
                (invoke ~status:1 ~mode target saved_inputs_example
                   ~options:[ option ]))
            [
              ("--step-limit=" ^ string_of_int (saved_steps - 1), "HCIRVM0007");
              ( "--output-work-limit=" ^ string_of_int (saved_work - 1),
                "HCIRVM0023" );
              ( "--output-byte-limit=" ^ string_of_int (saved_bytes - 1),
                "HCIRVM0022" );
            ];
          (fun action ->
            match options_example with
            | Some path -> action path
            | None ->
                temporary ".hc"
                  {|#exe {Print("%d;",GetOption(33));Print("%d;",Option(33,1));Print("%d;",StreamExePrint("Print(\"%%d;\",GetOption(33));Print(\"%%d;\",Option(33,0));Print(\"%%d;\",GetOption(33));42;"));Print("%d;",GetOption(33));}42;|}
                  action) (fun path ->
              let options = invoke ~mode target path in
              require
                (options |> member "final_value" |> member "value" |> to_string
                 = "42"
                && options |> member "output_hex" |> to_string
                   = "303b303b313b313b303b34323b313b")
                "child options or restored caller control changed";
              if native_only then
                require
                  (options |> member "native" |> member "fragments" |> to_list
                  |> List.for_all (fun fragment ->
                      fragment |> member "outcome" |> to_string = "success"))
                  "compiler option calls did not complete in machine code");
          temporary ".hc"
            {|#exe {Print("before;");I64 N=StreamExePrint("I64 Count=2;I64 Values[Count]={20,22};I64 ChildFn(){return Values[0]+Values[1];}Print(\"child;\");ChildFn();");Print("after;");StreamPrint("%d;",N);}|}
            (fun path ->
              let child = invoke ~mode target path in
              require
                (child |> member "final_value" |> member "value" |> to_string
                 = "42"
                && child |> member "output_hex" |> to_string
                   = "6265666f72653b6368696c643b61667465723b")
                "original child machine result or ordered output changed";
              if native_only then
                require
                  (child |> member "native" |> member "fragments" |> to_list
                  |> List.for_all (fun fragment ->
                      fragment |> member "outcome" |> to_string = "success"))
                  "actual child lacks completed native fragments");
          let declarations = invoke ~mode target declarations_example in
          require
            (declarations |> member "final_value" |> member "value" |> to_string
           = "42")
            "synchronous child declaration did not reach the outer parser";
          require
            (declarations |> member "output_hex" |> to_string = "6368696c64303b")
            "child declaration completion did not return zero";
          if native_only then
            require
              (declarations |> member "arithmetic" |> to_string
               = "runtime-native"
              && declarations |> member "native" |> member "fragments"
                 |> to_list
                 |> List.for_all (fun fragment ->
                     fragment |> member "outcome" |> to_string = "success"))
              "native declaration source did not complete its machine fragments")
        [ "jit"; "aot" ];
      let baseline = invoke target example in
      value baseline;
      let steps = baseline |> member "executed_steps" |> to_int in
      let work = baseline |> member "output_work" |> to_int in
      value
        (invoke target example
           ~options:
             [
               "--step-limit=" ^ string_of_int steps;
               "--output-work-limit=" ^ string_of_int work;
               "--generated-byte-limit=14";
               "--output-byte-limit=4";
             ]);
      List.iter
        (fun (option, code) ->
          let report = invoke ~status:1 ~options:[ option ] target example in
          error code report)
        [
          ("--step-limit=" ^ string_of_int (steps - 1), "HCIRVM0007");
          ("--output-work-limit=" ^ string_of_int (work - 1), "HCIRVM0023");
          ("--generated-byte-limit=13", "HCIRVM0028");
          ("--output-byte-limit=3", "HCIRVM0022");
        ];
      temporary ".hc" {|#exe {StreamPrint("#exe {StreamPrint(\"42;\");}");}|}
        (fun path ->
          let report = invoke target path in
          require
            (report |> member "final_value" |> member "value" |> to_string
           = "42")
            "nested native source did not resume its original parser");
      temporary ".hc"
        {|extern U0 StreamPrint(U8 *fmt,...);StreamPrint("hello");|}
        (fun path ->
          let report = invoke ~status:1 target path in
          error "HCIRVM0027" report;
          require
            (report |> member "output_work" |> to_int = 11)
            "inactive check ran before native formatting";
          require
            (report |> member "output_hex" |> to_string = "")
            "inactive bytes escaped");
      temporary ".hc"
        {|extern I64 StreamExePrint(U8 *fmt,...);StreamExePrint("%f",42);|}
        (fun path ->
          let report = invoke ~status:1 target path in
          error "HCIRVM0024" report;
          require
            (report |> member "output_work" |> to_int = 2)
            "JIT context check ran before formatting");
      if target = "host-jit-task" then (
        let report = invoke ~mode:"aot" target example in
        value report;
        let fragments =
          report |> member "native" |> member "fragments" |> to_list
        in
        require
          (List.hd (List.rev fragments)
          |> member "kind" |> to_string = "aot-module")
          "AOT module follows original native directives"))
    targets;
  Printf.printf "%d stream generation CLI executions passed\n%!" !count;
  Printf.printf "%d nested saved compiler input CLI cases passed\n%!"
    !saved_input_count
