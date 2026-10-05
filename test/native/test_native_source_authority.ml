open Holyc_lib
module Driver = Holyc_lib__Driver
module Source = Driver.Integer_source_execution
module Dispatch = Driver.Integer_task.Native_dispatch
module Task = Driver.Integer_task
module Unit = Driver.Integer_unit
module VM = Holyc_lib__Ir.Integer_interpreter
module Globals = Holyc_lib__Ir.Integer_globals
module Retained = Holyc_lib__Ir.Retained_function
module Body = Holyc_lib__Ir.Function_body
module Storage = Holyc_lib__Backend.X86_64_global_storage
module Image = X86_64_program
module Runtime = Native_program_execution

type raw_arena

external raw_arena_create : int -> raw_arena = "holyc_native_create_task_arena"

external raw_arena_admit : raw_arena -> int -> int -> (int * string) list -> int
  = "holyc_native_task_arena_admit"

external raw_static_copy : raw_arena -> int * int * int * string -> int
  = "holyc_native_task_static_copy"

external raw_arena_release : raw_arena -> unit
  = "holyc_native_release_task_arena"

type function_bundle = {
  link : Retained.t;
  definition : VM.function_definition;
  globals : Globals.t;
  runtime_calls : Holyc_lib__Ir.Runtime_call_context.t;
  functions : VM.function_definition list;
}

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let describe errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let compiled = function
  | Ok image -> image
  | Error errors ->
      errors
      |> List.map (fun (error : Image.error) ->
          error.code ^ ": " ^ error.message)
      |> String.concat "; " |> Alcotest.fail

let inputs text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-source-authority.hc" ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit () |> checked
  in
  (session, config, source)

let completed report =
  match Runtime.outcome report |> checked with
  | Image.Completed result -> result
  | Image.Fault fault ->
      Alcotest.failf "unexpected native fault at %d" fault.instruction_id

let rejected label result =
  Alcotest.(check bool) label true (Result.is_error result)

let command_function_bundle request =
  let program = Dispatch.command_program request in
  match
    ( Globals.function_publications (Unit.globals program),
      Unit.functions program )
  with
  | [ link ], [ definition ] ->
      {
        link;
        definition;
        globals = Unit.globals program;
        runtime_calls = Unit.runtime_calls program;
        functions = Unit.functions program;
      }
  | _ -> Alcotest.fail "expected one exact function definition publication"

let check_function_bundle label expected (source : VM.task_function_source) =
  Alcotest.(check bool)
    (label ^ " exact definition body")
    true
    (source.source_definition.body == expected.definition.body);
  Alcotest.(check bool)
    (label ^ " exact definition frame")
    true
    (source.source_definition.frame == expected.definition.frame);
  Alcotest.(check bool)
    (label ^ " exact source globals")
    true
    (source.source_globals == expected.globals);
  Alcotest.(check bool)
    (label ^ " exact runtime calls")
    true
    (source.source_runtime_calls == expected.runtime_calls);
  Alcotest.(check int)
    (label ^ " complete function bundle")
    (List.length expected.functions)
    (List.length source.source_functions);
  List.iter2
    (fun (left : VM.function_definition) (right : VM.function_definition) ->
      Alcotest.(check bool)
        (label ^ " physical bundled function")
        true
        (left.body == right.body && left.frame == right.frame))
    source.source_functions expected.functions

let compile_command_both_abis layout request =
  List.iter
    (fun abi ->
      let image =
        Image.compile_task_command ~status_abi:abi ~layout request |> compiled
      in
      Alcotest.(check bool)
        "function-bearing task command keeps requested ABI" true
        (Image.status_abi image = abi))
    [ Image.Windows_x64; Image.System_v_x64 ]

let compile_initializer_both_abis layout request =
  List.iter
    (fun abi ->
      let image =
        Image.compile_task_initializer ~status_abi:abi ~layout request
        |> compiled
      in
      Alcotest.(check bool)
        "function-bearing initializer keeps requested ABI" true
        (Image.status_abi image = abi))
    [ Image.Windows_x64; Image.System_v_x64 ]

let task_run session task index text =
  let source =
    Session.add_source session
      ~path:(Printf.sprintf "native-function-authority-%d.hc" index)
      ~contents:text
  in
  Task.run task ~source

let task_succeeds label = function
  | Ok _ -> ()
  | Error errors -> Alcotest.failf "%s: %s" label (describe errors)

let host_status_abi () =
  match Runtime.platform () with
  | Runtime.Windows_x86_64 -> Image.Windows_x64
  | Runtime.Linux_x86_64 -> Image.System_v_x64
  | Runtime.Unsupported -> Alcotest.fail "native source authority needs x86-64"

let actual_source ?max_layout_work ~adversarial () =
  let session, config, source = inputs "I64 A=41; I64 B=A+1; B;" in
  let compilation_errors errors =
    List.map
      (fun (error : Image.error) ->
        Diagnostic.make ~code:error.code ~severity:Diagnostic.Error
          ~message:error.message
          ~primary:(Driver.Integer_source.source_span source)
          ())
      errors
  in
  let layout =
    Storage.create_task_layout ?max_layout_work ~max_global_bytes:16 ()
    |> Result.map_error (fun errors ->
        errors
        |> List.map (fun (error : Storage.error) -> error.message)
        |> String.concat "; ")
    |> checked
  in
  let arena = Runtime.create_task_arena ~max_arena_bytes:32 layout |> checked in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> checked in
  let saved = ref [] and initializers = ref 0 in
  let execute image =
    saved := image :: !saved;
    Image.check_task_request image |> checked;
    let retained = Runtime.retain_task_fragment arena image |> checked in
    Fun.protect
      ~finally:(fun () -> Runtime.release retained |> checked)
      (fun () ->
        if adversarial && !initializers = 1 then (
          let foreign_budget =
            Runtime.create_budget ~max_steps:100_000 () |> checked
          in
          let closed = Runtime.retain_task_fragment arena image |> checked in
          Runtime.release closed |> checked;
          rejected "released code cannot consume its still-live source request"
            (Runtime.execute_retained_budget_report foreign_budget closed
            |> Runtime.outcome);
          Image.check_task_request image |> checked;
          let foreign_domain =
            Domain.spawn (fun () ->
                Runtime.execute_retained_budget_report foreign_budget retained
                |> Runtime.outcome)
          in
          rejected "foreign first entry cannot bind the original arena budget"
            (Domain.join foreign_domain);
          Alcotest.(check int)
            "rejected first entry leaves its foreign budget unused" 0
            (Runtime.budget_progress foreign_budget).executed_steps;
          Alcotest.(check int)
            "rejected first entry preserves the original budget" 0
            (Runtime.budget_progress budget).executed_steps;
          Image.check_task_request image |> checked);
        if adversarial && !initializers = 2 then (
          let prior = Runtime.budget_progress budget in
          let foreign_budget =
            Runtime.create_budget ~max_steps:100_000 () |> checked
          in
          rejected "fresh budget cannot reset original native allowance"
            (Runtime.execute_retained_budget_report foreign_budget retained
            |> Runtime.outcome);
          Alcotest.(check int)
            "foreign budget remains unused" 0
            (Runtime.budget_progress foreign_budget).executed_steps;
          Alcotest.(check int)
            "original allowance remains unchanged" prior.executed_steps
            (Runtime.budget_progress budget).executed_steps;
          Image.check_task_request image |> checked;
          let foreign_domain =
            Domain.spawn (fun () -> Image.check_task_activation image)
          in
          rejected "foreign execution domain cannot claim original leaf"
            (Domain.join foreign_domain);
          Image.check_task_request image |> checked;
          rejected "standalone execution cannot replay source image"
            (Runtime.execute_report ~max_steps:100_000 image |> Runtime.outcome);
          rejected "ordinary retention cannot create private task data"
            (Runtime.retain image);
          let foreign_layout =
            Image.create_task_layout ~max_global_bytes:16 |> compiled
          in
          let foreign_arena =
            Runtime.create_task_arena ~max_arena_bytes:32 foreign_layout
            |> checked
          in
          Fun.protect
            ~finally:(fun () ->
              Runtime.release_task_arena foreign_arena |> checked)
            (fun () ->
              rejected "foreign arena cannot borrow original source image"
                (Runtime.retain_task_fragment foreign_arena image));
          Image.check_task_request image |> checked);
        Gc.full_major ();
        let report = Runtime.execute_retained_budget_report budget retained in
        let result = completed report in
        let charged = (Runtime.budget_progress budget).executed_steps in
        rejected "source native entry can be claimed only once"
          (Image.check_task_request image);
        rejected "retained handle cannot replay its original parser leaf"
          (Runtime.execute_retained_budget_report budget retained
          |> Runtime.outcome);
        Alcotest.(check int)
          "replay rejection preserves charged work" charged
          (Runtime.budget_progress budget).executed_steps;
        (result, Runtime.value_captured report))
  in
  let native_dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun request ->
          incr initializers;
          match Image.compile_task_initializer ~layout request with
          | Error errors -> Error (compilation_errors errors)
          | Ok image ->
              ignore (execute image);
              Ok ());
      execute_command =
        (fun request ->
          match Image.compile_task_command ~layout request with
          | Error errors -> Error (compilation_errors errors)
          | Ok image ->
              let result, captured = execute image in
              Ok
                (if captured then
                   Dispatch.Captured
                     (Option.map
                        (fun (word : Image.word) ->
                          match word.type_ with
                          | Image.I64 -> Dispatch.I64 word.bits
                          | U64 -> Dispatch.U64 word.bits)
                        result.final_value)
                 else Dispatch.Unchanged));
    }
  in
  let report =
    Fun.protect
      ~finally:(fun () -> Runtime.release_task_arena arena |> checked)
      (fun () ->
        Source.run ~native_dispatch session ~config ~source ~max_steps:100_000)
  in
  List.iter
    (fun image ->
      rejected "closed source request cannot compile or enter again"
        (Image.check_task_request image);
      rejected "closed source entry stays consumed"
        (Image.check_task_activation image);
      rejected "released arena cannot be used by saved fragment"
        (Runtime.retain_task_fragment arena image))
    !saved;
  (report, Storage.task_layout_work layout, Runtime.budget_progress budget)

let array_source ?(max_arena_bytes = 41) () =
  let session, config, source = inputs "I64 A[2]={41,1}; I64 B=A[0]+A[1]; B;" in
  let compilation_errors errors =
    List.map
      (fun (error : Image.error) ->
        Diagnostic.make ~code:error.code ~severity:Diagnostic.Error
          ~message:error.message
          ~primary:(Driver.Integer_source.source_span source)
          ())
      errors
  in
  let native_error message =
    [
      Diagnostic.make ~code:"HCRUN0004" ~severity:Diagnostic.Error ~message
        ~primary:(Driver.Integer_source.source_span source)
        ();
    ]
  in
  let layout = Image.create_task_layout ~max_global_bytes:24 |> compiled in
  let arena = Runtime.create_task_arena ~max_arena_bytes layout |> checked in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> checked in
  let observed = ref [] and saved = ref [] in
  let execute image =
    observed := (Image.global_bytes image, Image.arena_bytes image) :: !observed;
    saved := image :: !saved;
    Image.check_task_request image |> checked;
    match Runtime.retain_task_fragment arena image with
    | Error message -> Error message
    | Ok retained ->
        Fun.protect
          ~finally:(fun () -> Runtime.release retained |> checked)
          (fun () ->
            let report =
              Runtime.execute_retained_budget_report budget retained
            in
            let result = completed report in
            rejected "array source request is consumed by its original entry"
              (Image.check_task_request image);
            Ok (report, result))
  in
  let native_dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun request ->
          match Image.compile_task_initializer ~layout request with
          | Error errors -> Error (compilation_errors errors)
          | Ok image -> (
              match execute image with
              | Ok _ -> Ok ()
              | Error message -> Error (native_error message)));
      execute_command =
        (fun request ->
          match Image.compile_task_command ~layout request with
          | Error errors -> Error (compilation_errors errors)
          | Ok image -> (
              match execute image with
              | Error message -> Error (native_error message)
              | Ok (report, result) ->
                  Ok
                    (if Runtime.value_captured report then
                       Dispatch.Captured
                         (Option.map
                            (fun (word : Image.word) ->
                              match word.type_ with
                              | Image.I64 -> Dispatch.I64 word.bits
                              | U64 -> Dispatch.U64 word.bits)
                            result.final_value)
                     else Dispatch.Unchanged)));
    }
  in
  let report =
    Fun.protect
      ~finally:(fun () -> Runtime.release_task_arena arena |> checked)
      (fun () ->
        Source.run ~native_dispatch session ~config ~source ~max_steps:100_000)
  in
  List.iter
    (fun image ->
      rejected "array request expires after its source callback"
        (Image.check_task_request image);
      rejected "released array arena cannot retain a saved fragment"
        (Runtime.retain_task_fragment arena image))
    !saved;
  ( report,
    List.rev !observed,
    Storage.task_layout_work layout,
    Runtime.budget_progress budget )

let array_layout_and_capacity () =
  let report, extents, work, _ = array_source () in
  Source.outcome report |> Result.map_error describe |> checked |> ignore;
  Alcotest.(check bool)
    "array native final word" true
    (Source.native_final_value report = Some (Dispatch.I64 42L));
  Alcotest.(check (list (pair int int)))
    "append-only array logical and arena extents"
    [ (16, 32); (16, 32); (24, 41); (24, 41) ]
    extents;
  Alcotest.(check int) "exact array layout work" 6 work;
  let report, extents, work, _ = array_source ~max_arena_bytes:40 () in
  rejected "one-byte-short task arena stops the scalar suffix"
    (Source.outcome report);
  Alcotest.(check (list (pair int int)))
    "short arena observes the exact rejected extent"
    [ (16, 32); (16, 32); (24, 41) ]
    extents;
  Alcotest.(check int)
    "rejected scalar suffix still charges its admitted layout visit" 4 work

let array_abi_compilation () =
  let session, config, source = inputs "I64 A[2]={41,1};" in
  let reached = ref false in
  let stop () =
    Error
      [
        Diagnostic.make ~code:"HCRUN0004" ~severity:Diagnostic.Error
          ~message:"array ABI probe stops before native entry"
          ~primary:(Driver.Integer_source.source_span source)
          ();
      ]
  in
  let native_dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun request ->
          List.iter
            (fun abi ->
              let layout =
                Image.create_task_layout ~max_global_bytes:16 |> compiled
              in
              let image =
                Image.compile_task_initializer ~status_abi:abi ~layout request
                |> compiled
              in
              Alcotest.(check bool)
                "array task keeps requested status ABI" true
                (Image.status_abi image = abi);
              Alcotest.(check int)
                "array ABI logical bytes" 16 (Image.global_bytes image);
              Alcotest.(check int)
                "array ABI arena bytes" 32 (Image.arena_bytes image))
            [ Image.Windows_x64; Image.System_v_x64 ];
          reached := true;
          stop ());
      execute_command =
        (fun _ -> Alcotest.fail "array ABI probe reached a command");
    }
  in
  ignore
    (Source.run ~native_dispatch session ~config ~source ~max_steps:100_000);
  Alcotest.(check bool)
    "array ABI probe reached its original leaf" true !reached

let foreign_array_source_layout () =
  let create_layout () =
    Image.create_task_layout ~max_global_bytes:16 |> compiled
  in
  let compile_original layout =
    let session, config, source = inputs "I64 A[2]={41,1};" in
    let observed = ref None in
    let stop () =
      Error
        [
          Diagnostic.make ~code:"HCRUN0004" ~severity:Diagnostic.Error
            ~message:"array layout probe stops before native entry"
            ~primary:(Driver.Integer_source.source_span source)
            ();
        ]
    in
    let native_dispatch : Dispatch.t =
      {
        execute_initializer =
          (fun request ->
            observed := Some (Image.compile_task_initializer ~layout request);
            stop ());
        execute_command =
          (fun _ -> Alcotest.fail "array layout probe reached a command");
      }
    in
    ignore
      (Source.run ~native_dispatch session ~config ~source ~max_steps:100_000);
    match !observed with
    | Some result -> result
    | None -> Alcotest.fail "array layout probe did not reach its original leaf"
  in
  let layout = create_layout () in
  let original = compile_original layout |> compiled in
  Alcotest.(check int)
    "foreign array probe logical bytes" 16
    (Image.global_bytes original);
  Alcotest.(check int)
    "foreign array probe arena bytes" 32
    (Image.arena_bytes original);
  rejected "saved array request expires after its callback"
    (Image.check_task_request original);
  let admitted = Storage.task_layout_work layout in
  Alcotest.(check int) "first array leaf consumes one layout visit" 1 admitted;
  (match compile_original layout with
  | Ok _ ->
      Alcotest.fail "equal array source in a foreign task acquired the layout"
  | Error errors ->
      Alcotest.(check bool)
        "foreign array source reaches the task-owner guard" true
        (List.exists
           (fun (error : Image.error) ->
             error.code = "HCBACK0003"
             && error.message
                = "native task storage belongs to another original task")
           errors));
  Alcotest.(check int)
    "foreign array source consumes no layout allowance" admitted
    (Storage.task_layout_work layout)

let foreign_owners_and_budget () =
  let report, work, progress = actual_source ~adversarial:true () in
  Source.outcome report |> Result.map_error describe |> checked |> ignore;
  Alcotest.(check bool)
    "actual native final word" true
    (Source.native_final_value report = Some (Dispatch.I64 42L));
  Alcotest.(check int) "real native cumulative work" 17 progress.executed_steps;
  Alcotest.(check bool) "layout visits are accounted" true (work > 0)

let layout_work_limits () =
  let _, exact, _ = actual_source ~adversarial:false () in
  let report, work, _ =
    actual_source ~max_layout_work:exact ~adversarial:false ()
  in
  Source.outcome report |> Result.map_error describe |> checked |> ignore;
  Alcotest.(check int) "exact cumulative layout allowance" exact work;
  (* A's leaf visits its one retained binding. The later B leaf cannot fit in
     the remaining allowance; A's admitted layout and native write survive. *)
  let report, work, progress =
    actual_source ~max_layout_work:1 ~adversarial:false ()
  in
  rejected "insufficient layout allowance stops original source"
    (Source.outcome report);
  Alcotest.(check int)
    "failed B layout preserves only A's admitted visit" 1 work;
  Alcotest.(check int)
    "failed B layout preserves A's five native instructions" 5
    progress.executed_steps;
  let report, work, progress =
    actual_source ~max_layout_work:(exact - 1) ~adversarial:false ()
  in
  rejected "one-below layout allowance stops the final command"
    (Source.outcome report);
  Alcotest.(check int)
    "failed final command preserves both initializer layouts" 3 work;
  Alcotest.(check int)
    "both original initializer entries remain charged" 13
    progress.executed_steps;
  List.iter
    (fun limit ->
      rejected "invalid layout work bound"
        (Storage.create_task_layout ~max_layout_work:limit ~max_global_bytes:16
           ()))
    [ 0; -1; Storage.hard_max_task_layout_work + 1 ]

let foreign_source_layout () =
  let create_layout () =
    Image.create_task_layout ~max_global_bytes:16 |> compiled
  in
  let compile_original layout =
    let session, config, source = inputs "I64 A=41;" in
    let observed = ref None in
    let stop () =
      Error
        [
          Diagnostic.make ~code:"HCRUN0004" ~severity:Diagnostic.Error
            ~message:"source layout probe stops before native entry"
            ~primary:(Driver.Integer_source.source_span source)
            ();
        ]
    in
    let native_dispatch : Dispatch.t =
      {
        execute_initializer =
          (fun request ->
            observed := Some (Image.compile_task_initializer ~layout request);
            stop ());
        execute_command =
          (fun _ -> Alcotest.fail "layout probe reached a command");
      }
    in
    ignore
      (Source.run ~native_dispatch session ~config ~source ~max_steps:100_000);
    match !observed with
    | Some result -> result
    | None -> Alcotest.fail "layout probe did not reach the original live leaf"
  in
  let layout = create_layout () in
  let original = compile_original layout |> compiled in
  rejected "saved original request expires after its callback"
    (Image.check_task_request original);
  let admitted = Storage.task_layout_work layout in
  (match compile_original layout with
  | Ok _ ->
      Alcotest.fail
        "equal source in a foreign task acquired the original layout"
  | Error errors ->
      Alcotest.(check bool)
        "foreign source reaches the original layout guard" true
        (List.exists
           (fun (error : Image.error) ->
             error.code = "HCBACK0003"
             && error.message
                = "native task storage belongs to another original task")
           errors));
  Alcotest.(check int)
    "foreign source consumes no layout allowance" admitted
    (Storage.task_layout_work layout);
  ignore (compile_original (create_layout ()) |> compiled)

let released_arena_preserves_request () =
  let session, config, source = inputs "I64 A=41;" in
  let layout = Image.create_task_layout ~max_global_bytes:8 |> compiled in
  let arena = Runtime.create_task_arena ~max_arena_bytes:16 layout |> checked in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> checked in
  let reached = ref false in
  let native_dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun request ->
          let image =
            Image.compile_task_initializer ~layout request |> compiled
          in
          let retained = Runtime.retain_task_fragment arena image |> checked in
          Fun.protect
            ~finally:(fun () -> Runtime.release retained |> checked)
            (fun () ->
              Runtime.release_task_arena arena |> checked;
              rejected "released arena rejects before native source claim"
                (Runtime.execute_retained_budget_report budget retained
                |> Runtime.outcome);
              Image.check_task_request image |> checked;
              reached := true;
              Error
                [
                  Diagnostic.make ~code:"HCRUN0004" ~severity:Diagnostic.Error
                    ~message:"released arena probe stops its source callback"
                    ~primary:(Driver.Integer_source.source_span source)
                    ();
                ]));
      execute_command =
        (fun _ -> Alcotest.fail "released arena probe reached a command");
    }
  in
  let report =
    Fun.protect
      ~finally:(fun () -> Runtime.release_task_arena arena |> checked)
      (fun () ->
        Source.run ~native_dispatch session ~config ~source ~max_steps:100_000)
  in
  rejected "released arena source stops" (Source.outcome report);
  Alcotest.(check bool)
    "released arena check ran at the live leaf" true !reached;
  Alcotest.(check int)
    "released arena never enters native code" 0
    (Runtime.budget_progress budget).executed_steps

let native_function_source_registry () =
  let session = Session.create () in
  let layout = Image.create_task_layout ~max_global_bytes:32 |> compiled in
  let first = ref None and wrap = ref None and replacement = ref None in
  let command_index = ref 0 and initializer_index = ref 0 in
  let exact_command label request bundle =
    let source =
      Dispatch.command_function_source request bundle.link |> checked
    in
    check_function_bundle label bundle source
  in
  let exact_initializer label request bundle =
    let source =
      Dispatch.initializer_function_source request bundle.link |> checked
    in
    check_function_bundle label bundle source
  in
  let native_dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun request ->
          incr initializer_index;
          compile_initializer_both_abis layout request;
          (match !initializer_index with
          | 1 ->
              Alcotest.(check bool)
                "first initializer precedes function publication" true
                (Option.is_none !first)
          | 2 ->
              let first_bundle = Option.get !first in
              let replacement_bundle = Option.get !replacement in
              exact_initializer "initializer historical source" request
                first_bundle;
              exact_initializer "initializer current source" request
                replacement_bundle;
              let copied =
                Retained.create (Retained.metadata first_bundle.link)
              in
              rejected
                "equal-metadata retained copy has no initializer authority"
                (Dispatch.initializer_function_source request copied);
              let foreign =
                Domain.spawn (fun () ->
                    Dispatch.initializer_function_source request
                      first_bundle.link)
              in
              rejected
                "foreign domain cannot inspect the live initializer request"
                (Domain.join foreign);
              exact_initializer
                "original initializer survives foreign inspection" request
                first_bundle
          | _ -> Alcotest.fail "unexpected native initializer request");
          let retained = Option.map (fun bundle -> bundle.link) !first in
          Dispatch.claim_initializer_request request |> checked;
          Option.iter
            (fun link ->
              rejected "entered initializer cannot resolve source metadata"
                (Dispatch.initializer_function_source request link))
            retained;
          Ok ());
      execute_command =
        (fun request ->
          incr command_index;
          match !command_index with
          | 1 ->
              let bundle = command_function_bundle request in
              compile_command_both_abis layout request;
              rejected "same-command function is unavailable before claim"
                (Dispatch.command_function_source request bundle.link);
              first := Some bundle;
              Dispatch.claim_command_request request |> checked;
              rejected
                "entered definition request cannot resolve source metadata"
                (Dispatch.command_function_source request bundle.link);
              Ok Dispatch.Unchanged
          | 2 ->
              let bundle = command_function_bundle request in
              compile_command_both_abis layout request;
              exact_command "retained dependency source" request
                (Option.get !first);
              rejected "new wrapper is unavailable before its own claim"
                (Dispatch.command_function_source request bundle.link);
              wrap := Some bundle;
              Dispatch.claim_command_request request |> checked;
              Ok Dispatch.Unchanged
          | 3 ->
              let bundle = command_function_bundle request in
              compile_command_both_abis layout request;
              exact_command "old definition before replacement claim" request
                (Option.get !first);
              rejected "replacement is unavailable before its own claim"
                (Dispatch.command_function_source request bundle.link);
              replacement := Some bundle;
              Dispatch.claim_command_request request |> checked;
              Ok Dispatch.Unchanged
          | 4 ->
              compile_command_both_abis layout request;
              let first_bundle = Option.get !first in
              let wrap_bundle = Option.get !wrap in
              let replacement_bundle = Option.get !replacement in
              exact_command "historical first definition" request first_bundle;
              exact_command "historical wrapper definition" request wrap_bundle;
              exact_command "current replacement definition" request
                replacement_bundle;
              let copied =
                Retained.create (Retained.metadata first_bundle.link)
              in
              rejected "equal-metadata retained copy has no command authority"
                (Dispatch.command_function_source request copied);
              let foreign =
                Domain.spawn (fun () ->
                    Dispatch.command_function_source request first_bundle.link)
              in
              rejected "foreign domain cannot inspect the live command request"
                (Domain.join foreign);
              Dispatch.claim_command_request request |> checked;
              rejected "entered call request cannot resolve source metadata"
                (Dispatch.command_function_source request first_bundle.link);
              Ok (Dispatch.Captured (Some (Dispatch.I64 42L)))
          | _ -> Alcotest.fail "unexpected native command request");
    }
  in
  let task = Task.create ~native_dispatch session |> checked in
  task_succeeds "original scalar declaration"
    (task_run session task 0 "I64 A=41;");
  task_succeeds "first function definition"
    (task_run session task 1 "I64 F(){return A+1;}");
  task_succeeds "wrapper definition"
    (task_run session task 2 "I64 Wrap(){return F();}");
  task_succeeds "replacement function definition"
    (task_run session task 3 "I64 F(){return 100;}");
  task_succeeds "initializer retained function source"
    (task_run session task 4 "I64 B=F();");
  task_succeeds "historical wrapper call request"
    (task_run session task 5 "Wrap();");
  Alcotest.(check int) "two original initializer requests" 2 !initializer_index;
  Alcotest.(check int) "three definitions and one call request" 4 !command_index;
  Alcotest.(check bool)
    "manual native capture records the final word" true
    (Task.native_final_value task = Some (Dispatch.I64 42L))

let native_function_preclaim_failure () =
  let session = Session.create () in
  let saved_link = ref None and saved_request = ref None in
  let commands = ref 0 in
  let native_dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun _ ->
          Alcotest.fail "preclaim function probe reached an initializer");
      execute_command =
        (fun request ->
          incr commands;
          match !commands with
          | 1 ->
              let bundle = command_function_bundle request in
              saved_link := Some bundle.link;
              saved_request := Some request;
              rejected "unclaimed definition has no exact native source"
                (Dispatch.command_function_source request bundle.link);
              let span = Option.get (Body.span bundle.definition.body) in
              Error
                [
                  Diagnostic.make ~code:"HCRUN0004" ~severity:Diagnostic.Error
                    ~message:"intentional preclaim function publication failure"
                    ~primary:span ();
                ]
          | _ -> Alcotest.fail "unexpected preclaim probe command");
    }
  in
  let task = Task.create ~native_dispatch session |> checked in
  rejected "intentional preclaim callback failure stops the first input"
    (task_run session task 20 "I64 Rejected(){return 41;}");
  let request = Option.get !saved_request and link = Option.get !saved_link in
  rejected "closed failed request cannot inspect source metadata"
    (Dispatch.command_function_source request link);
  rejected "closed failed request cannot replay its claim"
    (Dispatch.claim_command_request request);
  rejected "later input cannot call an unadmitted definition"
    (task_run session task 21 "Rejected();");
  Alcotest.(check int)
    "failed source cannot bypass its revoked admission" 1 !commands;
  Alcotest.(check bool)
    "failed preclaim source has no native value" true
    (Task.native_final_value task = None)

let native_function_preflight_retry () =
  let session = Session.create () in
  let layout = Image.create_task_layout ~max_global_bytes:8 |> compiled in
  let arena = Runtime.create_task_arena ~max_arena_bytes:16 layout |> checked in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> checked in
  let saved = ref None and commands = ref 0 in
  let native_dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun _ ->
          Alcotest.fail "function preflight probe reached an initializer");
      execute_command =
        (fun request ->
          incr commands;
          (match !commands with
          | 1 ->
              let bundle = command_function_bundle request in
              saved := Some bundle;
              rejected "insufficient code allowance rejects before claim"
                (Image.compile_task_command ~max_code_bytes:1 ~layout request);
              Dispatch.check_command_request request |> checked;
              rejected "rejected compilation has not published its function"
                (Dispatch.command_function_source request bundle.link);
              Alcotest.(check int)
                "rejected compilation never entered native code" 0
                (Runtime.budget_progress budget).executed_steps
          | 2 ->
              let bundle = Option.get !saved in
              let source =
                Dispatch.command_function_source request bundle.link |> checked
              in
              check_function_bundle "source after valid preflight retry" bundle
                source
          | _ -> Alcotest.fail "unexpected preflight retry command");
          let image = Image.compile_task_command ~layout request |> compiled in
          let retained = Runtime.retain_task_fragment arena image |> checked in
          Fun.protect
            ~finally:(fun () -> Runtime.release retained |> checked)
            (fun () ->
              let report =
                Runtime.execute_retained_budget_report budget retained
              in
              let result = completed report in
              Ok
                (if Runtime.value_captured report then
                   Dispatch.Captured
                     (Option.map
                        (fun (word : Image.word) ->
                          match word.type_ with
                          | Image.I64 -> Dispatch.I64 word.bits
                          | U64 -> Dispatch.U64 word.bits)
                        result.final_value)
                 else Dispatch.Unchanged)));
    }
  in
  Fun.protect
    ~finally:(fun () -> Runtime.release_task_arena arena |> checked)
    (fun () ->
      let task = Task.create ~native_dispatch session |> checked in
      task_succeeds "original request remains usable after compile rejection"
        (task_run session task 22 "I64 Retry(){return 42;} Retry();");
      Alcotest.(check int)
        "definition and later call each claim once" 2 !commands;
      Alcotest.(check bool)
        "retried original function executes natively" true
        (Task.native_final_value task = Some (Dispatch.I64 42L)))

let native_function_reached_fault_retains_source () =
  let session = Session.create () in
  let layout = Image.create_task_layout ~max_global_bytes:8 |> compiled in
  let arena = Runtime.create_task_arena ~max_arena_bytes:16 layout |> checked in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> checked in
  let saved = ref None and command_index = ref 0 in
  let fault_reached = ref false and later_resolved = ref false in
  let execute request =
    let image =
      Image.compile_task_command ~status_abi:(host_status_abi ()) ~layout
        request
      |> compiled
    in
    let retained = Runtime.retain_task_fragment arena image |> checked in
    Fun.protect
      ~finally:(fun () -> Runtime.release retained |> checked)
      (fun () ->
        let report = Runtime.execute_retained_budget_report budget retained in
        match Runtime.outcome report with
        | Error message -> Alcotest.fail message
        | Ok outcome -> (report, outcome))
  in
  let native_dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun _ -> Alcotest.fail "fault retention probe reached an initializer");
      execute_command =
        (fun request ->
          incr command_index;
          match !command_index with
          | 1 ->
              let bundle = command_function_bundle request in
              saved := Some bundle;
              let report, outcome = execute request in
              (match outcome with
              | Image.Completed execution ->
                  Alcotest.(check bool)
                    "definition command has no captured value" true
                    (execution.final_value = None
                    && not (Runtime.value_captured report))
              | Image.Fault _ ->
                  Alcotest.fail "definition command faulted before publication");
              Ok Dispatch.Unchanged
          | 2 -> (
              let _, outcome = execute request in
              match outcome with
              | Image.Fault fault ->
                  Alcotest.(check bool)
                    "actual reached native fault" true
                    (fault.kind = Image.Division_by_zero);
                  fault_reached := true;
                  let fallback =
                    Option.get (Body.span (Option.get !saved).definition.body)
                  in
                  Error
                    [
                      Diagnostic.make ~code:"HCIRVM0009"
                        ~severity:Diagnostic.Error
                        ~message:"intentional reached native division fault"
                        ~primary:(Option.value fault.span ~default:fallback)
                        ();
                    ]
              | Image.Completed _ ->
                  Alcotest.fail "division probe unexpectedly completed")
          | 3 -> (
              let bundle = Option.get !saved in
              let source =
                Dispatch.command_function_source request bundle.link |> checked
              in
              check_function_bundle "post-fault admitted source" bundle source;
              later_resolved := true;
              let report, outcome = execute request in
              match outcome with
              | Image.Completed execution ->
                  let captured = Runtime.value_captured report in
                  let value =
                    Option.map
                      (fun (word : Image.word) ->
                        match word.type_ with
                        | Image.I64 -> Dispatch.I64 word.bits
                        | U64 -> Dispatch.U64 word.bits)
                      execution.final_value
                  in
                  Ok
                    (if captured then Dispatch.Captured value
                     else Dispatch.Unchanged)
              | Image.Fault _ ->
                  Alcotest.fail "retained function call faulted after recovery")
          | _ -> Alcotest.fail "unexpected fault-retention command");
    }
  in
  let task = Task.create ~native_dispatch session |> checked in
  let first = task_run session task 30 "I64 Persist(){return 42;}1/0;" in
  rejected "reached native division stops the first input" first;
  Alcotest.(check bool) "native fault was actually entered" true !fault_reached;
  ignore (task_run session task 31 "Persist();");
  Alcotest.(check bool)
    "post-fault request resolves the previously admitted exact source" true
    !later_resolved;
  Alcotest.(check bool)
    "post-fault retained body still returns through native execution" true
    (Task.native_final_value task = Some (Dispatch.I64 42L));
  Runtime.release_task_arena arena |> checked

let native_provider_source_authority () =
  let module Calls = Holyc_lib__Ir.Runtime_call_context in
  let module Graph = Holyc_lib__Ir.Block_graph in
  let module Sequence = Holyc_lib__Ir.Instruction_sequence in
  let provider_calls bundle =
    Graph.blocks (Body.body bundle.definition.body)
    |> List.concat_map (fun block ->
        Sequence.instructions (Graph.instructions block))
    |> List.filter_map (fun instruction ->
        let raw = Sequence.description instruction in
        Calls.find_start bundle.runtime_calls
          ~owner:(Calls.Function bundle.definition.body) raw.instruction_id)
  in
  let provider_call bundle = List.hd (provider_calls bundle) in
  let declaration =
    "extern U0 PutChars(U64 ch);extern U0 Print(U8 *fmt,...);"
  in
  let definition =
    "I64 F(){U8 \
     Format[4];Format[0]=37;Format[1]=100;Format[2]=59;Format[3]=0;PutChars('A');Print(Format,42);return \
     42;}"
  in
  let foreign_bundle = ref None in
  let foreign_session = Session.create () in
  let foreign_dispatch : Dispatch.t =
    {
      execute_initializer = (fun _ -> Alcotest.fail "foreign initializer");
      execute_command =
        (fun request ->
          if Unit.functions (Dispatch.command_program request) <> [] then
            foreign_bundle := Some (command_function_bundle request);
          Dispatch.claim_command_request request |> checked;
          Ok Dispatch.Unchanged);
    }
  in
  let foreign_task =
    Task.create ~native_dispatch:foreign_dispatch foreign_session |> checked
  in
  task_succeeds "foreign provider declaration"
    (task_run foreign_session foreign_task 50 declaration);
  task_succeeds "foreign original body"
    (task_run foreign_session foreign_task 51 definition);
  let foreign = Option.get !foreign_bundle in
  let session = Session.create () in
  let layout = Image.create_task_layout ~max_global_bytes:8 |> compiled in
  let original = ref None and saved_request = ref None in
  let original_check available =
    let bundle = Option.get !original in
    Alcotest.(check int)
      "both original providers remain in the body" 2
      (List.length (provider_calls bundle));
    List.fold_left
      (fun checked_prior call ->
        Result.bind checked_prior (fun prior ->
            available ~runtime_calls:bundle.runtime_calls
              ~owner:(Calls.Function bundle.definition.body) call
            |> Result.map (fun available -> prior && available)))
      (Ok true) (provider_calls bundle)
  in
  let command_checks request =
    let available = Dispatch.command_provider_available request in
    Alcotest.(check bool)
      "original retained provider is available" true
      (original_check available |> checked);
    rejected "another task's equal source cannot authorize output"
      (available ~runtime_calls:foreign.runtime_calls
         ~owner:(Calls.Function foreign.definition.body) (provider_call foreign));
    let bundle = Option.get !original in
    rejected "foreign call cannot borrow the original context"
      (available ~runtime_calls:bundle.runtime_calls
         ~owner:(Calls.Function bundle.definition.body) (provider_call foreign));
    rejected "entry ownership cannot replace original function ownership"
      (available ~runtime_calls:bundle.runtime_calls ~owner:Calls.Entry
         (provider_call bundle));
    let worker = Domain.spawn (fun () -> original_check available) in
    rejected "another domain cannot use the live provider request"
      (Domain.join worker);
    Alcotest.(check bool)
      "rejection preserves legitimate provider inspection" true
      (original_check available |> checked)
  in
  let native_dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun request ->
          compile_initializer_both_abis layout request;
          let available = Dispatch.initializer_provider_available request in
          Alcotest.(check bool)
            "original initializer provider is available" true
            (original_check available |> checked);
          rejected "foreign initializer output owner"
            (available ~runtime_calls:foreign.runtime_calls
               ~owner:(Calls.Function foreign.definition.body)
               (provider_call foreign));
          Dispatch.claim_initializer_request request |> checked;
          rejected "entered initializer revokes provider inspection"
            (original_check available);
          Ok ());
      execute_command =
        (fun request ->
          let definitions = Unit.functions (Dispatch.command_program request) in
          (match definitions with
          | [ _ ] ->
              original := Some (command_function_bundle request);
              rejected "unadmitted body grants no provider authority"
                (original_check (Dispatch.command_provider_available request))
          | [] when Option.is_some !original ->
              saved_request := Some request;
              command_checks request
          | [] -> ()
          | _ -> Alcotest.fail "unexpected provider function bundle");
          compile_command_both_abis layout request;
          Dispatch.claim_command_request request |> checked;
          if Option.is_some !original then
            rejected "entered command revokes provider inspection"
              (original_check (Dispatch.command_provider_available request));
          Ok Dispatch.Unchanged);
    }
  in
  let task = Task.create ~native_dispatch session |> checked in
  task_succeeds "original provider declaration"
    (task_run session task 52 declaration);
  task_succeeds "original provider body" (task_run session task 53 definition);
  task_succeeds "original provider caller" (task_run session task 54 "F();");
  rejected "saved command cannot reuse provider authority"
    (original_check
       (Dispatch.command_provider_available (Option.get !saved_request)));
  task_succeeds "original provider initializer"
    (task_run session task 55 "I64 A=F();")

let native_literal_source_authority () =
  let module Calls = Holyc_lib__Ir.Runtime_call_context in
  let module Graph = Holyc_lib__Ir.Block_graph in
  let module Sequence = Holyc_lib__Ir.Instruction_sequence in
  let module Literals = Holyc_lib__Backend.X86_64_literal_storage in
  let definition = "I64 F(){U8 *p=\"A\";p[0]++;return p[0];}" in
  let foreign = ref None in
  let foreign_session = Session.create () in
  let foreign_dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun _ -> Alcotest.fail "foreign literal initializer");
      execute_command =
        (fun request ->
          foreign := Some (command_function_bundle request);
          Dispatch.claim_command_request request |> checked;
          Ok Dispatch.Unchanged);
    }
  in
  let foreign_task =
    Task.create ~native_dispatch:foreign_dispatch foreign_session |> checked
  in
  task_succeeds "foreign original literal body"
    (task_run foreign_session foreign_task 60 definition);
  let foreign = Option.get !foreign in
  let session = Session.create () in
  let layout =
    Image.create_task_layout_with_literals ~max_global_bytes:1
      ~max_literal_bytes:2
    |> compiled
  in
  let arena = Runtime.create_task_arena ~max_arena_bytes:98 layout |> checked in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> checked in
  let saved_request = ref None and original = ref None in
  let native_dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun _ -> Alcotest.fail "literal authority initializer");
      execute_command =
        (fun request ->
          (match Unit.functions (Dispatch.command_program request) with
          | [ _ ] ->
              let bundle = command_function_bundle request in
              original := Some bundle;
              let graph = Body.body bundle.definition.body in
              let owner = Calls.Function bundle.definition.body in
              ignore
                (Literals.source ~runtime_calls:bundle.runtime_calls ~owner
                   ~graph
                |> Result.map_error (fun errors ->
                    errors
                    |> List.map (fun (error : Literals.error) -> error.message)
                    |> String.concat "; ")
                |> checked);
              rejected "foreign context cannot own the original literal graph"
                (Literals.source ~runtime_calls:foreign.runtime_calls ~owner
                   ~graph);
              rejected "foreign graph cannot borrow original literal context"
                (Literals.source ~runtime_calls:bundle.runtime_calls
                   ~owner:(Calls.Function foreign.definition.body)
                   ~graph:(Body.body foreign.definition.body));
              rejected "entry owner cannot substitute for the function literal"
                (Literals.source ~runtime_calls:bundle.runtime_calls
                   ~owner:Calls.Entry ~graph);
              let copied =
                Graph.create
                  ~entry:(Graph.block_id (Graph.entry graph))
                  (Graph.blocks graph
                  |> List.map (fun block ->
                      {
                        Graph.block_id = Graph.block_id block;
                        instructions =
                          Sequence.instructions (Graph.instructions block)
                          |> List.map (fun instruction ->
                              let raw = Sequence.description instruction in
                              { raw with Sequence.flags = raw.flags });
                      }))
                |> Result.map_error (fun _ ->
                    "copied graph construction failed")
                |> checked
              in
              rejected "copied producers and graph confer no literal ownership"
                (Literals.source ~runtime_calls:bundle.runtime_calls ~owner
                   ~graph:copied);
              rejected "failed code compilation leaves literal request live"
                (Image.compile_task_command ~max_code_bytes:1 ~layout request);
              Dispatch.check_command_request request |> checked
          | [] ->
              saved_request := Some request;
              let bundle = Option.get !original in
              ignore
                (Dispatch.command_function_source request bundle.link |> checked)
          | _ -> Alcotest.fail "unexpected literal function bundle");
          let worker =
            Domain.spawn (fun () -> Image.compile_task_command ~layout request)
          in
          rejected
            "foreign domain cannot compile or append task literal storage"
            (Domain.join worker);
          List.iter
            (fun abi ->
              let image =
                Image.compile_task_command ~status_abi:abi ~layout request
                |> compiled
              in
              Alcotest.(check int)
                "both ABIs reuse the original producer's two bytes" 2
                (Image.literal_bytes image);
              Alcotest.(check int)
                "both ABIs reuse the exact canonical table extent" 98
                (Image.arena_bytes image))
            [ Image.Windows_x64; Image.System_v_x64 ];
          let image =
            Image.compile_task_command ~status_abi:(host_status_abi ()) ~layout
              request
            |> compiled
          in
          rejected
            "retention quota rejection leaves original literal request live"
            (Runtime.retain_task_fragment ~max_literal_bytes:1 arena image);
          Dispatch.check_command_request request |> checked;
          let retained =
            Runtime.retain_task_fragment ~max_literal_bytes:2 arena image
            |> checked
          in
          Fun.protect
            ~finally:(fun () -> Runtime.release retained |> checked)
            (fun () ->
              let report =
                Runtime.execute_retained_budget_report ~max_literal_bytes:2
                  budget retained
              in
              let result = completed report in
              rejected
                "entered source cannot append literals through a saved request"
                (Image.compile_task_command ~layout request);
              Ok
                (if Runtime.value_captured report then
                   Dispatch.Captured
                     (Option.map
                        (fun (word : Image.word) ->
                          match word.type_ with
                          | Image.I64 -> Dispatch.I64 word.bits
                          | U64 -> Dispatch.U64 word.bits)
                        result.final_value)
                 else Dispatch.Unchanged)));
    }
  in
  let task = Task.create ~native_dispatch session |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release_task_arena arena |> checked)
    (fun () ->
      task_succeeds "original literal definition"
        (task_run session task 61 definition);
      task_succeeds "first original literal mutation"
        (task_run session task 62 "F();");
      Gc.full_major ();
      task_succeeds "later source keeps original mutated literal"
        (task_run session task 63 "F();");
      Alcotest.(check bool)
        "actual native storage persists across source runs" true
        (Task.native_final_value task = Some (Dispatch.I64 67L));
      rejected "expired caller cannot readmit original literal storage"
        (Image.compile_task_command ~layout (Option.get !saved_request)))

let native_static_source_authority () =
  let module Allocation = Task.Native_static_allocation in
  let module Initializer = Task.Native_static_initializer in
  let session = Session.create () in
  let layout = Image.create_task_layout ~max_global_bytes:32 |> compiled in
  let arena = Runtime.create_task_arena ~max_arena_bytes:64 layout |> checked in
  let foreign_layout =
    Image.create_task_layout ~max_global_bytes:32 |> compiled
  in
  let foreign_arena =
    Runtime.create_task_arena ~max_arena_bytes:64 foreign_layout |> checked
  in
  let released_layout =
    Image.create_task_layout ~max_global_bytes:32 |> compiled
  in
  let released_arena =
    Runtime.create_task_arena ~max_arena_bytes:64 released_layout |> checked
  in
  Runtime.release_task_arena released_arena |> checked;
  let budget = Runtime.create_budget ~max_steps:100_000 () |> checked in
  let saved_allocation = ref None and saved_initializer = ref None in
  let allocation_count = ref 0 and initializer_count = ref 0 in
  let execute image =
    let retained = Runtime.retain_task_fragment arena image |> checked in
    Fun.protect
      ~finally:(fun () -> Runtime.release retained |> checked)
      (fun () ->
        let report = Runtime.execute_retained_budget_report budget retained in
        (completed report, Runtime.value_captured report))
  in
  let dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun request ->
          ignore
            (execute
               (Image.compile_task_initializer ~layout request |> compiled));
          Ok ());
      execute_command =
        (fun request ->
          let result, captured =
            execute (Image.compile_task_command ~layout request |> compiled)
          in
          Ok
            (if captured then
               Dispatch.Captured
                 (Option.map
                    (fun (word : Image.word) ->
                      match word.type_ with
                      | Image.I64 -> Dispatch.I64 word.bits
                      | U64 -> Dispatch.U64 word.bits)
                    result.final_value)
             else Dispatch.Unchanged));
    }
  in
  let allocate request =
    incr allocation_count;
    saved_allocation := Some request;
    Allocation.check request |> checked;
    Alcotest.(check int)
      "allocation metadata contains no data payload" 0
      (Globals.byte_size (Allocation.context request));
    let before = (Runtime.budget_progress budget).executed_steps in
    rejected "released arena cannot consume the offered allocation"
      (Runtime.allocate_task_static released_arena request);
    Allocation.check request |> checked;
    rejected "another domain cannot reserve the original static allocation"
      (Domain.join
         (Domain.spawn (fun () -> Runtime.allocate_task_static arena request)));
    Allocation.check request |> checked;
    Runtime.allocate_task_static arena request |> checked;
    rejected "original allocation cannot be replayed"
      (Runtime.allocate_task_static arena request);
    Alcotest.(check int)
      "allocation executes no source instructions" before
      (Runtime.budget_progress budget).executed_steps;
    Ok ()
  in
  let initialize request =
    incr initializer_count;
    saved_initializer := Some request;
    Initializer.check request |> checked;
    let module Fragment = Holyc_lib__Sema.Static_initializer_fragment in
    let module Program = Holyc_lib__Ir.Static_initializer_program in
    let module Destination = Holyc_lib__Ir.Static_initializer_destination in
    let fragment =
      Initializer.program request |> Program.destination |> Destination.fragment
    in
    let references = Fragment.references fragment in
    let create references =
      Fragment.create_selected ~references
        ~table:(Session.semantic_symbols session)
        ~namespace:(Fragment.namespace fragment)
        ~publication:(Fragment.publication fragment)
        ~receipt:(Fragment.receipt fragment)
        ~dimensions:(Fragment.dimensions fragment)
        ~environment:(Fragment.environment fragment)
        ~queries:(Fragment.queries fragment)
    in
    create references |> checked |> ignore;
    (match references with
    | [ first; (second_identifier, _) ] ->
        rejected
          "another occurrence selecting the same static cannot substitute"
          (create [ first; (second_identifier, snd first) ])
    | _ -> ());
    rejected "initializer cannot borrow an arena from another source"
      (Image.compile_task_static_initializer ~layout:foreign_layout request);
    Initializer.check request |> checked;
    rejected "static code limit failure leaves the original leaf offered"
      (Image.compile_task_static_initializer ~max_code_bytes:1 ~layout request);
    Initializer.check request |> checked;
    rejected "another domain cannot compile the original static leaf"
      (Domain.join
         (Domain.spawn (fun () ->
              Image.compile_task_static_initializer ~layout request)));
    List.iter
      (fun status_abi ->
        let image =
          Image.compile_task_static_initializer ~status_abi ~layout request
          |> compiled
        in
        Alcotest.(check int)
          "both ABIs reuse padded static and global bytes"
          (if !initializer_count = 1 then 16 else 24)
          (Image.global_bytes image))
      [ Image.Windows_x64; Image.System_v_x64 ];
    ignore
      (execute
         (Image.compile_task_static_initializer ~status_abi:(host_status_abi ())
            ~layout request
         |> compiled));
    rejected "entered original static leaf cannot compile again"
      (Image.compile_task_static_initializer ~layout request);
    Ok ()
  in
  let foreign_session = Session.create () in
  let foreign_dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun _ -> Alcotest.fail "foreign allocation initializer");
      execute_command = (fun _ -> Alcotest.fail "foreign allocation command");
    }
  in
  let foreign_allocate request =
    Runtime.allocate_task_static foreign_arena request |> checked;
    let original =
      Allocation.allocation request
      |> Holyc_lib__Ir.Integer_static_allocation.source
      |> Holyc_lib__Sema.Compiler_record.static_allocation_receipt
    in
    Error
      [
        Diagnostic.make ~code:"HCRUN0004" ~severity:Diagnostic.Error
          ~message:"stop after the foreign source allocation"
          ~primary:original.allocation_function.function_name.location.span ();
      ]
  in
  Fun.protect
    ~finally:(fun () ->
      Runtime.release_task_arena arena |> checked;
      Runtime.release_task_arena foreign_arena |> checked)
    (fun () ->
      let foreign_task =
        Task.create ~native_dispatch:foreign_dispatch
          ~native_static_allocation:foreign_allocate foreign_session
        |> checked
      in
      rejected "foreign source stops after its real native allocation"
        (task_run foreign_session foreign_task 70
           "I64 H(){static I64 A;return 0;}");
      let task =
        Task.create ~native_dispatch:dispatch ~native_static_allocation:allocate
          ~native_static_initializer:initialize session
        |> checked
      in
      task_succeeds "original native global prefix"
        (task_run session task 71 "I64 X=40;X++;");
      task_succeeds "live private allocation and initializer"
        (task_run session task 72 "I64 F(){static I64 A=X,B=A+A;return ++A;}");
      task_succeeds "first retained static call"
        (task_run session task 73 "F();");
      Gc.full_major ();
      Gc.compact ();
      task_succeeds "later retained static call after collection"
        (task_run session task 74 "F();");
      Alcotest.(check bool)
        "native static and previous global writes survive arena growth" true
        (Task.native_final_value task = Some (Dispatch.I64 43L));
      Alcotest.(check int)
        "each static allocation is offered once" 2 !allocation_count;
      Alcotest.(check int)
        "each static initializer is offered once" 2 !initializer_count;
      rejected "expired allocation cannot reserve another arena"
        (Runtime.allocate_task_static foreign_arena
           (Option.get !saved_allocation));
      rejected "expired initializer cannot borrow original arena storage"
        (Image.compile_task_static_initializer ~layout
           (Option.get !saved_initializer)))

let native_static_copy_authority () =
  let module Copy = Task.Native_static_copy in
  let module Storage = Holyc_lib__Backend.X86_64_global_storage in
  let module Destination = Holyc_lib__Ir.Static_initializer_destination in
  let module Layout = Holyc_lib__Ir.Integer_initializer_layout in
  let session = Session.create () in
  let layout = Image.create_task_layout ~max_global_bytes:64 |> compiled in
  let arena =
    Runtime.create_task_arena ~max_arena_bytes:512 layout |> checked
  in
  let foreign_layout =
    Image.create_task_layout ~max_global_bytes:64 |> compiled
  in
  let foreign_arena =
    Runtime.create_task_arena ~max_arena_bytes:512 foreign_layout |> checked
  in
  let released_layout =
    Image.create_task_layout ~max_global_bytes:64 |> compiled
  in
  let released_arena =
    Runtime.create_task_arena ~max_arena_bytes:512 released_layout |> checked
  in
  Runtime.release_task_arena released_arena |> checked;
  let budget = Runtime.create_budget ~max_steps:100_000 () |> checked in
  let saved = ref None and task_owner = ref None and count = ref 0 in
  let execute image =
    let retained = Runtime.retain_task_fragment arena image |> checked in
    Fun.protect
      ~finally:(fun () -> Runtime.release retained |> checked)
      (fun () ->
        let report = Runtime.execute_retained_budget_report budget retained in
        (completed report, Runtime.value_captured report))
  in
  let dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun request ->
          ignore
            (execute
               (Image.compile_task_initializer ~layout request |> compiled));
          Ok ());
      execute_command =
        (fun request ->
          let result, captured =
            execute (Image.compile_task_command ~layout request |> compiled)
          in
          Ok
            (if captured then
               Dispatch.Captured
                 (Option.map
                    (fun (word : Image.word) ->
                      match word.type_ with
                      | Image.I64 -> Dispatch.I64 word.bits
                      | U64 -> Dispatch.U64 word.bits)
                    result.final_value)
             else Dispatch.Unchanged));
    }
  in
  let copy request =
    incr count;
    saved := Some request;
    let owner = Option.get !task_owner in
    let before = (Task.progress owner).runtime.initializer_steps in
    let steps = (Runtime.budget_progress budget).executed_steps in
    Copy.check request |> checked;
    rejected "released arena cannot consume the live byte-copy leaf"
      (Runtime.copy_task_static released_arena request);
    rejected "foreign arena cannot write another source's allocation"
      (Runtime.copy_task_static foreign_arena request);
    rejected "another domain cannot consume the copy"
      (Domain.join
         (Domain.spawn (fun () -> Runtime.copy_task_static arena request)));
    Copy.check request |> checked;
    let destination = Copy.destination request in
    let original =
      match Destination.operation destination with
      | Layout.Copy_bytes bytes -> bytes
      | Scalar_store -> Alcotest.fail "copy operation lost"
    in
    Bytes.set (Bytes.unsafe_of_string original) 0 'Z';
    let fresh =
      match Destination.operation destination with
      | Layout.Copy_bytes bytes -> bytes
      | Scalar_store -> Alcotest.fail "copy operation lost"
    in
    Alcotest.(check bool)
      "observed source payload cannot mutate retained copy bytes" true
      (fresh.[0] <> 'Z');
    rejected "checked plan cannot use another admitted extent"
      (Storage.prepare_static_copy layout request ~admitted_arena_bytes:1);
    let admitted = 49 in
    let plan =
      Storage.prepare_static_copy layout request ~admitted_arena_bytes:admitted
      |> checked
    in
    rejected "copy plan cannot substitute a foreign layout"
      (Storage.check_static_copy plan ~layout:foreign_layout ~request);
    let _, _, _, bytes = Storage.static_copy_payload plan in
    Bytes.set (Bytes.unsafe_of_string bytes) 0 'Z';
    let _, _, _, fresh = Storage.static_copy_payload plan in
    Alcotest.(check bool)
      "copy plan observation cannot mutate retained bytes" true
      (fresh.[0] <> 'Z');
    Alcotest.(check int)
      "rejected copy consumers spend no allowance" before
      (Task.progress owner).runtime.initializer_steps;
    Runtime.copy_task_static arena request |> checked;
    Alcotest.(check int)
      "entered copy charges its original byte count once" (before + 2)
      (Task.progress owner).runtime.initializer_steps;
    Alcotest.(check int)
      "compiler copy executes no expression instructions" steps
      (Runtime.budget_progress budget).executed_steps;
    rejected "entered copy cannot be replayed"
      (Runtime.copy_task_static arena request);
    rejected "entered copy invalidates its earlier offered plan"
      (Storage.check_static_copy plan ~layout ~request);
    Ok ()
  in
  Fun.protect
    ~finally:(fun () ->
      Runtime.release_task_arena arena |> checked;
      Runtime.release_task_arena foreign_arena |> checked)
    (fun () ->
      let task =
        Task.create ~native_dispatch:dispatch
          ~native_static_allocation:(fun request ->
            Runtime.allocate_task_static arena request |> checked;
            Ok ())
          ~native_static_copy:copy session
        |> checked
      in
      task_owner := Some task;
      task_succeeds "earlier original global allocation"
        (task_run session task 80 "I64 X=40;X++;");
      task_succeeds "original nested direct copy leaves"
        (task_run session task 81
           "I64 F(){static U8 A[2][2]={\"AB\",\"CD\"};return ++A[1][0];}");
      task_succeeds "first original byte-array call"
        (task_run session task 82 "F();");
      Gc.full_major ();
      Gc.compact ();
      task_succeeds "later allocation preserves original copied storage"
        (task_run session task 83 "U8 Z[2]={20,22};F();");
      Alcotest.(check bool)
        "original copied bytes and mutation survive collection and arena growth"
        true
        (Task.native_final_value task = Some (Dispatch.I64 69L));
      Alcotest.(check int) "original nested copy leaves run once" 2 !count;
      rejected "expired parser leaf cannot copy into any arena"
        (Runtime.copy_task_static arena (Option.get !saved)))

let native_static_copy_host_bounds () =
  let handle = raw_arena_create 40 in
  let invalid action =
    match action () with
    | exception Invalid_argument _ -> ()
    | _ -> Alcotest.fail "malformed raw static copy changed native storage"
  in
  Fun.protect
    ~finally:(fun () -> raw_arena_release handle)
    (fun () ->
      ignore (raw_arena_admit handle 0 40 []);
      List.iter
        (fun descriptor ->
          invalid (fun () -> raw_static_copy handle descriptor))
        [
          (39, 0, 32, "AB");
          (40, -1, 32, "AB");
          (40, 39, 32, "AB");
          (40, 0, 40, "AB");
          (40, 0, 0, "AB");
          (40, 0, 32, "");
          (40, 0, 32, String.make 40 'A');
          (max_int, 0, 32, "AB");
        ];
      invalid (fun () -> raw_static_copy handle (Obj.magic 0));
      Alcotest.(check int)
        "real original flags remain available after malformed copies" 1
        (raw_static_copy handle (40, 1, 24, "B"));
      Alcotest.(check int)
        "original copy may overwrite earlier initialized elements" 2
        (raw_static_copy handle (40, 0, 32, "AB"));
      Alcotest.(check int)
        "raw storage writes can update initialized bytes" 1
        (raw_static_copy handle (40, 0, 32, "A"));
      Alcotest.(check int)
        "remaining original array elements stay available" 2
        (raw_static_copy handle (40, 2, 16, "CD"));
      raw_arena_release handle;
      invalid (fun () -> raw_static_copy handle (40, 0, 32, "A")));
  let corrupt = raw_arena_create 40 in
  Fun.protect
    ~finally:(fun () -> raw_arena_release corrupt)
    (fun () ->
      ignore (raw_arena_admit corrupt 0 40 [ (24, "\002") ]);
      invalid (fun () -> raw_static_copy corrupt (40, 0, 32, "AB"));
      Alcotest.(check int)
        "unaffected flag representations remain valid" 1
        (raw_static_copy corrupt (40, 0, 32, "A")))

let () =
  Alcotest.run "Native source authority"
    [
      ( "original source",
        [
          Alcotest.test_case "foreign owners, budgets, replay and collection"
            `Quick foreign_owners_and_budget;
          Alcotest.test_case "bounded original layout admission" `Quick
            layout_work_limits;
          Alcotest.test_case "foreign source catalog and expired snapshots"
            `Quick foreign_source_layout;
          Alcotest.test_case "fixed array layout, flags and exact capacity"
            `Quick array_layout_and_capacity;
          Alcotest.test_case "fixed array task fragments compile for both ABIs"
            `Quick array_abi_compilation;
          Alcotest.test_case "fixed array foreign and expired source authority"
            `Quick foreign_array_source_layout;
          Alcotest.test_case
            "released arena preserves the offered source request" `Quick
            released_arena_preserves_request;
          Alcotest.test_case
            "exact native function sources, history and request authority"
            `Quick native_function_source_registry;
          Alcotest.test_case
            "failed preclaim function source is never published" `Quick
            native_function_preclaim_failure;
          Alcotest.test_case
            "function compilation rejection leaves request retryable" `Quick
            native_function_preflight_retry;
          Alcotest.test_case
            "reached native fault retains admitted function source" `Quick
            native_function_reached_fault_retains_source;
          Alcotest.test_case
            "original provider contexts, both ABIs and request lifetimes" `Quick
            native_provider_source_authority;
          Alcotest.test_case
            "original literals, both ABIs, retry and request lifetimes" `Quick
            native_literal_source_authority;
          Alcotest.test_case
            "live static allocation, initializer and arena authority" `Quick
            native_static_source_authority;
          Alcotest.test_case "live static byte-copy source and arena authority"
            `Quick native_static_copy_authority;
          Alcotest.test_case
            "native static byte-copy raw host bounds and flag validation" `Quick
            native_static_copy_host_bounds;
        ] );
    ]
