open Holyc_lib
module Driver = Holyc_lib__Driver
module Source = Driver.Integer_source_execution
module Dispatch = Driver.Integer_task.Native_dispatch
module Storage = Holyc_lib__Backend.X86_64_global_storage
module Image = X86_64_program
module Runtime = Native_program_execution

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
        ] );
    ]
