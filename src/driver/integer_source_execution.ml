module VM = Ir.Integer_interpreter
module Task = Integer_task
module Unit = Integer_unit
module Parser = Frontend.Parser

type limits = {
  steps : int;
  global_bytes : int;
  literal_bytes : int;
  frame_bytes : int;
  call_depth : int;
  output_bytes : int;
  output_work : int;
}

type compilation = Isolated of Unit.compiled | Stateful of VM.t

type compilation_report = {
  compilation_outcome_ :
    (compilation Unit.checked, Common.Diagnostic.t list) result;
  source_span : Common.Span.t;
  source_dimension_work : int;
  task : Task.t option;
  compilation_progress_ : Task.progress option;
  task_units_ : Unit.compiled list;
  limits : limits;
}

type report = {
  outcome_ : (VM.t Unit.checked, Common.Diagnostic.t list) result;
  output_bytes_ : string;
  output_work_ : int;
  dimension_work_ : int;
  progress_ : Task.progress option;
  program_ : Unit.compiled option;
  task_units_ : Unit.compiled list;
}

let compilation_result report = report.compilation_outcome_

let compilation_outcome report =
  Result.bind report.compilation_outcome_ (fun checked ->
      match checked.Unit.value with
      | Isolated value -> Ok { checked with value }
      | Stateful _ ->
          Error
            (checked.diagnostics
            @ [
                Integer_source.diagnostic ~span:report.source_span "HCRUN0001"
                  "stateful JIT source has separate task units; use \
                   compilation_result or the source execution report";
              ]))

let compilation_progress report = report.compilation_progress_
let compilation_task_units (report : compilation_report) = report.task_units_

let task_dimensions progress =
  Option.fold ~none:0 ~some:(fun p -> p.Task.dimension_work) progress

let compilation_dimension_work report =
  report.source_dimension_work + task_dimensions report.compilation_progress_

let outcome report = report.outcome_
let output_bytes report = report.output_bytes_
let output_work report = report.output_work_
let dimension_work report = report.dimension_work_
let progress report = report.progress_
let program report = report.program_
let task_units (report : report) = report.task_units_
let ( let* ) = Result.bind

let install_providers ?(suspended = false) task =
  let session = Task.frontend task in
  let symbols = Session.symbols session in
  let headers =
    [
      ("StreamPrint", "extern U0 StreamPrint(U8 *fmt,...);");
      ("Print", "extern U0 Print(U8 *fmt,...);");
      ("PutChars", "extern U0 PutChars(U64 ch);");
    ]
    |> List.filter_map (fun (name, header) ->
        match
          Frontend.Symbol_visibility.Environment.find_preprocessor symbols name
        with
        | Absent -> Some header
        | Present _ | Shadowed_by_local -> None)
    |> String.concat "\n"
  in
  if headers = "" then Ok ()
  else
    let source =
      Session.add_source session ~path:"<hosted-task-providers>"
        ~contents:headers
    in
    if not suspended then Task.run task ~source |> Result.map ignore
    else
      let detached = Session.fork_frontend session in
      let* config =
        Frontend.Preprocessor.Config.create ~compilation_mode:Jit ()
        |> Result.map_error (fun message ->
            [
              Integer_source.diagnostic
                ~span:(Integer_source.source_span source)
                "HCIRVM0001" message;
            ])
      in
      let parsed =
        Parser.parse ~sources:(Session.sources detached)
          ~definitions:(Session.definitions detached)
          ~symbols:(Session.symbols detached) ~config source
      in
      match parsed.ast with
      | None -> Error parsed.diagnostics
      | Some ast ->
          let* command = Task.compile_ast task ast in
          Task.execute task command |> Result.map ignore

let compile_report ?(max_dimension_work = 100_000)
    ?(max_initializer_steps = 100_000) ?(max_steps = 100_000)
    ?(max_global_bytes = 1_048_576) ?(max_literal_bytes = 1_048_576)
    ?(max_frame_bytes = 1_048_576) ?(max_call_depth = 128)
    ?(max_output_bytes = 1_048_576) ?(max_output_work = 1_048_576) session
    ~config ~source =
  let limits =
    {
      steps = max_steps;
      global_bytes = max_global_bytes;
      literal_bytes = max_literal_bytes;
      frame_bytes = max_frame_bytes;
      call_depth = max_call_depth;
      output_bytes = max_output_bytes;
      output_work = max_output_work;
    }
  in
  let task = ref None in
  let completed_sequence = ref None in
  let source_dimension_work = ref 0 in
  let span = Integer_source.source_span source in
  let compilation_outcome_ =
    let* () =
      Integer_execution_diagnostics.validate_limits ~span ~max_steps
        ~max_initializer_steps ~max_global_bytes ~max_literal_bytes
        ~max_frame_bytes ~max_call_depth ~max_output_bytes ~max_output_work
    in
    if max_dimension_work <= 0 then
      Error
        [
          Integer_source.diagnostic ~span "HCIRVM0001"
            "max_dimension_work must be greater than zero";
        ]
    else
      let* ledger =
        Task_declarations.create_source ~max_dimension_work session ~source
        |> Result.map_error (fun message ->
            [ Integer_source.diagnostic ~span "HCRUN0004" message ])
      in
      (* Take the detached AOT task view before outer declarations. JIT defaults
         and directives activate their original source task on demand. *)
      let task_session =
        if Frontend.Preprocessor.Config.compilation_mode config = Aot then
          Some (Session.fork_frontend session)
        else None
      in
      let is_jit = Option.is_none task_session in
      let ensure_task directive =
        match !task with
        | Some task -> Ok task
        | None ->
            let create =
              match task_session with
              | Some task_session ->
                  fun () ->
                    Task.create ~max_steps ~max_initializer_steps
                      ~max_global_bytes ~max_literal_bytes ~max_frame_bytes
                      ~max_call_depth ~max_output_bytes ~max_output_work
                      ~max_generated_bytes:
                        (Frontend.Preprocessor.Config.max_generated_bytes config)
                      task_session
              | None ->
                  fun () ->
                    Task.adopt_source_for_activation ~max_steps
                      ~max_initializer_steps ~max_global_bytes
                      ~max_literal_bytes ~max_frame_bytes ~max_call_depth
                      ~max_output_bytes ~max_output_work
                      ~max_generated_bytes:
                        (Frontend.Preprocessor.Config.max_generated_bytes config)
                      session ~source ~ledger
            in
            let* retained =
              create ()
              |> Result.map_error (fun message ->
                  [
                    Integer_source.diagnostic ~span:directive "HCIRVM0001"
                      message;
                  ])
            in
            task := Some retained;
            let* () =
              if is_jit then Task.activate_source retained ~span:directive
              else Ok ()
            in
            Ok retained
      in
      let providers_installed = ref false in
      let execute_stream directive =
        let* retained = ensure_task directive in
        let* () =
          if !providers_installed then Ok ()
          else
            let* () = install_providers ~suspended:is_jit retained in
            providers_installed := true;
            Ok ()
        in
        Task.stream_executor retained directive
      in
      let commands : Parser.command_sink =
        {
          checkpoint =
            Some
              (fun event ->
                let* () = Task_declarations.observe_command ledger event in
                match (is_jit, !task, event) with
                | true, Some task, Parser.Command_resumed receipt ->
                    let* command =
                      Task.compile_source_ast task receipt.command_ast
                    in
                    Task.execute task command |> Result.map ignore
                | true, Some _, Parser.Sequence_completed receipt ->
                    completed_sequence := Some receipt;
                    Ok ()
                | _ -> Ok ());
          query = Some (Task_declarations.observe_query ledger);
          call =
            (if is_jit then
               Some
                 {
                   start = Task_declarations.observe_call_start ledger;
                   emit = Task_declarations.observe_call_emission ledger;
                 }
             else None);
          implicit_output =
            Some
              (fun selection ->
                let* () =
                  Task_declarations.observe_implicit_output ledger selection
                in
                match (is_jit, !task) with
                | true, None -> Ok ()
                | _ ->
                    Task_declarations.validate_implicit_output ledger selection
                      ~execution:is_jit);
          reference =
            Some
              (fun selection ->
                match (is_jit, !task) with
                | true, Some _ ->
                    Task_declarations.observe_execution_reference ledger
                      selection
                | _ ->
                    let* () =
                      Task_declarations.observe_reference ledger selection
                    in
                    if is_jit then Ok ()
                    else
                      Task_declarations.validate_source_reference ledger
                        selection);
          declaration =
            Some
              (fun event ->
                let* deferred_dimension =
                  match (is_jit, !task, event) with
                  | true, None, Parser.Array_dimension_preparing receipt
                    when Task_declarations.dimension_requires_runtime receipt ->
                      let* () =
                        Task_declarations.defer_source_runtime_dimension ledger
                          ~preparation:receipt event
                      in
                      let* _ = ensure_task receipt.dimension_opening.span in
                      Ok true
                  | _ -> Ok false
                in
                if deferred_dimension then Ok ()
                else
                  let* () = Task_declarations.observe ledger event in
                  match (is_jit, !task, event) with
                  | true, Some task, _ -> Task.observe_initializer task event
                  | true, None, Parser.Parameter_default_completed receipt -> (
                      match receipt.default_ast.value with
                      | Frontend.Ast.Expression_default _ ->
                          ensure_task receipt.default_ast.location.span
                          |> Result.map ignore
                      | Frontend.Ast.Lastclass_default _ -> Ok ())
                  | false, _, Parser.Parameter_default_completed receipt -> (
                      match receipt.default_ast.value with
                      | Frontend.Ast.Expression_default _ ->
                          let* task =
                            ensure_task receipt.default_ast.location.span
                          in
                          Task.prepare_source_default task ~session ~ledger
                            receipt
                      | Frontend.Ast.Lastclass_default _ -> Ok ())
                  | false, _, Parser.Function_header_completed header ->
                      Task_declarations.complete_source_defaults ledger header
                  | _ -> Ok ());
          dimension_count =
            Some (Task_declarations.grammar_dimension_count ledger);
          command = (fun _ -> Ok ());
          resume = (fun () -> Ok ());
        }
      in
      let parsed =
        Parser.parse ~execute_stream ~commands
          ~sources:(Session.sources session)
          ~definitions:(Session.definitions session)
          ~symbols:(Session.symbols session) ~config source
      in
      source_dimension_work :=
        if is_jit && Option.is_some !task then 0
        else Task_declarations.dimension_work ledger;
      match parsed.ast with
      | None -> Error parsed.diagnostics
      | Some _ when is_jit && Option.is_some !task ->
          let* result =
            Task.result (Option.get !task)
              ~sequence:(Option.get !completed_sequence)
          in
          Ok { Unit.value = Stateful result; diagnostics = parsed.diagnostics }
      | Some ast ->
          let* source_command =
            Task_declarations.seal_source ledger ast
            |> Result.map_error (fun errors -> parsed.diagnostics @ errors)
          in
          (match !task with
            | None ->
                Unit.compile_source_output ~source_command
                  ~max_initializer_steps session ~config parsed
            | Some task ->
                Task.compile_isolated task ~source_command session ~config
                  parsed)
          |> Result.map (fun checked ->
              { checked with Unit.value = Isolated checked.Unit.value })
  in
  {
    compilation_outcome_;
    source_span = span;
    source_dimension_work = !source_dimension_work;
    task = !task;
    compilation_progress_ = Option.map Task.progress !task;
    task_units_ = Option.fold ~none:[] ~some:Task.compiled_units !task;
    limits;
  }

let run ?max_dimension_work ?max_initializer_steps ?max_global_bytes
    ?max_literal_bytes ?max_frame_bytes ?max_call_depth ?max_output_bytes
    ?max_output_work session ~config ~source ~max_steps =
  let compilation =
    compile_report ?max_dimension_work ?max_initializer_steps ?max_global_bytes
      ?max_literal_bytes ?max_frame_bytes ?max_call_depth ?max_output_bytes
      ?max_output_work ~max_steps session ~config ~source
  in
  let span = Integer_source.source_span source in
  let program_ =
    match compilation.compilation_outcome_ with
    | Ok { value = Isolated program; _ } -> Some program
    | Ok { value = Stateful _; _ } -> None
    | Error _ -> None
  in
  let ordinary_report = ref None in
  let outcome_ =
    let* checked = compilation.compilation_outcome_ in
    match checked.value with
    | Stateful value -> Ok { checked with Unit.value }
    | Isolated compiled ->
        let execution =
          match compilation.task with
          | Some task -> Task.execute_isolated task compiled
          | None ->
              let limits = compilation.limits in
              let report =
                VM.execute_program_report
                  ~runtime_calls:(Unit.runtime_calls compiled)
                  ~globals:(Unit.globals compiled)
                  ~initialization:(Unit.initialization compiled)
                  ~max_global_bytes:limits.global_bytes
                  ~max_literal_bytes:limits.literal_bytes
                  ~max_steps:limits.steps ~max_frame_bytes:limits.frame_bytes
                  ~max_call_depth:limits.call_depth
                  ~max_output_bytes:limits.output_bytes
                  ~max_output_work:limits.output_work
                  ~functions:(Unit.functions compiled) (Unit.entry compiled)
              in
              ordinary_report := Some report;
              VM.report_outcome report
        in
        execution
        |> Result.map (fun value ->
            { Unit.value; diagnostics = checked.diagnostics })
        |> Result.map_error (fun errors ->
            checked.diagnostics
            @ Integer_execution_diagnostics.of_errors ~span errors)
  in
  let progress_ = Option.map Task.progress compilation.task in
  let output_bytes_, output_work_ =
    match (progress_, !ordinary_report) with
    | Some progress, _ ->
        (progress.runtime.output_bytes, progress.runtime.output_work)
    | None, Some report ->
        (VM.report_output_bytes report, VM.report_output_work report)
    | None, None -> ("", 0)
  in
  {
    outcome_;
    output_bytes_;
    output_work_;
    progress_;
    program_;
    dimension_work_ =
      compilation.source_dimension_work + task_dimensions progress_;
    task_units_ = compilation.task_units_;
  }
