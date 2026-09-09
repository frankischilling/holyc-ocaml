include Integer_unit

let ( let* ) = Result.bind

type compilation_report = {
  compilation_outcome_ : (compiled checked, Common.Diagnostic.t list) result;
  compilation_dimension_work_ : int;
}

let compilation_outcome report = report.compilation_outcome_
let compilation_dimension_work report = report.compilation_dimension_work_

let compile_report ?(max_dimension_work = 100_000)
    ?(max_initializer_steps = 100_000) session ~config ~source =
  let dimension_work = ref 0 in
  let compilation_outcome_ =
    if max_initializer_steps <= 0 then
      Error
        [
          Integer_source.diagnostic
            ~span:(Integer_source.source_span source)
            "HCIRVM0001" "max_initializer_steps must be greater than zero";
        ]
    else if max_dimension_work <= 0 then
      Error
        [
          Integer_source.diagnostic
            ~span:(Integer_source.source_span source)
            "HCIRVM0001" "max_dimension_work must be greater than zero";
        ]
    else
      let* ledger =
        Task_declarations.create_source ~max_dimension_work session ~source
        |> Result.map_error (fun message ->
            [
              Integer_source.diagnostic
                ~span:(Integer_source.source_span source)
                "HCRUN0004" message;
            ])
      in
      let commands : Frontend.Parser.command_sink =
        {
          checkpoint = Some (Task_declarations.observe_command ledger);
          query = Some (Task_declarations.observe_query ledger);
          reference = None;
          declaration = Some (Task_declarations.observe ledger);
          dimension_count =
            Some (Task_declarations.grammar_dimension_count ledger);
          command = (fun _ -> Ok ());
          resume = (fun () -> Ok ());
        }
      in
      let parsed =
        Frontend.Parser.parse ~commands ~sources:(Session.sources session)
          ~definitions:(Session.definitions session)
          ~symbols:(Session.symbols session) ~config source
      in
      dimension_work := Task_declarations.dimension_work ledger;
      match parsed.ast with
      | None -> Error parsed.diagnostics
      | Some ast ->
          let* source_command =
            Task_declarations.seal_source ledger ast
            |> Result.map_error (fun diagnostics ->
                parsed.diagnostics @ diagnostics)
          in
          compile_source_output ~source_command ~max_initializer_steps session
            ~config parsed
  in
  { compilation_outcome_; compilation_dimension_work_ = !dimension_work }

let compile ?max_dimension_work ?max_initializer_steps session ~config ~source =
  compile_report ?max_dimension_work ?max_initializer_steps session ~config
    ~source
  |> compilation_outcome

let lower session ~config ~source =
  let* compiled = compile session ~config ~source in
  match functions compiled.value with
  | []
    when Ir.Integer_globals.byte_size (globals compiled.value) = 0
         && not (has_entry_calls compiled.value) ->
      Ok { value = entry compiled.value; diagnostics = compiled.diagnostics }
  | _ ->
      Error
        (compiled.diagnostics
        @ [
            Integer_source.diagnostic
              ~span:(Integer_source.source_span source)
              "HCRUN0001"
              "named functions, calls and global storage require the \
               compiled-program API";
          ])

let run ?max_dimension_work ?(max_initializer_steps = 100_000)
    ?(max_global_bytes = 1_048_576) ?(max_literal_bytes = 1_048_576)
    ?(max_frame_bytes = 1_048_576) ?(max_call_depth = 128)
    ?(max_output_bytes = 1_048_576) ?(max_output_work = 1_048_576) session
    ~config ~source ~max_steps =
  let span = Integer_source.source_span source in
  let* () =
    Integer_execution_diagnostics.validate_limits ~span ~max_steps
      ~max_initializer_steps ~max_global_bytes ~max_literal_bytes
      ~max_frame_bytes ~max_call_depth ~max_output_bytes ~max_output_work
  in
  let* graph =
    compile ?max_dimension_work ~max_initializer_steps session ~config ~source
  in
  Ir.Integer_interpreter.execute_program ~globals:(globals graph.value)
    ~runtime_calls:(runtime_calls graph.value)
    ~initialization:(initialization graph.value)
    ~max_global_bytes ~max_literal_bytes ~max_steps ~max_frame_bytes
    ~max_call_depth ~max_output_bytes ~max_output_work
    ~functions:(functions graph.value) (entry graph.value)
  |> Result.map (fun value -> { value; diagnostics = graph.diagnostics })
  |> Result.map_error (fun errors ->
      graph.diagnostics @ Integer_execution_diagnostics.of_errors ~span errors)
