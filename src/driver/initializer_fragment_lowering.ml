module Destination = Ir.Initializer_fragment_destination
module Lower = Ir.Integer_program_lowering
module Typed = Sema.Function_call_expression_result

let classify ~context destination =
  let ( let* ) = Result.bind in
  let span = Destination.span destination in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let typed = Destination.typed destination in
  let records = Initializer_fragment_typing.records context in
  let rec calls = function
    | [] -> Ok []
    | call :: rest ->
        let* call =
          Sema.Top_level_function_call_target_classification.classify ~records
            call
          |> Result.map_error
               Sema.Top_level_function_call_target_classification
               .error_to_string
          |> diagnose
        in
        let* rest = calls rest in
        Ok (call :: rest)
  in
  calls (Typed.top_level_direct_calls typed)

let lower ~context ~authority destination =
  let ( let* ) = Result.bind in
  let span = Destination.span destination in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let typed = Destination.typed destination in
  let records = Initializer_fragment_typing.records context in
  let* top_calls = classify ~context destination in
  let* lowered =
    Lower.lower_complete
      ~globals:(Destination.globals destination)
      ~records ~top_calls ~span
      [ Lower.Initialize_fragment destination ]
  in
  let entry = Lower.graph lowered in
  let* description =
    match Lower.initializer_regions lowered with
    | [ description ] -> Ok description
    | _ ->
        Error "initializer fragment lowering did not retain its unique region"
        |> diagnose
  in
  let* initialization =
    Ir.Global_initialization.create_fragment ~destination ~entry description
  in
  let* runtime_calls =
    Ir.Runtime_call_context.create ~records
      ~function_sources:(Initializer_fragment_typing.function_sources context)
      ~top_level:typed ~initialization ~entry
      ~entry_calls:(Lower.runtime_calls lowered)
      ~functions:[]
  in
  Ir.Initializer_fragment_program.create ~authority ~destination ~entry
    ~initialization ~runtime_calls
  |> diagnose

let prepare ~context ~authority ~runtime destination =
  let ( let* ) = Result.bind in
  let module VM = Ir.Integer_interpreter in
  let module Program = Ir.Initializer_fragment_program in
  let before = VM.task_initializer_steps runtime in
  let span = Destination.span destination in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* top_calls = classify ~context destination in
  let* prepared =
    Integer_initializers.prepare_fragment
      ~retained_function_source:(VM.task_function_source runtime)
      ~on_progress:(fun steps ->
        VM.record_task_preparation runtime ~before ~steps)
      ~max_steps:(VM.task_initializer_limit runtime - before)
      ~top_calls ~functions:[] destination
  in
  let* code =
    match Integer_initializers.fragment_payload prepared with
    | Some payload -> Ok (Program.prepared_code payload)
    | None ->
        lower ~context ~authority destination
        |> Result.map Program.scheduled_code
  in
  Program.prepare ~authority ~destination ~code
    ~steps:(Integer_initializers.fragment_steps prepared)
  |> diagnose
