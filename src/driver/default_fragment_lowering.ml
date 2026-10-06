module Destination = Ir.Default_fragment_destination
module Lower = Ir.Integer_program_lowering
module Typed = Sema.Function_call_expression_result
module Program = Ir.Default_fragment_program

let lower_native ~context ~authority destination =
  let ( let* ) = Result.bind in
  let span = Destination.span destination in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* () =
    if
      Expression_facts.contains_string_literal
        (Sema.Default_fragment.expression (Destination.fragment destination))
    then
      diagnose
        (Error
           "HCRUN0006: native task defaults containing strings require owned \
            storage")
    else Ok ()
  in
  let typed = Destination.typed destination in
  let records = Initializer_fragment_typing.records context in
  let* top_calls =
    List.fold_right
      (fun call result ->
        let* call =
          Sema.Top_level_function_call_target_classification.classify ~records
            call
          |> Result.map_error
               Sema.Top_level_function_call_target_classification
               .error_to_string
          |> diagnose
        in
        let* rest = result in
        Ok (call :: rest))
      (Typed.top_level_direct_calls typed)
      (Ok [])
  in
  let globals = Destination.globals destination in
  let* lowered =
    Lower.lower_complete ~globals ~records ~top_calls
      ~top_callback_calls:(Ir.Callback_source.top_level_calls typed)
      ~span
      [
        Lower.Expression
          (Typed.top_level_root_value (Destination.root destination));
      ]
  in
  let entry = Lower.graph lowered in
  let* initialization =
    Ir.Global_initialization.create ~span ~globals ~entry []
  in
  let* runtime_calls =
    Ir.Runtime_call_context.create ~records
      ~function_sources:(Initializer_fragment_typing.function_sources context)
      ~top_level:typed ~initialization ~entry
      ~entry_calls:(Lower.runtime_calls lowered)
      ~functions:[]
  in
  Program.create ~authority ~destination ~lowered ~entry ~initialization
    ~runtime_calls
  |> diagnose

let prepare ~context ~authority ~runtime destination =
  let ( let* ) = Result.bind in
  let module VM = Ir.Integer_interpreter in
  let span = Destination.span destination in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* () =
    if
      Destination.fragment destination
      |> Sema.Default_fragment.expression
      |> Expression_facts.contains_string_literal
    then
      Error
        "HCRUN0006: defaults containing string storage require native \
         owned-default preparation" |> diagnose
    else Ok ()
  in
  let typed = Destination.typed destination in
  let records = Initializer_fragment_typing.records context in
  let rec classify = function
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
        let* rest = classify rest in
        Ok (call :: rest)
  in
  let* top_calls = classify (Typed.top_level_direct_calls typed) in
  let before = VM.task_initializer_steps runtime in
  let* classification, steps =
    Integer_initializers.prepare_default ~runtime ~authority
      ~top_callback_calls:(Ir.Callback_source.top_level_calls typed)
      ~retained_function_source:(VM.task_function_source runtime)
      ~on_progress:(fun steps ->
        VM.record_task_preparation runtime ~before ~steps)
      ~max_steps:(VM.task_initializer_limit runtime - before)
      ~top_calls destination
  in
  let* code =
    match classification with
    | Integer_initializers.Prepared_default proof ->
        Ok (VM.Prepared_default proof)
    | Scheduled_default ->
        let globals = Destination.globals destination in
        let* lowered =
          Lower.lower_complete ~globals ~records ~top_calls
            ~top_callback_calls:(Ir.Callback_source.top_level_calls typed)
            ~span
            [
              Lower.Expression
                (Typed.top_level_root_value (Destination.root destination));
            ]
        in
        let entry = Lower.graph lowered in
        let* initialization =
          Ir.Global_initialization.create ~span ~globals ~entry []
        in
        let* runtime_calls =
          Ir.Runtime_call_context.create ~records
            ~function_sources:
              (Initializer_fragment_typing.function_sources context)
            ~top_level:typed ~initialization ~entry
            ~entry_calls:(Lower.runtime_calls lowered)
            ~functions:[]
        in
        let* program =
          Program.create ~authority ~destination ~lowered ~entry ~initialization
            ~runtime_calls
          |> diagnose
        in
        let* execution =
          Program.prepare ~authority ~destination
            ~code:(Program.Scheduled program) ~steps
          |> diagnose
        in
        Ok (VM.Scheduled_default execution)
  in
  Ok code
