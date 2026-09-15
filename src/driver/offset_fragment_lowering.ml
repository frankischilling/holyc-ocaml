module Destination = Ir.Offset_fragment_destination
module Lower = Ir.Integer_program_lowering
module Typed = Sema.Function_call_expression_result
module Program = Ir.Offset_fragment_program

let prepare ~context ~authority ~runtime destination =
  let ( let* ) = Result.bind in
  let module VM = Ir.Integer_interpreter in
  let span = Destination.span destination in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
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
  let* _classification, steps =
    Integer_initializers.prepare_offset
      ~retained_function_source:(VM.task_function_source runtime)
      ~on_progress:(fun steps ->
        VM.record_task_preparation runtime ~before ~steps)
      ~max_steps:(VM.task_initializer_limit runtime - before)
      ~top_calls destination
  in
  let* code =
    let globals = Destination.globals destination in
    let* lowered =
      Lower.lower_complete ~globals ~records ~top_calls ~span
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
    |> Result.map (fun program -> Program.Scheduled program)
  in
  Program.prepare ~authority ~destination ~code ~steps |> diagnose
