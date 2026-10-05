module Destination = Ir.Static_initializer_destination
module Lower = Ir.Integer_program_lowering
module Typed = Sema.Function_call_expression_result

let lower ~context destination =
  let ( let* ) = Result.bind in
  let span = Destination.span destination in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let typed = Destination.typed destination in
  let records = Initializer_fragment_typing.records context in
  let* top_calls =
    List.fold_right
      (fun call rest ->
        let* call =
          Sema.Top_level_function_call_target_classification.classify ~records
            call
          |> Result.map_error
               Sema.Top_level_function_call_target_classification
               .error_to_string
          |> diagnose
        in
        let* rest = rest in
        Ok (call :: rest))
      (Typed.top_level_direct_calls typed)
      (Ok [])
  in
  let* lowered =
    Lower.lower_complete
      ~globals:(Destination.globals destination)
      ~records ~top_calls
      ~top_callback_calls:(Ir.Callback_source.top_level_calls typed)
      ~span
      [ Lower.Initialize_static_fragment destination ]
  in
  let entry = Lower.graph lowered in
  let* description =
    match Lower.initializer_regions lowered with
    | [ description ] -> Ok description
    | _ ->
        Error "static initializer lost its unique original region" |> diagnose
  in
  let* initialization =
    Ir.Global_initialization.create_static_fragment ~destination ~entry
      description
  in
  let* runtime_calls =
    Ir.Runtime_call_context.create ~records
      ~function_sources:(Initializer_fragment_typing.function_sources context)
      ~top_level:typed ~initialization ~entry
      ~entry_calls:(Lower.runtime_calls lowered)
      ~functions:[]
  in
  Ir.Static_initializer_program.create ~destination ~entry ~initialization
    ~runtime_calls
  |> diagnose
