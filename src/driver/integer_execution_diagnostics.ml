let of_errors ~span =
  List.map (fun (error : Ir.Integer_interpreter.error) ->
      let stage =
        match error.stage with
        | Ir.Integer_interpreter.Configuration -> "configuration"
        | Preflight -> "preflight"
        | Execution -> "execution"
      in
      let identity name = function
        | None -> []
        | Some id -> [ Printf.sprintf "%s=%d" name id ]
      in
      Common.Diagnostic.make ~code:error.code ~severity:Common.Diagnostic.Error
        ~message:error.message
        ~primary:(Option.value error.span ~default:span)
        ~notes:
          ([
             "stage=" ^ stage;
             Printf.sprintf "executed_steps=%d" error.executed_steps;
           ]
          @ identity "block_id" error.block_id
          @ identity "instruction_id" error.instruction_id
          @ identity "function_id" error.function_id
          @ identity "initializer_symbol_id" error.initializer_symbol_id
          @ Option.to_list
              (Option.map
                 (fun name -> "initializer=" ^ name)
                 error.initializer_name)
          @ Option.to_list
              (Option.map
                 (fun phase ->
                   "initializer_phase="
                   ^ Ir.Global_initialization.phase_name phase)
                 error.initializer_phase)
          @ Option.to_list
              (Option.map (fun name -> "function=" ^ name) error.function_name)
          )
        ())

let validate_limits ~span ~max_steps ~max_initializer_steps ~max_global_bytes
    ~max_literal_bytes ~max_frame_bytes ~max_call_depth ~max_output_bytes
    ~max_output_work =
  let invalid message =
    Error
      [
        Common.Diagnostic.make ~code:"HCIRVM0001"
          ~severity:Common.Diagnostic.Error ~message ~primary:span ();
      ]
  in
  if
    max_steps <= 0 || max_frame_bytes <= 0 || max_call_depth <= 0
    || max_global_bytes <= 0 || max_initializer_steps <= 0
    || max_literal_bytes <= 0 || max_output_bytes <= 0 || max_output_work <= 0
  then
    invalid
      "max_steps, max_frame_bytes, max_call_depth, max_global_bytes, \
       max_literal_bytes, max_initializer_steps, max_output_bytes and \
       max_output_work must be greater than zero"
  else if max_output_bytes > Sys.max_string_length then
    invalid "max_output_bytes exceeds the host string allocation bound"
  else Ok ()
