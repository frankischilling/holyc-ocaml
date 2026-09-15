type t = {
  authority_ : Sema.Initializer_fragment.authority;
  destination_ : Initializer_fragment_destination.t;
  entry_ : X87_stack.t;
  initialization_ : Global_initialization.t;
  runtime_calls_ : Runtime_call_context.t;
}

let authority value = value.authority_
let destination value = value.destination_
let entry value = value.entry_
let initialization value = value.initialization_
let runtime_calls value = value.runtime_calls_

let create ~authority ~destination ~entry ~initialization ~runtime_calls =
  if
    Sema.Initializer_fragment.authorized_fragment authority
    != Initializer_fragment_destination.fragment destination
  then Error "initializer program has another source authority"
  else if
    (not
       (Global_initialization.matches initialization ~entry
          ~globals:(Initializer_fragment_destination.globals destination)))
    || not
         (Runtime_call_context.matches runtime_calls ~entry
            ~initialization:(Some initialization) ~functions:[])
  then Error "initializer program has another graph, storage or call context"
  else
    Ok
      {
        authority_ = authority;
        destination_ = destination;
        entry_ = entry;
        initialization_ = initialization;
        runtime_calls_ = runtime_calls;
      }

type execution_code =
  | Prepared of Integer_array_initializers.payload
  | Scheduled of t

type execution = {
  execution_authority_ : Sema.Initializer_fragment.authority;
  execution_destination_ : Initializer_fragment_destination.t;
  execution_code_ : execution_code;
  execution_steps_ : int;
}

let execution_authority value = value.execution_authority_
let execution_destination value = value.execution_destination_
let execution_code value = value.execution_code_
let execution_steps value = value.execution_steps_
let prepared_code payload = Prepared payload
let scheduled_code program = Scheduled program

let prepare ~authority ~destination ~code ~steps =
  if
    steps < 0
    || Sema.Initializer_fragment.authorized_fragment authority
       != Initializer_fragment_destination.fragment destination
  then
    Error
      "initializer execution has another source authority or preparation count"
  else
    let valid =
      match
        ( code,
          Integer_initializer_layout.operation
            (Initializer_fragment_destination.layout destination) )
      with
      | ( Prepared (Integer_array_initializers.Word _),
          Integer_initializer_layout.Scalar_store ) -> true
      | ( Prepared (Integer_array_initializers.Bytes bytes),
          Integer_initializer_layout.Copy_bytes expected ) ->
          bytes = expected && steps = String.length bytes
      | Scheduled program, Integer_initializer_layout.Scalar_store ->
          program.authority_ == authority
          && program.destination_ == destination
          && steps = 0
      | _ -> false
    in
    if not valid then
      Error
        "initializer execution payload disagrees with its original preparation"
    else
      Ok
        {
          execution_authority_ = authority;
          execution_destination_ = destination;
          execution_code_ = code;
          execution_steps_ = steps;
        }
