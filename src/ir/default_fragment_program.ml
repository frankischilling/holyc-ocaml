type t = {
  authority_ : Sema.Default_fragment.authority;
  destination_ : Default_fragment_destination.t;
  entry_ : X87_stack.t;
  initialization_ : Global_initialization.t;
  runtime_calls_ : Runtime_call_context.t;
}

let destination value = value.destination_
let entry value = value.entry_
let initialization value = value.initialization_
let runtime_calls value = value.runtime_calls_

let create ~authority ~destination ~entry ~initialization ~runtime_calls =
  if
    Sema.Default_fragment.authorized_fragment authority
    != Default_fragment_destination.fragment destination
    || (not
          (Global_initialization.matches initialization ~entry
             ~globals:(Default_fragment_destination.globals destination)))
    || not
         (Runtime_call_context.matches runtime_calls ~entry
            ~initialization:(Some initialization) ~functions:[])
  then
    Error "default program has another source, graph, storage or call context"
  else
    Ok
      {
        authority_ = authority;
        destination_ = destination;
        entry_ = entry;
        initialization_ = initialization;
        runtime_calls_ = runtime_calls;
      }

type code = Prepared of int64 | Scheduled of t

type execution = {
  authority_ : Sema.Default_fragment.authority;
  destination_ : Default_fragment_destination.t;
  code_ : code;
  steps_ : int;
}

let authority value = value.authority_
let execution_destination value = value.destination_
let code value = value.code_
let steps value = value.steps_

let prepare ~authority ~destination ~code ~steps =
  if
    steps < 0
    || Sema.Default_fragment.authorized_fragment authority
       != Default_fragment_destination.fragment destination
    ||
    match code with
    | Prepared _ -> false
    | Scheduled program ->
        program.authority_ != authority
        || program.destination_ != destination
        || steps <> 0
  then
    Error "default execution has another source authority or preparation count"
  else
    Ok
      {
        authority_ = authority;
        destination_ = destination;
        code_ = code;
        steps_ = steps;
      }
