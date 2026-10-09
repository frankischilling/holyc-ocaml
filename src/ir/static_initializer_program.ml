type t = {
  destination_ : Static_initializer_destination.t;
  entry_ : X87_stack.t;
  initialization_ : Global_initialization.t;
  runtime_calls_ : Runtime_call_context.t;
}

let destination t = t.destination_
let entry t = t.entry_
let initialization t = t.initialization_
let runtime_calls t = t.runtime_calls_

let create ~destination ~entry ~initialization ~runtime_calls =
  if
    Option.is_some (Static_initializer_destination.copy_byte_count destination)
    || (not
          (Global_initialization.matches initialization ~entry
             ~globals:(Static_initializer_destination.globals destination)))
    || (not
          (Runtime_call_context.matches runtime_calls ~entry
             ~initialization:(Some initialization) ~functions:[]))
    || not
         (Global_initialization.has_static_fragment initialization destination)
  then
    Error
      "static initializer program has another graph, destination or call \
       context"
  else
    Ok
      {
        destination_ = destination;
        entry_ = entry;
        initialization_ = initialization;
        runtime_calls_ = runtime_calls;
      }
