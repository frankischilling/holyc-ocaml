type t = {
  scalar_lowered_ : Integer_program_lowering.t option;
  destination_ : Static_initializer_destination.t;
  entry_ : X87_stack.t;
  initialization_ : Global_initialization.t;
  runtime_calls_ : Runtime_call_context.t;
}

let destination t = t.destination_
let entry t = t.entry_
let initialization t = t.initialization_
let runtime_calls t = t.runtime_calls_
let is_scalar_evaluation t = Option.is_some t.scalar_lowered_

let create ?scalar_lowered ~destination ~entry ~initialization ~runtime_calls ()
    =
  let scalar = Static_initializer_destination.requires_settlement destination in
  if
    Option.is_some (Static_initializer_destination.copy_byte_count destination)
    || scalar <> Option.is_some scalar_lowered
    || (match scalar_lowered with
      | None -> false
      | Some lowered ->
          Integer_program_lowering.graph lowered != entry
          || not
               (Integer_program_lowering.owns_expression lowered
                  ~globals:(Static_initializer_destination.globals destination)
                  ~value:
                    (Sema.Function_call_expression_result.top_level_root_value
                       (Static_initializer_destination.root destination))))
    || (not
          (Global_initialization.matches initialization ~entry
             ~globals:(Static_initializer_destination.globals destination)))
    || (not
          (Runtime_call_context.matches runtime_calls ~entry
             ~initialization:(Some initialization) ~functions:[]))
    || (not
          (Runtime_call_context.owns_top_level runtime_calls
             (Static_initializer_destination.typed destination)))
    || (not scalar)
       && not
            (Global_initialization.has_static_fragment initialization
               destination)
  then
    Error
      "static initializer program has another graph, destination or call \
       context"
  else
    Ok
      {
        scalar_lowered_ = scalar_lowered;
        destination_ = destination;
        entry_ = entry;
        initialization_ = initialization;
        runtime_calls_ = runtime_calls;
      }
