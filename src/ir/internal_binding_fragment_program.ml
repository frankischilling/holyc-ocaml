type t = {
  authority_ : Sema.Internal_binding_fragment.authority;
  destination_ : Internal_binding_fragment_destination.t;
  lowering_ : Integer_program_lowering.t;
  entry_ : X87_stack.t;
  initialization_ : Global_initialization.t;
  runtime_calls_ : Runtime_call_context.t;
}

let destination value = value.destination_
let lowering value = value.lowering_
let entry value = value.entry_
let initialization value = value.initialization_
let runtime_calls value = value.runtime_calls_
let source_authority value = value.authority_

let create ~authority ~destination ~lowered ~entry ~initialization
    ~runtime_calls =
  if
    Sema.Internal_binding_fragment.authorized_fragment authority
    != Internal_binding_fragment_destination.fragment destination
    || Integer_program_lowering.graph lowered != entry
    || (not
          (Integer_program_lowering.owns_expression lowered
             ~globals:
               (Internal_binding_fragment_destination.globals destination)
             ~value:
               (Sema.Function_call_expression_result.top_level_root_value
                  (Internal_binding_fragment_destination.root destination))))
    || (not
          (Runtime_call_context.owns_top_level runtime_calls
             (Internal_binding_fragment_destination.typed destination)))
    || (not
          (Global_initialization.matches initialization ~entry
             ~globals:
               (Internal_binding_fragment_destination.globals destination)))
    || not
         (Runtime_call_context.matches runtime_calls ~entry
            ~initialization:(Some initialization) ~functions:[])
  then
    Error
      "internal binding program has another source, graph, storage or call \
       context"
  else
    Ok
      {
        authority_ = authority;
        destination_ = destination;
        lowering_ = lowered;
        entry_ = entry;
        initialization_ = initialization;
        runtime_calls_ = runtime_calls;
      }

type code = Scheduled of t

type execution = {
  authority_ : Sema.Internal_binding_fragment.authority;
  destination_ : Internal_binding_fragment_destination.t;
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
    || Sema.Internal_binding_fragment.authorized_fragment authority
       != Internal_binding_fragment_destination.fragment destination
    ||
    match code with
    | Scheduled program ->
        program.authority_ != authority || program.destination_ != destination
  then
    Error
      "internal binding execution has another source authority or preparation \
       count"
  else
    Ok
      {
        authority_ = authority;
        destination_ = destination;
        code_ = code;
        steps_ = steps;
      }
