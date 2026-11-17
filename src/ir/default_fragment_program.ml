type t = {
  lowered_ : Integer_program_lowering.t;
  authority_ : Sema.Default_fragment.authority;
  destination_ : Default_fragment_destination.t;
  entry_ : X87_stack.t;
  initialization_ : Global_initialization.t;
  runtime_calls_ : Runtime_call_context.t;
}

let destination value = value.destination_
let lowered value = value.lowered_
let entry value = value.entry_
let initialization value = value.initialization_
let runtime_calls value = value.runtime_calls_
let source_authority (value : t) = value.authority_

let is_closed_native_source value =
  let module Typed = Sema.Function_call_expression_result in
  let fragment = Default_fragment_destination.fragment value.destination_ in
  let globals = Default_fragment_destination.globals value.destination_ in
  let typed = Default_fragment_destination.typed value.destination_ in
  let graph = X87_stack.graph value.entry_ in
  let references = Sema.Default_fragment.references fragment in
  let calls = Typed.top_level_direct_calls typed in
  let intrinsics =
    Block_graph.blocks graph
    |> List.concat_map (fun block ->
        Block_graph.instructions block |> Instruction_sequence.instructions)
    |> List.filter_map (fun instruction ->
        let description = Instruction_sequence.description instruction in
        Runtime_call_context.find_intrinsic_instruction value.runtime_calls_
          ~owner:Runtime_call_context.Entry description.instruction_id)
  in
  Integer_globals.is_isolated_default globals
  && Integer_globals.byte_size globals = 0
  && Option.fold ~none:false ~some:(( == ) fragment)
       (Integer_globals.native_source_default_fragment globals)
  && Sema.Default_fragment.native_source_references_supported fragment
  && Runtime_call_context.matches_graph value.runtime_calls_
       ~owner:Runtime_call_context.Entry graph
  && List.length references = List.length calls
  && List.length intrinsics = List.length calls
  && List.for_all
       (fun intrinsic ->
         Numeric_intrinsic.supports
           (Runtime_call_context.intrinsic_opcode intrinsic))
       intrinsics
  && List.for_all
       (fun (_, selection) ->
         List.exists
           (fun call ->
             let callee =
               Typed.top_level_direct_source call
               |> Sema.Top_level_expression_tree.call_callee
               |> Sema.Top_level_outer_expression_binding.occurrence_source
             in
             Option.fold ~none:false ~some:(( == ) selection)
               (Sema.Top_level_expression_binding.occurrence_selection callee)
             && List.exists
                  (fun intrinsic ->
                    Runtime_call_context.intrinsic_declaration intrinsic
                    == Typed.top_level_direct_declaration call)
                  intrinsics)
           calls)
       references

let create ~authority ~destination ~lowered ~entry ~initialization
    ~runtime_calls =
  if
    Integer_program_lowering.graph lowered != entry
    || (not
          (Integer_program_lowering.owns_expression lowered
             ~globals:(Default_fragment_destination.globals destination)
             ~value:
               (Sema.Function_call_expression_result.top_level_root_value
                  (Default_fragment_destination.root destination))))
    || Sema.Default_fragment.authorized_fragment authority
       != Default_fragment_destination.fragment destination
    || (not
          (Runtime_call_context.owns_top_level runtime_calls
             (Default_fragment_destination.typed destination)))
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
        lowered_ = lowered;
        authority_ = authority;
        destination_ = destination;
        entry_ = entry;
        initialization_ = initialization;
        runtime_calls_ = runtime_calls;
      }

type code = Scheduled of t

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
