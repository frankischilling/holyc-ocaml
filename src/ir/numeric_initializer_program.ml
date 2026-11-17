module Typed = Sema.Function_call_expression_result

type source =
  | Global of Sema.Initializer_fragment.authority * Typed.top_level_root_result
  | Static of Sema.Static_initializer_fragment.t * Typed.top_level_root_result

type t = {
  source_ : source;
  root_ : Typed.top_level_root_result;
  globals_ : Integer_globals.t;
  lowered_ : Integer_program_lowering.t;
  entry_ : X87_stack.t;
  initialization_ : Global_initialization.t;
  runtime_calls_ : Runtime_call_context.t;
  span_ : Common.Span.t;
}

let source value = value.source_
let root value = value.root_
let globals value = value.globals_
let lowered value = value.lowered_
let entry value = value.entry_
let initialization value = value.initialization_
let runtime_calls value = value.runtime_calls_
let span value = value.span_

let create ~source ~globals ~lowered ~initialization ~runtime_calls =
  let root, expression, original =
    match source with
    | Global (authority, root) ->
        let fragment =
          Sema.Initializer_fragment.authorized_fragment authority
        in
        let expression =
          Sema.Initializer_fragment.leaf fragment
          |> Sema.Initializer_source.leaf_expression_ast
        in
        let original =
          Sema.Initializer_fragment.references fragment = []
          && Option.fold ~none:false ~some:(( == ) fragment)
               (Integer_globals.native_global_initializer_fragment globals)
          &&
          match
            Typed.top_level_root_source root
            |> Sema.Top_level_expression_tree.root_role
          with
          | Sema.Top_level_expression_tree.Initializer_fragment found ->
              found == fragment
          | _ -> false
        in
        (root, expression, original)
    | Static (fragment, root) ->
        let original =
          Sema.Static_initializer_fragment.references fragment = []
          && Option.fold ~none:false ~some:(( == ) fragment)
               (Integer_globals.native_static_initializer_fragment globals)
          &&
          match
            Typed.top_level_root_source root
            |> Sema.Top_level_expression_tree.root_role
          with
          | Sema.Top_level_expression_tree.Static_initializer_fragment found ->
              found == fragment
          | _ -> false
        in
        (root, Sema.Static_initializer_fragment.expression fragment, original)
  in
  let value = Typed.top_level_root_value root in
  let entry = Integer_program_lowering.graph lowered in
  if
    (not original)
    || Integer_globals.byte_size globals <> 0
    || (not
          (Initializer_value.expression_is_f64 value
          || Initializer_value.has_integer_word value))
    || not (Integer_program_lowering.owns_expression lowered ~globals ~value)
  then
    Error
      "native numeric initializer requires its original closed source and \
       expression graph"
  else if
    Integer_program_lowering.runtime_calls lowered <> []
    || Integer_program_lowering.initializer_regions lowered <> []
    || Integer_program_lowering.static_initializer_regions lowered <> []
    || Integer_program_lowering.publications lowered <> []
    || (not (Global_initialization.matches initialization ~globals ~entry))
    || (not
          (Option.fold ~none:false
             ~some:(List.exists (( == ) value))
             (Runtime_call_context.original_expression_results runtime_calls
                ~owner:Runtime_call_context.Entry)))
    || not
         (Runtime_call_context.matches runtime_calls ~entry
            ~initialization:(Some initialization) ~functions:[])
  then
    Error
      "native numeric initializer has another storage, initialization or call \
       context"
  else
    Ok
      {
        source_ = source;
        root_ = root;
        globals_ = globals;
        lowered_ = lowered;
        entry_ = entry;
        initialization_ = initialization;
        runtime_calls_ = runtime_calls;
        span_ = (Frontend.Ast.expression_location expression).span;
      }
