module Typed = Sema.Function_call_expression_result
module Fragment = Sema.Default_fragment

type t = {
  fragment_ : Fragment.t;
  typed_ : Typed.top_level_t;
  root_ : Typed.top_level_root_result;
  globals_ : Integer_globals.t;
  type_ : Sema.Type.t;
  span_ : Common.Span.t;
}

let fragment value = value.fragment_
let typed value = value.typed_
let root value = value.root_
let globals value = value.globals_
let type_ value = value.type_
let span value = value.span_
let symbol_opt value = Fragment.symbol_opt value.fragment_
let symbol value = Option.get (symbol_opt value)

let create_with_globals globals typed =
  let ( let* ) = Result.bind in
  let* root_ =
    match
      Typed.top_level_statements typed
      |> List.concat_map Typed.top_level_statement_roots
    with
    | [ root ] -> Ok root
    | _ -> Error "default destination requires one complete original expression"
  in
  let* fragment_ =
    match
      Typed.top_level_root_source root_
      |> Sema.Top_level_expression_tree.root_role
    with
    | Sema.Top_level_expression_tree.Default_fragment fragment -> Ok fragment
    | _ -> Error "default destination requires its original default root"
  in
  let type_specifier, pointer_layers, function_pointer =
    Fragment.parameter_parts fragment_
  in
  let* type_ =
    match function_pointer with
    | Some pointer when List.length pointer.indirection_layers = 1 ->
        (* LexExpression2Bin returns a word; PrsFunCall later materializes that
           saved word with the original member's RT_PTR storage class. *)
        Sema.Type.make_primitive ~form:Internal_storage ~primitive:I64
          ~pointer_depth:0
    | Some _ ->
        Error "HCRUN0001: callback defaults require one original pointer star"
    | None ->
        Sema.Source_type_reference.builtin type_specifier pointer_layers
        |> Result.map Sema.Type_reference.resolved_type
  in
  let value = Typed.top_level_root_value root_ in
  let* () =
    if
      Option.is_some (Integer_scalar_storage.of_type type_)
      && Typed.result_array_rank value = 0
      && (match Typed.result_category value with
        | Typed.Object_value | Typed.Lvalue -> true
        | _ -> false)
      && Option.fold ~none:false
           ~some:(fun type_ ->
             Option.is_some (Integer_scalar_storage.of_type type_))
           (Typed.result_type value)
    then Ok ()
    else
      Error
        "HCRUN0001: default preparation requires a checked scalar integer value"
  in
  let* globals_ = globals fragment_ in
  Ok
    {
      fragment_;
      typed_ = typed;
      root_;
      globals_;
      type_;
      span_ = (Fragment.ast fragment_).location.span;
    }

let create ~task_view =
  create_with_globals (Integer_globals.default_context task_view)

let create_source = create_with_globals Integer_globals.source_default_context

let create_native_source =
  create_with_globals Integer_globals.native_source_default_context
