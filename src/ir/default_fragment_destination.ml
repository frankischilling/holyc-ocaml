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

let symbol value =
  Fragment.publication value.fragment_
  |> Sema.Declaration_collection.publication_symbol

let create ~task_view typed =
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
  let receipt = Fragment.receipt fragment_ in
  let* () =
    if Option.is_some receipt.default_function_pointer then
      Error
        "HCRUN0001: function-pointer defaults require callable value storage"
    else Ok ()
  in
  let* reference =
    Sema.Source_type_reference.builtin receipt.default_type_specifier
      receipt.default_pointer_layers
  in
  let type_ = Sema.Type_reference.resolved_type reference in
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
  let* globals_ = Integer_globals.default_context task_view fragment_ in
  Ok
    {
      fragment_;
      typed_ = typed;
      root_;
      globals_;
      type_;
      span_ = receipt.default_ast.location.span;
    }
