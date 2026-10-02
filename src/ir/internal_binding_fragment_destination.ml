module Typed = Sema.Function_call_expression_result
module Fragment = Sema.Internal_binding_fragment

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

let create_with_globals globals typed =
  let ( let* ) = Result.bind in
  let* root_ =
    match
      Typed.top_level_statements typed
      |> List.concat_map Typed.top_level_statement_roots
    with
    | [ root ] -> Ok root
    | _ ->
        Error
          "internal binding destination requires one complete original \
           expression"
  in
  let* fragment_ =
    match
      Typed.top_level_root_source root_
      |> Sema.Top_level_expression_tree.root_role
    with
    | Sema.Top_level_expression_tree.Internal_binding_fragment fragment ->
        Ok fragment
    | _ ->
        Error
          "internal binding destination requires its original internal binding \
           root"
  in
  let receipt = Fragment.receipt fragment_ in
  let* type_ =
    Sema.Type.make_primitive ~form:Internal_storage ~primitive:I64
      ~pointer_depth:0
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
        "HCRUN0001: internal binding preparation requires a checked scalar \
         integer value"
  in
  let* globals_ = globals fragment_ in
  Ok
    {
      fragment_;
      typed_ = typed;
      root_;
      globals_;
      type_;
      span_ = receipt.binding_ast.location.span;
    }

let create ~task_view =
  create_with_globals (Integer_globals.internal_binding_context task_view)
