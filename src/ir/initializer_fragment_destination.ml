module Typed = Sema.Function_call_expression_result
module Fragment = Sema.Initializer_fragment
module Layout = Integer_initializer_layout
module Globals = Integer_globals

type t = {
  globals_ : Globals.t;
  fragment_ : Fragment.t;
  root_ : Typed.top_level_root_result;
  typed_ : Typed.top_level_t;
  layout_ : Layout.entry;
  storage_ : Globals.storage_slot;
  reference_ : Retained_global.t;
  span_ : Common.Span.t;
}

let globals value = value.globals_
let fragment value = value.fragment_
let root value = value.root_
let typed value = value.typed_
let layout value = value.layout_
let storage value = value.storage_
let reference value = value.reference_
let span value = value.span_
let ( let* ) = Result.bind

let create ~task_view ~reference ~slot ~layout typed =
  let* root =
    match
      Typed.top_level_statements typed
      |> List.concat_map Typed.top_level_statement_roots
    with
    | [ root ] -> Ok root
    | _ -> Error "initializer destination requires one complete typed fragment"
  in
  let* fragment =
    match
      Typed.top_level_root_source root
      |> Sema.Top_level_expression_tree.root_role
    with
    | Sema.Top_level_expression_tree.Initializer_fragment fragment ->
        Ok fragment
    | _ ->
        Error "initializer destination requires an original typed fragment root"
  in
  let declaration = Fragment.declaration fragment in
  let* () =
    if
      Globals.declared_record slot != declaration
      || Layout.leaf layout != Fragment.leaf fragment
      || not
           (Option.fold ~none:false ~some:(( == ) declaration)
              (Layout.declared_owner layout))
    then
      Error
        "initializer destination has another declared object or source layout"
    else Ok ()
  in
  let* globals_ = Globals.fragment_context task_view fragment in
  let storage_ = Globals.declared_storage slot in
  let* () =
    match Globals.retained_slot globals_ reference with
    | Some retained when Globals.same_storage retained storage_ -> Ok ()
    | _ ->
        Error
          "initializer destination is absent from its exact retained snapshot"
  in
  let value = Typed.top_level_root_value root in
  let* () =
    match Layout.operation layout with
    | Layout.Copy_bytes _ -> Ok ()
    | Layout.Scalar_store ->
        if
          Typed.result_array_rank value = 0
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
            "HCRUN0001: initializer fragment requires a scalar integer value"
  in
  let* span_ =
    match Typed.result_origin value with
    | Sema.Symbol.Source_location location -> Ok location.span
    | _ -> Error "initializer destination has no original expression span"
  in
  Ok
    {
      globals_;
      fragment_ = fragment;
      root_ = root;
      typed_ = typed;
      layout_ = layout;
      storage_;
      reference_ = reference;
      span_;
    }
