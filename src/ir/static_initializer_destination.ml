module Typed = Sema.Function_call_expression_result
module Fragment = Sema.Static_initializer_fragment
module Globals = Integer_globals

type t = {
  allocation_ : Integer_static_allocation.t;
  fragment_ : Fragment.t;
  globals_ : Globals.t;
  typed_ : Typed.top_level_t;
  root_ : Typed.top_level_root_result;
  cell_ : int;
  byte_ : int;
  next_ : Integer_initializer_layout.stream;
  operation_ : Integer_initializer_layout.operation;
  scalar_ : Sema.Initializer_type.scalar option;
  preparation_ : Integer_initializer_layout.stream_preparation option;
  span_ : Common.Span.t;
}

let allocation t = t.allocation_
let fragment t = t.fragment_
let globals t = t.globals_
let typed t = t.typed_
let root t = t.root_
let cell_offset t = t.cell_
let byte_offset t = t.byte_

let prepared_result t =
  match t.preparation_ with
  | None -> (t.next_, (t.cell_, t.byte_, t.operation_, t.scalar_))
  | Some preparation ->
      Integer_initializer_layout.stream_preparation_result preparation
      |> Result.get_ok

let next t = fst (prepared_result t)

let scalar t =
  let _, (_, _, _, scalar) = prepared_result t in
  scalar

let requires_settlement t =
  Option.fold ~none:false
    ~some:Integer_initializer_layout.stream_preparation_requires_settlement
    t.preparation_

let settle t =
  if not (Fragment.is_current t.fragment_) then
    Error "static settlement has an expired source leaf"
  else
    match t.preparation_ with
    | Some preparation ->
        Integer_initializer_layout.settle_stream_preparation preparation
    | None -> Error "static settlement has no original expression phase"

let copy_byte_count t =
  match t.operation_ with
  | Integer_initializer_layout.Scalar_store -> None
  | Copy_bytes bytes -> Some (String.length bytes)

let operation t =
  match t.operation_ with
  | Integer_initializer_layout.Scalar_store ->
      Integer_initializer_layout.Scalar_store
  | Copy_bytes bytes -> Copy_bytes (Bytes.to_string (Bytes.of_string bytes))

let span t = t.span_
let storage t = Globals.declared_static_storage t.allocation_

let create ~allocation ~task_view ~cursor typed =
  let ( let* ) = Result.bind in
  let* () =
    if cursor == Integer_static_allocation.cursor allocation then Ok ()
    else Error "static initializer has another original cursor"
  in
  let* root_ =
    match
      Typed.top_level_statements typed
      |> List.concat_map Typed.top_level_statement_roots
    with
    | [ root ] -> Ok root
    | _ -> Error "static initializer requires one original typed leaf"
  in
  let* fragment_ =
    match
      Typed.top_level_root_source root_
      |> Sema.Top_level_expression_tree.root_role
    with
    | Sema.Top_level_expression_tree.Static_initializer_fragment fragment ->
        Ok fragment
    | _ -> Error "static initializer requires its original fragment root"
  in
  let receipt = Fragment.receipt fragment_ in
  let source = Integer_static_allocation.source allocation in
  let same_callback =
    match
      ( Integer_static_allocation.callback_source allocation,
        Fragment.callback_source fragment_ )
    with
    | None, None -> true
    | Some (header, pointer), Some (original, checked) ->
        header == original && pointer == checked
    | _ -> false
  in
  let* () =
    if
      receipt.static_allocation
      != Sema.Compiler_record.static_allocation_receipt source
      || (not same_callback)
      || (not
            (Option.equal ( == )
               (Fragment.aggregate fragment_)
               (Sema.Compiler_record.static_allocation_aggregate source)))
      || (not
            (Sema.Type.equal (Fragment.type_ fragment_)
               (Integer_static_allocation.type_ allocation)))
      || Fragment.dimensions fragment_
         <> Integer_storage_shape.dimensions
              (Integer_static_allocation.shape allocation)
      || not (Fragment.is_current fragment_)
    then
      Error "static initializer replaced its live allocation or checked shape"
    else Ok ()
  in
  let* preparation_, (next_, (cell_, byte_, operation_, scalar_)) =
    Integer_static_allocation.prepare_leaf allocation receipt
  in
  let value = Typed.top_level_root_value root_ in
  let* () =
    match operation_ with
    | Integer_initializer_layout.Copy_bytes _ -> (
        match Fragment.expression fragment_ with
        | Frontend.Ast.String_literal _ -> Ok ()
        | _ -> Error "static byte copy requires its original string leaf")
    | Scalar_store ->
        let callback = Option.is_some (Fragment.callback_source fragment_) in
        if
          Typed.result_array_rank value = 0
          && (match Typed.result_category value with
            | Typed.Object_value | Typed.Lvalue -> true
            | Typed.Address_value ->
                callback
                && Option.is_some (Typed.result_function_declaration value)
            | Typed.Callback_value ->
                callback && Typed.result_is_callback_storage value
            | _ -> false)
          && (Option.fold ~none:false
                ~some:(fun type_ ->
                  Option.is_some (Integer_scalar_storage.of_type type_))
                (Typed.result_type value)
             || callback
                && (Typed.result_is_callback_storage value
                   || Typed.result_is_numeric_callback value))
        then Ok ()
        else
          Error "HCRUN0001: static initializer requires a scalar integer value"
  in
  let* globals_ =
    Globals.static_fragment_context task_view allocation fragment_
  in
  let span_ =
    (Frontend.Ast.expression_location (Fragment.expression fragment_)).span
  in
  Ok
    {
      allocation_ = allocation;
      fragment_;
      globals_;
      typed_ = typed;
      root_;
      cell_;
      byte_;
      next_;
      operation_;
      scalar_;
      preparation_;
      span_;
    }
