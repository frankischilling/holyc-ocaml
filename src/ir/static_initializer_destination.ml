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
  span_ : Common.Span.t;
}

let allocation t = t.allocation_
let fragment t = t.fragment_
let globals t = t.globals_
let typed t = t.typed_
let root t = t.root_
let cell_offset t = t.cell_
let byte_offset t = t.byte_
let next t = t.next_
let span t = t.span_
let storage t = Globals.declared_static_storage t.allocation_

let create ~allocation ~task_view ~cursor typed =
  let ( let* ) = Result.bind in
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
  let* () =
    if
      receipt.static_allocation
      != Sema.Compiler_record.static_allocation_receipt source
      || (not
            (Sema.Type.equal (Fragment.type_ fragment_)
               (Integer_static_allocation.type_ allocation)))
      || Fragment.dimensions fragment_
         <> Integer_storage_shape.dimensions
              (Integer_static_allocation.shape allocation)
      || not (Frontend.Parser.static_initializer_is_current receipt)
    then
      Error "static initializer replaced its live allocation or checked shape"
    else Ok ()
  in
  let value = Typed.top_level_root_value root_ in
  let* () =
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
    else Error "HCRUN0001: static initializer requires a scalar integer value"
  in
  let* next_, (cell_, byte_, operation) =
    Integer_initializer_layout.prepare_stream cursor
      ~delimiters:receipt.static_leaf_delimiters
      ~value:receipt.static_leaf_value
  in
  let* () =
    match operation with
    | Integer_initializer_layout.Scalar_store -> Ok ()
    | Copy_bytes _ ->
        Error
          "HCRUN0006: native static string copies require a separate checked \
           destination"
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
      span_;
    }
