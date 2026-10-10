module Typed = Sema.Function_call_expression_result
module Fragment = Sema.Default_fragment

type t = {
  fragment_ : Fragment.t;
  typed_ : Typed.top_level_t;
  root_ : Typed.top_level_root_result;
  globals_ : Integer_globals.t;
  type_ : Sema.Type.t;
  aggregate_value_type_ : Sema.Type.t option;
  span_ : Common.Span.t;
}

let fragment value = value.fragment_
let typed value = value.typed_
let root value = value.root_
let globals value = value.globals_
let type_ value = value.type_
let aggregate_value_type value = value.aggregate_value_type_
let span value = value.span_
let symbol_opt value = Fragment.symbol_opt value.fragment_
let symbol value = Option.get (symbol_opt value)

let is_callback value =
  let _, _, pointer = Fragment.parameter_parts value.fragment_ in
  Option.is_some pointer

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
  let _, _, function_pointer = Fragment.parameter_parts fragment_ in
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
        Fragment.parameter_type fragment_
        |> Result.map Sema.Type_reference.resolved_type
  in
  let aggregate_value_type_ =
    if Option.is_some function_pointer then None
    else
      Typed.top_level_aggregate_integer_value_type typed
        ~before_item_index:max_int type_
  in
  let* type_ =
    match (Sema.Type.base type_, aggregate_value_type_) with
    | Sema.Type.Aggregate _, Some _ when Sema.Type.pointer_depth type_ = 0 ->
        (* A default is saved before the member's load/store width is applied. *)
        Sema.Type.make_primitive ~form:Internal_storage ~primitive:I64
          ~pointer_depth:0
    | Sema.Type.Aggregate _, _ when Sema.Type.pointer_depth type_ = 0 ->
        Error
          "HCRUN0001: class defaults require original completed integer value \
           metadata"
    | _ -> Ok type_
  in
  let value = Typed.top_level_root_value root_ in
  let* () =
    if
      Option.is_some function_pointer
      && (Saved_parameter_value.accepts_callback_expression value
         || Typed.result_is_numeric_callback value)
    then Ok ()
    else if
      Option.is_some (Integer_scalar_storage.of_type type_)
      && Typed.result_array_rank value = 0
      && (match Typed.result_category value with
        | Typed.Object_value | Typed.Lvalue -> true
        | _ -> false)
      && Option.fold ~none:false
           ~some:(fun type_ ->
             Option.is_some (Integer_scalar_storage.of_type type_))
           (Typed.result_value_type value)
      && (Option.is_none aggregate_value_type_
         || Option.is_none (Typed.result_callback_parser_pointer value))
    then Ok ()
    else if
      Option.is_none function_pointer
      && Sema.Type.pointer_depth type_ = 1
      && Option.is_some
           (Option.bind
              (Result.to_option (Sema.Type.dereference type_))
              Integer_scalar_storage.of_type)
      && Option.fold ~none:false
           ~some:(Integer_scalar_storage.compatible_pointer type_)
           (Option.bind (Typed.result_type value) (fun source ->
                if Typed.result_is_array_address value then
                  Result.to_option (Sema.Type.pointer_to source)
                else Some source))
    then Ok ()
    else
      Error
        "HCRUN0001: default preparation requires a checked scalar integer or \
         owned data-pointer value"
  in
  let* globals_ = globals fragment_ in
  Ok
    {
      fragment_;
      typed_ = typed;
      root_;
      globals_;
      type_;
      aggregate_value_type_;
      span_ = (Fragment.ast fragment_).location.span;
    }

let create ~task_view =
  create_with_globals (Integer_globals.default_context task_view)

let create_source = create_with_globals Integer_globals.source_default_context

let create_native_source =
  create_with_globals Integer_globals.native_source_default_context
