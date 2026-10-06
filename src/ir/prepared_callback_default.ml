module Headers = Sema.Function_type_resolution

type t = {
  namespace : Sema.Declaration_collection.namespace;
  header : Frontend.Parser.completed_callback_signature;
  receipt : Frontend.Parser.completed_callback_default;
  source : Frontend.Ast.function_parameter;
  type_ : Sema.Type.t;
  value : Saved_parameter_value.t;
}

let value v = v.value
let bits v = Option.get (Saved_parameter_value.word_bits v.value)
let word_bits v = Saved_parameter_value.word_bits v.value
let callback_source v = Saved_parameter_value.callback_source v.value

let undefined_callback_source v =
  Saved_parameter_value.undefined_callback_source v.value

let type_ v = v.type_
let receipt v = v.receipt
let namespace v = v.namespace
let header v = v.header

let create_value ~namespace ~header ~receipt ~value =
  let ( let* ) = Result.bind in
  let* source =
    match
      List.nth_opt header.Frontend.Parser.callback_pointer.signature_parameters
        receipt.Frontend.Parser.callback_default_index
    with
    | Some source
      when header.callback_signature_publication
           == receipt.callback_default_signature
           && List.exists (( == ) receipt) header.callback_defaults
           && Option.fold ~none:false
                ~some:(( == ) receipt.callback_default_ast)
                source.default
           && List.exists
                (fun p ->
                  p.Frontend.Parser.callback_parameter_publication
                  == receipt.callback_default_parameter
                  && p.callback_parameter_ast == source)
                header.callback_parameters -> Ok source
    | _ ->
        Error
          "anonymous prepared default requires its original completed \
           signature and member"
  in
  let* type_ =
    match source.function_pointer with
    | Some p when List.length p.indirection_layers = 1 ->
        Sema.Type.make_primitive ~form:Internal_storage ~primitive:I64
          ~pointer_depth:1
    | Some _ ->
        Error
          "anonymous callback word default requires one original pointer star"
    | None ->
        Sema.Source_type_reference.builtin source.type_specifier
          source.pointer_layers
        |> Result.map Sema.Type_reference.resolved_type
  in
  let* () =
    if
      Option.is_none (Saved_parameter_value.word_bits value)
      && Option.is_none source.function_pointer
    then
      Error "owned anonymous default requires its original callback parameter"
    else Ok ()
  in
  Ok { namespace; header; receipt; source; type_; value }

let create ~namespace ~header ~receipt ~bits =
  create_value ~namespace ~header ~receipt
    ~value:(Saved_parameter_value.word bits)

let matches value ~pointer ~parameter =
  Option.fold ~none:false
    ~some:(( == ) value.header.callback_pointer)
    (Headers.function_pointer_source pointer)
  && List.exists (( == ) parameter)
       (Headers.signature_parameters
          (Headers.function_pointer_signature pointer))
  && Headers.parameter_index parameter = value.receipt.callback_default_index
  && Option.fold ~none:false ~some:(( == ) value.source)
       (Headers.parameter_source parameter)
  && (match Headers.parameter_declarator_kind parameter with
    | Headers.Object ->
        Option.is_none value.source.function_pointer
        && Sema.Type.equal value.type_
             (Sema.Type_reference.resolved_type
                (Headers.parameter_type_reference parameter))
    | Headers.Function_pointer p ->
        Option.is_some value.source.function_pointer
        && Option.fold ~none:false
             ~some:(Sema.Type.equal value.type_)
             (Result.to_option (Headers.function_pointer_storage_type p)))
  &&
  match Headers.parameter_default parameter with
  | Some (Headers.Expression_default _) -> true
  | _ -> false
