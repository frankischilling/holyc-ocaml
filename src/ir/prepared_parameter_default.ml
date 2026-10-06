module Headers = Sema.Function_type_resolution

type t = {
  publication : Sema.Declaration_collection.publication;
  header : Frontend.Parser.completed_function_header;
  receipt : Frontend.Parser.completed_parameter_default;
  source : Frontend.Ast.function_parameter;
  type_ : Sema.Type.t;
  value : Saved_parameter_value.t;
}

let value value = value.value
let bits value = Option.get (Saved_parameter_value.word_bits value.value)
let word_bits value = Saved_parameter_value.word_bits value.value
let callback_source value = Saved_parameter_value.callback_source value.value
let type_ value = value.type_
let receipt value = value.receipt
let publication value = value.publication
let header value = value.header

let create_value ~publication ~header ~receipt ~value =
  let ( let* ) = Result.bind in
  let* source =
    match
      List.nth_opt header.Frontend.Parser.parameters
        receipt.Frontend.Parser.default_parameter_index
    with
    | Some source
      when header.function_publication == receipt.default_function
           && Option.fold ~none:false
                ~some:(( == ) receipt.default_function)
                (Sema.Declaration_collection.publication_source_function
                   publication)
           && Option.fold ~none:false
                ~some:(( == ) receipt.default_ast)
                source.default
           && source.type_specifier == receipt.default_type_specifier
           && source.pointer_layers == receipt.default_pointer_layers
           && source.register_qualifiers == receipt.default_register_qualifiers
           &&
           match
             (source.function_pointer, receipt.default_function_pointer)
           with
           | None, None -> true
           | Some source, Some original -> source == original
           | _ -> false -> Ok source
    | _ -> Error "prepared default has another original completed parameter"
  in
  let* type_ =
    match source.function_pointer with
    | Some pointer when List.length pointer.indirection_layers = 1 ->
        Sema.Type.make_primitive ~form:Internal_storage ~primitive:I64
          ~pointer_depth:1
    | Some _ ->
        Error "prepared callback default requires one original pointer star"
    | None ->
        Sema.Source_type_reference.builtin source.type_specifier
          source.pointer_layers
        |> Result.map Sema.Type_reference.resolved_type
  in
  let* () =
    if
      Option.is_some (Saved_parameter_value.callback_source value)
      && Option.is_none source.function_pointer
    then Error "owned saved default requires its original callback parameter"
    else Ok ()
  in
  Ok { publication; header; receipt; source; type_; value }

let create ~publication ~header ~receipt ~bits =
  create_value ~publication ~header ~receipt
    ~value:(Saved_parameter_value.word bits)

let matches value ~header ~parameter =
  (match Headers.function_provisional_call header with
    | None ->
        Headers.function_symbol header
        == Sema.Declaration_collection.publication_symbol value.publication
    | Some shape ->
        Option.fold ~none:false
          ~some:(fun member ->
            (Sema.Provisional_function.member_source member).parameter_function
            == value.receipt.default_function
            && Option.fold ~none:false ~some:(( == ) value.receipt)
                 (Sema.Provisional_function.member_default_source member)
            && Option.fold ~none:false
                 ~some:(fun completed ->
                   completed.Frontend.Parser.parameter_ast == value.source)
                 (Sema.Provisional_function.member_completion member))
          (List.nth_opt
             (Sema.Function_record_phase.fixed_members shape)
             (Headers.parameter_index parameter)))
  && List.exists (( == ) parameter)
       (Headers.signature_parameters (Headers.function_signature header))
  && Headers.parameter_index parameter = value.receipt.default_parameter_index
  && Option.fold ~none:false ~some:(( == ) value.source)
       (Headers.parameter_source parameter)
  && (match Headers.parameter_declarator_kind parameter with
    | Headers.Object ->
        Option.is_none value.source.function_pointer
        && Sema.Type.equal value.type_
             (Sema.Type_reference.resolved_type
                (Headers.parameter_type_reference parameter))
    | Headers.Function_pointer pointer ->
        Option.is_some value.source.function_pointer
        && Option.fold ~none:false
             ~some:(Sema.Type.equal value.type_)
             (Result.to_option (Headers.function_pointer_storage_type pointer)))
  &&
  match Headers.parameter_default parameter with
  | Some (Headers.Expression_default _) -> true
  | _ -> false
