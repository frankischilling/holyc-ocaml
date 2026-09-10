module Headers = Sema.Function_type_resolution

type t = {
  publication : Sema.Declaration_collection.publication;
  header : Frontend.Parser.completed_function_header;
  receipt : Frontend.Parser.completed_parameter_default;
  source : Frontend.Ast.function_parameter;
  type_ : Sema.Type.t;
  bits : int64;
}

let bits value = value.bits
let type_ value = value.type_
let receipt value = value.receipt
let publication value = value.publication
let header value = value.header

let create ~publication ~header ~receipt ~bits =
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
      -> Ok source
    | _ -> Error "prepared default has another original completed parameter"
  in
  let* reference =
    Sema.Source_type_reference.builtin source.type_specifier
      source.pointer_layers
  in
  let type_ = Sema.Type_reference.resolved_type reference in
  Ok { publication; header; receipt; source; type_; bits }

let matches value ~header ~parameter =
  Headers.function_symbol header
  == Sema.Declaration_collection.publication_symbol value.publication
  && List.exists (( == ) parameter)
       (Headers.signature_parameters (Headers.function_signature header))
  && Headers.parameter_index parameter = value.receipt.default_parameter_index
  && Option.fold ~none:false ~some:(( == ) value.source)
       (Headers.parameter_source parameter)
  && Sema.Type.equal value.type_
       (Sema.Type_reference.resolved_type
          (Headers.parameter_type_reference parameter))
  && (match Headers.parameter_declarator_kind parameter with
    | Headers.Object -> true
    | _ -> false)
  &&
  match Headers.parameter_default parameter with
  | Some (Headers.Expression_default _) -> true
  | _ -> false
