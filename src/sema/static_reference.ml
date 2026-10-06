type t = {
  allocation_ : Compiler_record.static_allocation;
  identifier_ : Frontend.Ast.identifier;
  symbol_ : Symbol.t;
  type_ : Type.t;
  storage_type_ : Type.t;
  return_reference_ : Type_reference.t option;
  callback_ : Function_type_resolution.function_pointer option;
  dimensions_ : int64 list;
}

let allocation value = value.allocation_
let symbol value = value.symbol_
let type_ value = value.type_
let storage_type value = value.storage_type_
let return_reference value = value.return_reference_
let callback_pointer value = value.callback_
let dimensions value = value.dimensions_
let owns_identifier value identifier = value.identifier_ == identifier

let create ?callback
    ?(selected_aggregate : Function_type_resolution.selected_aggregate_resolver =
      fun _ -> None) ~table ~header ~allocation ~selection () =
  let ( let* ) = Result.bind in
  let receipt = Compiler_record.static_allocation_receipt allocation in
  let* symbol_ =
    match Function_collection.static_symbol header allocation with
    | Some symbol
      when Symbol_table.owns_symbol table symbol
           && Compiler_record.static_allocation_owns_table allocation table
           && Option.fold ~none:false
                ~some:(( == ) receipt.allocation_local)
                (Frontend.Parser.selected_local selection) -> Ok symbol
    | _ ->
        Error
          "static reference lacks its original token selection and \
           partial-header symbol"
  in
  let* type_, storage_type_, return_reference_, callback_ =
    match receipt.allocation_local.local_source with
    | Frontend.Parser.Local_variable local
      when local.local_pointer_layers = []
           && Option.is_none local.local_function_pointer
           && Option.is_none callback ->
        Source_type_reference.builtin local.local_type_specifier []
        |> Result.map (fun reference ->
            let type_ = Type_reference.resolved_type reference in
            (type_, type_, None, None))
    | Frontend.Parser.Local_variable local -> (
        let module Headers = Function_type_resolution in
        match (local.local_function_pointer, callback) with
        | Some original, Some (completed, pointer)
          when List.length original.Frontend.Ast.indirection_layers = 1
               && completed.Frontend.Parser.callback_pointer == original
               && completed.callback_signature_publication.callback_command
                  == receipt.allocation_local.local_command
               && Option.fold ~none:false ~some:(( == ) original)
                    (Headers.function_pointer_source pointer) ->
            let* reference =
              Source_type_reference.callback_storage ~header:completed original
            in
            let* return_reference =
              Source_type_reference.builtin local.local_type_specifier
                local.local_pointer_layers
            in
            let* () =
              Headers.validate_source_callback_types ~table
                ~namespace:
                  (Compiler_record.static_allocation_namespace allocation)
                ~selected_aggregate pointer
            in
            Ok
              ( Type_reference.resolved_type return_reference,
                Type_reference.resolved_type reference,
                Some return_reference,
                Some pointer )
        | _ ->
            Error
              "HCRUN0001: static reference lacks its original one-star \
               callback header")
    | _ -> Error "HCRUN0001: static references require integer objects"
  in
  let dimensions_ =
    Compiler_record.static_allocation_dimensions allocation
    |> List.map Compiler_record.dimension_count
  in
  Ok
    {
      allocation_ = allocation;
      identifier_ = Frontend.Parser.selected_identifier selection;
      symbol_;
      type_;
      storage_type_;
      return_reference_;
      callback_;
      dimensions_;
    }
