type t = {
  allocation_ : Compiler_record.static_allocation;
  identifier_ : Frontend.Ast.identifier;
  symbol_ : Symbol.t;
  type_ : Type.t;
  dimensions_ : int64 list;
}

let allocation value = value.allocation_
let symbol value = value.symbol_
let type_ value = value.type_
let dimensions value = value.dimensions_
let owns_identifier value identifier = value.identifier_ == identifier

let create ~table ~header ~allocation ~selection =
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
  let* type_ =
    match receipt.allocation_local.local_source with
    | Frontend.Parser.Local_variable local
      when local.local_pointer_layers = []
           && Option.is_none local.local_function_pointer ->
        Source_type_reference.builtin local.local_type_specifier []
        |> Result.map Type_reference.resolved_type
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
      dimensions_;
    }
