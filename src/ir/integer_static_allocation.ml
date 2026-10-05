module Record = Sema.Compiler_record
module Collection = Sema.Function_collection
module Parser = Frontend.Parser
module Shape = Integer_storage_shape

type t = {
  source_ : Record.static_allocation;
  symbol_ : Sema.Symbol.t;
  type_ : Sema.Type.t;
  shape_ : Shape.t;
}

let source value = value.source_
let symbol value = value.symbol_
let type_ value = value.type_
let shape value = value.shape_

let owns_table value table =
  Record.static_allocation_owns_table value.source_ table

let ( let* ) = Result.bind

let create ~table ~header source_ =
  let receipt = Record.static_allocation_receipt source_ in
  let* () =
    if
      (not (Parser.function_local_allocation_is_current receipt))
      || (not (Record.static_allocation_owns_table source_ table))
      || Collection.function_symbol header
         != Sema.Declaration_collection.publication_symbol
              (Record.static_allocation_publication source_)
    then Error "static storage requires its original live source allocation"
    else Ok ()
  in
  let* symbol_ =
    match Collection.static_symbol header source_ with
    | Some symbol when Sema.Symbol_table.owns_symbol table symbol -> Ok symbol
    | _ -> Error "static storage lacks its original partial-header symbol"
  in
  let* type_ =
    match receipt.allocation_local.local_source with
    | Parser.Local_variable local
      when local.local_pointer_layers = []
           && Option.is_none local.local_function_pointer ->
        Sema.Source_type_reference.builtin local.local_type_specifier []
        |> Result.map Sema.Type_reference.resolved_type
    | _ -> Error "HCRUN0001: retained static storage requires integer objects"
  in
  let dimensions =
    Record.static_allocation_dimensions source_
    |> List.map Record.dimension_count
  in
  let* shape_ =
    Shape.create ~type_ ~dimensions
    |> Result.map_error (function
      | Shape.Unsupported_type ->
          "HCRUN0001: retained static storage requires nonzero integer types"
      | Shape.Invalid_extent | Shape.Overflow ->
          "HCRUN0004: retained static storage has an invalid checked shape")
  in
  Ok { source_; symbol_; type_; shape_ }

let check_completed value completed =
  let module Source = Sema.Static_local_source in
  let module Frame = Sema.Function_frame_layout in
  let location = Source.location completed in
  if
    Source.allocation completed != value.source_
    || Frame.location_symbol location != value.symbol_
    || (not
          (Sema.Type.equal value.type_ (Frame.location_checked_type location)))
    || List.map Frame.dimension_value (Frame.location_dimensions location)
       <> Shape.dimensions value.shape_
  then
    Error "completed static storage replaced its original allocation or symbol"
  else Ok ()
