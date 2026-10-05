module Record = Sema.Compiler_record
module Collection = Sema.Function_collection
module Parser = Frontend.Parser
module Shape = Integer_storage_shape

type t = {
  source_ : Record.static_allocation;
  symbol_ : Sema.Symbol.t;
  type_ : Sema.Type.t;
  shape_ : Shape.t;
  mutable cursor_ : Integer_initializer_layout.stream;
  mutable executed_ : (Parser.static_initializer_preparation * int * int) list;
}

let source value = value.source_
let symbol value = value.symbol_
let type_ value = value.type_
let shape value = value.shape_
let cursor value = value.cursor_

let native_leaf_executed value root =
  let module Typed = Sema.Function_call_expression_result in
  match
    Typed.initializer_source root
    |> Sema.Function_call_resolution.initializer_leaf
  with
  | None -> false
  | Some leaf ->
      List.exists
        (fun (receipt, _, _) ->
          match receipt.Parser.static_leaf_value with
          | Frontend.Ast.Scalar_initializer expression ->
              Sema.Initializer_source.leaf_expression_ast leaf == expression
              && Sema.Initializer_source.leaf_path leaf
                 = receipt.static_leaf_path
              && Option.is_some
                   (Parser.static_initializer_completed_declarator receipt)
          | _ -> false)
        value.executed_

let record_native_leaf value receipt ~cell_offset ~byte_offset
    ~operation:expected =
  let ( let* ) = Result.bind in
  if
    (not (Parser.static_initializer_is_current receipt))
    || receipt.Parser.static_allocation
       != Sema.Compiler_record.static_allocation_receipt value.source_
    || List.exists (fun (original, _, _) -> original == receipt) value.executed_
    ||
    match (receipt.static_leaf_predecessor, List.rev value.executed_) with
    | None, [] -> receipt.static_leaf_index <> 0
    | Some prior, (previous, _, _) :: _ ->
        prior != previous
        || receipt.static_leaf_index <> prior.static_leaf_index + 1
    | _ -> true
  then
    Error "static initializer completion replaced or replayed its original leaf"
  else
    let* next, (cell, bytes, operation) =
      Integer_initializer_layout.prepare_stream value.cursor_
        ~delimiters:receipt.static_leaf_delimiters
        ~value:receipt.static_leaf_value
    in
    if cell <> cell_offset || bytes <> byte_offset || operation <> expected then
      Error "native static completion has another original leaf destination"
    else (
      value.executed_ <- value.executed_ @ [ (receipt, cell, bytes) ];
      value.cursor_ <- next;
      Ok ())

let check_native_layout value roots =
  if
    List.for_all
      (fun (receipt, cell, bytes) ->
        List.length
          (List.filter
             (fun (root, actual_cell, actual_bytes) ->
               let leaf =
                 Sema.Function_call_expression_result.initializer_source root
                 |> Sema.Function_call_resolution.initializer_leaf
               in
               match (receipt.Parser.static_leaf_value, leaf) with
               | Frontend.Ast.Scalar_initializer expression, Some leaf ->
                   Sema.Initializer_source.leaf_expression_ast leaf
                   == expression
                   && Sema.Initializer_source.leaf_path leaf
                      = receipt.static_leaf_path
                   && cell = actual_cell && bytes = actual_bytes
               | _ -> false)
             roots)
        = 1)
      value.executed_
  then Ok ()
  else
    Error
      "completed static layout replaced a successfully executed original leaf \
       destination"

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
  Ok
    {
      source_;
      symbol_;
      type_;
      shape_;
      cursor_ = Integer_initializer_layout.begin_stream shape_;
      executed_ = [];
    }

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
