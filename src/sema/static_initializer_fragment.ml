type t = {
  table : Symbol_table.t;
  namespace_ : Declaration_collection.namespace;
  publication_ : Declaration_collection.publication;
  receipt_ : Frontend.Parser.static_initializer_preparation;
  expression_ : Frontend.Ast.expression;
  type_ : Type.t;
  dimensions_ : int64 list;
  environment_ : Outer_environment.t;
  queries_ : Query_selection.t list;
}

let owns_table value table = value.table == table
let namespace value = value.namespace_
let publication value = value.publication_
let receipt value = value.receipt_
let expression value = value.expression_
let type_ value = value.type_
let dimensions value = value.dimensions_
let leaf_path value = value.receipt_.Frontend.Parser.static_leaf_path

let leaf_delimiters value =
  value.receipt_.Frontend.Parser.static_leaf_delimiters

let environment value = value.environment_
let queries value = value.queries_
let references _ = []

let origin value =
  Initializer_source.origin_of_location
    (Frontend.Ast.expression_location value.expression_)

let reference_for _ _ =
  Error "native static initializer cannot read storage or call a function"

let query_for value expression =
  match
    List.find_opt
      (fun q -> Query_selection.expression q == expression)
      value.queries_
  with
  | Some q -> Ok q
  | None -> Error "static initializer lacks its original query"

let create ~table ~namespace ~publication
    ~(receipt : Frontend.Parser.static_initializer_preparation) ~dimensions
    ~environment ~queries =
  let ( let* ) = Result.bind in
  let module Parser = Frontend.Parser in
  let module Ast = Frontend.Ast in
  let allocation = receipt.Parser.static_allocation in
  let* () =
    if
      (not (Parser.static_initializer_is_current receipt))
      || (not
            (Declaration_collection.namespace_owns_publication namespace
               publication))
      || (not (Declaration_collection.namespace_owns_table namespace table))
      || (not
            (Option.fold ~none:false
               ~some:(( == ) allocation.allocation_function)
               (Declaration_collection.publication_source_function publication)))
      || not (Outer_environment.owns_table environment table)
    then
      Error
        "native static initializer requires its original source owner and \
         callback"
    else Ok ()
  in
  let* expression =
    match receipt.static_leaf_value with
    | Ast.Scalar_initializer expression -> Ok expression
    | _ ->
        Error
          "HCRUN0004: native static initializer receipt is not a source leaf"
  in
  let* type_, source_dimensions =
    match
      (allocation.allocation_storage, allocation.allocation_local.local_source)
    with
    | Ast.Static_local, Parser.Local_variable source
      when source.local_pointer_layers = []
           && Option.is_none source.local_function_pointer -> (
        match source.local_type_specifier with
        | Ast.Primitive_type_specifier p ->
            Type.make_primitive ~form:Type.Public_spelling
              ~primitive:p.primitive ~pointer_depth:0
            |> Result.map (fun type_ -> (type_, source.local_array_dimensions))
        | Ast.Internal_type_specifier p ->
            Type.make_primitive ~form:Type.Internal_storage
              ~primitive:p.primitive ~pointer_depth:0
            |> Result.map (fun type_ -> (type_, source.local_array_dimensions))
        | _ ->
            Error
              "HCRUN0001: native static initializers require integer object \
               types")
    | _ ->
        Error
          "HCRUN0001: native static initializers require ordinary integer \
           object storage"
  in
  let* () =
    match Type.base type_ with
    | Type.Primitive (_, p)
      when (Primitive_type.info p).category = Primitive_type.Integer
           && (Primitive_type.info p).byte_size > 0 -> Ok ()
    | _ ->
        Error
          "HCRUN0001: native static initializers require nonzero integer types"
  in
  let* () =
    if source_dimensions = [] && receipt.static_leaf_path <> [] then
      Error
        "HCRUN0001: native static scalar initializers require a scalar \
         expression"
    else Ok ()
  in
  let* () =
    if List.length source_dimensions = List.length dimensions then Ok ()
    else
      Error
        "HCRUN0001: native static array initializer requires every original \
         dimension to have a checked fixed bound"
  in
  let* () =
    if Initializer_source.expression_identifier_nodes expression = [] then Ok ()
    else
      Error
        "HCRUN0006: native static initializers require closed expressions \
         without value or function references"
  in
  let* () = Query_selection.validate_manifest ~table ~expression queries in
  Ok
    {
      table;
      namespace_ = namespace;
      publication_ = publication;
      receipt_ = receipt;
      expression_ = expression;
      type_;
      dimensions_ = dimensions;
      environment_ = environment;
      queries_ = queries;
    }
