type t = {
  table : Symbol_table.t;
  namespace_ : Declaration_collection.namespace;
  publication_ : Declaration_collection.publication;
  receipt_ : Frontend.Parser.static_initializer_preparation;
  activation_ : Source_activation.t option;
  expression_ : Frontend.Ast.expression;
  type_ : Type.t;
  callback_source_ :
    (Frontend.Parser.completed_callback_signature
    * Function_type_resolution.function_pointer)
    option;
  dimensions_ : int64 list;
  environment_ : Outer_environment.t;
  queries_ : Query_selection.t list;
  references_ : (Frontend.Ast.identifier * Reference_selection.t) list;
}

let owns_table value table = value.table == table
let namespace value = value.namespace_
let publication value = value.publication_
let receipt value = value.receipt_

let is_current value =
  Frontend.Parser.static_initializer_is_current value.receipt_
  || Source_activation.static_initializer value.activation_ value.receipt_

let expression value = value.expression_
let type_ value = value.type_
let callback_source value = value.callback_source_
let dimensions value = value.dimensions_
let leaf_path value = value.receipt_.Frontend.Parser.static_leaf_path

let leaf_delimiters value =
  value.receipt_.Frontend.Parser.static_leaf_delimiters

let environment value = value.environment_
let queries value = value.queries_
let references value = value.references_

let origin value =
  Initializer_source.origin_of_location
    (Frontend.Ast.expression_location value.expression_)

let reference_for value identifier =
  match
    List.find_opt
      (fun (original, _) -> original == identifier)
      value.references_
  with
  | Some (_, selection) -> Ok selection
  | None -> Error "static initializer lacks its original identifier selection"

let query_for value expression =
  match
    List.find_opt
      (fun q -> Query_selection.expression q == expression)
      value.queries_
  with
  | Some q -> Ok q
  | None -> Error "static initializer lacks its original query"

let create_selected ?activation ?callback
    ?(selected_aggregate : Function_type_resolution.selected_aggregate_resolver =
      fun _ -> None) ~references ~table ~namespace ~publication
    ~(receipt : Frontend.Parser.static_initializer_preparation) ~dimensions
    ~environment ~queries () =
  let ( let* ) = Result.bind in
  let module Parser = Frontend.Parser in
  let module Ast = Frontend.Ast in
  let allocation = receipt.Parser.static_allocation in
  let* () =
    if
      (not
         (Parser.static_initializer_is_current receipt
         || Source_activation.static_initializer activation receipt))
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
           && Option.is_none source.local_function_pointer
           && Option.is_none callback -> (
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
    | Ast.Static_local, Parser.Local_variable source -> (
        let module Headers = Function_type_resolution in
        match (source.local_function_pointer, callback) with
        | Some original, Some (header, pointer)
          when List.length original.Ast.indirection_layers = 1
               && header.Parser.callback_pointer == original
               && header.callback_signature_publication.callback_command
                  == allocation.allocation_local.local_command
               && Option.fold ~none:false ~some:(( == ) original)
                    (Headers.function_pointer_source pointer) ->
            let* reference =
              Source_type_reference.callback_storage ~header original
            in
            let* () =
              Headers.validate_source_callback_types ~table ~namespace
                ~selected_aggregate pointer
            in
            Ok
              ( Type_reference.resolved_type reference,
                source.local_array_dimensions )
        | _ ->
            Error
              "HCRUN0001: static callback initializer lacks its original \
               one-star header")
    | _ ->
        Error
          "HCRUN0001: native static initializers require ordinary integer \
           object storage"
  in
  let* () =
    match Type.base type_ with
    | Type.Primitive (_, p)
      when Option.is_some (Primitive_type.integer_storage_info p) -> Ok ()
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
    let rec validate expected actual =
      match (expected, actual) with
      | [], [] -> Ok ()
      | (identifier : Ast.identifier) :: rest, (original, selection) :: tail
        when identifier == original ->
          let* () =
            Reference_selection.validate ~table ~name:identifier.spelling
              selection
          in
          let* () =
            match Reference_selection.kind selection with
            | Reference_selection.Outer (owner, binding)
              when owner == environment
                   && Outer_environment.owns_binding environment binding ->
                Ok ()
            | Reference_selection.Absent | Reference_selection.Unavailable ->
                Ok ()
            | Reference_selection.Static_local reference ->
                let original =
                  Static_reference.allocation reference
                  |> Compiler_record.static_allocation_receipt
                in
                if
                  original.allocation_function == allocation.allocation_function
                  && Static_reference.owns_identifier reference identifier
                then Ok ()
                else
                  Error
                    "static initializer replaced its original local occurrence \
                     or declaring function"
            | _ ->
                Error "static initializer selected another source environment"
          in
          validate rest tail
      | _ ->
          Error
            "HCRUN0006: static initializer requires every original identifier \
             selection"
    in
    validate
      (Initializer_source.expression_identifier_nodes expression)
      references
  in
  let* () = Query_selection.validate_manifest ~table ~expression queries in
  Ok
    {
      table;
      namespace_ = namespace;
      publication_ = publication;
      receipt_ = receipt;
      activation_ = activation;
      expression_ = expression;
      type_;
      callback_source_ = callback;
      dimensions_ = dimensions;
      environment_ = environment;
      queries_ = queries;
      references_ = references;
    }

let create ?activation ?callback ?selected_aggregate ~table ~namespace
    ~publication ~receipt ~dimensions ~environment ~queries () =
  create_selected ?activation ?callback ?selected_aggregate ~references:[]
    ~table ~namespace ~publication ~receipt ~dimensions ~environment ~queries ()
