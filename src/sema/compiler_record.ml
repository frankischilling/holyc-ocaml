module Visibility = Frontend.Symbol_visibility
module Parser = Frontend.Parser
module Ast = Frontend.Ast

type t = {
  table : Symbol_table.t;
  entry : Visibility.entry;
  symbol : Symbol.t;
  primitive : Primitive_type.t option;
  byte_size : int64;
  internal : bool;
}

type sizeof_owner =
  | Hash_record of t
  | Local_record of {
      table : Symbol_table.t;
      source : Parser.local_publication;
      byte_size : int64;
      internal : bool;
    }

type sizeof_read = { owner : sizeof_owner; root : Parser.query_root }

type query_role = Query_source.role =
  | Sizeof_root
  | Offset_root
  | Defined_operand

type query_read = {
  table : Symbol_table.t;
  receipt : Parser.completed_query;
  role : query_role;
  name : string;
  origin : Symbol.origin;
  sizeof_read : sizeof_read option;
}

type dimension_preparation = {
  dimension_table : Symbol_table.t;
  dimension_namespace : Declaration_collection.namespace;
  preparation : Parser.array_dimension_preparation;
  queries : query_read list;
  count : int64;
  work : int;
}

type declared_dimension = {
  prepared : dimension_preparation;
  completed : Parser.completed_array_dimension;
}

let ( let* ) = Result.bind

let validate_dimension ~table ~(dimension : Ast.array_dimension) checked =
  if checked.prepared.dimension_table != table then
    Error "checked array dimension belongs to another semantic table"
  else if checked.completed.dimension_ast != dimension then
    Error "checked array dimension belongs to another source node"
  else Ok ()

let dimension_count checked = checked.prepared.count
let dimension_work checked = checked.prepared.work
let dimension_receipt checked = checked.completed
let prepared_dimension_count checked = checked.count

let validate_dimension_queries checked queries =
  let original = checked.prepared.queries in
  if
    List.length original = List.length queries
    && List.for_all2 ( == ) original queries
  then Ok ()
  else Error "checked dimension has a substituted query manifest"

let declared_array_size ~table ~namespace ~command ~name ~dimensions ~checked
    size =
  let rec loop index predecessor total dimensions checked =
    match (dimensions, checked) with
    | [], [] -> Ok total
    | dimension :: dimensions, value :: checked ->
        let* () = validate_dimension ~table ~dimension value in
        let prepared = value.prepared in
        let source = prepared.preparation in
        let owner = source.dimension_owner in
        if
          prepared.dimension_namespace != namespace
          || owner.dimensions_command != command
          || owner.dimensions_name != name
          || source.dimension_index <> index
          ||
          match (source.dimension_predecessor, predecessor) with
          | None, None -> false
          | Some left, Some right -> left != right
          | _ -> true
        then
          Error "declared array size lacks its original ordered owner evidence"
        else
          (* PrsArrayDims retains I64 products, including the zero extent of
             an unsized first dimension. Storage admission has its own limits. *)
          loop (index + 1) (Some value.completed)
            (Int64.mul total prepared.count)
            dimensions checked
    | _ ->
        Error "sizeof requires the selected declaration's checked array extent"
  in
  loop 0 None size dimensions checked

let seed_primitive ~table ~entry ~symbol ~primitive =
  if
    (not (Symbol_table.owns_symbol table symbol))
    || Symbol.kind symbol <> Symbol.Internal_type
    || Visibility.kind entry <> Visibility.Internal_type
    || Symbol.name symbol <> Visibility.name entry
  then Error "primitive compiler record has a foreign symbol or entry"
  else
    let info = Primitive_type.info primitive in
    if
      Primitive_type.of_spelling (Visibility.name entry) <> Some primitive
      && Primitive_type.of_storage_spelling (Visibility.name entry)
         <> Some primitive
    then Error "primitive compiler record disagrees with its seeded type"
    else
      Ok
        {
          table;
          entry;
          symbol;
          primitive = Some primitive;
          byte_size = Int64.of_int info.byte_size;
          internal = true;
        }

let seed_public_union ~table ~entry ~symbol
    ~(source : Generated.Primitive_raw_types.public_union) =
  let path = Generated.Primitive_raw_types.kernel_source_path in
  let line = source.source_line in
  if
    (not
       (List.exists (( == ) source) Generated.Primitive_raw_types.public_unions))
    || (not (Symbol_table.owns_symbol table symbol))
    || Symbol.kind symbol <> Symbol.Aggregate_type
    || Visibility.kind entry <> Visibility.Class
    || Symbol.name symbol <> source.public_spelling
    || Visibility.name entry <> source.public_spelling
    || Symbol.origin symbol <> Symbol.Pinned_source { path; line }
    || Visibility.origin entry <> Visibility.Pinned_source { path; line }
  then Error "public union compiler record has a foreign seeded association"
  else
    match Primitive_type.of_storage_spelling source.storage_spelling with
    | None -> Error "public union lacks its checked primitive backing"
    | Some primitive ->
        let info = Primitive_type.info primitive in
        if
          info.declaration_form <> Primitive_type.Public_union
          || info.spelling <> source.public_spelling
          || info.declaration_source_line <> source.source_line
        then
          Error
            "public union compiler record disagrees with its pinned declaration"
        else
          Ok
            {
              table;
              entry;
              symbol;
              primitive = Some primitive;
              byte_size = Int64.of_int info.byte_size;
              internal = false;
            }

let scalar_size type_ =
  if Type.pointer_depth type_ > 0 then
    Ok (Int64.of_int Primitive_type.pointer_byte_size)
  else
    match Type.base type_ with
    | Type.Primitive (_, primitive) ->
        Ok (Int64.of_int (Primitive_type.info primitive).byte_size)
    | Type.Aggregate _ -> Error "sizeof requires the selected aggregate layout"

let rebind_primitive ~table ~symbol record =
  if
    Option.is_none record.primitive
    || (not (Symbol_table.owns_symbol table symbol))
    || Symbol.kind symbol <> Symbol.kind record.symbol
    || Symbol.name symbol <> Symbol.name record.symbol
    || Symbol.origin symbol <> Symbol.origin record.symbol
  then Error "forked primitive compiler record has a foreign symbol"
  else Ok { record with table; symbol }

let published_scalar ?(dimensions = []) ~table ~namespace publication =
  if
    (not (Declaration_collection.namespace_owns_table namespace table))
    || not
         (Declaration_collection.namespace_owns_publication namespace
            publication)
  then Error "compiler record publication belongs to another namespace or table"
  else
    match Declaration_collection.publication_source_global publication with
    | None -> Error "compiler record has no original parser global publication"
    | Some source ->
        if Option.is_some source.global_function_pointer then
          Error "sizeof requires the selected function-pointer signature"
        else
          let* type_reference =
            Source_type_reference.builtin source.global_header.type_specifier
              source.global_pointer_layers
          in
          let* byte_size =
            scalar_size (Type_reference.resolved_type type_reference)
          in
          let* byte_size =
            declared_array_size ~table ~namespace
              ~command:source.global_header.declaration_command
              ~name:source.global_name ~dimensions:source.global_dimensions
              ~checked:dimensions byte_size
          in
          Ok
            {
              table;
              entry = source.global_entry;
              symbol = Declaration_collection.publication_symbol publication;
              primitive = None;
              byte_size;
              internal = false;
            }

let bind_retained_scalar ~table ~entry global =
  let symbol = Global_type_resolution.global_symbol global in
  if
    (not (Symbol_table.owns_symbol table symbol))
    || Symbol.kind symbol <> Symbol.Global_variable
    || Visibility.kind entry <> Visibility.Global_variable
    || Symbol.name symbol <> Visibility.name entry
  then Error "retained compiler record has a foreign symbol or entry"
  else if Global_type_resolution.global_array_dimensions global <> [] then
    Error "sizeof requires the retained global's checked array extent"
  else
    match Global_type_resolution.global_declarator_kind global with
    | Global_type_resolution.Function_pointer _ ->
        Error "sizeof requires the retained function-pointer signature"
    | Global_type_resolution.Object ->
        let* byte_size =
          scalar_size
            (Type_reference.resolved_type
               (Global_type_resolution.global_type_reference global))
        in
        Ok
          {
            table;
            entry;
            symbol;
            primitive = None;
            byte_size;
            internal = false;
          }

let read_sizeof ~table ~(root : Parser.query_root) (record : t) =
  if record.table != table || not (Symbol_table.owns_symbol table record.symbol)
  then Error "sizeof compiler record belongs to another semantic table"
  else
    match (root.query_node, root.query_lookup) with
    | Parser.Sizeof_target _, Visibility.Present entry
      when entry == record.entry -> Ok { owner = Hash_record record; root }
    | _ -> Error "sizeof read does not select this exact compiler record"

let read_local_sizeof ~dimensions ~table ~namespace ~function_publication
    ~(root : Parser.query_root) =
  if
    (not (Declaration_collection.namespace_owns_table namespace table))
    || not
         (Declaration_collection.namespace_owns_publication namespace
            function_publication)
  then Error "local sizeof owner belongs to another namespace or table"
  else
    match
      ( root.query_node,
        root.query_lookup,
        root.query_local,
        Declaration_collection.publication_source_function function_publication
      )
    with
    | ( Parser.Sizeof_target name,
        Visibility.Shadowed_by_local,
        Some source,
        Some function_ )
      when source.local_environment == root.query_environment
           && source.local_command == root.query_command
           && source.local_spelling = name.spelling
           && function_.function_header.declaration_command
              == source.local_command
           && function_.function_environment == source.local_environment ->
        let declared_size type_specifier pointer_layers function_pointer =
          let* type_reference =
            Source_type_reference.builtin type_specifier pointer_layers
          in
          match function_pointer with
          | Some (pointer : Ast.function_pointer_declarator) ->
              let* depth =
                Source_type_reference.pointer_depth pointer.indirection_layers
              in
              if depth = 0 then
                Error "selected function pointer lacks original indirection"
              else
                (* PrsType (PrsVar.HC:350-356) assigns the RT_PTR companion
                 class, independently of the signature's return type. The
                 original declarator is retained by this local publication. *)
                Ok (Int64.of_int Primitive_type.pointer_byte_size, false)
          | None ->
              let type_ = Type_reference.resolved_type type_reference in
              let* byte_size = scalar_size type_ in
              let internal =
                Type.pointer_depth type_ = 0
                &&
                match Type.base type_ with
                | Type.Primitive (Type.Internal_storage, _) -> true
                | Type.Primitive (Type.Public_spelling, primitive) ->
                    (Primitive_type.info primitive).declaration_form
                    = Primitive_type.Internal_type
                | Type.Aggregate _ -> false
              in
              Ok (byte_size, internal)
        in
        let* byte_size, internal =
          match source.local_source with
          | Parser.Local_parameter parameter ->
              declared_size parameter.type_specifier parameter.pointer_layers
                parameter.function_pointer
          | Parser.Local_variable local ->
              let* byte_size, internal =
                declared_size local.local_type_specifier
                  local.local_pointer_layers local.local_function_pointer
              in
              let* byte_size =
                declared_array_size ~table ~namespace
                  ~command:source.local_command ~name:local.local_name
                  ~dimensions:local.local_array_dimensions ~checked:dimensions
                  byte_size
              in
              Ok (byte_size, internal)
          | Parser.Variadic_count _ ->
              Ok
                ( Int64.of_int (Primitive_type.info Primitive_type.I64).byte_size,
                  true )
          | Parser.Variadic_vector _ ->
              (* PrsVar.HC:392-403 gives argv an internal I64 class and a
               127-element extent, independently of its eight-byte slot. *)
              Ok
                ( Int64.of_int
                    (127 * (Primitive_type.info Primitive_type.I64).byte_size),
                  true )
        in
        Ok { owner = Local_record { table; source; byte_size; internal }; root }
    | _ ->
        Error
          "local sizeof read lacks its original function and source publication"

let sizeof_table read =
  match read.owner with
  | Hash_record record -> record.table
  | Local_record local -> local.table

let complete_sizeof ~table ~(receipt : Parser.completed_query) read =
  if sizeof_table read != table || read.root != receipt.query_root then
    Error "sizeof completion belongs to another read or semantic table"
  else
    match receipt.query_expression with
    | Ast.Sizeof_expression expression when expression.sizeof_members = [] ->
        Ok ()
    | _ -> Error "sizeof requires its original checked member reads"

let sizeof_value read ~pointer =
  if pointer then Int64.of_int Primitive_type.pointer_byte_size
  else
    match read.owner with
    | Hash_record record -> record.byte_size
    | Local_record local -> local.byte_size

let sizeof_primitive read =
  match read.owner with
  | Hash_record record -> record.primitive
  | Local_record _ -> None

let sizeof_is_internal read =
  match read.owner with
  | Hash_record record -> record.internal
  | Local_record local -> local.internal

let origin (location : Frontend.Ast.location) =
  Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let complete_query ?sizeof_read ~table
    ~(receipt : Frontend.Parser.completed_query) () =
  let open Frontend in
  let root = receipt.query_root in
  if
    root.query_environment
    != Parser.context_environment root.query_command.command_context
  then Error "query selection has a foreign parser environment"
  else
    let facts =
      match (root.query_node, receipt.query_expression) with
      | Parser.Defined_target operand, Ast.Defined_expression expression
        when operand == expression.defined_operand ->
          Some
            ( Defined_operand,
              operand.defined_operand_spelling,
              origin operand.defined_operand_location )
      | Parser.Sizeof_target target, Ast.Sizeof_expression expression
        when target == expression.sizeof_target ->
          Some (Sizeof_root, target.spelling, origin target.location)
      | Parser.Offset_target target, Ast.Offset_expression expression
        when target == expression.offset_target ->
          Some (Offset_root, target.spelling, origin target.location)
      | _ -> None
    in
    match facts with
    | None -> Error "query selection does not retain its original AST root"
    | Some (role, name, origin) ->
        let checked =
          match sizeof_read with
          | None -> Ok ()
          | Some read -> complete_sizeof ~table ~receipt read
        in
        Result.map
          (fun () -> { table; receipt; role; name; origin; sizeof_read })
          checked

let validate_query ~table ~role ~name ~origin selection =
  if selection.table != table then
    Error "query selection belongs to another symbol table"
  else if
    selection.role <> role || selection.name <> name
    || selection.origin <> origin
  then Error "query selection belongs to another source occurrence or role"
  else Ok ()

let query_expression selection = selection.receipt.query_expression
let query_owns_table selection table = selection.table == table

let query_is_local selection =
  match selection.receipt.query_root.query_lookup with
  | Frontend.Symbol_visibility.Shadowed_by_local -> true
  | Frontend.Symbol_visibility.Absent | Frontend.Symbol_visibility.Present _ ->
      false

let query_presence selection =
  match selection.role with
  | Defined_operand -> Some selection.receipt.query_root.query_present
  | Sizeof_root | Offset_root -> None

let query_sizeof selection =
  match (selection.sizeof_read, selection.receipt.query_expression) with
  | Some read, Frontend.Ast.Sizeof_expression expression ->
      let pointer = expression.sizeof_pointer_layers <> [] in
      Some (sizeof_primitive read, sizeof_value read ~pointer, pointer)
  | _ -> None

let query_constant selection =
  match query_presence selection with
  | Some present -> Some (if present then 1L else 0L)
  | None -> query_sizeof selection |> Option.map (fun (_, value, _) -> value)

let validate_query_manifest ~table ~expression queries =
  let rec loop expressions queries =
    match (expressions, queries) with
    | [], [] -> Ok ()
    | expression :: expressions, query :: queries
      when query.table == table && query.receipt.query_expression == expression
      -> loop expressions queries
    | _ -> Error "query manifest lacks its exact ordered source reads or table"
  in
  loop (Query_source.source_queries expression) queries

let prepare_dimension ~table ~namespace ~max_work
    ~(preparation : Parser.array_dimension_preparation) ~queries =
  let module Numeric = Closed_numeric_expression in
  let work = ref 0 in
  let result =
    let owner = preparation.dimension_owner in
    if not (Declaration_collection.namespace_owns_table namespace table) then
      Error "array preparation belongs to another namespace or table"
    else if max_work < 0 then Error "array preparation has a negative allowance"
    else if
      owner.dimensions_environment
      != Parser.context_environment owner.dimensions_command.command_context
    then Error "array preparation has a foreign parser environment"
    else if
      List.exists
        (fun query ->
          query.receipt.query_root.query_command != owner.dimensions_command
          || query.receipt.query_root.query_environment
             != owner.dimensions_environment)
        queries
    then Error "array preparation has query reads from another parser command"
    else
      let* count =
        match preparation.dimension_expression with
        | None ->
            if preparation.dimension_index <> 0 || queries <> [] then
              Error
                "only the first array dimension can be empty, without query \
                 reads"
            else Ok 0L
        | Some expression ->
            let* () = validate_query_manifest ~table ~expression queries in
            let expression =
              Numeric.of_ast ~allow_floating:true ~query_expression ~queries
                expression
            in
            let rec closed = function
              | Numeric.Selected_query_expression query ->
                  if Option.is_some (query_constant query) then Ok ()
                  else
                    Error
                      "array preparation requires the selected query's checked \
                       value"
              | Numeric.Integer_expression _
              | Numeric.Unsigned_integer_expression _
              | Numeric.Floating_expression _ -> Ok ()
              | Numeric.Unary_expression { operand; _ } -> closed operand
              | Numeric.Binary_expression { left; right; _ } ->
                  let* () = closed left in
                  closed right
              | Numeric.Current_position_expression _ ->
                  Error
                    "array preparation requires unresolved current-position \
                     evidence"
              | Numeric.Dependency_expression { detail; _ } ->
                  Error
                    ("array preparation requires checked runtime evaluation: "
                   ^ detail)
              | Numeric.Unsupported_expression { description; _ } ->
                  Error
                    ("array preparation has an unsupported expression: "
                   ^ description)
            in
            let* () = closed expression in
            let consume () =
              if !work >= max_work then
                Error
                  (Numeric.make_error "HCIRVM0007"
                     (Numeric.Invalid_input "array preparation work limit")
                     "the bounded array dimension preparation work limit was \
                      exhausted")
              else (
                incr work;
                Ok ())
            in
            let* count =
              Numeric.evaluate_expression ~consume
                ~query_origin:(fun query -> query.origin)
                ~query_value:query_constant ~context:Numeric.Array_dimension
                ~current_position:0L expression
              |> Result.map_error Numeric.error_to_string
            in
            if count < 0L then
              Error (Printf.sprintf "array extent %Ld is negative" count)
            else Ok count
      in
      Ok
        {
          dimension_table = table;
          dimension_namespace = namespace;
          preparation;
          queries;
          count;
          work = !work;
        }
  in
  (result, !work)

let complete_dimension ~receipt prepared =
  let source = prepared.preparation in
  let dimension = receipt.Parser.dimension_ast in
  if
    receipt.dimension_preparation != source
    || dimension.opening_bracket != source.dimension_opening
    ||
    match (dimension.dimension_expression, source.dimension_expression) with
    | None, None -> false
    | Some left, Some right -> left != right
    | _ -> true
  then
    Error
      "checked dimension completion substituted its original preparation or \
       children"
  else Ok { prepared; completed = receipt }
