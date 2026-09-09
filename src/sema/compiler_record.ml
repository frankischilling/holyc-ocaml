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

let ( let* ) = Result.bind

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

let published_scalar ~table ~namespace publication =
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
        if source.global_dimensions <> [] then
          Error "sizeof requires the selected global's checked array extent"
        else if Option.is_some source.global_function_pointer then
          Error "sizeof requires the selected function-pointer signature"
        else
          let* type_reference =
            Source_type_reference.builtin source.global_header.type_specifier
              source.global_pointer_layers
          in
          let* byte_size =
            scalar_size (Type_reference.resolved_type type_reference)
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

let read_sizeof ~table ~(root : Parser.query_root) record =
  if record.table != table || not (Symbol_table.owns_symbol table record.symbol)
  then Error "sizeof compiler record belongs to another semantic table"
  else
    match (root.query_node, root.query_lookup) with
    | Parser.Sizeof_target _, Visibility.Present entry
      when entry == record.entry -> Ok { owner = Hash_record record; root }
    | _ -> Error "sizeof read does not select this exact compiler record"

let read_local_sizeof ~table ~namespace ~function_publication
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
              if local.local_array_dimensions <> [] then
                Error "local sizeof requires the original checked array extent"
              else
                declared_size local.local_type_specifier
                  local.local_pointer_layers local.local_function_pointer
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
