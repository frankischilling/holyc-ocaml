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

type sizeof_read = { record : t; root : Parser.query_root }

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
    (not record.internal)
    || (not (Symbol_table.owns_symbol table symbol))
    || Symbol.kind symbol <> Symbol.Internal_type
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
      when entry == record.entry -> Ok { record; root }
    | _ -> Error "sizeof read does not select this exact compiler record"

let complete_sizeof ~table ~(receipt : Parser.completed_query) read =
  if read.record.table != table || read.root != receipt.query_root then
    Error "sizeof completion belongs to another read or semantic table"
  else
    match receipt.query_expression with
    | Ast.Sizeof_expression expression when expression.sizeof_members = [] ->
        Ok ()
    | _ -> Error "sizeof requires its original checked member reads"

let sizeof_value read ~pointer =
  if pointer then Int64.of_int Primitive_type.pointer_byte_size
  else read.record.byte_size

let sizeof_primitive read = read.record.primitive
let sizeof_is_internal read = read.record.internal
