type declaration_kind =
  | Aggregate_forward
  | Aggregate_definition
  | Aggregate_attached_global
  | Global_variable
  | Function_prototype
  | Function_definition

type declaration = {
  name : string;
  declaration_kind : declaration_kind;
  origin : Symbol.origin;
  item_index : int;
  declarator_index : int option;
}

type entry = {
  symbol : Symbol.t;
  declaration_kind : declaration_kind;
  item_index : int;
  declarator_index : int option;
}

type t = { scope : Symbol_table.scope; entries : entry list }

type namespace = {
  table : Symbol_table.t;
  scope : Symbol_table.scope;
  owner : unit ref;
}

type publication = {
  owner : unit ref;
  symbol : Symbol.t;
  source_global : Frontend.Parser.global_publication option;
  source_function : Frontend.Parser.function_publication option;
}

let scope (collection : t) = collection.scope
let entries collection = collection.entries
let entry_symbol (entry : entry) = entry.symbol
let entry_kind entry = entry.declaration_kind
let entry_item_index entry = entry.item_index
let entry_declarator_index entry = entry.declarator_index

let declaration_kind_name = function
  | Aggregate_forward -> "aggregate-forward"
  | Aggregate_definition -> "aggregate-definition"
  | Aggregate_attached_global -> "aggregate-attached-global"
  | Global_variable -> "global-variable"
  | Function_prototype -> "function-prototype"
  | Function_definition -> "function-definition"

let make_declaration ~name ~declaration_kind ~origin ~item_index
    ?declarator_index () =
  if String.equal name "" then Error "semantic declaration name cannot be empty"
  else if item_index < 0 then
    Error "semantic declaration item index cannot be negative"
  else
    match declarator_index with
    | Some index when index < 0 ->
        Error "semantic declarator index cannot be negative"
    | None | Some _ ->
        Ok { name; declaration_kind; origin; item_index; declarator_index }

let symbol_kind = function
  | Aggregate_forward | Aggregate_definition -> Symbol.Aggregate_type
  | Aggregate_attached_global | Global_variable -> Symbol.Global_variable
  | Function_prototype | Function_definition -> Symbol.Function

let create_module_scope table module_name =
  let parent = Symbol_table.root table in
  match module_name with
  | Some name when not (String.equal name "") ->
      Symbol_table.create_scope table ~parent ~kind:Symbol_table.Module ~name ()
  | None | Some _ ->
      Symbol_table.create_scope table ~parent ~kind:Symbol_table.Module ()

let add_entry table scope declaration =
  match
    Symbol_table.add table ~scope ~name:declaration.name
      ~kind:(symbol_kind declaration.declaration_kind)
      ~origin:declaration.origin
  with
  | Error _ as error -> error
  | Ok symbol ->
      Ok
        {
          symbol;
          declaration_kind = declaration.declaration_kind;
          item_index = declaration.item_index;
          declarator_index = declaration.declarator_index;
        }

let collect ~table ?module_name declarations =
  match create_module_scope table module_name with
  | Error _ as error -> error
  | Ok scope ->
      let rec add entries_rev = function
        | [] -> Ok { scope; entries = List.rev entries_rev }
        | declaration :: rest -> (
            match add_entry table scope declaration with
            | Error _ as error -> error
            | Ok entry -> add (entry :: entries_rev) rest)
      in
      add [] declarations

let create_namespace ~table ?module_name () =
  create_module_scope table module_name
  |> Result.map (fun scope -> { table; scope; owner = ref () })

let namespace_scope (namespace : namespace) = namespace.scope
let publication_symbol (publication : publication) = publication.symbol
let publication_source_global publication = publication.source_global
let publication_source_function publication = publication.source_function

let namespace_owns_publication (namespace : namespace) publication =
  publication.owner == namespace.owner
  && Symbol_table.owns_symbol namespace.table publication.symbol

let namespace_owns_table (namespace : namespace) table =
  namespace.table == table

let publish (namespace : namespace) ~name ~kind ~origin =
  match kind with
  | Symbol.Global_variable | Symbol.Function | Symbol.Aggregate_type ->
      Symbol_table.add namespace.table ~scope:namespace.scope ~name ~kind
        ~origin
      |> Result.map (fun symbol ->
          {
            owner = namespace.owner;
            symbol;
            source_global = None;
            source_function = None;
          })
  | _ ->
      Error
        "semantic declaration publication needs a top-level declaration kind"

let publish_global namespace (source : Frontend.Parser.global_publication) =
  let location = source.global_name.location in
  let origin =
    Symbol.Source_location
      {
        span = location.span;
        source_segments = location.source_segments;
        generated_from = location.generated_from;
        defined_at = location.defined_at;
      }
  in
  publish namespace ~name:source.global_name.spelling
    ~kind:Symbol.Global_variable ~origin
  |> Result.map (fun publication ->
      { publication with source_global = Some source })

let publish_function namespace (source : Frontend.Parser.function_publication) =
  let location = source.function_name.location in
  let origin =
    Symbol.Source_location
      {
        span = location.span;
        source_segments = location.source_segments;
        generated_from = location.generated_from;
        defined_at = location.defined_at;
      }
  in
  publish namespace ~name:source.function_name.spelling ~kind:Symbol.Function
    ~origin
  |> Result.map (fun publication ->
      { publication with source_function = Some source })

let view (namespace : namespace) publications =
  let rec validate previous seen entries_rev = function
    | [] -> Ok { scope = namespace.scope; entries = List.rev entries_rev }
    | ((publication : publication), (declaration : declaration)) :: rest ->
        let symbol = publication.symbol in
        let position = (declaration.item_index, declaration.declarator_index) in
        let valid_shape =
          match
            (declaration.declaration_kind, declaration.declarator_index)
          with
          | Aggregate_attached_global, None -> false
          | ( ( Aggregate_forward
              | Aggregate_definition
              | Function_prototype
              | Function_definition ),
              Some _ ) -> false
          | _ -> true
        in
        if
          publication.owner != namespace.owner
          || (not (Symbol_table.owns_symbol namespace.table symbol))
          || not
               (Symbol.Scope_id.equal (Symbol.scope_id symbol)
                  (Symbol_table.scope_id namespace.scope))
        then
          Error "semantic declaration publication belongs to another namespace"
        else if List.exists (fun prior -> prior == publication) seen then
          Error "semantic declaration view repeats a publication"
        else if
          Symbol.name symbol <> declaration.name
          || (not
                (Symbol.equal_kind (Symbol.kind symbol)
                   (symbol_kind declaration.declaration_kind)))
          || Symbol.origin symbol <> declaration.origin
        then Error "semantic declaration view does not match its publication"
        else if
          (not valid_shape)
          || Option.fold ~none:false
               ~some:(fun prior -> compare prior position >= 0)
               previous
        then
          Error
            "semantic declaration view has invalid source order or declarator \
             shape"
        else
          let entry =
            {
              symbol;
              declaration_kind = declaration.declaration_kind;
              item_index = declaration.item_index;
              declarator_index = declaration.declarator_index;
            }
          in
          validate (Some position) (publication :: seen) (entry :: entries_rev)
            rest
  in
  validate None [] [] publications
