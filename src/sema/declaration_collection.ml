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
  source_prototype : Frontend.Ast.function_prototype option;
}

type function_source =
  | Collected_prototype of Frontend.Ast.function_prototype
  | Published_function of Frontend.Parser.function_publication

type entry = {
  symbol : Symbol.t;
  aggregate_identity : Symbol.t option;
  declaration_kind : declaration_kind;
  item_index : int;
  declarator_index : int option;
  function_source : function_source option;
}

type t = { scope : Symbol_table.scope; entries : entry list }

type publication = {
  owner : unit ref;
  symbol : Symbol.t;
  aggregate_identity : Symbol.t option;
  source_global : Frontend.Parser.global_publication option;
  source_function : Frontend.Parser.function_publication option;
  source_aggregate : Frontend.Parser.aggregate_publication option;
}

type namespace = {
  table : Symbol_table.t;
  scope : Symbol_table.scope;
  owner : unit ref;
  mutable source_globals : publication list;
  mutable source_aggregates : publication list;
}

let scope (collection : t) = collection.scope
let entries collection = collection.entries
let entry_symbol (entry : entry) = entry.symbol
let entry_aggregate_identity (entry : entry) = entry.aggregate_identity
let entry_kind entry = entry.declaration_kind
let entry_item_index entry = entry.item_index
let entry_declarator_index entry = entry.declarator_index

let entry_matches_function_source entry
    (prototype : Frontend.Ast.function_prototype) =
  match entry.function_source with
  | Some (Collected_prototype source) -> source == prototype
  | Some (Published_function source) ->
      let header = source.function_header in
      source.function_name == prototype.name
      && header.modifiers == prototype.modifiers
      && Option.fold ~none:false ~some:(( == ) prototype.binding) header.binding
      && header.type_specifier == prototype.return_type
      && source.function_pointer_layers == prototype.return_pointer_layers
      && source.function_opening_parenthesis == prototype.opening_parenthesis
  | None -> false

let declaration_kind_name = function
  | Aggregate_forward -> "aggregate-forward"
  | Aggregate_definition -> "aggregate-definition"
  | Aggregate_attached_global -> "aggregate-attached-global"
  | Global_variable -> "global-variable"
  | Function_prototype -> "function-prototype"
  | Function_definition -> "function-definition"

let make_declaration_internal ~name ~declaration_kind ~origin ~item_index
    ?declarator_index ?source_prototype () =
  if String.equal name "" then Error "semantic declaration name cannot be empty"
  else if item_index < 0 then
    Error "semantic declaration item index cannot be negative"
  else
    match declarator_index with
    | Some index when index < 0 ->
        Error "semantic declarator index cannot be negative"
    | None | Some _ ->
        Ok
          {
            name;
            declaration_kind;
            origin;
            item_index;
            declarator_index;
            source_prototype;
          }

let make_declaration ~name ~declaration_kind ~origin ~item_index
    ?declarator_index () =
  make_declaration_internal ~name ~declaration_kind ~origin ~item_index
    ?declarator_index ()

let make_function_prototype_declaration
    ~(prototype : Frontend.Ast.function_prototype) ~item_index =
  let location = prototype.name.location in
  let origin =
    Symbol.Source_location
      {
        span = location.span;
        source_segments = location.source_segments;
        generated_from = location.generated_from;
        defined_at = location.defined_at;
      }
  in
  make_declaration_internal ~name:prototype.name.spelling
    ~declaration_kind:Function_prototype ~origin ~item_index
    ~source_prototype:prototype ()

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
          aggregate_identity = None;
          declaration_kind = declaration.declaration_kind;
          item_index = declaration.item_index;
          declarator_index = declaration.declarator_index;
          function_source =
            Option.map
              (fun prototype -> Collected_prototype prototype)
              declaration.source_prototype;
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
  |> Result.map (fun scope ->
      {
        table;
        scope;
        owner = ref ();
        source_globals = [];
        source_aggregates = [];
      })

let namespace_scope (namespace : namespace) = namespace.scope
let publication_symbol (publication : publication) = publication.symbol

let publication_aggregate_identity (publication : publication) =
  publication.aggregate_identity

let publication_source_global publication = publication.source_global
let publication_source_function publication = publication.source_function
let publication_source_aggregate publication = publication.source_aggregate

let namespace_owns_publication (namespace : namespace)
    (publication : publication) =
  publication.owner == namespace.owner
  && Symbol_table.owns_symbol namespace.table publication.symbol
  && Option.fold ~none:true
       ~some:(fun identity ->
         Symbol_table.owns_symbol namespace.table identity
         && Symbol.Scope_id.equal (Symbol.scope_id identity)
              (Symbol_table.scope_id namespace.scope))
       publication.aggregate_identity

let namespace_owns_table (namespace : namespace) table =
  namespace.table == table

let source_global_for_symbol (namespace : namespace) symbol =
  List.find_opt
    (fun (publication : publication) -> publication.symbol == symbol)
    namespace.source_globals

let publish (namespace : namespace) ~name ~kind ~origin =
  match kind with
  | Symbol.Global_variable | Symbol.Function | Symbol.Aggregate_type ->
      Symbol_table.add namespace.table ~scope:namespace.scope ~name ~kind
        ~origin
      |> Result.map (fun symbol ->
          {
            owner = namespace.owner;
            symbol;
            aggregate_identity =
              (if Symbol.equal_kind kind Symbol.Aggregate_type then Some symbol
               else None);
            source_global = None;
            source_function = None;
            source_aggregate = None;
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
      let publication = { publication with source_global = Some source } in
      namespace.source_globals <- publication :: namespace.source_globals;
      publication)

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

let publish_aggregate namespace (source : Frontend.Parser.aggregate_publication)
    =
  let location = source.aggregate_name.location in
  let origin =
    Symbol.Source_location
      {
        span = location.span;
        source_segments = location.source_segments;
        generated_from = location.generated_from;
        defined_at = location.defined_at;
      }
  in
  if not (Frontend.Parser.aggregate_publication_is_current source) then
    Error "aggregate publication requires its original callback"
  else
    publish namespace ~name:source.aggregate_name.spelling
      ~kind:Symbol.Aggregate_type ~origin
    |> Result.map (fun publication ->
        let prior =
          Option.bind source.aggregate_previous (fun previous_entry ->
              List.find_opt
                (fun candidate ->
                  match candidate.source_aggregate with
                  | Some previous ->
                      previous.aggregate_entry == previous_entry
                      && previous.aggregate_environment
                         == source.aggregate_environment
                  | None -> false)
                namespace.source_aggregates)
        in
        let prior_is_forward =
          Option.fold ~none:false
            ~some:(fun candidate ->
              match candidate.source_aggregate with
              | Some previous -> (
                  (* The parser has disjoint aggregate grammars: only its extern
                     forward path supplies an aggregate binding; definitions
                     publish with [None]. The exact previous frontend entry was
                     captured before this declaration was inserted. *)
                  match previous.aggregate_header.binding with
                  | Some binding -> binding.kind = Frontend.Ast.Extern
                  | None -> false)
              | None -> false)
            prior
        in
        let aggregate_identity =
          if Option.is_none source.aggregate_header.binding && prior_is_forward
          then Option.bind prior (fun candidate -> candidate.aggregate_identity)
          else publication.aggregate_identity
        in
        let publication =
          {
            publication with
            aggregate_identity;
            source_aggregate = Some source;
          }
        in
        namespace.source_aggregates <-
          publication :: namespace.source_aggregates;
        publication)

let view (namespace : namespace) publications =
  let rec validate previous seen entries_rev = function
    | [] -> Ok { scope = namespace.scope; entries = List.rev entries_rev }
    | ((publication : publication), (declaration : declaration)) :: rest ->
        let symbol = publication.symbol in
        let position = (declaration.item_index, declaration.declarator_index) in
        let function_source =
          match declaration.declaration_kind with
          | Function_prototype | Function_definition ->
              Option.map
                (fun source -> Published_function source)
                publication.source_function
          | Aggregate_forward
          | Aggregate_definition
          | Aggregate_attached_global
          | Global_variable -> None
        in
        let aggregate_identity_valid =
          match
            (declaration.declaration_kind, publication.aggregate_identity)
          with
          | (Aggregate_forward | Aggregate_definition), Some identity ->
              Symbol_table.owns_symbol namespace.table identity
              && Symbol.equal_kind (Symbol.kind identity) Symbol.Aggregate_type
              && String.equal (Symbol.name identity) declaration.name
              && Symbol.Scope_id.equal (Symbol.scope_id identity)
                   (Symbol_table.scope_id namespace.scope)
          | (Aggregate_forward | Aggregate_definition), None -> false
          | ( ( Aggregate_attached_global
              | Global_variable
              | Function_prototype
              | Function_definition ),
              None ) -> true
          | ( ( Aggregate_attached_global
              | Global_variable
              | Function_prototype
              | Function_definition ),
              Some _ ) -> false
        in
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
        else if not aggregate_identity_valid then
          Error
            "semantic declaration publication has an invalid aggregate identity"
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
              aggregate_identity = publication.aggregate_identity;
              declaration_kind = declaration.declaration_kind;
              item_index = declaration.item_index;
              declarator_index = declaration.declarator_index;
              function_source;
            }
          in
          validate (Some position) (publication :: seen) (entry :: entries_rev)
            rest
  in
  validate None [] [] publications
