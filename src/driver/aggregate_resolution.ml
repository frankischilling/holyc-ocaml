let origin (identifier : Frontend.Ast.identifier) =
  let location = identifier.location in
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

type ast_declaration = {
  identifier : Frontend.Ast.identifier;
  declaration_kind : Sema.Declaration_collection.declaration_kind;
  aggregate_kind : Sema.Aggregate_resolution.aggregate_kind;
  item_index : int;
}

let aggregate_kind = function
  | Frontend.Ast.Class_aggregate -> Sema.Aggregate_resolution.Class
  | Frontend.Ast.Union_aggregate -> Sema.Aggregate_resolution.Union

let ast_declarations (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item -> (item_index, item))
  |> List.filter_map (function
    | item_index, Frontend.Ast.Aggregate_forward_declaration forward ->
        Some
          {
            identifier = forward.name;
            declaration_kind = Sema.Declaration_collection.Aggregate_forward;
            aggregate_kind = aggregate_kind forward.aggregate_kind;
            item_index;
          }
    | item_index, Frontend.Ast.Aggregate_definition definition ->
        Some
          {
            identifier = definition.name;
            declaration_kind = Sema.Declaration_collection.Aggregate_definition;
            aggregate_kind = aggregate_kind definition.aggregate_kind;
            item_index;
          }
    | _ -> None)

let aggregate_entries declarations =
  Sema.Declaration_collection.entries declarations
  |> List.filter (fun entry ->
      match Sema.Declaration_collection.entry_kind entry with
      | Sema.Declaration_collection.Aggregate_forward
      | Sema.Declaration_collection.Aggregate_definition -> true
      | Sema.Declaration_collection.Aggregate_attached_global
      | Sema.Declaration_collection.Global_variable
      | Sema.Declaration_collection.Function_prototype
      | Sema.Declaration_collection.Function_definition -> false)

let semantic_declaration_kind = function
  | Sema.Declaration_collection.Aggregate_forward ->
      Ok Sema.Aggregate_resolution.Forward
  | Sema.Declaration_collection.Aggregate_definition ->
      Ok Sema.Aggregate_resolution.Definition
  | Sema.Declaration_collection.Aggregate_attached_global
  | Sema.Declaration_collection.Global_variable
  | Sema.Declaration_collection.Function_prototype
  | Sema.Declaration_collection.Function_definition ->
      Error "semantic aggregate resolution received a nonaggregate declaration"

let declaration_fact entry ast =
  let entry_kind = Sema.Declaration_collection.entry_kind entry in
  let item_index = Sema.Declaration_collection.entry_item_index entry in
  let symbol = Sema.Declaration_collection.entry_symbol entry in
  if entry_kind <> ast.declaration_kind then
    Error "semantic aggregate declaration does not match the AST kind"
  else if item_index <> ast.item_index then
    Error "semantic aggregate declaration does not match the AST item order"
  else if Sema.Declaration_collection.entry_declarator_index entry <> None then
    Error "semantic aggregate declaration cannot have a declarator index"
  else if not (String.equal (Sema.Symbol.name symbol) ast.identifier.spelling)
  then Error "semantic aggregate declaration does not match the AST name"
  else if Sema.Symbol.origin symbol <> origin ast.identifier then
    Error "semantic aggregate declaration does not match the AST origin"
  else
    match semantic_declaration_kind entry_kind with
    | Error _ as error -> error
    | Ok declaration_kind ->
        Sema.Aggregate_resolution.make_declaration ~symbol ~declaration_kind
          ~aggregate_kind:ast.aggregate_kind ~item_index

let declaration_facts declarations module_ =
  let entries = aggregate_entries declarations in
  let ast = ast_declarations module_ in
  let rec pair facts_rev entries ast =
    match (entries, ast) with
    | [], [] -> Ok (List.rev facts_rev)
    | entry :: entry_rest, ast_declaration :: ast_rest -> (
        match declaration_fact entry ast_declaration with
        | Error _ as error -> error
        | Ok fact -> pair (fact :: facts_rev) entry_rest ast_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "semantic aggregate declarations do not match the AST"
  in
  pair [] entries ast

let resolve ~table ~declarations module_ =
  match declaration_facts declarations module_ with
  | Error _ as error -> error
  | Ok facts ->
      Sema.Aggregate_resolution.resolve ~table
        ~parent:(Sema.Declaration_collection.scope declarations)
        facts
