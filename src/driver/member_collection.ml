let origin (identifier : Frontend.Ast.identifier) =
  let location = identifier.location in
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let member_fact ~member_path declarator_index
    (declarator : Frontend.Ast.aggregate_member_declarator) =
  Sema.Member_collection.make_member ~name:declarator.member_name.spelling
    ~origin:(origin declarator.member_name)
    ~member_path ~declarator_index

let declarator_facts ~member_path declarators =
  let rec collect declarator_index facts_rev = function
    | [] -> Ok (List.rev facts_rev)
    | declarator :: rest -> (
        match member_fact ~member_path declarator_index declarator with
        | Error _ as error -> error
        | Ok fact -> collect (declarator_index + 1) (fact :: facts_rev) rest)
  in
  collect 0 [] declarators

let rec member_facts ~path_prefix members =
  let rec collect member_index facts_rev = function
    | [] -> Ok (List.rev facts_rev |> List.concat)
    | member :: rest -> (
        let member_path = path_prefix @ [ member_index ] in
        let facts =
          match member with
          | Frontend.Ast.Aggregate_member_declaration declaration ->
              declarator_facts ~member_path declaration.member_declarators
          | Frontend.Ast.Anonymous_union_member anonymous_union ->
              member_facts ~path_prefix:member_path
                anonymous_union.anonymous_union_members
          | Frontend.Ast.Aggregate_offset_directive _
          | Frontend.Ast.Empty_aggregate_member _ -> Ok []
        in
        match facts with
        | Error _ as error -> error
        | Ok facts -> collect (member_index + 1) (facts :: facts_rev) rest)
  in
  collect 0 [] members

let aggregate_definition_entries declarations =
  Sema.Declaration_collection.entries declarations
  |> List.filter (fun entry ->
      Sema.Declaration_collection.entry_kind entry
      = Sema.Declaration_collection.Aggregate_definition)

let aggregate_definitions (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item -> (item_index, item))
  |> List.filter_map (function
    | item_index, Frontend.Ast.Aggregate_definition definition ->
        Some (item_index, definition)
    | _ -> None)

let aggregate_fact entry
    (item_index, (definition : Frontend.Ast.aggregate_definition)) =
  let entry_item_index = Sema.Declaration_collection.entry_item_index entry in
  let symbol = Sema.Declaration_collection.entry_symbol entry in
  if entry_item_index <> item_index then
    Error "semantic aggregate declaration does not match the AST item order"
  else if not (String.equal (Sema.Symbol.name symbol) definition.name.spelling)
  then Error "semantic aggregate declaration does not match the AST name"
  else if Sema.Symbol.origin symbol <> origin definition.name then
    Error "semantic aggregate declaration does not match the AST origin"
  else
    match member_facts ~path_prefix:[] definition.members with
    | Error _ as error -> error
    | Ok members ->
        Sema.Member_collection.make_aggregate ~symbol ~item_index members

let aggregate_facts declarations module_ =
  let entries = aggregate_definition_entries declarations in
  let definitions = aggregate_definitions module_ in
  let rec pair facts_rev entries definitions =
    match (entries, definitions) with
    | [], [] -> Ok (List.rev facts_rev)
    | entry :: entry_rest, definition :: definition_rest -> (
        match aggregate_fact entry definition with
        | Error _ as error -> error
        | Ok fact -> pair (fact :: facts_rev) entry_rest definition_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "semantic aggregate declarations do not match the AST"
  in
  pair [] entries definitions

let collect ~table ~declarations module_ =
  match aggregate_facts declarations module_ with
  | Error _ as error -> error
  | Ok facts ->
      Sema.Member_collection.collect ~table
        ~parent:(Sema.Declaration_collection.scope declarations)
        facts
