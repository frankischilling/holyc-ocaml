let origin (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let same_symbol left right =
  Sema.Symbol.Id.equal (Sema.Symbol.id left) (Sema.Symbol.id right)

type ast_declaration = {
  declaration_kind : Sema.Declaration_collection.declaration_kind;
  identity_kind : Sema.Function_resolution.declaration_kind;
  item_index : int;
  name : Frontend.Ast.identifier;
}

let prototype_kind (binding : Frontend.Ast.declaration_binding) =
  match (binding.kind, binding.spelling, binding.target) with
  | Frontend.Ast.Extern, "extern", Frontend.Ast.No_binding_target ->
      Ok Sema.Function_resolution.Extern
  | Frontend.Ast.Extern, "_extern", Frontend.Ast.Symbol_binding_target _ ->
      Ok Sema.Function_resolution.Bound_extern
  | Frontend.Ast.Import, "import", Frontend.Ast.No_binding_target
  | Frontend.Ast.Import, "_import", Frontend.Ast.Symbol_binding_target _ ->
      Ok Sema.Function_resolution.Import
  | Frontend.Ast.Intern, "_intern", Frontend.Ast.Expression_binding_target _ ->
      Ok Sema.Function_resolution.Intern
  | Frontend.Ast.Extern, _, _ ->
      Error "semantic function extern binding has an inconsistent source shape"
  | Frontend.Ast.Import, _, _ ->
      Error "semantic function import binding has an inconsistent source shape"
  | Frontend.Ast.Intern, _, _ ->
      Error "semantic function intern binding has an inconsistent source shape"

let ast_declarations (module_ : Frontend.Ast.module_) =
  let rec collect item_index declarations_rev = function
    | [] -> Ok (List.rev declarations_rev)
    | Frontend.Ast.Function_prototype prototype :: rest -> (
        match prototype_kind prototype.binding with
        | Error _ as error -> error
        | Ok identity_kind ->
            collect (item_index + 1)
              ({
                 declaration_kind =
                   Sema.Declaration_collection.Function_prototype;
                 identity_kind;
                 item_index;
                 name = prototype.name;
               }
              :: declarations_rev)
              rest)
    | Frontend.Ast.Function_definition definition :: rest ->
        collect (item_index + 1)
          ({
             declaration_kind = Sema.Declaration_collection.Function_definition;
             identity_kind = Sema.Function_resolution.Definition;
             item_index;
             name = definition.name;
           }
          :: declarations_rev)
          rest
    | _ :: rest -> collect (item_index + 1) declarations_rev rest
  in
  collect 0 [] module_.items

let function_entries declarations =
  Sema.Declaration_collection.entries declarations
  |> List.filter (fun entry ->
      match Sema.Declaration_collection.entry_kind entry with
      | Sema.Declaration_collection.Function_prototype
      | Sema.Declaration_collection.Function_definition -> true
      | Sema.Declaration_collection.Aggregate_forward
      | Sema.Declaration_collection.Aggregate_definition
      | Sema.Declaration_collection.Aggregate_attached_global
      | Sema.Declaration_collection.Global_variable -> false)

let declaration_fact entry function_ ast =
  let entry_symbol = Sema.Declaration_collection.entry_symbol entry in
  let function_symbol =
    Sema.Function_type_resolution.function_symbol function_
  in
  let entry_item_index = Sema.Declaration_collection.entry_item_index entry in
  let function_item_index =
    Sema.Function_type_resolution.function_item_index function_
  in
  if Sema.Declaration_collection.entry_kind entry <> ast.declaration_kind then
    Error "semantic function identity declaration does not match the AST kind"
  else if entry_item_index <> ast.item_index then
    Error "semantic function identity declaration does not match the AST order"
  else if Sema.Declaration_collection.entry_declarator_index entry <> None then
    Error
      "semantic function identity declaration cannot have a declarator index"
  else if function_item_index <> ast.item_index then
    Error "semantic function identity type does not match the AST order"
  else if not (same_symbol entry_symbol function_symbol) then
    Error "semantic function identity type has the wrong declaration symbol"
  else if not (String.equal (Sema.Symbol.name entry_symbol) ast.name.spelling)
  then
    Error "semantic function identity declaration does not match the AST name"
  else if Sema.Symbol.origin entry_symbol <> origin ast.name.location then
    Error "semantic function identity declaration does not match the AST origin"
  else
    Sema.Function_resolution.make_declaration ~function_ ~kind:ast.identity_kind

let declaration_facts declarations functions module_ =
  match ast_declarations module_ with
  | Error _ as error -> error
  | Ok ast ->
      let entries = function_entries declarations in
      let functions = Sema.Function_type_resolution.functions functions in
      let rec pair facts_rev entries functions ast =
        match (entries, functions, ast) with
        | [], [], [] -> Ok (List.rev facts_rev)
        | entry :: entry_rest, function_ :: function_rest, ast :: ast_rest -> (
            match declaration_fact entry function_ ast with
            | Error _ as error -> error
            | Ok fact ->
                pair (fact :: facts_rev) entry_rest function_rest ast_rest)
        | [], _, _ | _, [], _ | _, _, [] ->
            Error
              "semantic function identities do not match the function \
               declarations"
      in
      pair [] entries functions ast

let semantic_mode = function
  | Frontend.Preprocessor.Jit -> Sema.Function_resolution.Jit
  | Frontend.Preprocessor.Aot -> Sema.Function_resolution.Aot

let resolve ~table ~declarations ~functions ~compilation_mode module_ =
  let parent = Sema.Declaration_collection.scope declarations in
  if not (Sema.Symbol_table.owns_scope table parent) then
    Error
      "semantic function identity module belongs to a different symbol table"
  else if Sema.Symbol_table.scope_kind parent <> Sema.Symbol_table.Module then
    Error "semantic function identities require a module declaration collection"
  else
    match declaration_facts declarations functions module_ with
    | Error _ as error -> error
    | Ok facts ->
        Sema.Function_resolution.resolve ~table ~parent
          ~compilation_mode:(semantic_mode compilation_mode)
          facts
