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

type ast_global = {
  declaration_kind : Sema.Declaration_collection.declaration_kind;
  item_index : int;
  declarator_index : int option;
  name : Frontend.Ast.identifier;
  binding : Frontend.Ast.declaration_binding option;
}

let declarator_ast ~declaration_kind ~item_index ~binding declarator_index
    (declarator : Frontend.Ast.global_declarator) =
  {
    declaration_kind;
    item_index;
    declarator_index = Some declarator_index;
    name = declarator.name;
    binding;
  }

let ast_globals (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item ->
      match item with
      | Frontend.Ast.Global_variable variable ->
          [
            {
              declaration_kind = Sema.Declaration_collection.Global_variable;
              item_index;
              declarator_index = None;
              name = variable.name;
              binding = variable.binding;
            };
          ]
      | Frontend.Ast.Global_declaration declaration ->
          declaration.declarators
          |> List.mapi
               (declarator_ast
                  ~declaration_kind:Sema.Declaration_collection.Global_variable
                  ~item_index ~binding:declaration.binding)
      | Frontend.Ast.Aggregate_definition definition ->
          definition.attached_declarators
          |> List.mapi
               (declarator_ast
                  ~declaration_kind:
                    Sema.Declaration_collection.Aggregate_attached_global
                  ~item_index ~binding:None)
      | Frontend.Ast.Aggregate_forward_declaration _
      | Frontend.Ast.Function_prototype _
      | Frontend.Ast.Function_definition _
      | Frontend.Ast.Top_level_statement _ -> [])
  |> List.concat

let global_entries declarations =
  Sema.Declaration_collection.entries declarations
  |> List.filter (fun entry ->
      match Sema.Declaration_collection.entry_kind entry with
      | Sema.Declaration_collection.Aggregate_attached_global
      | Sema.Declaration_collection.Global_variable -> true
      | Sema.Declaration_collection.Aggregate_forward
      | Sema.Declaration_collection.Aggregate_definition
      | Sema.Declaration_collection.Function_prototype
      | Sema.Declaration_collection.Function_definition -> false)

let binding_target = function
  | Frontend.Ast.No_binding_target ->
      Ok Sema.Global_resolution.no_binding_target
  | Frontend.Ast.Symbol_binding_target identifier ->
      Sema.Global_resolution.make_symbol_binding_target
        ~name:identifier.spelling
        ~origin:(origin identifier.location)
  | Frontend.Ast.Expression_binding_target expression ->
      Ok
        (Sema.Global_resolution.make_expression_binding_target
           ~origin:(origin (Frontend.Ast.expression_location expression)))

let binding_kind = function
  | Frontend.Ast.Extern -> Sema.Global_resolution.Extern_binding
  | Frontend.Ast.Import -> Sema.Global_resolution.Import_binding
  | Frontend.Ast.Intern -> Sema.Global_resolution.Intern_binding

let source_binding (binding : Frontend.Ast.declaration_binding) =
  match binding_target binding.target with
  | Error _ as error -> error
  | Ok target ->
      Sema.Global_resolution.make_source_binding
        ~kind:(binding_kind binding.kind)
        ~spelling:binding.spelling ~origin:(origin binding.location) ~target

let validate_fact ~table ~compiler_option_mask entry global (ast : ast_global) =
  let entry_symbol = Sema.Declaration_collection.entry_symbol entry in
  let global_symbol = Sema.Global_type_resolution.global_symbol global in
  if not (Sema.Symbol_table.owns_symbol table entry_symbol) then
    Error "semantic global record belongs to a different symbol table"
  else if Sema.Declaration_collection.entry_kind entry <> ast.declaration_kind
  then Error "semantic global record does not match the AST kind"
  else if Sema.Declaration_collection.entry_item_index entry <> ast.item_index
  then Error "semantic global record does not match the AST order"
  else if
    Sema.Declaration_collection.entry_declarator_index entry
    <> ast.declarator_index
  then Error "semantic global record has the wrong declarator index"
  else if Sema.Global_type_resolution.global_item_index global <> ast.item_index
  then Error "semantic global record type does not match the AST order"
  else if
    Sema.Global_type_resolution.global_declarator_index global
    <> ast.declarator_index
  then Error "semantic global record type has the wrong declarator index"
  else if not (same_symbol entry_symbol global_symbol) then
    Error "semantic global record type has the wrong declaration symbol"
  else if not (String.equal (Sema.Symbol.name entry_symbol) ast.name.spelling)
  then Error "semantic global record does not match the AST name"
  else if Sema.Symbol.origin entry_symbol <> origin ast.name.location then
    Error "semantic global record does not match the AST origin"
  else
    match ast.binding with
    | None ->
        Sema.Global_resolution.make_declaration ~compiler_option_mask ~global ()
    | Some binding -> (
        match source_binding binding with
        | Error _ as error -> error
        | Ok binding ->
            Sema.Global_resolution.make_declaration ~compiler_option_mask
              ~global ~binding ())

let declaration_facts ~table ~compiler_option_mask ~declarations ~globals
    module_ =
  let entries = global_entries declarations in
  let globals = Sema.Global_type_resolution.globals globals in
  let ast = ast_globals module_ in
  let rec pair facts_rev entries globals ast =
    match (entries, globals, ast) with
    | [], [], [] -> Ok (List.rev facts_rev)
    | entry :: entry_rest, global :: global_rest, ast :: ast_rest -> (
        match validate_fact ~table ~compiler_option_mask entry global ast with
        | Error _ as error -> error
        | Ok fact -> pair (fact :: facts_rev) entry_rest global_rest ast_rest)
    | [], _, _ | _, [], _ | _, _, [] ->
        Error "semantic global records do not match the global declarations"
  in
  pair [] entries globals ast

let semantic_mode = function
  | Frontend.Preprocessor.Jit -> Sema.Global_resolution.Jit
  | Frontend.Preprocessor.Aot -> Sema.Global_resolution.Aot

let resolve ?(compiler_option_mask = Sema.Compiler_option.initial_mask) ~table
    ~declarations ~globals ~compilation_mode module_ =
  let parent = Sema.Declaration_collection.scope declarations in
  if not (Sema.Symbol_table.owns_scope table parent) then
    Error "semantic global record module belongs to a different symbol table"
  else if Sema.Symbol_table.scope_kind parent <> Sema.Symbol_table.Module then
    Error "semantic global records require a module declaration collection"
  else
    match
      declaration_facts ~table ~compiler_option_mask ~declarations ~globals
        module_
    with
    | Error _ as error -> error
    | Ok facts ->
        Sema.Global_resolution.resolve ~table ~parent
          ~compilation_mode:(semantic_mode compilation_mode)
          facts
