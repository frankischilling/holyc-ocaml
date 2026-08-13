type ast_declaration = {
  item_index : int;
  name : Frontend.Ast.identifier;
  modifiers : Frontend.Ast.declaration_modifier list;
  import_name : string option;
  underscore_target : bool;
}

let starts_with_underscore name =
  String.length name > 0 && Char.equal name.[0] '_'

let prototype_import_name (prototype : Frontend.Ast.function_prototype) =
  match (prototype.binding.kind, prototype.binding.target) with
  | Frontend.Ast.Import, Frontend.Ast.No_binding_target ->
      Some prototype.name.spelling
  | Frontend.Ast.Import, Frontend.Ast.Symbol_binding_target target ->
      Some target.spelling
  | Frontend.Ast.Import, Frontend.Ast.Expression_binding_target _ -> None
  | Frontend.Ast.Extern, _ | Frontend.Ast.Intern, _ -> None

let prototype_has_underscore_target
    (prototype : Frontend.Ast.function_prototype) =
  match prototype.binding.target with
  | Frontend.Ast.Symbol_binding_target target ->
      starts_with_underscore target.spelling
  | Frontend.Ast.No_binding_target | Frontend.Ast.Expression_binding_target _ ->
      false

let ast_declarations (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item ->
      match item with
      | Frontend.Ast.Function_prototype prototype ->
          Some
            {
              item_index;
              name = prototype.name;
              modifiers = prototype.modifiers;
              import_name = prototype_import_name prototype;
              underscore_target = prototype_has_underscore_target prototype;
            }
      | Frontend.Ast.Function_definition definition ->
          Some
            {
              item_index;
              name = definition.name;
              modifiers = definition.modifiers;
              import_name = None;
              underscore_target = false;
            }
      | Frontend.Ast.Aggregate_forward_declaration _
      | Frontend.Ast.Aggregate_definition _
      | Frontend.Ast.Global_variable _
      | Frontend.Ast.Global_declaration _
      | Frontend.Ast.Top_level_statement _ -> None)
  |> List.filter_map Fun.id

let flag_modifier = function
  | Frontend.Ast.Public -> Sema.Function_flag.Modifier.Public
  | Frontend.Ast.Static -> Sema.Function_flag.Modifier.Static
  | Frontend.Ast.Interrupt -> Sema.Function_flag.Modifier.Interrupt
  | Frontend.Ast.Has_error_code -> Sema.Function_flag.Modifier.Has_error_code
  | Frontend.Ast.Argument_pop -> Sema.Function_flag.Modifier.Argument_pop
  | Frontend.Ast.No_argument_pop -> Sema.Function_flag.Modifier.No_argument_pop

let staging_mask ast =
  let mask =
    List.fold_left
      (fun mask (modifier : Frontend.Ast.declaration_modifier) ->
        Sema.Function_flag.apply_modifier ~mask (flag_modifier modifier.kind))
      0L ast.modifiers
  in
  if ast.underscore_target then
    Sema.Function_flag.apply_modifier ~mask
      Sema.Function_flag.Modifier.Underscore_name
  else mask

let validate_pair declaration ast =
  let site = Sema.Function_resolution.resolved_declaration_site declaration in
  let function_ = Sema.Function_resolution.declaration_site_function site in
  let symbol = Sema.Function_type_resolution.function_symbol function_ in
  if
    Sema.Function_type_resolution.function_item_index function_
    <> ast.item_index
  then Error "function record classification does not match the AST item"
  else if not (String.equal (Sema.Symbol.name symbol) ast.name.spelling) then
    Error "function record classification does not match the AST name"
  else Ok (staging_mask ast, ast.import_name)

let classify ?(compiler_option_mask = Sema.Compiler_option.initial_mask)
    ~resolution module_ =
  let declarations = Sema.Function_resolution.declarations resolution in
  let ast = ast_declarations module_ in
  let rec pair states declarations ast =
    match (declarations, ast) with
    | [], [] ->
        Sema.Function_record_classification.classify resolution
          (List.rev states)
    | declaration :: declaration_rest, ast_declaration :: ast_rest -> (
        match validate_pair declaration ast_declaration with
        | Error _ as error -> error
        | Ok (staging_mask, import_name) ->
            let state =
              Sema.Function_record_classification.make_declaration_state
                ~staging_mask ~compiler_option_mask ?import_name ()
            in
            pair (state :: states) declaration_rest ast_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error
          "function record classification does not match the AST declarations"
  in
  pair [] declarations ast
