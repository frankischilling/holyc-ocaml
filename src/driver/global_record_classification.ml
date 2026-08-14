type ast_record = {
  item_index : int;
  declarator_index : int option;
  name : Frontend.Ast.identifier;
  modifiers : Frontend.Ast.declaration_modifier list;
}

let declarator ~item_index ~modifiers declarator_index
    (declarator : Frontend.Ast.global_declarator) =
  {
    item_index;
    declarator_index = Some declarator_index;
    name = declarator.name;
    modifiers;
  }

let ast_records (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item ->
      match item with
      | Frontend.Ast.Global_variable variable ->
          [
            {
              item_index;
              declarator_index = None;
              name = variable.name;
              modifiers = variable.modifiers;
            };
          ]
      | Frontend.Ast.Global_declaration declaration ->
          declaration.declarators
          |> List.mapi (declarator ~item_index ~modifiers:declaration.modifiers)
      | Frontend.Ast.Aggregate_definition definition ->
          definition.attached_declarators
          |> List.mapi (declarator ~item_index ~modifiers:definition.modifiers)
      | Frontend.Ast.Aggregate_forward_declaration _
      | Frontend.Ast.Function_prototype _
      | Frontend.Ast.Function_definition _
      | Frontend.Ast.Top_level_statement _ -> [])
  |> List.concat

let flag_modifier = function
  | Frontend.Ast.Public -> Sema.Function_flag.Modifier.Public
  | Frontend.Ast.Static -> Sema.Function_flag.Modifier.Static
  | Frontend.Ast.Interrupt -> Sema.Function_flag.Modifier.Interrupt
  | Frontend.Ast.Has_error_code -> Sema.Function_flag.Modifier.Has_error_code
  | Frontend.Ast.Argument_pop -> Sema.Function_flag.Modifier.Argument_pop
  | Frontend.Ast.No_argument_pop -> Sema.Function_flag.Modifier.No_argument_pop

let staging_mask modifiers =
  List.fold_left
    (fun mask (modifier : Frontend.Ast.declaration_modifier) ->
      Sema.Function_flag.apply_modifier ~mask (flag_modifier modifier.kind))
    0L modifiers

let validate_pair record ast =
  let global = Sema.Global_resolution.global_record_global record in
  let symbol = Sema.Global_resolution.global_record_symbol record in
  if Sema.Global_type_resolution.global_item_index global <> ast.item_index then
    Error "global record classification does not match the AST item"
  else if
    Sema.Global_type_resolution.global_declarator_index global
    <> ast.declarator_index
  then Error "global record classification does not match the AST declarator"
  else if not (String.equal (Sema.Symbol.name symbol) ast.name.spelling) then
    Error "global record classification does not match the AST name"
  else
    let declaration = Sema.Global_resolution.global_record_declaration record in
    Ok
      ( staging_mask ast.modifiers,
        Sema.Global_resolution.declaration_compiler_option_mask declaration )

let classify ?compiler_option_mask ~resolution module_ =
  let records = Sema.Global_resolution.records resolution in
  let ast = ast_records module_ in
  let rec pair states records ast =
    match (records, ast) with
    | [], [] ->
        Sema.Global_record_classification.classify resolution (List.rev states)
    | record :: record_rest, ast_record :: ast_rest -> (
        match validate_pair record ast_record with
        | Error _ as error -> error
        | Ok (staging_mask, resolution_option_mask) ->
            let compiler_option_mask =
              Option.value compiler_option_mask ~default:resolution_option_mask
            in
            let state =
              Sema.Global_record_classification.make_record_state ~staging_mask
                ~compiler_option_mask
            in
            pair (state :: states) record_rest ast_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "global record classification does not match the AST records"
  in
  pair [] records ast
