let origin (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

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

type function_ast = {
  kind : Sema.Declaration_collection.declaration_kind;
  item_index : int;
  name : Frontend.Ast.identifier;
  body : Frontend.Ast.statement option;
}

let function_asts (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item -> (item_index, item))
  |> List.filter_map (function
    | item_index, Frontend.Ast.Function_prototype prototype ->
        Some
          {
            kind = Sema.Declaration_collection.Function_prototype;
            item_index;
            name = prototype.name;
            body = None;
          }
    | item_index, Frontend.Ast.Function_definition definition ->
        Some
          {
            kind = Sema.Declaration_collection.Function_definition;
            item_index;
            name = definition.name;
            body = definition.body;
          }
    | _ -> None)

type local_ast = {
  declaration_index : int;
  declarator_index : int;
  storage : Sema.Local_type_resolution.storage;
  name : Frontend.Ast.identifier;
  dimensions : Frontend.Ast.array_dimension list;
}

let local_declaration declaration_index
    (declaration : Frontend.Ast.local_declaration) =
  let storage =
    match declaration.local_storage with
    | Frontend.Ast.Automatic_local -> Sema.Local_type_resolution.Automatic
    | Frontend.Ast.Static_local -> Sema.Local_type_resolution.Static
  in
  declaration.local_declarators
  |> List.mapi (fun declarator_index declarator ->
      {
        declaration_index;
        declarator_index;
        storage;
        name = declarator.Frontend.Ast.local_name;
        dimensions = declarator.local_array_dimensions;
      })

let append_results first second =
  let first_locals, next_index = first in
  let second_locals, next_index = second next_index in
  (first_locals @ second_locals, next_index)

let rec statement_locals declaration_index = function
  | Frontend.Ast.Local_declaration_statement declaration ->
      (local_declaration declaration_index declaration, declaration_index + 1)
  | Frontend.Ast.Block_statement block ->
      statements_locals declaration_index block.block_statements
  | Frontend.Ast.Do_while_statement do_while ->
      statement_locals declaration_index do_while.do_body
  | Frontend.Ast.For_statement for_ ->
      append_results (statement_locals declaration_index for_.for_initializer)
        (fun declaration_index ->
          append_results
            (match for_.for_update with
            | None -> ([], declaration_index)
            | Some update -> statement_locals declaration_index update)
            (fun declaration_index ->
              statement_locals declaration_index for_.for_body))
  | Frontend.Ast.If_statement if_ ->
      append_results (statement_locals declaration_index if_.if_then_branch)
        (fun declaration_index ->
          match if_.if_else_clause with
          | None -> ([], declaration_index)
          | Some else_ -> statement_locals declaration_index else_.else_branch)
  | Frontend.Ast.Lock_statement lock ->
      statement_locals declaration_index lock.lock_body
  | Frontend.Ast.Sequence_statement sequence ->
      sequence_locals declaration_index sequence.sequence_elements
  | Frontend.Ast.Switch_statement switch ->
      switch_locals declaration_index switch.switch_elements
  | Frontend.Ast.Try_catch_statement try_catch ->
      append_results (statement_locals declaration_index try_catch.try_body)
        (fun declaration_index ->
          statement_locals declaration_index try_catch.catch_body)
  | Frontend.Ast.While_statement while_ ->
      statement_locals declaration_index while_.while_body
  | Frontend.Ast.Assembly_block_statement _
  | Frontend.Ast.Inline_assembly_statement _
  | Frontend.Ast.Break_statement _
  | Frontend.Ast.Empty_statement _
  | Frontend.Ast.Expression_statement _
  | Frontend.Ast.Goto_statement _
  | Frontend.Ast.Implicit_output_statement _
  | Frontend.Ast.Label_statement _
  | Frontend.Ast.No_warn_statement _
  | Frontend.Ast.Return_statement _ -> ([], declaration_index)

and statements_locals declaration_index statements =
  let rec collect locals_rev declaration_index = function
    | [] -> (List.rev locals_rev |> List.concat, declaration_index)
    | statement :: rest ->
        let locals, next_index = statement_locals declaration_index statement in
        collect (locals :: locals_rev) next_index rest
  in
  collect [] declaration_index statements

and sequence_locals declaration_index elements =
  elements
  |> List.map (fun (element : Frontend.Ast.statement_sequence_element) ->
      element.sequence_statement)
  |> statements_locals declaration_index

and switch_locals declaration_index elements =
  let rec collect locals_rev declaration_index = function
    | [] -> (List.rev locals_rev |> List.concat, declaration_index)
    | element :: rest ->
        let locals, next_index =
          match element with
          | Frontend.Ast.Switch_statement_element statement ->
              statement_locals declaration_index statement
          | Frontend.Ast.Switch_subswitch_element subswitch ->
              switch_locals declaration_index subswitch.subswitch_elements
          | Frontend.Ast.Switch_case_element _
          | Frontend.Ast.Switch_default_element _ -> ([], declaration_index)
        in
        collect (locals :: locals_rev) next_index rest
  in
  collect [] declaration_index elements

let closed_expression ast = Sema.Closed_layout_expression.of_ast ast

let dimension_expression (dimension : Frontend.Ast.array_dimension) =
  match dimension.dimension_expression with
  | None -> Sema.Function_frame_layout.Empty_dimension
  | Some ast ->
      Sema.Function_frame_layout.Closed_expression (closed_expression ast)

let validate_dimension expected_index semantic
    (ast : Frontend.Ast.array_dimension) =
  let ast_origin = origin ast.location in
  if Sema.Local_type_resolution.array_dimension_index semantic <> expected_index
  then Error "function frame local dimensions are outside source order"
  else if
    Sema.Local_type_resolution.array_dimension_origin semantic <> ast_origin
  then Error "function frame local dimension has the wrong source span"
  else if
    Sema.Local_type_resolution.array_dimension_opening_origin semantic
    <> origin ast.opening_bracket
  then Error "function frame local dimension has the wrong opening bracket"
  else if
    Sema.Local_type_resolution.array_dimension_expression_origin semantic
    <> Option.map
         (fun expression ->
           origin (Frontend.Ast.expression_location expression))
         ast.dimension_expression
  then Error "function frame local dimension has the wrong expression span"
  else if
    match
      Sema.Local_type_resolution.array_dimension_source_expression semantic
    with
    | None -> false
    | Some original -> (
        match ast.dimension_expression with
        | Some expression -> expression != original
        | None -> true)
  then
    Error "function frame local dimension has a substituted source expression"
  else if
    Sema.Local_type_resolution.array_dimension_closing_origin semantic
    <> origin ast.closing_bracket
  then Error "function frame local dimension has the wrong closing bracket"
  else
    Ok
      {
        Sema.Function_frame_layout.dimension = semantic;
        expression_origin =
          Option.map
            (fun expression ->
              origin (Frontend.Ast.expression_location expression))
            ast.dimension_expression;
        expression = dimension_expression ast;
      }

let dimension_inputs semantic ast =
  let rec pair index inputs_rev semantic ast =
    match (semantic, ast) with
    | [], [] -> Ok (List.rev inputs_rev)
    | semantic :: semantic_rest, ast :: ast_rest -> (
        match validate_dimension index semantic ast with
        | Error _ as error -> error
        | Ok input ->
            pair (index + 1) (input :: inputs_rev) semantic_rest ast_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "function frame local dimensions do not match the AST"
  in
  pair 0 [] semantic ast

let validate_local semantic ast =
  let symbol = Sema.Local_type_resolution.local_symbol semantic in
  if
    Sema.Local_type_resolution.local_declaration_index semantic
    <> ast.declaration_index
  then Error "function frame local has the wrong declaration position"
  else if
    Sema.Local_type_resolution.local_declarator_index semantic
    <> ast.declarator_index
  then Error "function frame local has the wrong declarator position"
  else if Sema.Local_type_resolution.local_storage semantic <> ast.storage then
    Error "function frame local has the wrong storage class"
  else if not (String.equal (Sema.Symbol.name symbol) ast.name.spelling) then
    Error "function frame local has the wrong name"
  else if Sema.Symbol.origin symbol <> origin ast.name.location then
    Error "function frame local has the wrong source origin"
  else
    match
      dimension_inputs
        (Sema.Local_type_resolution.local_array_dimensions semantic)
        ast.dimensions
    with
    | Error _ as error -> error
    | Ok dimensions ->
        Ok { Sema.Function_frame_layout.local = semantic; dimensions }

let local_inputs local_function body =
  let semantic = Sema.Local_type_resolution.function_locals local_function in
  let ast =
    match body with
    | None -> []
    | Some statement -> statement_locals 0 statement |> fst
  in
  let rec pair inputs_rev semantic ast =
    match (semantic, ast) with
    | [], [] -> Ok (List.rev inputs_rev)
    | semantic :: semantic_rest, ast :: ast_rest -> (
        match validate_local semantic ast with
        | Error _ as error -> error
        | Ok input -> pair (input :: inputs_rev) semantic_rest ast_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "function frame resolved locals do not match the AST"
  in
  pair [] semantic ast

let validate_function ~table ~scope declaration indexed typed local ast =
  let declaration_symbol =
    Sema.Declaration_collection.entry_symbol declaration
  in
  let indexed_symbol = Sema.Function_binding_index.function_symbol indexed in
  let indexed_scope = Sema.Function_binding_index.function_scope indexed in
  let typed_symbol = Sema.Function_type_resolution.function_symbol typed in
  let typed_scope = Sema.Function_type_resolution.function_scope typed in
  let local_symbol = Sema.Local_type_resolution.function_symbol local in
  let local_scope = Sema.Local_type_resolution.function_scope local in
  if
    not
      (Sema.Symbol_table.owns_symbol table declaration_symbol
      && Sema.Symbol_table.owns_symbol table indexed_symbol
      && Sema.Symbol_table.owns_symbol table typed_symbol
      && Sema.Symbol_table.owns_symbol table local_symbol)
  then Error "function frame inputs use another symbol table"
  else if
    not
      (Sema.Symbol_table.owns_scope table indexed_scope
      && Sema.Symbol_table.owns_scope table typed_scope
      && Sema.Symbol_table.owns_scope table local_scope)
  then Error "function frame scopes use another symbol table"
  else if Sema.Declaration_collection.entry_kind declaration <> ast.kind then
    Error "function frame declaration does not match the AST kind"
  else if
    Sema.Declaration_collection.entry_item_index declaration <> ast.item_index
    || Sema.Function_binding_index.function_item_index indexed <> ast.item_index
    || Sema.Function_type_resolution.function_item_index typed <> ast.item_index
    || Sema.Local_type_resolution.function_item_index local <> ast.item_index
  then Error "function frame inputs do not match the AST order"
  else if Sema.Declaration_collection.entry_declarator_index declaration <> None
  then Error "function frame declaration has an unexpected declarator index"
  else if
    not (String.equal (Sema.Symbol.name declaration_symbol) ast.name.spelling)
  then Error "function frame declaration does not match the AST name"
  else if Sema.Symbol.origin declaration_symbol <> origin ast.name.location then
    Error "function frame declaration does not match the AST origin"
  else if
    indexed_symbol != declaration_symbol
    || typed_symbol != declaration_symbol
    || local_symbol != declaration_symbol
  then Error "function frame passes have different function identities"
  else if indexed_scope != typed_scope || indexed_scope != local_scope then
    Error "function frame passes have different function scopes"
  else if
    match Sema.Symbol_table.parent indexed_scope with
    | Some parent -> parent != scope
    | None -> true
  then Error "function frame scope does not belong to the module"
  else
    match ast.kind with
    | Sema.Declaration_collection.Function_prototype -> Ok None
    | Sema.Declaration_collection.Function_definition -> (
        match local_inputs local ast.body with
        | Error _ as error -> error
        | Ok locals ->
            Ok
              (Some
                 {
                   Sema.Function_frame_layout.indexed_function = indexed;
                   typed_function = typed;
                   local_function = local;
                   locals;
                 }))
    | Sema.Declaration_collection.Aggregate_forward
    | Sema.Declaration_collection.Aggregate_definition
    | Sema.Declaration_collection.Aggregate_attached_global
    | Sema.Declaration_collection.Global_variable ->
        Error "function frame input contains a nonfunction declaration"

let function_inputs ~table ~scope declarations indexed typed locals ast =
  let rec pair inputs_rev declarations indexed typed locals ast =
    match (declarations, indexed, typed, locals, ast) with
    | [], [], [], [], [] -> Ok (List.rev inputs_rev)
    | ( declaration :: declaration_rest,
        indexed :: indexed_rest,
        typed :: typed_rest,
        local :: local_rest,
        ast :: ast_rest ) -> (
        match
          validate_function ~table ~scope declaration indexed typed local ast
        with
        | Error _ as error -> error
        | Ok None ->
            pair inputs_rev declaration_rest indexed_rest typed_rest local_rest
              ast_rest
        | Ok (Some input) ->
            pair (input :: inputs_rev) declaration_rest indexed_rest typed_rest
              local_rest ast_rest)
    | [], _, _, _, _
    | _, [], _, _, _
    | _, _, [], _, _
    | _, _, _, [], _
    | _, _, _, _, [] ->
        Error "function frame pass results contain different function counts"
  in
  pair [] declarations indexed typed locals ast

let layout ~table ~declarations ~bindings ~function_types ~local_types
    ~aggregate_layouts module_ =
  let scope = Sema.Declaration_collection.scope declarations in
  let result =
    if not (Sema.Symbol_table.owns_scope table scope) then
      Error "function frame declarations belong to another symbol table"
    else if Sema.Symbol_table.scope_kind scope <> Sema.Symbol_table.Module then
      Error "function frames require a module declaration collection"
    else
      match
        function_inputs ~table ~scope
          (function_entries declarations)
          (Sema.Function_binding_index.functions bindings)
          (Sema.Function_type_resolution.functions function_types)
          (Sema.Local_type_resolution.functions local_types)
          (function_asts module_)
      with
      | Error _ as error -> error
      | Ok inputs ->
          Sema.Function_frame_layout.layout ~table ~parent:scope
            ~aggregate_layouts inputs
          |> Result.map_error Sema.Function_frame_layout.error_to_string
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0069: " ^ message)
    result
