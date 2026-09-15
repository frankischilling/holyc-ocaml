module Binding = Sema.Global_dimension_binding

let ( let* ) = Result.bind
let invalid message = Error ("HCSEMA0027: " ^ message)

let origin (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

type ast_global = {
  name : Frontend.Ast.identifier;
  item_index : int;
  declarator_index : int option;
  declarator_origin : Sema.Symbol.origin;
  dimensions : Frontend.Ast.array_dimension list;
}

let declarator_ast item_index declarator_index
    (declarator : Frontend.Ast.global_declarator) =
  {
    name = declarator.name;
    item_index;
    declarator_index = Some declarator_index;
    declarator_origin = origin declarator.location;
    dimensions = declarator.array_dimensions;
  }

let ast_globals (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index -> function
    | Frontend.Ast.Global_variable variable ->
        [
          {
            name = variable.name;
            item_index;
            declarator_index = None;
            declarator_origin = origin variable.location;
            dimensions = variable.array_dimensions;
          };
        ]
    | Frontend.Ast.Global_declaration declaration ->
        List.mapi (declarator_ast item_index) declaration.declarators
    | Frontend.Ast.Aggregate_definition definition ->
        List.mapi (declarator_ast item_index) definition.attached_declarators
    | Frontend.Ast.Aggregate_forward_declaration _
    | Frontend.Ast.Function_prototype _
    | Frontend.Ast.Function_definition _
    | Frontend.Ast.Top_level_statement _ -> [])
  |> List.concat

let dimension_inputs original ast =
  let rec loop index reversed original ast =
    match (original, ast) with
    | [], [] -> Ok (List.rev reversed)
    | dimension :: rest, (ast : Frontend.Ast.array_dimension) :: ast_rest ->
        let source_expression =
          Binding.dimension_source dimension
          |> Sema.Global_type_resolution.array_dimension_source_expression
        in
        let same_expression =
          match (source_expression, ast.dimension_expression) with
          | None, None -> true
          | Some source, Some expression -> source == expression
          | _ -> false
        in
        if
          (not same_expression)
          || Binding.dimension_index dimension <> index
          || Binding.dimension_origin dimension <> origin ast.location
          || Binding.dimension_opening_origin dimension
             <> origin ast.opening_bracket
          || Binding.dimension_closing_origin dimension
             <> origin ast.closing_bracket
        then
          invalid "global array layout dimension does not match the source AST"
        else
          let expression = ast.Frontend.Ast.dimension_expression in
          let input =
            {
              Sema.Global_array_layout.dimension;
              expression_origin =
                Option.map
                  (fun expression ->
                    origin (Frontend.Ast.expression_location expression))
                  expression;
              expression;
            }
          in
          loop (index + 1) (input :: reversed) rest ast_rest
    | _ -> invalid "global array layout dimension count does not match the AST"
  in
  loop 0 [] original ast

let inputs original ast =
  let rec loop reversed original ast =
    match (original, ast) with
    | [], [] -> Ok (List.rev reversed)
    | global :: rest, ast :: ast_rest ->
        let record = Binding.global_record global in
        if
          Sema.Symbol.name (Binding.global_symbol global) <> ast.name.spelling
          || Sema.Symbol.origin (Binding.global_symbol global)
             <> origin ast.name.location
          || Binding.global_item_index global <> ast.item_index
          || Binding.global_declarator_index global <> ast.declarator_index
        then
          invalid
            "global array layout declaration has a different name or source \
             position"
        else if
          record |> Sema.Global_resolution.global_record_global
          |> Sema.Global_type_resolution.global_declarator_origin
          <> ast.declarator_origin
        then invalid "global array layout declaration has the wrong AST origin"
        else
          let* dimensions =
            dimension_inputs (Binding.global_dimensions global) ast.dimensions
          in
          let input = { Sema.Global_array_layout.global; dimensions } in
          loop (input :: reversed) rest ast_rest
    | _ -> invalid "global array layout declarations do not match the AST"
  in
  loop [] original ast

let layout ~table ~bindings module_ =
  if not (Binding.owns_table bindings table) then
    invalid "global array layout bindings belong to another symbol table"
  else
    (* Binding already checked the original before-owner traversal. Require its
       retained source children instead of resolving them again after parsing. *)
    let* inputs = inputs (Binding.globals bindings) (ast_globals module_) in
    Sema.Global_array_layout.layout ~table ~bindings inputs
