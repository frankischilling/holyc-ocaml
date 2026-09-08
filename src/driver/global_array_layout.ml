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
  declarator_origin : Sema.Symbol.origin;
  dimensions : Frontend.Ast.array_dimension list;
}

let declarator_ast (declarator : Frontend.Ast.global_declarator) =
  {
    declarator_origin = origin declarator.location;
    dimensions = declarator.array_dimensions;
  }

let ast_globals (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.concat_map (function
    | Frontend.Ast.Global_variable variable ->
        [
          {
            declarator_origin = origin variable.location;
            dimensions = variable.array_dimensions;
          };
        ]
    | Frontend.Ast.Global_declaration declaration ->
        List.map declarator_ast declaration.declarators
    | Frontend.Ast.Aggregate_definition definition ->
        List.map declarator_ast definition.attached_declarators
    | Frontend.Ast.Aggregate_forward_declaration _
    | Frontend.Ast.Function_prototype _
    | Frontend.Ast.Function_definition _
    | Frontend.Ast.Top_level_statement _ -> [])

let same_resolution left right =
  match (left, right) with
  | Binding.Module_binding left, Binding.Module_binding right -> left == right
  | Binding.Outer_binding left, Binding.Outer_binding right -> left == right
  | _ -> false

let rec same_occurrences actual expected =
  match (actual, expected) with
  | [], [] -> true
  | actual :: rest, expected :: tail ->
      Binding.occurrence_index actual = Binding.occurrence_index expected
      && Binding.occurrence_dimension_index actual
         = Binding.occurrence_dimension_index expected
      && Binding.occurrence_name actual = Binding.occurrence_name expected
      && Binding.occurrence_origin actual = Binding.occurrence_origin expected
      && same_resolution
           (Binding.occurrence_resolution actual)
           (Binding.occurrence_resolution expected)
      && same_occurrences rest tail
  | _ -> false

let dimension_inputs original verified ast =
  let rec loop reversed original verified ast =
    match (original, verified, ast) with
    | [], [], [] -> Ok (List.rev reversed)
    | dimension :: rest, checked :: checked_rest, ast :: ast_rest ->
        if
          Binding.dimension_source dimension != Binding.dimension_source checked
          || not
               (same_occurrences
                  (Binding.dimension_occurrences dimension)
                  (Binding.dimension_occurrences checked))
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
          loop (input :: reversed) rest checked_rest ast_rest
    | _ -> invalid "global array layout dimension count does not match the AST"
  in
  loop [] original verified ast

let inputs original verified ast =
  let rec loop reversed original verified ast =
    match (original, verified, ast) with
    | [], [], [] -> Ok (List.rev reversed)
    | global :: rest, checked :: checked_rest, ast :: ast_rest ->
        let record = Binding.global_record global in
        if
          record != Binding.global_record checked
          || Binding.global_publication global
             != Binding.global_publication checked
        then
          invalid "global array layout declaration has foreign binding evidence"
        else if
          record |> Sema.Global_resolution.global_record_global
          |> Sema.Global_type_resolution.global_declarator_origin
          <> ast.declarator_origin
        then invalid "global array layout declaration has the wrong AST origin"
        else
          let* dimensions =
            dimension_inputs
              (Binding.global_dimensions global)
              (Binding.global_dimensions checked)
              ast.dimensions
          in
          let input = { Sema.Global_array_layout.global; dimensions } in
          loop (input :: reversed) rest checked_rest ast_rest
    | _ -> invalid "global array layout declarations do not match the AST"
  in
  loop [] original verified ast

let layout ~table ~bindings module_ =
  if not (Binding.owns_table bindings table) then
    invalid "global array layout bindings belong to another symbol table"
  else
    (* Check the before-owner binding traversal, then require the exact source
       expression retained when its declaration was first collected. *)
    let* verified =
      Global_dimension_binding.resolve ~table
        ~environment:(Binding.environment bindings)
        ~expressions:(Binding.expressions bindings)
        ~globals:(Binding.source_globals bindings)
        module_
    in
    let* inputs =
      inputs (Binding.globals bindings) (Binding.globals verified)
        (ast_globals module_)
    in
    Sema.Global_array_layout.layout ~table ~bindings inputs
