type leaf = {
  index : int;
  path : int list;
  expression : Frontend.Ast.expression;
}

type tree = Scalar of leaf | Braced of tree list | Unbraced of tree list

type t = {
  source : Frontend.Ast.initial_value;
  tree_ : tree;
  leaves_ : leaf list;
}

let origin_of_location (location : Frontend.Ast.location) =
  Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let create source =
  let rec build index path = function
    | Frontend.Ast.Scalar_initializer expression ->
        let leaf = { index; path; expression } in
        (index + 1, Scalar leaf, [ leaf ])
    | Frontend.Ast.Braced_initializer group ->
        let next, children, leaves =
          elements index path group.initializer_elements
        in
        (next, Braced children, leaves)
    | Frontend.Ast.Unbraced_array_initializer group ->
        let next, children, leaves =
          elements index path group.unbraced_initializer_elements
        in
        (next, Unbraced children, leaves)
  and elements index path values =
    let next, children, leaves =
      values
      |> List.mapi (fun offset value -> (offset, value))
      |> List.fold_left
           (fun (index, children, leaves)
                (offset, (value : Frontend.Ast.initializer_element)) ->
             let next, child, child_leaves =
               build index (path @ [ offset ]) value.initializer_element_value
             in
             (next, child :: children, List.rev_append child_leaves leaves))
           (index, [], [])
    in
    (next, List.rev children, List.rev leaves)
  in
  let _, tree_, leaves_ = build 0 [] source in
  { source; tree_; leaves_ }

let source_ast source = source.source
let tree source = source.tree_
let leaves source = source.leaves_

let origin source =
  source.source |> Frontend.Ast.initial_value_location |> origin_of_location

let matches_ast source ast = source.source == ast
let owns_leaf source leaf = List.exists (( == ) leaf) source.leaves_
let leaf_index leaf = leaf.index
let leaf_path leaf = leaf.path
let leaf_expression_ast leaf = leaf.expression

let leaf_origin leaf =
  leaf.expression |> Frontend.Ast.expression_location |> origin_of_location

let leaf_identifiers leaf =
  let rec expression reversed = function
    | Frontend.Ast.Identifier_expression identifier ->
        (identifier.spelling, origin_of_location identifier.location)
        :: reversed
    | Frontend.Ast.Parenthesized_expression grouped ->
        expression reversed grouped.grouped_expression
    | Frontend.Ast.Prefix_expression prefix ->
        expression reversed prefix.prefix_operand
    | Frontend.Ast.Postfix_expression postfix ->
        expression reversed postfix.postfix_operand
    | Frontend.Ast.Postfix_cast_expression cast ->
        expression reversed cast.cast_operand
    | Frontend.Ast.Binary_expression binary ->
        expression (expression reversed binary.binary_left) binary.binary_right
    | Frontend.Ast.Call_expression call ->
        List.fold_left
          (fun reversed (argument : Frontend.Ast.call_argument) ->
            match argument.call_argument_value with
            | Frontend.Ast.Omitted_call_argument -> reversed
            | Frontend.Ast.Provided_call_argument value ->
                expression reversed value)
          (expression reversed call.call_callee)
          call.call_arguments
    | Frontend.Ast.Index_expression index ->
        expression (expression reversed index.index_base) index.index_value
    | Frontend.Ast.Member_expression member ->
        expression reversed member.member_base
    | Frontend.Ast.Integer_literal _
    | Frontend.Ast.Float_literal _
    | Frontend.Ast.Character_literal _
    | Frontend.Ast.String_literal _
    | Frontend.Ast.Current_position_expression _
    | Frontend.Ast.Sizeof_expression _
    | Frontend.Ast.Offset_expression _
    | Frontend.Ast.Defined_expression _ -> reversed
  in
  List.rev (expression [] leaf.expression)
