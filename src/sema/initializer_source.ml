type leaf = {
  index : int;
  path : int list;
  expression : Frontend.Ast.expression;
  receipt : Frontend.Parser.completed_initializer_leaf option;
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
        let leaf = { index; path; expression; receipt = None } in
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

type pending = {
  start : Frontend.Parser.global_initializer_start;
  mutable leaves_rev : leaf list;
  mutable finished : bool;
}

let begin_parser start =
  if not (Frontend.Parser.initializer_start_is_current start) then
    Error "initializer start is outside its original parser callback"
  else Ok { start; leaves_rev = []; finished = false }

let observe_parser_leaf pending
    (receipt : Frontend.Parser.completed_initializer_leaf) =
  let index, predecessor =
    match pending.leaves_rev with
    | [] -> (0, None)
    | previous :: _ -> (previous.index + 1, previous.receipt)
  in
  let same_predecessor =
    match (predecessor, receipt.leaf_predecessor) with
    | None, None -> true
    | Some left, Some right -> left == right
    | _ -> false
  in
  if
    pending.finished
    || (not (Frontend.Parser.initializer_leaf_is_current receipt))
    || receipt.leaf_initializer != pending.start
    || receipt.leaf_index <> index
    || not same_predecessor
  then Error "initializer leaf is foreign, repeated, delayed or out of order"
  else
    match receipt.leaf_value with
    | Frontend.Ast.Scalar_initializer expression ->
        let leaf =
          {
            index;
            path = receipt.leaf_path;
            expression;
            receipt = Some receipt;
          }
        in
        pending.leaves_rev <- leaf :: pending.leaves_rev;
        Ok leaf
    | _ -> Error "initializer leaf is not an original scalar expression"

let parser_leaf pending receipt =
  match
    List.find_opt
      (fun leaf ->
        Option.fold ~none:false
          ~some:(fun saved -> saved == receipt)
          leaf.receipt)
      pending.leaves_rev
  with
  | Some leaf -> Ok leaf
  | None -> Error "initializer leaf has no original observed receipt"

let complete_parser pending event =
  let ( let* ) = Result.bind in
  let* initial =
    match event with
    | Frontend.Parser.Global_completed (owner, completed)
      when (not pending.finished) && owner == pending.start.initializer_owner
      -> (
        match completed.global_initial_value with
        | Some initial
          when initial.global_initializer_equals
               == pending.start.initializer_equals -> Ok initial
        | _ ->
            Error
              "initializer completion substituted its original equals location")
    | _ -> Error "initializer completion is foreign, repeated or out of order"
  in
  let rec build path value leaves =
    match value with
    | Frontend.Ast.Scalar_initializer expression -> (
        match leaves with
        | ({ receipt = Some receipt; _ } as leaf) :: rest
          when receipt.leaf_value == value
               && leaf.expression == expression
               && leaf.path = path -> Ok (Scalar leaf, rest)
        | _ ->
            Error
              "initializer completion substituted or omitted an original leaf")
    | Frontend.Ast.Braced_initializer group ->
        let* children, rest = elements path group.initializer_elements leaves in
        Ok (Braced children, rest)
    | Frontend.Ast.Unbraced_array_initializer group ->
        let* children, rest =
          elements path group.unbraced_initializer_elements leaves
        in
        Ok (Unbraced children, rest)
  and elements path values leaves =
    let rec loop index children leaves = function
      | [] -> Ok (List.rev children, leaves)
      | (value : Frontend.Ast.initializer_element) :: rest ->
          let* child, leaves =
            build (path @ [ index ]) value.initializer_element_value leaves
          in
          loop (index + 1) (child :: children) leaves rest
    in
    loop 0 [] leaves values
  in
  let leaves_ = List.rev pending.leaves_rev in
  let source = initial.global_initializer_value in
  let* tree_, rest = build [] source leaves_ in
  if rest <> [] then Error "initializer completion omitted reached leaves"
  else (
    pending.finished <- true;
    Ok { source; tree_; leaves_ })

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
let leaf_parser_receipt leaf = leaf.receipt

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
