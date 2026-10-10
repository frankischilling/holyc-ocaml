module Ast = Frontend.Ast

module Expressions = Hashtbl.Make (struct
  type t = Ast.expression

  let equal = ( == )

  let hash expression =
    let span = (Ast.expression_location expression).span in
    Hashtbl.hash (Common.Source_id.to_int span.source, span.start, span.stop)
end)

type occurrence = {
  before_item_index : int;
  first_identifier : Ast.identifier option;
}

type function_ = {
  table : Symbol_table.t;
  parent : Symbol_table.scope;
  symbol : Symbol.t;
  item_index : int;
  occurrences : occurrence option Expressions.t;
  locals : (Ast.identifier * int) list;
  aggregate_references :
    (Ast.identifier -> Compiler_record.aggregate_reference_visibility option)
    option;
}

type t = {
  table : Symbol_table.t;
  parent : Symbol_table.scope;
  functions : function_ list;
}

let origin = Initializer_source.origin_of_location
let function_item_index (value : function_) = value.item_index
let function_symbol (value : function_) = value.symbol

let owns (value : t) ~table ~parent =
  value.table == table && value.parent == parent

let function_owns (value : function_) ~table ~parent =
  value.table == table && value.parent == parent

let function_table (value : function_) = value.table

let find_function value ~symbol ~item_index =
  List.find_opt
    (fun function_ ->
      function_.symbol == symbol && function_.item_index = item_index)
    value.functions

let before_expression value wanted =
  Option.bind
    (Expressions.find_opt value.occurrences wanted)
    (Option.map (fun occurrence -> occurrence.before_item_index))

let before_local value wanted =
  List.find_map
    (fun (source, index) -> if source == wanted then Some index else None)
    value.locals

let permits_aggregate (value : function_) ~source ~item_index ~symbol =
  if not (Symbol_table.owns_symbol value.table symbol) then false
  else
    match Option.join (Expressions.find_opt value.occurrences source) with
    | None -> false
    | Some occurrence when item_index >= occurrence.before_item_index -> false
    | Some _ when item_index > value.item_index -> true
    | Some occurrence ->
        Option.fold ~none:false
          ~some:(fun resolve ->
            Option.fold ~none:false
              ~some:(fun identifier ->
                Option.fold ~none:false
                  ~some:
                    (Compiler_record.aggregate_reference_allows
                       ~table:value.table ~parent:value.parent ~identifier
                       ~symbol)
                  (resolve identifier))
              occurrence.first_identifier)
          value.aggregate_references

let create ~table ~declarations ~bodies ?aggregate_references module_ =
  let parent = Declaration_collection.scope declarations in
  let items = Ast.declaration_items module_ in
  let entries = Declaration_collection.entries declarations in
  let invalid message = Error ("function aggregate visibility: " ^ message) in
  if not (Symbol_table.owns_scope table parent) then
    invalid "declarations belong to another table"
  else
    let rec functions reversed = function
      | [] -> Ok { table; parent; functions = List.rev reversed }
      | (item_index, Ast.Function_definition definition) :: rest -> (
          let entry =
            List.find_opt
              (fun entry ->
                Declaration_collection.entry_kind entry
                = Declaration_collection.Function_definition
                && Declaration_collection.entry_item_index entry = item_index)
              entries
          in
          match entry with
          | None -> invalid "function has no original declaration entry"
          | Some entry ->
              let symbol = Declaration_collection.entry_symbol entry in
              if
                (not (Symbol_table.owns_symbol table symbol))
                || Symbol.name symbol <> definition.name.spelling
                || Symbol.origin symbol <> origin definition.name.location
                || not
                     (Symbol.Scope_id.equal (Symbol.scope_id symbol)
                        (Symbol_table.scope_id parent))
              then invalid "function identity disagrees with its source"
              else if
                not
                  (List.exists
                     (fun (header, body) ->
                       body == definition
                       && Result.is_ok
                            (Frontend.Parser.function_body_compiler_options
                               header definition))
                     bodies)
              then functions reversed rest
              else
                let frontier = ref (item_index + 1) in
                (* This private table is populated only during this body walk.
                   None permanently marks a node seen at conflicting frontiers. *)
                let occurrences = Expressions.create 64 in
                let locals = ref [] in
                let prefer first following =
                  match first with
                  | Some _ -> first
                  | None -> following
                in
                let rec visit_expression value =
                  let before_item_index = !frontier in
                  let first_identifier =
                    match value with
                    | Ast.Identifier_expression identifier -> Some identifier
                    | Ast.Parenthesized_expression group ->
                        visit_expression group.grouped_expression
                    | Ast.Prefix_expression value ->
                        visit_expression value.prefix_operand
                    | Ast.Postfix_expression value ->
                        visit_expression value.postfix_operand
                    | Ast.Postfix_cast_expression value ->
                        visit_expression value.cast_operand
                    | Ast.Binary_expression value ->
                        let left = visit_expression value.binary_left in
                        let right = visit_expression value.binary_right in
                        prefer left right
                    | Ast.Call_expression value ->
                        let callee = visit_expression value.call_callee in
                        List.fold_left
                          (fun first argument ->
                            match argument.Ast.call_argument_value with
                            | Ast.Omitted_call_argument -> first
                            | Ast.Provided_call_argument value ->
                                let following = visit_expression value in
                                prefer first following)
                          callee value.call_arguments
                    | Ast.Index_expression value ->
                        let base = visit_expression value.index_base in
                        let index = visit_expression value.index_value in
                        prefer base index
                    | Ast.Member_expression value ->
                        visit_expression value.member_base
                    | _ -> None
                  in
                  (match Expressions.find_opt occurrences value with
                  | None ->
                      Expressions.add occurrences value
                        (Some { before_item_index; first_identifier })
                  | Some (Some earlier)
                    when earlier.before_item_index = before_item_index -> ()
                  | Some _ -> Expressions.replace occurrences value None);
                  first_identifier
                in
                let expression value = ignore (visit_expression value) in
                let rec initial = function
                  | Ast.Scalar_initializer value -> expression value
                  | Ast.Braced_initializer group ->
                      List.iter
                        (fun item -> initial item.Ast.initializer_element_value)
                        group.initializer_elements
                  | Ast.Unbraced_array_initializer group ->
                      List.iter
                        (fun item -> initial item.Ast.initializer_element_value)
                        group.unbraced_initializer_elements
                in
                let rec statement value =
                  match value with
                  | Ast.Aggregate_declaration_statement value -> (
                      match
                        List.find_opt
                          (fun (_, item) ->
                            item == value.aggregate_statement_item)
                          items
                      with
                      | Some (index, _) -> frontier := index + 1
                      | None -> ())
                  | Ast.Local_declaration_statement value ->
                      List.iter
                        (fun local ->
                          locals := (local.Ast.local_name, !frontier) :: !locals;
                          Option.iter
                            (fun value ->
                              initial value.Ast.local_initializer_value)
                            local.local_initializer)
                        value.local_declarators
                  | Ast.Block_statement value ->
                      List.iter statement value.block_statements
                  | Ast.Do_while_statement value ->
                      statement value.do_body;
                      expression value.do_while_condition
                  | Ast.For_statement value ->
                      statement value.for_initializer;
                      expression value.for_condition;
                      Option.iter statement value.for_update;
                      statement value.for_body
                  | Ast.If_statement value ->
                      expression value.if_condition;
                      statement value.if_then_branch;
                      Option.iter
                        (fun value -> statement value.Ast.else_branch)
                        value.if_else_clause
                  | Ast.Lock_statement value -> statement value.lock_body
                  | Ast.Sequence_statement value ->
                      List.iter
                        (fun value -> statement value.Ast.sequence_statement)
                        value.sequence_elements
                  | Ast.Switch_statement value ->
                      expression value.switch_expression;
                      elements value.switch_elements
                  | Ast.Try_catch_statement value ->
                      statement value.try_body;
                      statement value.catch_body
                  | Ast.While_statement value ->
                      expression value.while_condition;
                      statement value.while_body
                  | Ast.Expression_statement value ->
                      expression value.expression_statement_expression
                  | Ast.Return_statement value ->
                      Option.iter expression value.return_value
                  | Ast.Implicit_output_statement value ->
                      (match value.fixed_argument with
                      | Ast.Marker_fixed_argument value
                      | Ast.Expression_fixed_argument value -> expression value
                      | Ast.Absent_fixed_argument -> ());
                      List.iter
                        (fun value -> expression value.Ast.value)
                        value.arguments
                  | _ -> ()
                and elements values =
                  List.iter
                    (function
                      | Ast.Switch_statement_element value -> statement value
                      | Ast.Switch_subswitch_element value ->
                          elements value.subswitch_elements
                      | Ast.Switch_case_element value -> (
                          match value.switch_case_pattern with
                          | Ast.Single_case value -> expression value
                          | Ast.Ranged_case value ->
                              expression value.case_range_start;
                              expression value.case_range_end
                          | _ -> ())
                      | Ast.Switch_default_element _ -> ())
                    values
                in
                Option.iter statement definition.body;
                functions
                  ({
                     table;
                     parent;
                     symbol;
                     item_index;
                     occurrences;
                     locals = !locals;
                     aggregate_references;
                   }
                  :: reversed)
                  rest)
      | _ :: rest -> functions reversed rest
    in
    functions [] items
