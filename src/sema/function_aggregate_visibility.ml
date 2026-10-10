module Ast = Frontend.Ast

type function_ = {
  table : Symbol_table.t;
  parent : Symbol_table.scope;
  symbol : Symbol.t;
  item_index : int;
  occurrences : (Ast.expression * int) list;
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
  let matches =
    List.filter_map
      (fun (source, index) -> if source == wanted then Some index else None)
      value.occurrences
  in
  match matches with
  | first :: rest when List.for_all (( = ) first) rest -> Some first
  | _ -> None

let before_local value wanted =
  List.find_map
    (fun (source, index) -> if source == wanted then Some index else None)
    value.locals

let rec first_identifier = function
  | Ast.Identifier_expression identifier -> Some identifier
  | Ast.Parenthesized_expression value ->
      first_identifier value.grouped_expression
  | Ast.Prefix_expression value -> first_identifier value.prefix_operand
  | Ast.Postfix_expression value -> first_identifier value.postfix_operand
  | Ast.Postfix_cast_expression value -> first_identifier value.cast_operand
  | Ast.Member_expression value -> first_identifier value.member_base
  | Ast.Index_expression value -> (
      match first_identifier value.index_base with
      | Some _ as identifier -> identifier
      | None -> first_identifier value.index_value)
  | Ast.Binary_expression value -> (
      match first_identifier value.binary_left with
      | Some _ as identifier -> identifier
      | None -> first_identifier value.binary_right)
  | Ast.Call_expression value -> (
      match first_identifier value.call_callee with
      | Some _ as identifier -> identifier
      | None ->
          List.find_map
            (fun argument ->
              match argument.Ast.call_argument_value with
              | Ast.Omitted_call_argument -> None
              | Ast.Provided_call_argument expression ->
                  first_identifier expression)
            value.call_arguments)
  | _ -> None

let permits_aggregate (value : function_) ~source ~item_index ~symbol =
  if not (Symbol_table.owns_symbol value.table symbol) then false
  else
    match before_expression value source with
    | None -> false
    | Some frontier when item_index >= frontier -> false
    | Some _ when item_index > value.item_index -> true
    | Some _ ->
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
              (first_identifier source))
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
                let occurrences = ref [] in
                let locals = ref [] in
                let rec expression value =
                  occurrences := (value, !frontier) :: !occurrences;
                  match value with
                  | Ast.Parenthesized_expression group ->
                      expression group.grouped_expression
                  | Ast.Prefix_expression value ->
                      expression value.prefix_operand
                  | Ast.Postfix_expression value ->
                      expression value.postfix_operand
                  | Ast.Postfix_cast_expression value ->
                      expression value.cast_operand
                  | Ast.Binary_expression value ->
                      expression value.binary_left;
                      expression value.binary_right
                  | Ast.Call_expression value ->
                      expression value.call_callee;
                      List.iter
                        (fun argument ->
                          match argument.Ast.call_argument_value with
                          | Ast.Omitted_call_argument -> ()
                          | Ast.Provided_call_argument value -> expression value)
                        value.call_arguments
                  | Ast.Index_expression value ->
                      expression value.index_base;
                      expression value.index_value
                  | Ast.Member_expression value -> expression value.member_base
                  | _ -> ()
                in
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
                     occurrences = !occurrences;
                     locals = !locals;
                     aggregate_references;
                   }
                  :: reversed)
                  rest)
      | _ :: rest -> functions reversed rest
    in
    functions [] items
