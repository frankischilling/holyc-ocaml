type switch_case_position = Single_case | Range_start | Range_end

type switch_case_pattern =
  | Implicit_case
  | Single_case_pattern
  | Ranged_case_pattern of { ellipsis_origin : Symbol.origin }

type root_role =
  | Expression_statement of { statement_index : int }
  | Implicit_output_fixed of {
      output_index : int;
      target : Function_call_resolution.implicit_output_target;
      source : Function_call_resolution.implicit_output_fixed_source;
      marker_origin : Symbol.origin;
    }
  | Implicit_output_argument of { output_index : int; argument_index : int }
  | Condition of {
      condition_index : int;
      role : Function_call_resolution.condition_role;
      keyword_origin : Symbol.origin;
    }
  | Switch_selector of {
      selector_index : int;
      mode : Function_call_resolution.selector_mode;
      keyword_origin : Symbol.origin;
    }
  | Switch_case_value of { case_index : int; position : switch_case_position }
  | Local_array_dimension of {
      declaration_index : int;
      declarator_index : int;
      dimension_index : int;
    }
  | Local_initializer of {
      declaration_index : int;
      declarator_index : int;
      element_path : int list;
    }
  | Return_value of { return_index : int }

type root = {
  index : int;
  role : root_role;
  expression : Function_call_resolution.argument_expression;
  origin : Symbol.origin;
}

type switch_case = {
  index : int;
  keyword_origin : Symbol.origin;
  pattern : switch_case_pattern;
  origin : Symbol.origin;
}

type call = {
  source : Function_call_resolution.call;
  callee : Top_level_outer_expression_binding.occurrence;
  callee_expression : Function_call_resolution.argument_expression;
  result_expression : Function_call_resolution.argument_expression;
}

type statement = {
  source : Top_level_outer_expression_binding.statement;
  roots : root list;
  calls : call list;
  switch_cases : switch_case list;
}

type expression_node = {
  index : int;
  source : Function_call_resolution.argument_expression;
}

type t = {
  table : Symbol_table.t;
  source_ : Top_level_outer_expression_binding.t;
  statements_ : statement list;
  all_roots_ : root list;
  all_calls_ : call list;
  all_switch_cases_ : switch_case list;
  all_expression_nodes_ : expression_node list;
}

type error_kind = Invalid_input of string
type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let invalid_input ?origin message =
  { code = "HCSEMA0055"; kind = Invalid_input message; origin }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message

let error_to_string error = error.code ^ ": " ^ error_message error
let owns_table result table = result.table == table
let source result = result.source_
let statements result = result.statements_
let all_roots result = result.all_roots_
let all_calls result = result.all_calls_
let all_switch_cases result = result.all_switch_cases_
let all_expression_nodes result = result.all_expression_nodes_
let statement_source (statement : statement) = statement.source
let statement_roots (statement : statement) = statement.roots
let statement_calls (statement : statement) = statement.calls
let statement_switch_cases (statement : statement) = statement.switch_cases
let root_index (root : root) = root.index
let root_role (root : root) = root.role
let root_expression (root : root) = root.expression
let root_origin (root : root) = root.origin
let switch_case_index (case_ : switch_case) = case_.index
let switch_case_keyword_origin (case_ : switch_case) = case_.keyword_origin
let switch_case_pattern (case_ : switch_case) = case_.pattern
let switch_case_origin (case_ : switch_case) = case_.origin
let call_source (call : call) = call.source
let call_callee (call : call) = call.callee
let call_callee_expression (call : call) = call.callee_expression
let call_result_expression (call : call) = call.result_expression
let expression_node_index (node : expression_node) = node.index
let expression_node_source (node : expression_node) = node.source

let switch_case_position_name = function
  | Single_case -> "single"
  | Range_start -> "range-start"
  | Range_end -> "range-end"

let switch_case_pattern_name = function
  | Implicit_case -> "implicit"
  | Single_case_pattern -> "single"
  | Ranged_case_pattern _ -> "ranged"

let root_role_name = function
  | Expression_statement { statement_index } ->
      Printf.sprintf "expression-statement:%d" statement_index
  | Implicit_output_fixed { output_index; target; source; _ } ->
      Printf.sprintf "implicit-output:%d:%s:%s" output_index
        (Function_call_resolution.implicit_output_target_name target)
        (Function_call_resolution.implicit_output_fixed_source_name source)
  | Implicit_output_argument { output_index; argument_index } ->
      Printf.sprintf "implicit-output:%d:argument:%d" output_index
        argument_index
  | Condition { condition_index; role; _ } ->
      Printf.sprintf "condition:%d:%s" condition_index
        (match role with
        | Function_call_resolution.If_condition -> "if"
        | Function_call_resolution.While_condition -> "while"
        | Function_call_resolution.Do_while_condition -> "do-while"
        | Function_call_resolution.For_condition -> "for")
  | Switch_selector { selector_index; mode; _ } ->
      Printf.sprintf "switch-selector:%d:%s" selector_index
        (match mode with
        | Function_call_resolution.Bounded_switch -> "bounded"
        | Function_call_resolution.No_bound_switch -> "no-bound")
  | Switch_case_value { case_index; position } ->
      Printf.sprintf "switch-case:%d:%s" case_index
        (switch_case_position_name position)
  | Local_array_dimension
      { declaration_index; declarator_index; dimension_index } ->
      Printf.sprintf "local:%d:%d:dimension:%d" declaration_index
        declarator_index dimension_index
  | Local_initializer { declaration_index; declarator_index; element_path } ->
      let path = element_path |> List.map string_of_int |> String.concat "." in
      Printf.sprintf "local:%d:%d:initializer:%s" declaration_index
        declarator_index path
  | Return_value { return_index } -> Printf.sprintf "return:%d" return_index

let valid_origin = function
  | Symbol.Pinned_source { path; line } ->
      (not (String.equal path "")) && line >= 1
  | Symbol.Source_location _ -> true
  | Symbol.Synthesized description -> not (String.equal description "")

let role_is_valid = function
  | Expression_statement { statement_index }
  | Return_value { return_index = statement_index } -> statement_index >= 0
  | Implicit_output_fixed { output_index; marker_origin; _ } ->
      output_index >= 0 && valid_origin marker_origin
  | Implicit_output_argument { output_index; argument_index } ->
      output_index >= 0 && argument_index >= 0
  | Condition { condition_index; keyword_origin; _ } ->
      condition_index >= 0 && valid_origin keyword_origin
  | Switch_selector { selector_index; keyword_origin; _ } ->
      selector_index >= 0 && valid_origin keyword_origin
  | Switch_case_value { case_index; _ } -> case_index >= 0
  | Local_array_dimension
      { declaration_index; declarator_index; dimension_index } ->
      declaration_index >= 0 && declarator_index >= 0 && dimension_index >= 0
  | Local_initializer { declaration_index; declarator_index; element_path } ->
      declaration_index >= 0 && declarator_index >= 0
      && List.for_all (fun index -> index >= 0) element_path

let make_root ~index ~role ~expression ~origin =
  if index < 0 then
    Error (invalid_input "top-level expression root index cannot be negative")
  else if not (role_is_valid role) then
    Error (invalid_input ~origin "top-level expression role is invalid")
  else if not (valid_origin origin) then
    Error (invalid_input "top-level expression root has an invalid origin")
  else Ok { index; role; expression; origin }

let make_switch_case ~index ~keyword_origin ~pattern ~origin =
  if index < 0 then
    Error (invalid_input "top-level switch case index cannot be negative")
  else if not (valid_origin keyword_origin) then
    Error (invalid_input ~origin "top-level switch case keyword is invalid")
  else if not (valid_origin origin) then
    Error (invalid_input "top-level switch case has an invalid origin")
  else
    match pattern with
    | Ranged_case_pattern { ellipsis_origin }
      when not (valid_origin ellipsis_origin) ->
        Error
          (invalid_input ~origin "top-level switch range ellipsis is invalid")
    | Implicit_case | Single_case_pattern | Ranged_case_pattern _ ->
        Ok { index; keyword_origin; pattern; origin }

let make_call ~source ~callee ~callee_expression ~result_expression =
  let callee_index =
    Top_level_outer_expression_binding.occurrence_index callee
  in
  if
    Function_call_resolution.call_callee_occurrence_index source <> callee_index
  then Error (invalid_input "top-level call has the wrong callee occurrence")
  else if
    not
      (String.equal
         (Function_call_resolution.call_callee_name source)
         (Top_level_outer_expression_binding.occurrence_name callee))
  then
    Error
      (invalid_input "top-level call callee spelling does not match its binding")
  else if
    Function_call_resolution.call_callee_origin source
    <> Top_level_outer_expression_binding.occurrence_origin callee
  then
    Error
      (invalid_input "top-level call callee origin does not match its binding")
  else if
    Function_call_resolution.argument_expression_origin result_expression
    <> Function_call_resolution.call_origin source
  then
    Error (invalid_input "top-level call result origin does not match its call")
  else
    match
      Function_call_resolution.argument_expression_kind result_expression
    with
    | Function_call_resolution.Unresolved_expression
        Function_call_resolution.Call_expression ->
        Ok { source; callee; callee_expression; result_expression }
    | _ ->
        Error (invalid_input "top-level call result is not a call expression")

let indexes_are_contiguous accessor values =
  let rec loop expected = function
    | [] -> true
    | value :: rest -> accessor value = expected && loop (expected + 1) rest
  in
  loop 0 values

let indexes_increase accessor values =
  let rec loop previous = function
    | [] -> true
    | value :: rest ->
        let current = accessor value in
        (match previous with
          | None -> true
          | Some value -> value < current)
        && loop (Some current) rest
  in
  loop None values

let make_statement ~source ~roots ~calls ~switch_cases =
  if not (indexes_increase root_index roots) then
    Error
      (invalid_input
         ~origin:(Top_level_outer_expression_binding.statement_origin source)
         "top-level expression roots are not in identity order")
  else if
    not
      (indexes_increase
         (fun (call : call) -> Function_call_resolution.call_index call.source)
         calls)
  then
    Error
      (invalid_input
         ~origin:(Top_level_outer_expression_binding.statement_origin source)
         "top-level calls are not in identity order")
  else if not (indexes_increase switch_case_index switch_cases) then
    Error
      (invalid_input
         ~origin:(Top_level_outer_expression_binding.statement_origin source)
         "top-level switch cases are not in identity order")
  else Ok { source; roots; calls; switch_cases }

let rec flatten_expression rev expression =
  let rev = expression :: rev in
  match Function_call_resolution.argument_expression_kind expression with
  | Function_call_resolution.Parenthesized_expression grouped ->
      flatten_expression rev grouped
  | Function_call_resolution.Prefix_expression prefix ->
      flatten_expression rev (Function_call_resolution.prefix_operand prefix)
  | Function_call_resolution.Postfix_expression postfix ->
      flatten_expression rev (Function_call_resolution.postfix_operand postfix)
  | Function_call_resolution.Postfix_cast_expression (operand, _) ->
      flatten_expression rev operand
  | Function_call_resolution.Binary_expression binary ->
      let rev =
        flatten_expression rev (Function_call_resolution.binary_left binary)
      in
      flatten_expression rev (Function_call_resolution.binary_right binary)
  | Function_call_resolution.Index_expression index ->
      let rev =
        flatten_expression rev (Function_call_resolution.index_base index)
      in
      flatten_expression rev (Function_call_resolution.index_value index)
  | Function_call_resolution.Member_access_expression member ->
      flatten_expression rev (Function_call_resolution.member_base member)
  | Function_call_resolution.Integer_literal _
  | Function_call_resolution.Float_literal _
  | Function_call_resolution.Character_literal _
  | Function_call_resolution.String_literal _
  | Function_call_resolution.Bound_identifier_expression _
  | Function_call_resolution.Top_level_bound_identifier_expression _
  | Function_call_resolution.Defined_expression _
  | Function_call_resolution.Unresolved_expression _ -> rev

let call_expressions rev (call : call) =
  let source = call.source in
  let rev = flatten_expression rev call.callee_expression in
  let rev =
    match Function_call_resolution.call_computed_callee source with
    | None -> rev
    | Some expression -> flatten_expression rev expression
  in
  List.fold_left
    (fun rev argument ->
      match Function_call_resolution.argument_expression argument with
      | None -> rev
      | Some expression -> flatten_expression rev expression)
    rev
    (Function_call_resolution.call_arguments source)

let expression_nodes statements =
  let expressions =
    List.fold_left
      (fun rev statement ->
        let rev =
          List.fold_left
            (fun rev root -> flatten_expression rev root.expression)
            rev statement.roots
        in
        List.fold_left call_expressions rev statement.calls)
      [] statements
    |> List.rev
  in
  List.mapi (fun index source -> { index; source }) expressions

let validate_statement_sources source statements =
  let expected = Top_level_outer_expression_binding.statements source in
  let rec loop = function
    | [], [] -> Ok ()
    | ( (expected : Top_level_outer_expression_binding.statement)
        :: expected_rest,
        (actual : statement) :: actual_rest )
      when expected == actual.source -> loop (expected_rest, actual_rest)
    | _ ->
        Error
          (invalid_input
             "top-level expression statements do not match their binding batch")
  in
  loop (expected, statements)

let validate_global_indexes statements =
  let roots = List.concat_map (fun statement -> statement.roots) statements in
  let calls = List.concat_map (fun statement -> statement.calls) statements in
  let switch_cases =
    List.concat_map (fun statement -> statement.switch_cases) statements
  in
  if not (indexes_are_contiguous root_index roots) then
    Error (invalid_input "top-level expression root indexes are not contiguous")
  else if
    not
      (indexes_are_contiguous
         (fun (call : call) -> Function_call_resolution.call_index call.source)
         calls)
  then Error (invalid_input "top-level call indexes are not contiguous")
  else Ok (roots, calls, switch_cases)

let create ~table ~source statements =
  if not (Top_level_outer_expression_binding.owns_table source table) then
    Error
      (invalid_input
         "top-level expression bindings belong to another symbol table")
  else
    match validate_statement_sources source statements with
    | Error _ as error -> error
    | Ok () -> (
        match validate_global_indexes statements with
        | Error _ as error -> error
        | Ok (all_roots, all_calls, all_switch_cases) ->
            Ok
              {
                table;
                source_ = source;
                statements_ = statements;
                all_roots_ = all_roots;
                all_calls_ = all_calls;
                all_switch_cases_ = all_switch_cases;
                all_expression_nodes_ = expression_nodes statements;
              })
