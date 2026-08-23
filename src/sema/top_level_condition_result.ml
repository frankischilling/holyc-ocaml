type branch_test = Branch_on_zero | Branch_on_nonzero

type condition = {
  statement : Function_call_expression_result.top_level_statement_result;
  root : Function_call_expression_result.top_level_root_result;
  index : int;
  role : Function_call_resolution.condition_role;
  keyword_origin : Symbol.origin;
  branch_test : branch_test;
  value : Function_call_expression_result.expression_result;
}

type t = {
  table : Symbol_table.t;
  source_ : Function_call_expression_result.top_level_t;
  conditions_ : condition list;
}

type error_kind = Invalid_input of string
type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let invalid_input ?origin message =
  { code = "HCSEMA0063"; kind = Invalid_input message; origin }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message

let error_to_string error = error.code ^ ": " ^ error_message error
let owns_table result table = result.table == table
let source result = result.source_
let conditions result = result.conditions_
let condition_statement condition = condition.statement
let condition_root condition = condition.root
let condition_index condition = condition.index
let condition_role condition = condition.role
let condition_keyword_origin condition = condition.keyword_origin
let condition_branch_test condition = condition.branch_test
let condition_value condition = condition.value

let branch_test_name = function
  | Branch_on_zero -> "zero"
  | Branch_on_nonzero -> "nonzero"

let branch_test = function
  | Function_call_resolution.If_condition
  | Function_call_resolution.While_condition
  | Function_call_resolution.For_condition -> Branch_on_zero
  | Function_call_resolution.Do_while_condition -> Branch_on_nonzero

let collect ~table source =
  if not (Function_call_expression_result.top_level_owns_table source table)
  then
    Error
      (invalid_input
         "top-level condition results belong to another symbol table")
  else
    let rec roots statement expected rev = function
      | [] -> Ok (expected, rev)
      | root :: rest -> (
          let source_root =
            Function_call_expression_result.top_level_root_source root
          in
          match Top_level_expression_tree.root_role source_root with
          | Top_level_expression_tree.Condition
              { condition_index; role; keyword_origin } ->
              if condition_index <> expected then
                Error
                  (invalid_input ~origin:keyword_origin
                     (Printf.sprintf
                        "top-level condition index %d appears where index %d \
                         was expected"
                        condition_index expected))
              else if
                Function_call_expression_result.top_level_root_result_use root
                <> None
              then
                Error
                  (invalid_input ~origin:keyword_origin
                     "top-level condition unexpectedly carries a \
                      discarded-result flag")
              else
                let value =
                  Function_call_expression_result.top_level_root_value root
                in
                let condition =
                  {
                    statement;
                    root;
                    index = condition_index;
                    role;
                    keyword_origin;
                    branch_test = branch_test role;
                    value;
                  }
                in
                roots statement (expected + 1) (condition :: rev) rest
          | _ -> roots statement expected rev rest)
    in
    let rec statements expected rev = function
      | [] -> Ok (List.rev rev)
      | statement :: rest -> (
          match
            roots statement expected rev
              (Function_call_expression_result.top_level_statement_roots
                 statement)
          with
          | Error _ as error -> error
          | Ok (expected, rev) -> statements expected rev rest)
    in
    match
      statements 0 []
        (Function_call_expression_result.top_level_statements source)
    with
    | Error _ as error -> error
    | Ok conditions_ -> Ok { table; source_ = source; conditions_ }
