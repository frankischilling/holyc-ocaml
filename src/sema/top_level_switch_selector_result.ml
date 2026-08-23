type selector = {
  statement : Function_call_expression_result.top_level_statement_result;
  root : Function_call_expression_result.top_level_root_result;
  index : int;
  mode : Function_call_resolution.selector_mode;
  keyword_origin : Symbol.origin;
  value : Function_call_expression_result.expression_result;
}

type t = {
  table : Symbol_table.t;
  source_ : Function_call_expression_result.top_level_t;
  selectors_ : selector list;
}

type error_kind = Invalid_input of string
type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let invalid_input ?origin message =
  { code = "HCSEMA0064"; kind = Invalid_input message; origin }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message

let error_to_string error = error.code ^ ": " ^ error_message error
let owns_table result table = result.table == table
let source result = result.source_
let selectors result = result.selectors_
let selector_statement selector = selector.statement
let selector_root selector = selector.root
let selector_index selector = selector.index
let selector_mode selector = selector.mode
let selector_keyword_origin selector = selector.keyword_origin
let selector_value selector = selector.value

let selector_mode_name = function
  | Function_call_resolution.Bounded_switch -> "bounded"
  | Function_call_resolution.No_bound_switch -> "no-bound"

let collect ~table source =
  if not (Function_call_expression_result.top_level_owns_table source table)
  then
    Error
      (invalid_input
         "top-level switch selector results belong to another symbol table")
  else
    let rec roots statement expected rev = function
      | [] -> Ok (expected, rev)
      | root :: rest -> (
          let source_root =
            Function_call_expression_result.top_level_root_source root
          in
          match Top_level_expression_tree.root_role source_root with
          | Top_level_expression_tree.Switch_selector
              { selector_index; mode; keyword_origin } ->
              if selector_index <> expected then
                Error
                  (invalid_input ~origin:keyword_origin
                     (Printf.sprintf
                        "top-level switch selector index %d appears where \
                         index %d was expected"
                        selector_index expected))
              else if
                Function_call_expression_result.top_level_root_result_use root
                <> None
              then
                Error
                  (invalid_input ~origin:keyword_origin
                     "top-level switch selector unexpectedly carries a \
                      discarded-result flag")
              else
                let selector =
                  {
                    statement;
                    root;
                    index = selector_index;
                    mode;
                    keyword_origin;
                    value =
                      Function_call_expression_result.top_level_root_value root;
                  }
                in
                roots statement (expected + 1) (selector :: rev) rest
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
    | Ok selectors_ -> Ok { table; source_ = source; selectors_ }
