type case_value = {
  root : Function_call_expression_result.top_level_root_result;
  value : Function_call_expression_result.expression_result;
  conversion : Function_call_expression_result.intrinsic_conversion;
}

type case_pattern =
  | Implicit_case_result
  | Single_case_result of case_value
  | Ranged_case_result of {
      ellipsis_origin : Symbol.origin;
      start_value : case_value;
      end_value : case_value;
    }

type case_result = {
  statement : Function_call_expression_result.top_level_statement_result;
  source : Top_level_expression_tree.switch_case;
  index : int;
  keyword_origin : Symbol.origin;
  origin : Symbol.origin;
  pattern : case_pattern;
}

type t = {
  table : Symbol_table.t;
  source_ : Function_call_expression_result.top_level_t;
  cases_ : case_result list;
}

type error_kind = Invalid_input of string
type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let invalid_input ?origin message =
  { code = "HCSEMA0065"; kind = Invalid_input message; origin }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message

let error_to_string error = error.code ^ ": " ^ error_message error
let owns_table result table = result.table == table
let source result = result.source_
let cases result = result.cases_
let case_statement (case_ : case_result) = case_.statement
let case_source (case_ : case_result) = case_.source
let case_index (case_ : case_result) = case_.index
let case_keyword_origin (case_ : case_result) = case_.keyword_origin
let case_origin (case_ : case_result) = case_.origin
let case_pattern (case_ : case_result) = case_.pattern
let case_value_root value = value.root
let case_value_result value = value.value
let case_value_conversion value = value.conversion

let case_pattern_name = function
  | Implicit_case_result -> "implicit"
  | Single_case_result _ -> "single"
  | Ranged_case_result _ -> "ranged"

let conversion value =
  match Function_call_expression_result.result_class value with
  | Function_call_expression_result.F64_result ->
      Function_call_expression_result.Result_to_int
  | Function_call_expression_result.Integer_result
  | Function_call_expression_result.Unresolved_actual_class ->
      Function_call_expression_result.No_intrinsic_conversion

let collect ~table source =
  if not (Function_call_expression_result.top_level_owns_table source table)
  then
    Error
      (invalid_input "top-level switch cases belong to another symbol table")
  else
    let case_roots statement =
      statement |> Function_call_expression_result.top_level_statement_roots
      |> List.filter_map (fun root ->
          let source_root =
            Function_call_expression_result.top_level_root_source root
          in
          match Top_level_expression_tree.root_role source_root with
          | Top_level_expression_tree.Switch_case_value { case_index; position }
            -> Some (root, case_index, position)
          | _ -> None)
    in
    let take_value case_ expected_position = function
      | (root, index, position) :: rest
        when index = Top_level_expression_tree.switch_case_index case_
             && position = expected_position ->
          if
            Function_call_expression_result.top_level_root_result_use root
            <> None
          then
            Error
              (invalid_input
                 ~origin:
                   (Top_level_expression_tree.switch_case_keyword_origin case_)
                 "top-level switch case value unexpectedly carries a \
                  discarded-result flag")
          else
            let value =
              Function_call_expression_result.top_level_root_value root
            in
            Ok ({ root; value; conversion = conversion value }, rest)
      | _ ->
          Error
            (invalid_input
               ~origin:
                 (Top_level_expression_tree.switch_case_keyword_origin case_)
               (Printf.sprintf
                  "top-level switch case %d is missing its %s value root"
                  (Top_level_expression_tree.switch_case_index case_)
                  (Top_level_expression_tree.switch_case_position_name
                     expected_position)))
    in
    let rec statement_cases statement expected rev roots = function
      | [] -> (
          match roots with
          | [] -> Ok (expected, rev)
          | (_, index, _) :: _ ->
              Error
                (invalid_input
                   (Printf.sprintf
                      "top-level switch case value root %d has no case metadata"
                      index)))
      | case_ :: rest -> (
          let index = Top_level_expression_tree.switch_case_index case_ in
          let keyword_origin =
            Top_level_expression_tree.switch_case_keyword_origin case_
          in
          if index <> expected then
            Error
              (invalid_input ~origin:keyword_origin
                 (Printf.sprintf
                    "top-level switch case index %d appears where index %d was \
                     expected"
                    index expected))
          else
            let finish pattern roots =
              let case_result =
                {
                  statement;
                  source = case_;
                  index;
                  keyword_origin;
                  origin = Top_level_expression_tree.switch_case_origin case_;
                  pattern;
                }
              in
              statement_cases statement (expected + 1) (case_result :: rev)
                roots rest
            in
            match Top_level_expression_tree.switch_case_pattern case_ with
            | Top_level_expression_tree.Implicit_case -> (
                match roots with
                | (_, root_index, _) :: _ when root_index = index ->
                    Error
                      (invalid_input ~origin:keyword_origin
                         "implicit top-level switch case unexpectedly has a \
                          value root")
                | _ -> finish Implicit_case_result roots)
            | Top_level_expression_tree.Single_case_pattern -> (
                match
                  take_value case_ Top_level_expression_tree.Single_case roots
                with
                | Error _ as error -> error
                | Ok (value, roots) -> finish (Single_case_result value) roots)
            | Top_level_expression_tree.Ranged_case_pattern { ellipsis_origin }
              -> (
                match
                  take_value case_ Top_level_expression_tree.Range_start roots
                with
                | Error _ as error -> error
                | Ok (start_value, roots) -> (
                    match
                      take_value case_ Top_level_expression_tree.Range_end roots
                    with
                    | Error _ as error -> error
                    | Ok (end_value, roots) ->
                        finish
                          (Ranged_case_result
                             { ellipsis_origin; start_value; end_value })
                          roots)))
    in
    let rec statements expected rev = function
      | [] -> Ok (List.rev rev)
      | statement :: rest -> (
          let source_statement =
            Function_call_expression_result.top_level_statement_source statement
          in
          match
            statement_cases statement expected rev (case_roots statement)
              (Top_level_expression_tree.statement_switch_cases source_statement)
          with
          | Error _ as error -> error
          | Ok (expected, rev) -> statements expected rev rest)
    in
    match
      statements 0 []
        (Function_call_expression_result.top_level_statements source)
    with
    | Error _ as error -> error
    | Ok cases_ -> Ok { table; source_ = source; cases_ }
