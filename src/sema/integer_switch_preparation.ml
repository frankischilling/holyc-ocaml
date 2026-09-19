module Ast = Frontend.Ast
module Parser = Frontend.Parser
module Numeric = Closed_numeric_expression

type budget = {
  max_work : int;
  mutable used_work : int;
  mutable table_entries : int;
}

type case = {
  case_receipt : Parser.completed_switch_case;
  case_source_ : Ast.switch_case_label;
  lower : int64;
  upper : int64;
}

type t = {
  receipt_ : Parser.completed_switch;
  source_ : Ast.switch_statement;
  lower_bound_ : int64;
  range_ : int;
  cases_ : case list;
  default_ : Ast.switch_default_label option;
  work_ : int;
}

type endpoint = { original : Parser.switch_case_preparation; value : int64 }

type owner_state = {
  mutable count : int;
  mutable predecessor : Parser.completed_switch_case option;
  mutable first : endpoint option;
  mutable last : endpoint option;
  mutable next_value : int64;
  mutable cases_rev : case list;
  mutable own_work : int;
  mutable failed : bool;
  mutable complete : bool;
}

module Owners = Hashtbl.Make (struct
  type t = Parser.switch_owner

  let equal = ( == )
  let hash owner = Hashtbl.hash owner.Parser.switch_keyword.span
end)

module Sources = Hashtbl.Make (struct
  type t = Ast.switch_statement

  let equal = ( == )
  let hash source = Hashtbl.hash source.Ast.switch_keyword.span
end)

type tracker = {
  budget : budget;
  owners : owner_state Owners.t;
  sources : t Sources.t;
  mutable completed_rev : t list;
}

let hard_max_table_entries = 65_536
let max_case_labels = 65_535

let create_budget ~max_work =
  if max_work <= 0 then Error "max_switch_work must be greater than zero"
  else Ok { max_work; used_work = 0; table_entries = 0 }

let budget_limit budget = budget.max_work
let budget_work budget = budget.used_work
let budget_table_entries budget = budget.table_entries

let create_tracker ~budget =
  {
    budget;
    owners = Owners.create 16;
    sources = Sources.create 16;
    completed_rev = [];
  }

let source prepared = prepared.source_
let receipt prepared = prepared.receipt_
let command prepared = prepared.receipt_.switch_owner.switch_command
let lower_bound prepared = prepared.lower_bound_
let range prepared = prepared.range_
let cases prepared = prepared.cases_
let default prepared = prepared.default_
let work prepared = prepared.work_
let case_source case = case.case_source_
let case_lower_bound case = case.lower
let case_upper_bound case = case.upper
let find tracker source = Sources.find_opt tracker.sources source
let preparations tracker = List.rev tracker.completed_rev

exception Invalid of Common.Diagnostic.t list

let fail at code message =
  raise
    (Invalid
       [
         Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error ~message
           ~primary:at ();
       ])

let invalid at message = fail at "HCRUN0004" message
let unsupported at message = fail at "HCRUN0001" message

let same_option left right =
  match (left, right) with
  | None, None -> true
  | Some left, Some right -> left == right
  | _ -> false

let owner_state tracker (owner : Parser.switch_owner) =
  let state =
    match Owners.find_opt tracker.owners owner with
    | Some state -> state
    | None ->
        let state =
          {
            count = 0;
            predecessor = None;
            first = None;
            last = None;
            next_value = Int64.min_int;
            cases_rev = [];
            own_work = 0;
            failed = false;
            complete = false;
          }
        in
        Owners.add tracker.owners owner state;
        state
  in
  if state.failed || state.complete then
    invalid owner.switch_keyword.span
      "switch preparation owner is failed or already completed";
  state

let check_order state index predecessor at =
  if index <> state.count || not (same_option predecessor state.predecessor)
  then invalid at "switch case does not retain its original ordered predecessor";
  if state.count >= max_case_labels then
    fail at "HCSW0004" "switch case labels exceed the bounded storage allowance"

let rec without_position = function
  | Numeric.Current_position_expression origin
  | Numeric.Captured_position_expression (origin, _) ->
      Numeric.Unsupported_expression
        { description = "current compiler position in a switch case"; origin }
  | Numeric.Unary_expression expression ->
      Numeric.Unary_expression
        { expression with operand = without_position expression.operand }
  | Numeric.Binary_expression expression ->
      Numeric.Binary_expression
        {
          expression with
          left = without_position expression.left;
          right = without_position expression.right;
        }
  | expression -> expression

let evaluate tracker state (preparation : Parser.switch_case_preparation) =
  let original = preparation.switch_case_expression in
  let location = Ast.expression_location original in
  let expression =
    Numeric.of_ast ~allow_floating:true ~query_expression:Fun.id ~queries:[]
      original
    |> without_position
  in
  let consume () =
    if tracker.budget.used_work >= tracker.budget.max_work then
      Error
        (Numeric.make_error ~origin:(Numeric.origin location) "HCSW0003"
           (Numeric.Metadata_overflow "switch preparation work limit")
           "switch case preparation exhausted max_switch_work")
    else (
      tracker.budget.used_work <- tracker.budget.used_work + 1;
      state.own_work <- state.own_work + 1;
      Ok ())
  in
  match
    Numeric.evaluate_expression ~consume
      ~query_origin:(fun expression ->
        Numeric.origin (Ast.expression_location expression))
      ~query_value:(fun _ -> None)
      ~context:Numeric.Switch_case ~current_position:0L expression
  with
  | Ok value -> { original = preparation; value }
  | Error error ->
      let at =
        match Numeric.error_origin error with
        | Some (Symbol.Source_location location) -> location.span
        | _ -> location.span
      in
      let code =
        match Numeric.error_kind error with
        | Numeric.Unresolved_dependency _ | Numeric.Invalid_layout_expression _
          -> "HCRUN0001"
        | _ -> Numeric.error_code error
      in
      fail at code (Numeric.error_message error)

let prepare_endpoint tracker (preparation : Parser.switch_case_preparation) =
  let at = (Ast.expression_location preparation.switch_case_expression).span in
  if not (Parser.switch_case_preparation_is_current preparation) then
    invalid at "switch endpoint requires its exact current parser callback";
  let state = owner_state tracker preparation.switch_owner in
  check_order state preparation.switch_case_index
    preparation.switch_case_predecessor at;
  match preparation.switch_case_endpoint with
  | Parser.Switch_case_start ->
      if
        Option.is_some state.first || Option.is_some state.last
        || Option.is_some preparation.switch_case_endpoint_predecessor
      then invalid at "switch start endpoint is duplicated or out of order";
      state.first <- Some (evaluate tracker state preparation)
  | Parser.Switch_case_end -> (
      match (state.first, state.last) with
      | Some first, None
        when same_option preparation.switch_case_endpoint_predecessor
               (Some first.original) ->
          state.last <- Some (evaluate tracker state preparation)
      | _ ->
          invalid at
            "switch range end does not retain its successful original start")

let complete_case tracker (completed : Parser.completed_switch_case) =
  let label = completed.switch_case_ast in
  let at = label.switch_case_location.span in
  if not (Parser.switch_case_completion_is_current completed) then
    invalid at "switch case requires its exact current completion callback";
  let state = owner_state tracker completed.completed_case_owner in
  check_order state completed.completed_case_index
    completed.completed_case_predecessor at;
  if
    (not
       (same_option completed.switch_case_start_preparation
          (Option.map (fun endpoint -> endpoint.original) state.first)))
    || not
         (same_option completed.switch_case_end_preparation
            (Option.map (fun endpoint -> endpoint.original) state.last))
  then invalid at "switch case does not own its original successful endpoints";
  let lower, upper =
    match (label.switch_case_pattern, state.first, state.last) with
    | Ast.Implicit_case, None, None ->
        let value =
          if state.next_value = Int64.min_int then 0L
          else Int64.succ state.next_value
        in
        (value, value)
    | Ast.Single_case expression, Some first, None
      when expression == first.original.switch_case_expression ->
        (first.value, first.value)
    | Ast.Ranged_case range, Some first, Some last
      when range.case_range_start == first.original.switch_case_expression
           && range.case_range_end == last.original.switch_case_expression ->
        (Int64.min first.value last.value, Int64.max first.value last.value)
    | _ ->
        invalid at "switch case pattern does not match its original endpoints"
  in
  let prepared =
    { case_receipt = completed; case_source_ = label; lower; upper }
  in
  state.cases_rev <- prepared :: state.cases_rev;
  state.predecessor <- Some completed;
  state.count <- state.count + 1;
  state.first <- None;
  state.last <- None;
  state.next_value <- upper

let complete_switch tracker (completed : Parser.completed_switch) =
  let source = completed.switch_ast in
  let at = source.switch_location.span in
  if not (Parser.switch_completion_is_current completed) then
    invalid at "switch requires its exact current completion callback";
  let owner = completed.switch_owner in
  let state = owner_state tracker owner in
  if
    source.switch_mode <> owner.switch_mode
    || source.switch_expression != owner.switch_expression
    || Common.Span.compare source.switch_keyword.span owner.switch_keyword.span
       <> 0
    || Common.Span.compare source.switch_opening_brace.span
         owner.switch_opening_brace.span
       <> 0
    || Option.is_some state.first || Option.is_some state.last
  then invalid at "switch completion does not retain its original source owner";
  if source.switch_mode <> Ast.Bounded_switch then
    unsupported at "integer execution does not admit no-bound switches";
  let defaults = ref [] in
  let source_cases = ref [] in
  List.iter
    (function
      | Ast.Switch_case_element label -> source_cases := label :: !source_cases
      | Ast.Switch_default_element label -> defaults := label :: !defaults
      | Ast.Switch_statement_element _ -> ()
      | Ast.Switch_subswitch_element _ ->
          unsupported at "integer execution does not admit sub-switch regions")
    source.switch_elements;
  let default =
    match !defaults with
    | [] -> None
    | [ label ] -> Some label
    | _ ->
        unsupported at
          "integer execution does not admit multiple default labels in one \
           switch"
  in
  let cases = List.rev state.cases_rev in
  if
    List.length completed.switch_cases <> state.count
    || List.length !source_cases <> state.count
    || (not
          (List.for_all2
             (fun case original -> case.case_receipt == original)
             cases completed.switch_cases))
    || not
         (List.for_all2
            (fun case original -> case.case_source_ == original)
            state.cases_rev !source_cases)
  then invalid at "switch completion omitted or replaced an original case";
  let lo, hi =
    match cases with
    | [] -> fail at "HCSW0001" "switch has no valid case range"
    | first :: rest ->
        List.fold_left
          (fun (lo, hi) case ->
            (Int64.min lo case.lower, Int64.max hi case.upper))
          (first.lower, first.upper) rest
  in
  let lo = if lo > 0L && lo <= 16L then 0L else lo in
  let difference = Int64.sub hi lo in
  if lo > hi || Int64.unsigned_compare difference 0xfffeL > 0 then
    fail at "HCSW0001"
      "switch case range must contain between 1 and 65535 values";
  let range = Int64.to_int difference + 1 in
  let entries = range + 1 in
  if entries > hard_max_table_entries - tracker.budget.table_entries then
    fail at "HCSW0004"
      "switch dispatch tables exceed the bounded storage allowance";
  tracker.budget.table_entries <- tracker.budget.table_entries + entries;
  let sorted =
    List.sort (fun left right -> Int64.compare left.lower right.lower) cases
  in
  let rec disjoint = function
    | previous :: (next :: _ as rest) ->
        if previous.upper >= next.lower then
          fail at "HCSW0002" "duplicate or overlapping switch case values";
        disjoint rest
    | [] | [ _ ] -> ()
  in
  disjoint sorted;
  if Sources.mem tracker.sources source then
    invalid at "original switch source was completed more than once";
  let prepared =
    {
      receipt_ = completed;
      source_ = source;
      lower_bound_ = lo;
      range_ = range;
      cases_ = cases;
      default_ = default;
      work_ = state.own_work;
    }
  in
  state.complete <- true;
  Sources.add tracker.sources source prepared;
  tracker.completed_rev <- prepared :: tracker.completed_rev

let observe tracker event =
  let owner =
    match event with
    | Parser.Switch_case_preparing preparation -> Some preparation.switch_owner
    | Parser.Switch_case_completed completed ->
        Some completed.completed_case_owner
    | Parser.Switch_completed completed -> Some completed.switch_owner
    | _ -> None
  in
  try
    (match event with
    | Parser.Switch_case_preparing preparation ->
        prepare_endpoint tracker preparation
    | Parser.Switch_case_completed completed -> complete_case tracker completed
    | Parser.Switch_completed completed -> complete_switch tracker completed
    | _ -> ());
    Ok ()
  with Invalid diagnostics ->
    Option.iter
      (fun owner ->
        Option.iter
          (fun state -> state.failed <- true)
          (Owners.find_opt tracker.owners owner))
      owner;
    Error diagnostics
