type binding_input = {
  binding : Function_binding_index.binding;
  initial_flag_mask : int64;
}

type function_input = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  is_definition : bool;
  bindings : binding_input list;
}

type warning_kind = Unused_variable | Unneeded_no_warn

type warning = {
  kind : warning_kind;
  code : string;
  function_symbol : Symbol.t;
  binding_symbol : Symbol.t;
  origin : Symbol.origin;
}

type binding_analysis = {
  source : Function_binding_index.binding;
  initial_flag_mask : int64;
  effective_flag_mask : int64;
  ordinary_use_count : int;
  suppression_count : int;
  source_use_count : int;
  suppression_origins : Symbol.origin list;
}

type analyzed_function = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  is_definition : bool;
  bindings : binding_analysis list;
  warnings : warning list;
}

module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)

type t = {
  table : Symbol_table.t;
  compiler_option_mask : int64;
  functions : analyzed_function list;
  warnings : warning list;
  by_symbol : analyzed_function Int_map.t;
}

type error_kind = Invalid_input of string

type error = {
  code : string;
  kind : error_kind;
  origin : Symbol.origin option;
}

let make_binding_input ~binding ~initial_flag_mask =
  { binding; initial_flag_mask }

let make_function_input ~symbol ~scope ~item_index ~is_definition bindings =
  if item_index < 0 then
    Error "local warning function position cannot be negative"
  else Ok { symbol; scope; item_index; is_definition; bindings }

let compiler_option_mask (result : t) = result.compiler_option_mask
let functions (result : t) = result.functions
let warnings (result : t) = result.warnings
let function_symbol (function_ : analyzed_function) = function_.symbol
let function_scope (function_ : analyzed_function) = function_.scope
let function_item_index (function_ : analyzed_function) = function_.item_index

let function_is_definition (function_ : analyzed_function) =
  function_.is_definition

let function_bindings (function_ : analyzed_function) = function_.bindings
let function_warnings (function_ : analyzed_function) = function_.warnings
let binding_source (binding : binding_analysis) = binding.source

let binding_initial_flag_mask (binding : binding_analysis) =
  binding.initial_flag_mask

let binding_effective_flag_mask (binding : binding_analysis) =
  binding.effective_flag_mask

let binding_has_flag (binding : binding_analysis) flag =
  Member_flag.is_set ~mask:binding.effective_flag_mask flag

let binding_ordinary_use_count (binding : binding_analysis) =
  binding.ordinary_use_count

let binding_suppression_count (binding : binding_analysis) =
  binding.suppression_count

let binding_source_use_count (binding : binding_analysis) =
  binding.source_use_count

let binding_suppression_origins (binding : binding_analysis) =
  binding.suppression_origins
let warning_kind (warning : warning) = warning.kind
let warning_code (warning : warning) = warning.code
let warning_function_symbol (warning : warning) = warning.function_symbol
let warning_binding_symbol (warning : warning) = warning.binding_symbol
let warning_origin (warning : warning) = warning.origin

let warning_kind_name = function
  | Unused_variable -> "unused-variable"
  | Unneeded_no_warn -> "unneeded-no-warn"

let warning_message (warning : warning) =
  match warning.kind with
  | Unused_variable ->
      Printf.sprintf "unused variable %S in function %S"
        (Symbol.name warning.binding_symbol)
        (Symbol.name warning.function_symbol)
  | Unneeded_no_warn ->
      Printf.sprintf "unneeded no_warn for %S in function %S"
        (Symbol.name warning.binding_symbol)
        (Symbol.name warning.function_symbol)

let invalid_input ?origin message =
  { code = "HCSEMA0033"; kind = Invalid_input message; origin }

let error_code (error : error) = error.code
let error_kind (error : error) = error.kind
let error_origin (error : error) = error.origin

let error_message (error : error) =
  match error.kind with Invalid_input message -> message

let error_to_string error = error.code ^ ": " ^ error_message error
let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int

let same_symbol left right =
  Symbol.Id.equal (Symbol.id left) (Symbol.id right)

let same_scope left right =
  Symbol.Scope_id.equal
    (Symbol_table.scope_id left)
    (Symbol_table.scope_id right)

let same_binding (left : Function_binding_index.binding)
    (right : Function_binding_index.binding) =
  same_symbol left.symbol right.symbol
  && left.kind = right.kind
  && left.ordinal = right.ordinal
  && left.parameter_index = right.parameter_index
  && left.local_declaration_index = right.local_declaration_index
  && left.local_declarator_index = right.local_declarator_index

let known_member_flag_mask =
  Member_flag.all
  |> List.fold_left
       (fun mask flag -> Int64.logor mask (Member_flag.to_mask flag))
       0L

let known_compiler_option_mask =
  Compiler_option.all
  |> List.fold_left
       (fun mask option -> Int64.logor mask (Compiler_option.mask option))
       0L

let has_unknown_bits value known =
  not (Int64.equal (Int64.logand value (Int64.lognot known)) 0L)

let validate_binding_input table expected (input : binding_input) =
  if not (Symbol_table.owns_symbol table input.binding.symbol) then
    Error (invalid_input "local warning binding belongs to another symbol table")
  else if not (same_binding expected input.binding) then
    Error
      (invalid_input ~origin:(Symbol.origin input.binding.symbol)
         "local warning binding does not match the function index")
  else if has_unknown_bits input.initial_flag_mask known_member_flag_mask then
    Error
      (invalid_input ~origin:(Symbol.origin input.binding.symbol)
         "local warning binding contains unknown member-list flag bits")
  else Ok ()

let validate_binding_inputs table expected inputs =
  let rec loop seen expected inputs =
    match (expected, inputs) with
    | [], [] -> Ok ()
    | expected :: expected_rest, input :: input_rest ->
        let number = symbol_number input.binding.symbol in
        if Int_set.mem number seen then
          Error
            (invalid_input ~origin:(Symbol.origin input.binding.symbol)
               "local warning binding is repeated")
        else (
          match validate_binding_input table expected input with
          | Error _ as error -> error
          | Ok () ->
              loop (Int_set.add number seen) expected_rest input_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input
             "local warning bindings do not match the function index count")
  in
  loop Int_set.empty expected inputs

let validate_function table parent indexed expressions previous_item
    (input : function_input) =
  let indexed_symbol = Function_binding_index.function_symbol indexed in
  let indexed_scope = Function_binding_index.function_scope indexed in
  let indexed_item = Function_binding_index.function_item_index indexed in
  let expression_symbol =
    Function_expression_binding.function_symbol expressions
  in
  let expression_scope = Function_expression_binding.function_scope expressions in
  let expression_item =
    Function_expression_binding.function_item_index expressions
  in
  if input.item_index <= previous_item then
    Error
      (invalid_input
         "local warning functions do not follow module source order")
  else if
    not
      (Symbol_table.owns_symbol table input.symbol
      && Symbol_table.owns_symbol table indexed_symbol
      && Symbol_table.owns_symbol table expression_symbol)
  then
    Error (invalid_input "local warning function belongs to another symbol table")
  else if
    not
      (Symbol_table.owns_scope table input.scope
      && Symbol_table.owns_scope table indexed_scope
      && Symbol_table.owns_scope table expression_scope)
  then
    Error
      (invalid_input "local warning function scope belongs to another symbol table")
  else if
    not
      (same_symbol input.symbol indexed_symbol
      && same_symbol input.symbol expression_symbol)
  then Error (invalid_input "local warning function symbols do not match")
  else if
    not
      (same_scope input.scope indexed_scope
      && same_scope input.scope expression_scope)
  then Error (invalid_input "local warning function scopes do not match")
  else if input.item_index <> indexed_item || input.item_index <> expression_item
  then Error (invalid_input "local warning function positions do not match")
  else if Symbol_table.scope_kind input.scope <> Symbol_table.Function then
    Error (invalid_input "local warning analysis requires a function scope")
  else if
    match Symbol_table.parent input.scope with
    | Some scope_parent -> not (same_scope scope_parent parent)
    | None -> true
  then Error (invalid_input "local warning function has the wrong module parent")
  else
    validate_binding_inputs table
      (Function_binding_index.function_bindings indexed)
      input.bindings

let validate table parent bindings expressions compiler_option_mask inputs =
  if not (Symbol_table.owns_scope table parent) then
    Error
      (invalid_input "local warning parent belongs to another symbol table")
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error (invalid_input "local warning analysis requires a module scope")
  else if has_unknown_bits compiler_option_mask known_compiler_option_mask then
    Error (invalid_input "local warning analysis received unknown option bits")
  else
    let indexed = Function_binding_index.functions bindings in
    let resolved = Function_expression_binding.functions expressions in
    let rec loop previous_item seen indexed resolved inputs =
      match (indexed, resolved, inputs) with
      | [], [], [] -> Ok ()
      | ( indexed :: indexed_rest,
          resolved :: resolved_rest,
          (input : function_input) :: input_rest ) ->
          let number = symbol_number input.symbol in
          if Int_set.mem number seen then
            Error (invalid_input "local warning function is repeated")
          else (
            match
              validate_function table parent indexed resolved previous_item input
            with
            | Error _ as error -> error
            | Ok () ->
                loop input.item_index (Int_set.add number seen) indexed_rest
                  resolved_rest input_rest)
      | [], _, _ | _, [], _ | _, _, [] ->
          Error
            (invalid_input
               "local warning inputs contain different function counts")
    in
    loop (-1) Int_set.empty indexed resolved inputs

type counts = {
  input : binding_input;
  effective_flag_mask : int64;
  ordinary_use_count : int;
  suppression_count : int;
  suppression_origins_rev : Symbol.origin list;
}

let initial_counts input =
  {
    input;
    effective_flag_mask = input.initial_flag_mask;
    ordinary_use_count = 0;
    suppression_count = 0;
    suppression_origins_rev = [];
  }

let checked_increment function_symbol
    (binding : Function_binding_index.binding) count label =
  if count = max_int then
    Error
      (invalid_input ~origin:(Symbol.origin binding.symbol)
         (Printf.sprintf "function %S exhausts the %s counter for %S"
            (Symbol.name function_symbol) label (Symbol.name binding.symbol)))
  else Ok (count + 1)

let update_count states (binding : Function_binding_index.binding) update =
  let key = symbol_number binding.symbol in
  match Int_map.find_opt key states with
  | Some state when same_binding state.input.binding binding ->
      Result.map (fun state -> Int_map.add key state states) (update state)
  | Some _ | None ->
      Error
        (invalid_input ~origin:(Symbol.origin binding.symbol)
           "local warning event refers to an unindexed binding")

let apply_event function_symbol states = function
  | Function_expression_binding.Bound_use occurrence -> (
      match Function_expression_binding.occurrence_resolution occurrence with
      | Function_expression_binding.Nonlocal_candidate -> Ok states
      | Function_expression_binding.Function_binding binding ->
          update_count states binding (fun state ->
              Result.map
                (fun count -> { state with ordinary_use_count = count })
                (checked_increment function_symbol binding
                   state.ordinary_use_count "ordinary-use")))
  | Function_expression_binding.No_warn_suppression suppression ->
      let binding =
        Function_expression_binding.suppression_binding suppression
      in
      update_count states binding (fun state ->
          Result.map
            (fun count ->
              {
                state with
                effective_flag_mask =
                  Member_flag.set ~mask:state.effective_flag_mask
                    Member_flag.No_unused_warning;
                suppression_count = count;
                suppression_origins_rev =
                  Function_expression_binding.suppression_origin suppression
                  :: state.suppression_origins_rev;
              })
            (checked_increment function_symbol binding state.suppression_count
               "no_warn-suppression"))
  | Function_expression_binding.Initializer_use_reset reset ->
      let binding =
        Function_expression_binding.initializer_use_reset_binding reset
      in
      update_count states binding (fun state ->
          Ok
            {
              state with
              ordinary_use_count = 0;
              suppression_count = 0;
            })

let source_use_count function_symbol state =
  if state.ordinary_use_count > max_int - state.suppression_count then
    Error
      (invalid_input ~origin:(Symbol.origin state.input.binding.symbol)
         (Printf.sprintf "function %S exhausts the source-use counter for %S"
            (Symbol.name function_symbol)
            (Symbol.name state.input.binding.symbol)))
  else Ok (state.ordinary_use_count + state.suppression_count)

let binding_analysis function_symbol state =
  Result.map
    (fun source_use_count ->
      {
        source = state.input.binding;
        initial_flag_mask = state.input.initial_flag_mask;
        effective_flag_mask = state.effective_flag_mask;
        ordinary_use_count = state.ordinary_use_count;
        suppression_count = state.suppression_count;
        source_use_count;
        suppression_origins = List.rev state.suppression_origins_rev;
      })
    (source_use_count function_symbol state)

let warning_for compiler_option_mask function_symbol analysis =
  let binding = analysis.source in
  let name = Symbol.name binding.symbol in
  if
    Member_flag.is_set ~mask:analysis.effective_flag_mask
      Member_flag.No_unused_warning
  then
    if analysis.source_use_count > 1 && not (String.equal name "_anon_") then
      let origin =
        match analysis.suppression_origins with
        | origin :: _ -> origin
        | [] -> Symbol.origin binding.symbol
      in
      Some
        {
          kind = Unneeded_no_warn;
          code = "HCSEMA0035";
          function_symbol;
          binding_symbol = binding.symbol;
          origin;
        }
    else None
  else if
    analysis.source_use_count = 0
    && Compiler_option.is_enabled ~mask:compiler_option_mask
         Compiler_option.Warn_unused_var
  then
    Some
      {
        kind = Unused_variable;
        code = "HCSEMA0034";
        function_symbol;
        binding_symbol = binding.symbol;
        origin = Symbol.origin binding.symbol;
      }
  else None

let analyze_function compiler_option_mask expressions (input : function_input) =
  let states =
    List.fold_left
      (fun states binding ->
        Int_map.add (symbol_number binding.binding.symbol)
          (initial_counts binding) states)
      Int_map.empty input.bindings
  in
  let rec apply states = function
    | [] -> Ok states
    | event :: rest -> (
        match apply_event input.symbol states event with
        | Error _ as error -> error
        | Ok states -> apply states rest)
  in
  match
    apply states
      (Function_expression_binding.function_binding_events expressions)
  with
  | Error _ as error -> error
  | Ok states ->
      let rec finish bindings_rev = function
        | [] ->
            let bindings = List.rev bindings_rev in
            let warnings =
              if input.is_definition then
                List.filter_map
                  (warning_for compiler_option_mask input.symbol)
                  bindings
              else []
            in
            Ok
              {
                symbol = input.symbol;
                scope = input.scope;
                item_index = input.item_index;
                is_definition = input.is_definition;
                bindings;
                warnings;
              }
        | binding :: rest -> (
            let key = symbol_number binding.binding.symbol in
            match Int_map.find_opt key states with
            | None ->
                Error
                  (invalid_input
                     "local warning analysis lost an indexed binding")
            | Some state -> (
                match binding_analysis input.symbol state with
                | Error _ as error -> error
                | Ok analysis -> finish (analysis :: bindings_rev) rest))
      in
      finish [] input.bindings

let analyze_validated table compiler_option_mask expressions inputs =
  let resolved = Function_expression_binding.functions expressions in
  let rec loop functions_rev warnings_rev by_symbol resolved inputs =
    match (resolved, inputs) with
    | [], [] ->
        Ok
          {
            table;
            compiler_option_mask;
            functions = List.rev functions_rev;
            warnings = List.rev warnings_rev;
            by_symbol;
          }
    | expression :: expression_rest, input :: input_rest -> (
        match analyze_function compiler_option_mask expression input with
        | Error _ as error -> error
        | Ok function_ ->
            loop (function_ :: functions_rev)
              (List.rev_append function_.warnings warnings_rev)
              (Int_map.add (symbol_number function_.symbol) function_ by_symbol)
              expression_rest input_rest)
    | [], _ :: _ | _ :: _, [] -> assert false
  in
  loop [] [] Int_map.empty resolved inputs

let analyze ~table ~parent ~bindings ~expressions ~compiler_option_mask inputs =
  match
    validate table parent bindings expressions compiler_option_mask inputs
  with
  | Error _ as error -> error
  | Ok () ->
      analyze_validated table compiler_option_mask expressions inputs

let find_function result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_symbol with
    | Some function_ when function_.symbol == symbol -> Some function_
    | Some _ | None -> None
