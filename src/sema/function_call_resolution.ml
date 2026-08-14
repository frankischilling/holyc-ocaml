type call_syntax = Parenthesized | Parenthesis_free
type argument_kind = Provided | Omitted

type unresolved_expression_kind =
  | Identifier_expression
  | Current_position_expression
  | Sizeof_expression
  | Offset_expression
  | Defined_expression
  | Postfix_expression
  | Postfix_cast_expression
  | Binary_expression
  | Call_expression
  | Index_expression
  | Member_expression

type prefix_operator =
  | Unary_plus
  | Unary_minus
  | Logical_not
  | Bitwise_not
  | Dereference
  | Address_of
  | Pre_increment
  | Pre_decrement

type argument_expression_kind =
  | Integer_literal
  | Float_literal
  | Character_literal
  | String_literal
  | Parenthesized_expression of argument_expression
  | Prefix_expression of prefix_expression
  | Postfix_cast_expression of argument_expression * Type_reference.t
  | Unresolved_expression of unresolved_expression_kind

and argument_expression = {
  expression_kind : argument_expression_kind;
  expression_origin : Symbol.origin;
}

and prefix_expression = {
  prefix_operator : prefix_operator;
  prefix_operator_origin : Symbol.origin;
  prefix_operand : argument_expression;
}

type argument = {
  index : int;
  kind : argument_kind;
  expression : argument_expression option;
  origin : Symbol.origin;
}

type call = {
  index : int;
  callee_occurrence_index : int;
  callee_name : string;
  callee_origin : Symbol.origin;
  origin : Symbol.origin;
  syntax : call_syntax;
  arguments : argument list;
}

type function_input = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  calls : call list;
}

type default_use = {
  default : Function_type_resolution.parameter_default;
  omission : argument option;
}

type fixed_value =
  | Provided_argument of argument
  | Declared_default of default_use

type fixed_argument = {
  parameter : Function_type_resolution.parameter;
  value : fixed_value;
}

type direct_call = {
  source : call;
  occurrence : Module_expression_binding.occurrence;
  active_header : Function_type_resolution.resolved_function;
  target_symbol : Symbol.t;
  fixed_arguments : fixed_argument list;
  variadic_arguments : argument list;
  variadic_count : int64;
}

type deferred_reason =
  | Local_callee of Function_binding_index.binding
  | Global_callee of Module_expression_binding.publication
  | Aggregate_callee of Module_expression_binding.publication
  | Outer_callee

type call_resolution =
  | Direct_call of direct_call
  | Deferred_call of {
      call : call;
      occurrence : Module_expression_binding.occurrence;
      reason : deferred_reason;
    }

type resolved_function = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  calls : call_resolution list;
}

module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)
module String_map = Map.Make (String)

type t = {
  table : Symbol_table.t;
  compilation_mode : Function_resolution.compilation_mode;
  functions : resolved_function list;
  by_symbol : resolved_function Int_map.t;
}

type error_kind =
  | Invalid_input of string
  | Missing_required_argument of {
      call : call;
      parameter : Function_type_resolution.parameter;
      omission : argument option;
    }
  | Extra_fixed_argument of {
      call : call;
      argument : argument;
      fixed_count : int;
    }
  | Omitted_variadic_argument of { call : call; argument : argument }

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let functions (result : t) = result.functions
let compilation_mode (result : t) = result.compilation_mode
let owns_table (result : t) table = result.table == table
let function_symbol (function_ : resolved_function) = function_.symbol
let function_scope (function_ : resolved_function) = function_.scope
let function_item_index (function_ : resolved_function) = function_.item_index
let function_calls (function_ : resolved_function) = function_.calls
let call_index (call : call) = call.index
let call_callee_occurrence_index (call : call) = call.callee_occurrence_index
let call_callee_name (call : call) = call.callee_name
let call_callee_origin (call : call) = call.callee_origin
let call_origin (call : call) = call.origin
let call_syntax (call : call) = call.syntax
let call_arguments (call : call) = call.arguments
let argument_index (argument : argument) = argument.index
let argument_kind (argument : argument) = argument.kind
let argument_expression (argument : argument) = argument.expression
let argument_origin (argument : argument) = argument.origin
let prefix_operator (prefix : prefix_expression) = prefix.prefix_operator

let prefix_operator_origin (prefix : prefix_expression) =
  prefix.prefix_operator_origin

let prefix_operand (prefix : prefix_expression) = prefix.prefix_operand
let default_parameter_default (use : default_use) = use.default
let default_omission (use : default_use) = use.omission
let fixed_parameter (fixed : fixed_argument) = fixed.parameter
let fixed_value (fixed : fixed_argument) = fixed.value
let direct_source (direct : direct_call) = direct.source
let direct_occurrence (direct : direct_call) = direct.occurrence
let direct_active_header (direct : direct_call) = direct.active_header
let direct_target_symbol (direct : direct_call) = direct.target_symbol
let direct_fixed_arguments (direct : direct_call) = direct.fixed_arguments
let direct_variadic_arguments (direct : direct_call) = direct.variadic_arguments
let direct_variadic_count (direct : direct_call) = direct.variadic_count
let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int

let call_syntax_name = function
  | Parenthesized -> "parenthesized"
  | Parenthesis_free -> "parenthesis-free"

let argument_kind_name = function
  | Provided -> "provided"
  | Omitted -> "omitted"

let prefix_operator_name = function
  | Unary_plus -> "unary-plus"
  | Unary_minus -> "unary-minus"
  | Logical_not -> "logical-not"
  | Bitwise_not -> "bitwise-not"
  | Dereference -> "dereference"
  | Address_of -> "address-of"
  | Pre_increment -> "pre-increment"
  | Pre_decrement -> "pre-decrement"

let unresolved_expression_kind_name = function
  | Identifier_expression -> "identifier"
  | Current_position_expression -> "current-position"
  | Sizeof_expression -> "sizeof"
  | Offset_expression -> "offset"
  | Defined_expression -> "defined"
  | Postfix_expression -> "postfix"
  | Postfix_cast_expression -> "postfix-cast"
  | Binary_expression -> "binary"
  | Call_expression -> "call"
  | Index_expression -> "index"
  | Member_expression -> "member"

let argument_expression_kind_name = function
  | Integer_literal -> "integer-literal"
  | Float_literal -> "float-literal"
  | Character_literal -> "character-literal"
  | String_literal -> "string-literal"
  | Parenthesized_expression _ -> "parenthesized"
  | Prefix_expression _ -> "prefix"
  | Postfix_cast_expression _ -> "postfix-cast"
  | Unresolved_expression kind -> unresolved_expression_kind_name kind

let deferred_reason_name = function
  | Local_callee _ -> "local-callee"
  | Global_callee _ -> "global-callee"
  | Aggregate_callee _ -> "aggregate-callee"
  | Outer_callee -> "outer-callee"

let invalid_input message =
  { code = "HCSEMA0039"; kind = Invalid_input message; origin = None }

let missing_required_argument (call : call)
    (parameter : Function_type_resolution.parameter)
    (omission : argument option) =
  let origin =
    match omission with
    | Some argument -> argument.origin
    | None -> call.origin
  in
  {
    code = "HCSEMA0040";
    kind = Missing_required_argument { call; parameter; omission };
    origin = Some origin;
  }

let extra_fixed_argument (call : call) (argument : argument) fixed_count =
  {
    code = "HCSEMA0041";
    kind = Extra_fixed_argument { call; argument; fixed_count };
    origin = Some argument.origin;
  }

let omitted_variadic_argument (call : call) (argument : argument) =
  {
    code = "HCSEMA0042";
    kind = Omitted_variadic_argument { call; argument };
    origin = Some argument.origin;
  }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let parameter_display parameter =
  let position = Function_type_resolution.parameter_index parameter + 1 in
  match Function_type_resolution.parameter_name parameter with
  | Some name -> Printf.sprintf "argument %d (%s)" position name
  | None -> Printf.sprintf "argument %d" position

let error_message error =
  match error.kind with
  | Invalid_input message -> message
  | Missing_required_argument { call; parameter; _ } ->
      Printf.sprintf "call to %S is missing required %s" call.callee_name
        (parameter_display parameter)
  | Extra_fixed_argument { call; argument; fixed_count } ->
      Printf.sprintf
        "call to %S provides argument %d, but its active header has %d fixed %s"
        call.callee_name (argument.index + 1) fixed_count
        (if fixed_count = 1 then "parameter" else "parameters")
  | Omitted_variadic_argument { call; argument } ->
      Printf.sprintf
        "call to %S omits variadic argument %d; variadic positions require an \
         expression"
        call.callee_name (argument.index + 1)

let error_to_string error = error.code ^ ": " ^ error_message error

let make_argument_expression ~kind ~origin =
  { expression_kind = kind; expression_origin = origin }

let valid_origin = function
  | Symbol.Pinned_source { path; line } ->
      (not (String.equal path "")) && line >= 1
  | Symbol.Source_location _ -> true
  | Symbol.Synthesized description -> not (String.equal description "")

let make_prefix_argument_expression ~operator ~operator_origin ~operand =
  if not (valid_origin operator_origin) then
    Error "call argument prefix operator has an invalid source origin"
  else
    Ok
      (Prefix_expression
         {
           prefix_operator = operator;
           prefix_operator_origin = operator_origin;
           prefix_operand = operand;
         })

let argument_expression_kind expression = expression.expression_kind
let argument_expression_origin expression = expression.expression_origin

let make_argument ~index ~kind ~expression ~origin =
  if index < 0 then Error "call argument index cannot be negative"
  else
    match (kind, expression) with
    | Provided, Some _ | Omitted, None -> Ok { index; kind; expression; origin }
    | Provided, None -> Error "provided call argument has no expression"
    | Omitted, Some _ -> Error "omitted call argument has an expression"

let validate_argument_indexes (arguments : argument list) =
  let rec loop expected = function
    | [] -> Ok ()
    | (argument : argument) :: rest ->
        if argument.index <> expected then
          Error "call argument indexes are not contiguous"
        else loop (expected + 1) rest
  in
  loop 0 arguments

let make_call ~index ~callee_occurrence_index ~callee_name ~callee_origin
    ~origin ~syntax (arguments : argument list) =
  if index < 0 then Error "function call index cannot be negative"
  else if callee_occurrence_index < 0 then
    Error "function call callee occurrence index cannot be negative"
  else if String.equal callee_name "" then
    Error "function call callee name cannot be empty"
  else
    match validate_argument_indexes arguments with
    | Error _ as error -> error
    | Ok () ->
        Ok
          {
            index;
            callee_occurrence_index;
            callee_name;
            callee_origin;
            origin;
            syntax;
            arguments;
          }

let make_function ~symbol ~scope ~item_index (calls : call list) :
    (function_input, string) result =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error "function call owner is not a function"
  else if Symbol_table.scope_kind scope <> Symbol_table.Function then
    Error "function call owner does not use a function scope"
  else if item_index < 0 then
    Error "function call owner item index cannot be negative"
  else Ok ({ symbol; scope; item_index; calls } : function_input)

let same_symbol left right = Symbol.Id.equal (Symbol.id left) (Symbol.id right)

let same_scope left right =
  Symbol.Scope_id.equal
    (Symbol_table.scope_id left)
    (Symbol_table.scope_id right)

let symbol_in_scope symbol scope =
  Symbol.Scope_id.equal (Symbol.scope_id symbol) (Symbol_table.scope_id scope)

let publish_aggregates_before visible publications item_index =
  let rec loop visible = function
    | publication :: rest
      when Module_expression_binding.publication_item_index publication
           < item_index ->
        let visible =
          if
            Module_expression_binding.publication_kind publication
            = Module_expression_binding.Aggregate
          then
            let symbol =
              Module_expression_binding.publication_canonical_symbol publication
            in
            String_map.add (Symbol.name symbol) symbol visible
          else visible
        in
        loop visible rest
    | remaining -> (visible, remaining)
  in
  loop visible publications

let validate_cast_target table parent visible target =
  let resolved = Type_reference.resolved_type target in
  match Type.base resolved with
  | Type.Primitive _ -> Ok ()
  | Type.Aggregate symbol -> (
      if not (Symbol_table.owns_symbol table symbol) then
        Error
          (invalid_input
             "function call cast target belongs to another symbol table")
      else if not (symbol_in_scope symbol parent) then
        Error (invalid_input "function call cast target has the wrong scope")
      else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type)
      then Error (invalid_input "function call cast target is not an aggregate")
      else
        match String_map.find_opt (Type_reference.spelling target) visible with
        | None ->
            Error
              (invalid_input
                 "function call cast target is not source-visible at the owner \
                  item")
        | Some expected when same_symbol expected symbol -> Ok ()
        | Some _ ->
            Error
              (invalid_input
                 "function call cast target does not match the source-visible \
                  aggregate identity"))

let rec validate_argument_expression table parent visible expression =
  match argument_expression_kind expression with
  | Parenthesized_expression grouped ->
      validate_argument_expression table parent visible grouped
  | Prefix_expression prefix ->
      validate_argument_expression table parent visible prefix.prefix_operand
  | Postfix_cast_expression (operand, target) -> (
      match validate_cast_target table parent visible target with
      | Error _ as error -> error
      | Ok () -> validate_argument_expression table parent visible operand)
  | Integer_literal
  | Float_literal
  | Character_literal
  | String_literal
  | Unresolved_expression _ -> Ok ()

let validate_argument_expressions table parent visible calls =
  let rec arguments = function
    | [] -> Ok ()
    | argument :: rest -> (
        match argument.expression with
        | None -> arguments rest
        | Some expression -> (
            match
              validate_argument_expression table parent visible expression
            with
            | Error _ as error -> error
            | Ok () -> arguments rest))
  in
  let rec loop = function
    | [] -> Ok ()
    | call :: rest -> (
        match arguments call.arguments with
        | Error _ as error -> error
        | Ok () -> loop rest)
  in
  loop calls

let validate_type_function table parent previous_item seen function_ =
  let symbol = Function_type_resolution.function_symbol function_ in
  let scope = Function_type_resolution.function_scope function_ in
  let item_index = Function_type_resolution.function_item_index function_ in
  let number = symbol_number symbol in
  if item_index <= previous_item then
    Error (invalid_input "function type results do not follow source order")
  else if Int_set.mem number seen then
    Error (invalid_input "function type result repeats a declaration symbol")
  else if not (Symbol_table.owns_symbol table symbol) then
    Error (invalid_input "function type result belongs to another symbol table")
  else if not (Symbol_table.owns_scope table scope) then
    Error (invalid_input "function type scope belongs to another symbol table")
  else if not (symbol_in_scope symbol parent) then
    Error (invalid_input "function type symbol has the wrong module scope")
  else if Symbol_table.scope_kind scope <> Symbol_table.Function then
    Error (invalid_input "function type result has a nonfunction scope")
  else if
    match Symbol_table.parent scope with
    | Some owner -> not (same_scope owner parent)
    | None -> true
  then Error (invalid_input "function type scope has the wrong module parent")
  else Ok (item_index, Int_set.add number seen)

let type_map table parent function_types =
  let rec loop previous_item seen by_symbol = function
    | [] -> Ok by_symbol
    | function_ :: rest -> (
        match
          validate_type_function table parent previous_item seen function_
        with
        | Error _ as error -> error
        | Ok (item_index, seen) ->
            loop item_index seen
              (Int_map.add
                 (symbol_number
                    (Function_type_resolution.function_symbol function_))
                 function_ by_symbol)
              rest)
  in
  loop (-1) Int_set.empty Int_map.empty
    (Function_type_resolution.functions function_types)

let validate_resolved_declarations table parent types functions =
  let rec loop seen by_source = function
    | [] ->
        if Int_set.cardinal seen = Int_map.cardinal types then Ok by_source
        else
          Error
            (invalid_input
               "function identity results omit typed function declarations")
    | declaration :: rest -> (
        let site = Function_resolution.resolved_declaration_site declaration in
        let function_ = Function_resolution.declaration_site_function site in
        let source_symbol =
          Function_type_resolution.function_symbol function_
        in
        let target =
          Function_resolution.resolved_declaration_identity_symbol declaration
        in
        let number = symbol_number source_symbol in
        if Int_set.mem number seen then
          Error
            (invalid_input
               "function identity results repeat a source declaration")
        else if
          not
            (Symbol_table.owns_symbol table source_symbol
            && Symbol_table.owns_symbol table target)
        then
          Error
            (invalid_input
               "function identity result belongs to another symbol table")
        else if
          not
            (symbol_in_scope source_symbol parent
            && symbol_in_scope target parent)
        then
          Error (invalid_input "function identity result has the wrong scope")
        else
          match Int_map.find_opt number types with
          | Some expected when expected == function_ ->
              loop (Int_set.add number seen)
                (Int_map.add number declaration by_source)
                rest
          | Some _ ->
              Error
                (invalid_input
                   "function identity result uses a different type declaration")
          | None ->
              Error
                (invalid_input
                   "function identity result has no typed source declaration"))
  in
  loop Int_set.empty Int_map.empty (Function_resolution.declarations functions)

let validate_calls calls occurrences =
  let occurrence_by_index =
    List.fold_left
      (fun map occurrence ->
        Int_map.add
          (Module_expression_binding.occurrence_index occurrence)
          occurrence map)
      Int_map.empty occurrences
  in
  let rec loop expected_call previous_occurrence = function
    | [] -> Ok ()
    | call :: rest -> (
        if call.index <> expected_call then
          Error (invalid_input "function call indexes are not contiguous")
        else if call.callee_occurrence_index <= previous_occurrence then
          Error
            (invalid_input
               "function calls do not follow callee occurrence order")
        else
          match
            Int_map.find_opt call.callee_occurrence_index occurrence_by_index
          with
          | None ->
              Error
                (invalid_input "function call has no matching callee occurrence")
          | Some occurrence ->
              if
                not
                  (String.equal call.callee_name
                     (Module_expression_binding.occurrence_name occurrence))
              then
                Error
                  (invalid_input
                     "function call callee spelling does not match its \
                      occurrence")
              else if
                call.callee_origin
                <> Module_expression_binding.occurrence_origin occurrence
              then
                Error
                  (invalid_input
                     "function call callee origin does not match its occurrence")
              else loop (expected_call + 1) call.callee_occurrence_index rest)
  in
  loop 0 (-1) calls

let validate_function_input table parent visible expected
    (input : function_input) =
  let symbol = Module_expression_binding.function_symbol expected in
  let scope = Module_expression_binding.function_scope expected in
  let item_index = Module_expression_binding.function_item_index expected in
  if not (Symbol_table.owns_symbol table input.symbol) then
    Error (invalid_input "function call owner belongs to another symbol table")
  else if not (Symbol_table.owns_scope table input.scope) then
    Error (invalid_input "function call scope belongs to another symbol table")
  else if not (same_symbol input.symbol symbol) then
    Error
      (invalid_input
         "function call owner does not match module expression binding")
  else if not (same_scope input.scope scope) then
    Error
      (invalid_input
         "function call scope does not match module expression binding")
  else if input.item_index <> item_index then
    Error
      (invalid_input "function call owner has the wrong source item position")
  else if not (symbol_in_scope input.symbol parent) then
    Error (invalid_input "function call owner has the wrong module scope")
  else
    match validate_argument_expressions table parent visible input.calls with
    | Error _ as error -> error
    | Ok () ->
        validate_calls input.calls
          (Module_expression_binding.function_occurrences expected)

let validate_function_inputs table parent expressions inputs =
  let rec pair visible publications expected inputs =
    match (expected, inputs) with
    | [], [] -> Ok ()
    | expected :: expected_rest, (input : function_input) :: input_rest -> (
        let visible, publications =
          publish_aggregates_before visible publications input.item_index
        in
        match validate_function_input table parent visible expected input with
        | Error _ as error -> error
        | Ok () -> pair visible publications expected_rest input_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input
             "function call inputs do not match module expression functions")
  in
  pair String_map.empty
    (Module_expression_binding.publications expressions)
    (Module_expression_binding.functions expressions)
    inputs

let provided_or_default (call : call)
    (parameter : Function_type_resolution.parameter)
    (source_argument : argument option) =
  match source_argument with
  | Some ({ kind = Provided; _ } as argument) ->
      Ok { parameter; value = Provided_argument argument }
  | Some ({ kind = Omitted; _ } as argument) -> (
      match Function_type_resolution.parameter_default parameter with
      | Some default ->
          Ok
            {
              parameter;
              value = Declared_default { default; omission = Some argument };
            }
      | None -> Error (missing_required_argument call parameter (Some argument))
      )
  | None -> (
      match Function_type_resolution.parameter_default parameter with
      | Some default ->
          Ok
            { parameter; value = Declared_default { default; omission = None } }
      | None -> Error (missing_required_argument call parameter None))

let bind_direct_arguments call header =
  let parameters =
    Function_type_resolution.function_signature header
    |> Function_type_resolution.signature_parameters
  in
  let rec fixed fixed_rev parameters arguments =
    match parameters with
    | parameter :: parameter_rest -> (
        let source_argument, argument_rest =
          match arguments with
          | argument :: rest -> (Some argument, rest)
          | [] -> (None, [])
        in
        match provided_or_default call parameter source_argument with
        | Error _ as error -> error
        | Ok bound -> fixed (bound :: fixed_rev) parameter_rest argument_rest)
    | [] -> Ok (List.rev fixed_rev, arguments)
  in
  match fixed [] parameters call.arguments with
  | Error _ as error -> error
  | Ok (fixed_arguments, extras) -> (
      match Function_type_resolution.function_variadic_bindings header with
      | None -> (
          match extras with
          | [] -> Ok (fixed_arguments, [], 0L)
          | argument :: _ ->
              Error
                (extra_fixed_argument call argument (List.length parameters)))
      | Some _ ->
          let rec variadic count (rev : argument list) = function
            | [] -> Ok (fixed_arguments, List.rev rev, count)
            | (argument : argument) :: rest -> (
                match argument.kind with
                | Omitted -> Error (omitted_variadic_argument call argument)
                | Provided ->
                    if Int64.equal count Int64.max_int then
                      Error
                        (invalid_input "variadic argument count exceeds I64")
                    else variadic (Int64.succ count) (argument :: rev) rest)
          in
          variadic 0L [] extras)

let same_publication_target publication declaration =
  let site = Function_resolution.resolved_declaration_site declaration in
  let function_ = Function_resolution.declaration_site_function site in
  same_symbol
    (Module_expression_binding.publication_source_symbol publication)
    (Function_type_resolution.function_symbol function_)
  && same_symbol
       (Module_expression_binding.publication_canonical_symbol publication)
       (Function_resolution.resolved_declaration_identity_symbol declaration)

let resolve_call types declarations occurrence call =
  match Module_expression_binding.occurrence_resolution occurrence with
  | Module_expression_binding.Local_binding binding ->
      Ok (Deferred_call { call; occurrence; reason = Local_callee binding })
  | Module_expression_binding.Outer_candidate ->
      Ok (Deferred_call { call; occurrence; reason = Outer_callee })
  | Module_expression_binding.Module_binding publication -> (
      match Module_expression_binding.publication_kind publication with
      | Module_expression_binding.Global_variable ->
          Ok
            (Deferred_call
               { call; occurrence; reason = Global_callee publication })
      | Module_expression_binding.Aggregate ->
          Ok
            (Deferred_call
               { call; occurrence; reason = Aggregate_callee publication })
      | Module_expression_binding.Function -> (
          let source =
            Module_expression_binding.publication_source_symbol publication
          in
          let number = symbol_number source in
          match
            (Int_map.find_opt number types, Int_map.find_opt number declarations)
          with
          | Some active_header, Some declaration
            when same_publication_target publication declaration -> (
              match bind_direct_arguments call active_header with
              | Error _ as error -> error
              | Ok (fixed_arguments, variadic_arguments, variadic_count) ->
                  Ok
                    (Direct_call
                       {
                         source = call;
                         occurrence;
                         active_header;
                         target_symbol =
                           Module_expression_binding
                           .publication_canonical_symbol publication;
                         fixed_arguments;
                         variadic_arguments;
                         variadic_count;
                       }))
          | Some _, Some _ ->
              Error
                (invalid_input
                   "function call publication disagrees with function identity \
                    resolution")
          | None, _ | _, None ->
              Error
                (invalid_input
                   "function call publication has no active typed header")))

let resolve_function types declarations expected (input : function_input) =
  let occurrences = Module_expression_binding.function_occurrences expected in
  let occurrence_by_index =
    List.fold_left
      (fun map occurrence ->
        Int_map.add
          (Module_expression_binding.occurrence_index occurrence)
          occurrence map)
      Int_map.empty occurrences
  in
  let rec calls rev = function
    | [] ->
        Ok
          {
            symbol = input.symbol;
            scope = input.scope;
            item_index = input.item_index;
            calls = List.rev rev;
          }
    | call :: rest -> (
        match
          Int_map.find_opt call.callee_occurrence_index occurrence_by_index
        with
        | None ->
            Error
              (invalid_input
                 "function call lost its validated callee occurrence")
        | Some occurrence -> (
            match resolve_call types declarations occurrence call with
            | Error _ as error -> error
            | Ok call -> calls (call :: rev) rest))
  in
  calls [] input.calls

let resolve_validated types declarations expressions inputs =
  let rec pair functions_rev by_symbol expected inputs =
    match (expected, inputs) with
    | [], [] -> Ok (List.rev functions_rev, by_symbol)
    | expected :: expected_rest, input :: input_rest -> (
        match resolve_function types declarations expected input with
        | Error _ as error -> error
        | Ok function_ ->
            pair
              (function_ :: functions_rev)
              (Int_map.add (symbol_number function_.symbol) function_ by_symbol)
              expected_rest input_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error (invalid_input "validated function call inputs changed shape")
  in
  pair [] Int_map.empty (Module_expression_binding.functions expressions) inputs

let resolve ~table ~parent ~function_types ~functions ~expressions inputs =
  if not (Symbol_table.owns_scope table parent) then
    Error (invalid_input "function call parent belongs to another symbol table")
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error (invalid_input "function call resolution requires a module scope")
  else if not (Module_expression_binding.owns_table expressions table) then
    Error
      (invalid_input "function call expressions belong to another symbol table")
  else if
    Function_resolution.compilation_mode functions
    <> Module_expression_binding.compilation_mode expressions
  then
    Error
      (invalid_input
         "function call identity and expression compilation modes differ")
  else
    match type_map table parent function_types with
    | Error _ as error -> error
    | Ok types -> (
        match validate_resolved_declarations table parent types functions with
        | Error _ as error -> error
        | Ok declarations -> (
            match validate_function_inputs table parent expressions inputs with
            | Error _ as error -> error
            | Ok () -> (
                match
                  resolve_validated types declarations expressions inputs
                with
                | Error _ as error -> error
                | Ok (functions_result, by_symbol) ->
                    Ok
                      {
                        table;
                        compilation_mode =
                          Function_resolution.compilation_mode functions;
                        functions = functions_result;
                        by_symbol;
                      })))

let find_function result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_symbol with
    | Some function_ when same_symbol function_.symbol symbol -> Some function_
    | Some _ | None -> None
