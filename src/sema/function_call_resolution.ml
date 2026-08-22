type call_syntax = Parenthesized | Parenthesis_free

type callee_form =
  | Identifier_callee
  | Dereferenced_identifier_callee of int
  | Member_callee

type argument_kind = Provided | Omitted

type unresolved_expression_kind =
  | Identifier_expression
  | Current_position_expression
  | Sizeof_expression
  | Offset_expression
  | Defined_expression
  | Postfix_cast_expression
  | Call_expression

type prefix_operator =
  | Unary_plus
  | Unary_minus
  | Logical_not
  | Bitwise_not
  | Dereference
  | Address_of
  | Pre_increment
  | Pre_decrement

type postfix_operator = Post_increment | Post_decrement
type member_access_kind = Direct_member | Pointer_member

type identifier_value_shape =
  | Object_value
  | Array_value
  | Function_pointer_value
  | Direct_function_value

type direct_function_address_path =
  | Jit_extern_slot
  | Jit_immediate
  | Aot_absolute
  | Reject_aot_extern
  | Reject_aot_import
  | Reject_internal

type identifier_value = {
  identifier_value_type_ : Type.t;
  identifier_value_shape_ : identifier_value_shape;
  identifier_value_array_rank_ : int;
  identifier_value_function_declaration_ :
    Function_resolution.resolved_declaration option;
  identifier_value_function_address_path_ : direct_function_address_path option;
}

type argument_expression_kind =
  | Integer_literal
  | Float_literal
  | Character_literal
  | String_literal
  | Parenthesized_expression of argument_expression
  | Prefix_expression of prefix_expression
  | Postfix_expression of postfix_expression
  | Postfix_cast_expression of argument_expression * Type_reference.t
  | Binary_expression of binary_expression
  | Index_expression of index_expression
  | Member_access_expression of member_expression
  | Bound_identifier_expression of bound_identifier
  | Top_level_bound_identifier_expression of top_level_bound_identifier
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

and postfix_expression = {
  postfix_operator : postfix_operator;
  postfix_operator_origin : Symbol.origin;
  postfix_operand : argument_expression;
}

and binary_expression = {
  binary_operator : Generated.Intermediate_codes.t;
  binary_operator_origin : Symbol.origin;
  binary_left : argument_expression;
  binary_right : argument_expression;
}

and index_expression = {
  index_base : argument_expression;
  index_opening_origin : Symbol.origin;
  index_value : argument_expression;
  index_closing_origin : Symbol.origin;
}

and member_expression = {
  member_base : argument_expression;
  member_access_kind : member_access_kind;
  member_operator_origin : Symbol.origin;
  member_name : string;
  member_origin : Symbol.origin;
}

and bound_identifier = {
  bound_identifier_occurrence_ : Module_expression_binding.occurrence;
  bound_identifier_type_ : Type.t;
  bound_identifier_shape_ : identifier_value_shape;
  bound_identifier_array_rank_ : int;
  bound_identifier_function_declaration_ :
    Function_resolution.resolved_declaration option;
  bound_identifier_function_address_path_ : direct_function_address_path option;
}

and top_level_bound_identifier = {
  top_level_bound_identifier_occurrence_ :
    Top_level_outer_expression_binding.occurrence;
}

type argument = {
  index : int;
  kind : argument_kind;
  expression : argument_expression option;
  origin : Symbol.origin;
}

type callable = {
  callable_return_type_ : Type_reference.t;
  callable_pointer_ : Function_type_resolution.function_pointer;
}

type call = {
  index : int;
  callee_occurrence_index : int;
  callee_name : string;
  callee_origin : Symbol.origin;
  callee_form : callee_form;
  callable : callable option;
  computed_callee : argument_expression option;
  origin : Symbol.origin;
  syntax : call_syntax;
  arguments : argument list;
}

type condition_role =
  | If_condition
  | While_condition
  | Do_while_condition
  | For_condition

type condition_input = {
  index : int;
  role : condition_role;
  keyword_origin : Symbol.origin;
  expression : argument_expression;
  origin : Symbol.origin;
}

type selector_mode = Bounded_switch | No_bound_switch

type selector_input = {
  index : int;
  mode : selector_mode;
  keyword_origin : Symbol.origin;
  expression : argument_expression;
  origin : Symbol.origin;
}

type expression_statement_input = {
  index : int;
  expression : argument_expression;
  origin : Symbol.origin;
}

type implicit_output_target = Print_output | Put_chars_output

type implicit_output_fixed_source =
  | Marker_fixed_output
  | Following_expression_output

type implicit_output_argument = {
  index : int;
  leading_comma_origin : Symbol.origin;
  expression : argument_expression;
  origin : Symbol.origin;
}

type implicit_output_input = {
  index : int;
  target : implicit_output_target;
  marker_origin : Symbol.origin;
  fixed_source : implicit_output_fixed_source;
  fixed_expression : argument_expression;
  arguments : implicit_output_argument list;
  origin : Symbol.origin;
}

type switch_case_pattern =
  | Implicit_case
  | Single_case of argument_expression
  | Ranged_case of {
      start_expression : argument_expression;
      ellipsis_origin : Symbol.origin;
      end_expression : argument_expression;
    }

type switch_case_input = {
  index : int;
  keyword_origin : Symbol.origin;
  pattern : switch_case_pattern;
  origin : Symbol.origin;
}

type return_input = {
  index : int;
  keyword_origin : Symbol.origin;
  expression : argument_expression option;
  origin : Symbol.origin;
}

type function_input = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  calls : call list;
  expression_statements : expression_statement_input list;
  implicit_outputs : implicit_output_input list;
  conditions : condition_input list;
  selectors : selector_input list;
  switch_cases : switch_case_input list;
  returns : return_input list;
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

type indirect_call = {
  source : call;
  occurrence : Module_expression_binding.occurrence;
  callable : callable;
  member_lookup : Aggregate_member_index.lookup option;
  fixed_arguments : fixed_argument list;
  variadic_arguments : argument list;
  variadic_count : int64;
}

type deferred_reason =
  | Local_callee of Function_binding_index.binding
  | Global_callee of Module_expression_binding.publication
  | Aggregate_callee of Module_expression_binding.publication
  | Computed_member_callee of argument_expression
  | Outer_callee

type call_resolution =
  | Direct_call of direct_call
  | Indirect_call of indirect_call
  | Deferred_call of {
      call : call;
      occurrence : Module_expression_binding.occurrence;
      reason : deferred_reason;
    }

type resolved_function = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  return_type : Type_reference.t;
  calls : call_resolution list;
  expression_statements : expression_statement_input list;
  implicit_outputs : implicit_output_input list;
  conditions : condition_input list;
  selectors : selector_input list;
  switch_cases : switch_case_input list;
  returns : return_input list;
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
let function_return_type (function_ : resolved_function) = function_.return_type
let function_calls (function_ : resolved_function) = function_.calls

let function_expression_statements (function_ : resolved_function) =
  function_.expression_statements

let function_implicit_outputs (function_ : resolved_function) =
  function_.implicit_outputs

let function_conditions (function_ : resolved_function) = function_.conditions
let function_selectors (function_ : resolved_function) = function_.selectors

let function_switch_cases (function_ : resolved_function) =
  function_.switch_cases

let function_returns (function_ : resolved_function) = function_.returns
let call_index (call : call) = call.index
let call_callee_occurrence_index (call : call) = call.callee_occurrence_index
let call_callee_name (call : call) = call.callee_name
let call_callee_origin (call : call) = call.callee_origin
let call_callee_form (call : call) = call.callee_form
let call_callable (call : call) = call.callable
let call_computed_callee (call : call) = call.computed_callee
let call_origin (call : call) = call.origin
let call_syntax (call : call) = call.syntax
let call_arguments (call : call) = call.arguments
let condition_index (condition : condition_input) = condition.index
let condition_role (condition : condition_input) = condition.role

let condition_keyword_origin (condition : condition_input) =
  condition.keyword_origin

let condition_expression (condition : condition_input) = condition.expression
let condition_origin (condition : condition_input) = condition.origin
let selector_index (selector : selector_input) = selector.index
let selector_mode (selector : selector_input) = selector.mode

let selector_keyword_origin (selector : selector_input) =
  selector.keyword_origin

let selector_expression (selector : selector_input) = selector.expression
let selector_origin (selector : selector_input) = selector.origin

let expression_statement_index (statement : expression_statement_input) =
  statement.index

let expression_statement_expression (statement : expression_statement_input) =
  statement.expression

let expression_statement_origin (statement : expression_statement_input) =
  statement.origin

let implicit_output_index (output : implicit_output_input) = output.index
let implicit_output_target (output : implicit_output_input) = output.target

let implicit_output_marker_origin (output : implicit_output_input) =
  output.marker_origin

let implicit_output_fixed_source (output : implicit_output_input) =
  output.fixed_source

let implicit_output_fixed_expression (output : implicit_output_input) =
  output.fixed_expression

let implicit_output_arguments (output : implicit_output_input) =
  output.arguments

let implicit_output_origin (output : implicit_output_input) = output.origin

let implicit_output_argument_index (argument : implicit_output_argument) =
  argument.index

let implicit_output_argument_leading_comma_origin
    (argument : implicit_output_argument) =
  argument.leading_comma_origin

let implicit_output_argument_expression (argument : implicit_output_argument) =
  argument.expression

let implicit_output_argument_origin (argument : implicit_output_argument) =
  argument.origin

let switch_case_index (case_ : switch_case_input) = case_.index

let switch_case_keyword_origin (case_ : switch_case_input) =
  case_.keyword_origin

let switch_case_pattern (case_ : switch_case_input) = case_.pattern
let switch_case_origin (case_ : switch_case_input) = case_.origin
let return_index (return_ : return_input) = return_.index
let return_keyword_origin (return_ : return_input) = return_.keyword_origin
let return_expression (return_ : return_input) = return_.expression
let return_origin (return_ : return_input) = return_.origin
let argument_index (argument : argument) = argument.index
let argument_kind (argument : argument) = argument.kind
let argument_expression (argument : argument) = argument.expression
let argument_origin (argument : argument) = argument.origin
let prefix_operator (prefix : prefix_expression) = prefix.prefix_operator

let prefix_operator_origin (prefix : prefix_expression) =
  prefix.prefix_operator_origin

let prefix_operand (prefix : prefix_expression) = prefix.prefix_operand
let postfix_operator (postfix : postfix_expression) = postfix.postfix_operator

let postfix_operator_origin (postfix : postfix_expression) =
  postfix.postfix_operator_origin

let postfix_operand (postfix : postfix_expression) = postfix.postfix_operand
let binary_operator (binary : binary_expression) = binary.binary_operator

let binary_operator_origin (binary : binary_expression) =
  binary.binary_operator_origin

let binary_left (binary : binary_expression) = binary.binary_left
let binary_right (binary : binary_expression) = binary.binary_right
let index_base (index : index_expression) = index.index_base
let index_opening_origin (index : index_expression) = index.index_opening_origin
let index_value (index : index_expression) = index.index_value
let index_closing_origin (index : index_expression) = index.index_closing_origin
let member_base (member : member_expression) = member.member_base
let member_access_kind (member : member_expression) = member.member_access_kind

let member_operator_origin (member : member_expression) =
  member.member_operator_origin

let member_name (member : member_expression) = member.member_name
let member_origin (member : member_expression) = member.member_origin

let bound_identifier_occurrence identifier =
  identifier.bound_identifier_occurrence_

let bound_identifier_type identifier = identifier.bound_identifier_type_
let bound_identifier_shape identifier = identifier.bound_identifier_shape_

let bound_identifier_array_rank identifier =
  identifier.bound_identifier_array_rank_

let bound_identifier_function_declaration identifier =
  identifier.bound_identifier_function_declaration_

let bound_identifier_function_address_path identifier =
  identifier.bound_identifier_function_address_path_

let top_level_bound_identifier_occurrence identifier =
  identifier.top_level_bound_identifier_occurrence_

let identifier_value_type value = value.identifier_value_type_
let identifier_value_shape value = value.identifier_value_shape_
let identifier_value_array_rank value = value.identifier_value_array_rank_

let identifier_value_function_declaration value =
  value.identifier_value_function_declaration_

let identifier_value_function_address_path value =
  value.identifier_value_function_address_path_

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
let callable_return_type callable = callable.callable_return_type_
let callable_pointer callable = callable.callable_pointer_

let callable_signature callable =
  Function_type_resolution.function_pointer_signature callable.callable_pointer_

let indirect_source (indirect : indirect_call) = indirect.source
let indirect_occurrence (indirect : indirect_call) = indirect.occurrence
let indirect_callable (indirect : indirect_call) = indirect.callable
let indirect_member_lookup (indirect : indirect_call) = indirect.member_lookup

let indirect_fixed_arguments (indirect : indirect_call) =
  indirect.fixed_arguments

let indirect_variadic_arguments (indirect : indirect_call) =
  indirect.variadic_arguments

let indirect_variadic_count (indirect : indirect_call) = indirect.variadic_count
let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int

let call_syntax_name = function
  | Parenthesized -> "parenthesized"
  | Parenthesis_free -> "parenthesis-free"

let callee_form_name = function
  | Identifier_callee -> "identifier"
  | Dereferenced_identifier_callee depth ->
      Printf.sprintf "dereferenced-identifier:%d" depth
  | Member_callee -> "member"

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

let postfix_operator_name = function
  | Post_increment -> "post-increment"
  | Post_decrement -> "post-decrement"

let member_access_kind_name = function
  | Direct_member -> "direct"
  | Pointer_member -> "pointer"

let implicit_output_target_name = function
  | Print_output -> "Print"
  | Put_chars_output -> "PutChars"

let implicit_output_fixed_source_name = function
  | Marker_fixed_output -> "marker"
  | Following_expression_output -> "following-expression"

let binary_operator_name = Generated.Intermediate_codes.to_source_name

let identifier_value_shape_name = function
  | Object_value -> "object"
  | Array_value -> "array"
  | Function_pointer_value -> "function-pointer"
  | Direct_function_value -> "direct-function"

let direct_function_address_path_name = function
  | Jit_extern_slot -> "jit-extern-slot"
  | Jit_immediate -> "jit-immediate"
  | Aot_absolute -> "aot-absolute"
  | Reject_aot_extern -> "reject-aot-extern"
  | Reject_aot_import -> "reject-aot-import"
  | Reject_internal -> "reject-internal"

let direct_function_address_path compilation_mode declaration =
  let site = Function_resolution.resolved_declaration_site declaration in
  if
    Function_resolution.declaration_site_source_kind site
    = Function_resolution.Intern
  then Ok Reject_internal
  else
    match
      (compilation_mode, Function_resolution.declaration_site_state site)
    with
    | Function_resolution.Jit, Function_resolution.Unresolved_extern ->
        Ok Jit_extern_slot
    | Function_resolution.Jit, Function_resolution.Resolved -> Ok Jit_immediate
    | Function_resolution.Aot, Function_resolution.Resolved -> Ok Aot_absolute
    | Function_resolution.Aot, Function_resolution.Unresolved_extern ->
        Ok Reject_aot_extern
    | Function_resolution.Aot, Function_resolution.Imported ->
        Ok Reject_aot_import
    | Function_resolution.Jit, Function_resolution.Imported ->
        Error "JIT function resolution contains an imported declaration"

let unresolved_expression_kind_name = function
  | Identifier_expression -> "identifier"
  | Current_position_expression -> "current-position"
  | Sizeof_expression -> "sizeof"
  | Offset_expression -> "offset"
  | Defined_expression -> "defined"
  | Postfix_cast_expression -> "postfix-cast"
  | Call_expression -> "call"

let argument_expression_kind_name = function
  | Integer_literal -> "integer-literal"
  | Float_literal -> "float-literal"
  | Character_literal -> "character-literal"
  | String_literal -> "string-literal"
  | Parenthesized_expression _ -> "parenthesized"
  | Prefix_expression _ -> "prefix"
  | Postfix_expression _ -> "postfix"
  | Postfix_cast_expression _ -> "postfix-cast"
  | Binary_expression _ -> "binary"
  | Index_expression _ -> "index"
  | Member_access_expression _ -> "member"
  | Bound_identifier_expression _ -> "bound-identifier"
  | Top_level_bound_identifier_expression _ -> "top-level-bound-identifier"
  | Unresolved_expression kind -> unresolved_expression_kind_name kind

let deferred_reason_name = function
  | Local_callee _ -> "local-callee"
  | Global_callee _ -> "global-callee"
  | Aggregate_callee _ -> "aggregate-callee"
  | Computed_member_callee _ -> "computed-member-callee"
  | Outer_callee -> "outer-callee"

let invalid_input message =
  { code = "HCSEMA0039"; kind = Invalid_input message; origin = None }

let invalid_input_at origin message =
  { code = "HCSEMA0039"; kind = Invalid_input message; origin = Some origin }

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

let make_postfix_argument_expression ~operator ~operator_origin ~operand =
  if not (valid_origin operator_origin) then
    Error "call argument postfix operator has an invalid source origin"
  else
    Ok
      (Postfix_expression
         {
           postfix_operator = operator;
           postfix_operator_origin = operator_origin;
           postfix_operand = operand;
         })

let checked_binary_operator operator =
  let source_name = binary_operator_name operator in
  List.exists
    (fun (candidate : Generated.Operator_tables.binary_operator) ->
      String.equal candidate.ic_name source_name)
    Generated.Operator_tables.binary_operators

let make_binary_argument_expression ~operator ~operator_origin ~left ~right =
  if not (checked_binary_operator operator) then
    Error
      (Printf.sprintf "%s is not a checked binary operator"
         (binary_operator_name operator))
  else if not (valid_origin operator_origin) then
    Error "call argument binary operator has an invalid source origin"
  else
    Ok
      (Binary_expression
         {
           binary_operator = operator;
           binary_operator_origin = operator_origin;
           binary_left = left;
           binary_right = right;
         })

let make_index_argument_expression ~base ~opening_origin ~index ~closing_origin
    =
  if not (valid_origin opening_origin) then
    Error "call argument index has an invalid opening-bracket origin"
  else if not (valid_origin closing_origin) then
    Error "call argument index has an invalid closing-bracket origin"
  else
    Ok
      (Index_expression
         {
           index_base = base;
           index_opening_origin = opening_origin;
           index_value = index;
           index_closing_origin = closing_origin;
         })

let make_member_argument_expression ~base ~access_kind ~operator_origin
    ~member_name ~member_origin =
  if not (valid_origin operator_origin) then
    Error "call argument member has an invalid operator origin"
  else if String.equal member_name "" then
    Error "call argument member name cannot be empty"
  else if not (valid_origin member_origin) then
    Error "call argument member has an invalid name origin"
  else
    Ok
      (Member_access_expression
         {
           member_base = base;
           member_access_kind = access_kind;
           member_operator_origin = operator_origin;
           member_name;
           member_origin;
         })

let function_declaration_matches_publication declaration publication =
  let site = Function_resolution.resolved_declaration_site declaration in
  let function_ = Function_resolution.declaration_site_function site in
  Symbol.Id.equal
    (function_ |> Function_type_resolution.function_symbol |> Symbol.id)
    (publication |> Module_expression_binding.publication_source_symbol
   |> Symbol.id)
  && Symbol.Id.equal
       (declaration |> Function_resolution.resolved_declaration_identity_symbol
      |> Symbol.id)
       (publication |> Module_expression_binding.publication_canonical_symbol
      |> Symbol.id)

let make_identifier_value ~resolved_type ~shape ~array_rank
    ?function_declaration ?function_address_path () =
  if array_rank < 0 then Error "identifier value array rank cannot be negative"
  else if shape = Array_value && array_rank = 0 then
    Error "array identifier value has no array dimensions"
  else if shape <> Array_value && array_rank <> 0 then
    Error "nonarray identifier value has array dimensions"
  else if
    shape = Direct_function_value
    && (Option.is_none function_declaration
       || Option.is_none function_address_path)
  then Error "direct function identifier has no checked address path"
  else if
    shape <> Direct_function_value
    && (Option.is_some function_declaration
       || Option.is_some function_address_path)
  then Error "nonfunction identifier has a function address path"
  else
    Ok
      {
        identifier_value_type_ = resolved_type;
        identifier_value_shape_ = shape;
        identifier_value_array_rank_ = array_rank;
        identifier_value_function_declaration_ = function_declaration;
        identifier_value_function_address_path_ = function_address_path;
      }

let global_identifier_value global =
  let dimensions = Global_type_resolution.global_array_dimensions global in
  let resolved_type =
    global |> Global_type_resolution.global_type_reference
    |> Type_reference.resolved_type
  in
  let shape =
    if dimensions <> [] then Array_value
    else
      match Global_type_resolution.global_declarator_kind global with
      | Global_type_resolution.Function_pointer _ -> Function_pointer_value
      | Global_type_resolution.Object -> Object_value
  in
  make_identifier_value ~resolved_type ~shape
    ~array_rank:(List.length dimensions) ()

let direct_function_identifier_value ~declaration ~address_path =
  match
    Type.make_primitive ~form:Type.Internal_storage
      ~primitive:Primitive_type.I64 ~pointer_depth:0
  with
  | Error _ as error -> error
  | Ok resolved_type ->
      make_identifier_value ~resolved_type ~shape:Direct_function_value
        ~array_rank:0 ~function_declaration:declaration
        ~function_address_path:address_path ()

let make_bound_identifier_argument_expression ~occurrence ~resolved_type ~shape
    ~array_rank ?function_declaration ?function_address_path () =
  let name = Module_expression_binding.occurrence_name occurrence in
  if String.equal name "" then
    Error "bound call argument identifier cannot have an empty name"
  else if
    match
      (Module_expression_binding.occurrence_resolution occurrence, shape)
    with
    | Module_expression_binding.Local_binding _, Direct_function_value -> true
    | Module_expression_binding.Local_binding _, _ -> false
    | ( Module_expression_binding.Module_binding publication,
        Direct_function_value ) ->
        Module_expression_binding.publication_kind publication
        <> Module_expression_binding.Function
    | Module_expression_binding.Module_binding publication, _ ->
        Module_expression_binding.publication_kind publication
        <> Module_expression_binding.Global_variable
    | Module_expression_binding.Outer_candidate, _ -> true
  then Error "bound call argument occurrence is not a typed value binding"
  else if
    match
      ( Module_expression_binding.occurrence_resolution occurrence,
        function_declaration )
    with
    | Module_expression_binding.Module_binding publication, Some declaration ->
        not (function_declaration_matches_publication declaration publication)
    | _, _ -> false
  then Error "bound direct function declaration does not match its publication"
  else
    match
      make_identifier_value ~resolved_type ~shape ~array_rank
        ?function_declaration ?function_address_path ()
    with
    | Error _ as error -> error
    | Ok value ->
        Ok
          (Bound_identifier_expression
             {
               bound_identifier_occurrence_ = occurrence;
               bound_identifier_type_ = value.identifier_value_type_;
               bound_identifier_shape_ = value.identifier_value_shape_;
               bound_identifier_array_rank_ = value.identifier_value_array_rank_;
               bound_identifier_function_declaration_ =
                 value.identifier_value_function_declaration_;
               bound_identifier_function_address_path_ =
                 value.identifier_value_function_address_path_;
             })

let make_top_level_bound_identifier_argument_expression ~occurrence =
  if
    String.equal
      (Top_level_outer_expression_binding.occurrence_name occurrence)
      ""
  then Error "bound top-level identifier cannot have an empty name"
  else
    Ok
      (Top_level_bound_identifier_expression
         { top_level_bound_identifier_occurrence_ = occurrence })

let argument_expression_kind expression = expression.expression_kind
let argument_expression_origin expression = expression.expression_origin

let make_argument ~index ~kind ~expression ~origin =
  if index < 0 then Error "call argument index cannot be negative"
  else
    match (kind, expression) with
    | Provided, Some _ | Omitted, None -> Ok { index; kind; expression; origin }
    | Provided, None -> Error "provided call argument has no expression"
    | Omitted, Some _ -> Error "omitted call argument has an expression"

let make_callable ~return_type ~function_pointer =
  { callable_return_type_ = return_type; callable_pointer_ = function_pointer }

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
    ?(callee_form = Identifier_callee) ?callable ?computed_callee ~origin
    ~syntax (arguments : argument list) =
  if index < 0 then Error "function call index cannot be negative"
  else if callee_occurrence_index < 0 then
    Error "function call callee occurrence index cannot be negative"
  else if String.equal callee_name "" then
    Error "function call callee name cannot be empty"
  else if
    match callee_form with
    | Identifier_callee -> false
    | Dereferenced_identifier_callee depth -> depth <= 0
    | Member_callee -> false
  then Error "function call callee dereference depth must be positive"
  else if
    match (callee_form, computed_callee) with
    | Member_callee, None -> true
    | (Identifier_callee | Dereferenced_identifier_callee _), Some _ -> true
    | Member_callee, Some _
    | (Identifier_callee | Dereferenced_identifier_callee _), None -> false
  then Error "function call computed callee does not match its callee form"
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
            callee_form;
            callable;
            computed_callee;
            origin;
            syntax;
            arguments;
          }

let make_return ~index ~keyword_origin ~expression ~origin =
  if index < 0 then Error "function return index cannot be negative"
  else if not (valid_origin keyword_origin) then
    Error "function return keyword has an invalid source origin"
  else if not (valid_origin origin) then
    Error "function return statement has an invalid source origin"
  else Ok { index; keyword_origin; expression; origin }

let make_condition ~index ~role ~keyword_origin ~expression ~origin =
  if index < 0 then Error "function condition index cannot be negative"
  else if not (valid_origin keyword_origin) then
    Error "function condition keyword has an invalid source origin"
  else if not (valid_origin origin) then
    Error "function condition statement has an invalid source origin"
  else Ok { index; role; keyword_origin; expression; origin }

let make_selector ~index ~mode ~keyword_origin ~expression ~origin =
  if index < 0 then Error "function switch selector index cannot be negative"
  else if not (valid_origin keyword_origin) then
    Error "function switch keyword has an invalid source origin"
  else if not (valid_origin origin) then
    Error "function switch statement has an invalid source origin"
  else Ok { index; mode; keyword_origin; expression; origin }

let make_expression_statement ~index ~expression ~origin =
  if index < 0 then
    Error "function expression statement index cannot be negative"
  else if not (valid_origin origin) then
    Error "function expression statement has an invalid source origin"
  else Ok { index; expression; origin }

let make_implicit_output_argument ~index ~leading_comma_origin ~expression
    ~origin =
  if index < 0 then Error "implicit output argument index cannot be negative"
  else if not (valid_origin leading_comma_origin) then
    Error "implicit output argument comma has an invalid source origin"
  else if not (valid_origin origin) then
    Error "implicit output argument has an invalid source origin"
  else Ok { index; leading_comma_origin; expression; origin }

let validate_implicit_output_argument_indexes arguments =
  let rec loop expected = function
    | [] -> Ok ()
    | (argument : implicit_output_argument) :: rest ->
        if argument.index <> expected then
          Error "implicit output argument indexes are not contiguous"
        else loop (expected + 1) rest
  in
  loop 0 arguments

let make_implicit_output ~index ~target ~marker_origin ~fixed_source
    ~fixed_expression ~arguments ~origin =
  if index < 0 then Error "function implicit output index cannot be negative"
  else if not (valid_origin marker_origin) then
    Error "function implicit output marker has an invalid source origin"
  else if not (valid_origin origin) then
    Error "function implicit output statement has an invalid source origin"
  else if target = Put_chars_output && arguments <> [] then
    Error "implicit PutChars output cannot have variadic arguments"
  else
    match validate_implicit_output_argument_indexes arguments with
    | Error _ as error -> error
    | Ok () ->
        Ok
          {
            index;
            target;
            marker_origin;
            fixed_source;
            fixed_expression;
            arguments;
            origin;
          }

let make_ranged_case_pattern ~start_expression ~ellipsis_origin ~end_expression
    =
  if not (valid_origin ellipsis_origin) then
    Error "function switch case ellipsis has an invalid source origin"
  else Ok (Ranged_case { start_expression; ellipsis_origin; end_expression })

let make_switch_case ~index ~keyword_origin ~pattern ~origin =
  if index < 0 then Error "function switch case index cannot be negative"
  else if not (valid_origin keyword_origin) then
    Error "function switch case keyword has an invalid source origin"
  else if not (valid_origin origin) then
    Error "function switch case label has an invalid source origin"
  else
    match pattern with
    | Ranged_case { ellipsis_origin; _ } when not (valid_origin ellipsis_origin)
      -> Error "function switch case ellipsis has an invalid source origin"
    | Implicit_case | Single_case _ | Ranged_case _ ->
        Ok { index; keyword_origin; pattern; origin }

let validate_condition_indexes conditions =
  let rec loop expected = function
    | [] -> Ok ()
    | (condition : condition_input) :: rest ->
        if condition.index <> expected then
          Error "function condition indexes are not contiguous"
        else loop (expected + 1) rest
  in
  loop 0 conditions

let validate_selector_indexes selectors =
  let rec loop expected = function
    | [] -> Ok ()
    | (selector : selector_input) :: rest ->
        if selector.index <> expected then
          Error "function switch selector indexes are not contiguous"
        else loop (expected + 1) rest
  in
  loop 0 selectors

let validate_expression_statement_indexes statements =
  let rec loop expected = function
    | [] -> Ok ()
    | (statement : expression_statement_input) :: rest ->
        if statement.index <> expected then
          Error "function expression statement indexes are not contiguous"
        else loop (expected + 1) rest
  in
  loop 0 statements

let validate_implicit_output_indexes outputs =
  let rec loop expected = function
    | [] -> Ok ()
    | (output : implicit_output_input) :: rest ->
        if output.index <> expected then
          Error "function implicit output indexes are not contiguous"
        else loop (expected + 1) rest
  in
  loop 0 outputs

let validate_switch_case_indexes cases =
  let rec loop expected = function
    | [] -> Ok ()
    | (case_ : switch_case_input) :: rest ->
        if case_.index <> expected then
          Error "function switch case indexes are not contiguous"
        else loop (expected + 1) rest
  in
  loop 0 cases

let validate_return_indexes returns =
  let rec loop expected = function
    | [] -> Ok ()
    | (return_ : return_input) :: rest ->
        if return_.index <> expected then
          Error "function return indexes are not contiguous"
        else loop (expected + 1) rest
  in
  loop 0 returns

let make_function ~symbol ~scope ~item_index ?(expression_statements = [])
    ?(implicit_outputs = []) ?(conditions = []) ?(selectors = [])
    ?(switch_cases = []) ?(returns = []) (calls : call list) :
    (function_input, string) result =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error "function call owner is not a function"
  else if Symbol_table.scope_kind scope <> Symbol_table.Function then
    Error "function call owner does not use a function scope"
  else if item_index < 0 then
    Error "function call owner item index cannot be negative"
  else
    match validate_expression_statement_indexes expression_statements with
    | Error _ as error -> error
    | Ok () -> (
        match validate_implicit_output_indexes implicit_outputs with
        | Error _ as error -> error
        | Ok () -> (
            match validate_condition_indexes conditions with
            | Error _ as error -> error
            | Ok () -> (
                match validate_selector_indexes selectors with
                | Error _ as error -> error
                | Ok () -> (
                    match validate_switch_case_indexes switch_cases with
                    | Error _ as error -> error
                    | Ok () -> (
                        match validate_return_indexes returns with
                        | Error _ as error -> error
                        | Ok () ->
                            Ok
                              ({
                                 symbol;
                                 scope;
                                 item_index;
                                 calls;
                                 expression_statements;
                                 implicit_outputs;
                                 conditions;
                                 selectors;
                                 switch_cases;
                                 returns;
                               }
                                : function_input))))))

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

let rec validate_argument_expression table parent visible declarations
    compilation_mode expression =
  match argument_expression_kind expression with
  | Parenthesized_expression grouped ->
      validate_argument_expression table parent visible declarations
        compilation_mode grouped
  | Prefix_expression prefix ->
      validate_argument_expression table parent visible declarations
        compilation_mode prefix.prefix_operand
  | Postfix_expression postfix ->
      validate_argument_expression table parent visible declarations
        compilation_mode postfix.postfix_operand
  | Binary_expression binary -> (
      match
        validate_argument_expression table parent visible declarations
          compilation_mode binary.binary_left
      with
      | Error _ as error -> error
      | Ok () ->
          validate_argument_expression table parent visible declarations
            compilation_mode binary.binary_right)
  | Index_expression index -> (
      match
        validate_argument_expression table parent visible declarations
          compilation_mode index.index_base
      with
      | Error _ as error -> error
      | Ok () ->
          validate_argument_expression table parent visible declarations
            compilation_mode index.index_value)
  | Member_access_expression member ->
      validate_argument_expression table parent visible declarations
        compilation_mode member.member_base
  | Postfix_cast_expression (operand, target) -> (
      match validate_cast_target table parent visible target with
      | Error _ as error -> error
      | Ok () ->
          validate_argument_expression table parent visible declarations
            compilation_mode operand)
  | Bound_identifier_expression identifier -> (
      let occurrence = bound_identifier_occurrence identifier in
      let resolved_type = bound_identifier_type identifier in
      if
        not
          (valid_origin
             (Module_expression_binding.occurrence_origin occurrence))
      then Error (invalid_input "bound call argument has an invalid origin")
      else if
        expression.expression_origin
        <> Module_expression_binding.occurrence_origin occurrence
      then
        Error
          (invalid_input
             "bound call argument origin does not match its occurrence")
      else if
        match
          ( Module_expression_binding.occurrence_resolution occurrence,
            identifier.bound_identifier_shape_ )
        with
        | Module_expression_binding.Local_binding _, Direct_function_value ->
            true
        | Module_expression_binding.Local_binding _, _ -> false
        | ( Module_expression_binding.Module_binding publication,
            Direct_function_value ) ->
            Module_expression_binding.publication_kind publication
            <> Module_expression_binding.Function
        | Module_expression_binding.Module_binding publication, _ ->
            Module_expression_binding.publication_kind publication
            <> Module_expression_binding.Global_variable
        | Module_expression_binding.Outer_candidate, _ -> true
      then
        Error
          (invalid_input
             "bound call argument occurrence is not a typed value binding")
      else if
        identifier.bound_identifier_shape_ = Array_value
        && identifier.bound_identifier_array_rank_ = 0
      then Error (invalid_input "bound array call argument has no dimensions")
      else if
        identifier.bound_identifier_shape_ <> Array_value
        && identifier.bound_identifier_array_rank_ <> 0
      then Error (invalid_input "bound nonarray call argument has dimensions")
      else if identifier.bound_identifier_shape_ = Direct_function_value then
        match Module_expression_binding.occurrence_resolution occurrence with
        | Module_expression_binding.Module_binding publication -> (
            let source =
              Module_expression_binding.publication_source_symbol publication
            in
            let expected =
              Int_map.find_opt (symbol_number source) declarations
            in
            match
              ( expected,
                identifier.bound_identifier_function_declaration_,
                identifier.bound_identifier_function_address_path_ )
            with
            | Some expected, Some declaration, Some path
              when expected == declaration -> (
                match
                  direct_function_address_path compilation_mode declaration
                with
                | Ok expected_path when expected_path = path -> Ok ()
                | Ok _ ->
                    Error
                      (invalid_input
                         "bound direct function has the wrong address path")
                | Error message -> Error (invalid_input message))
            | Some _, Some _, Some _ ->
                Error
                  (invalid_input
                     "bound direct function uses a foreign resolved declaration")
            | None, _, _ ->
                Error
                  (invalid_input
                     "bound direct function has no resolved declaration")
            | _, _, _ ->
                Error
                  (invalid_input
                     "bound direct function has incomplete address metadata"))
        | Module_expression_binding.Local_binding _
        | Module_expression_binding.Outer_candidate ->
            Error
              (invalid_input "bound direct function has no module publication")
      else
        match Type.base resolved_type with
        | Type.Primitive _ -> Ok ()
        | Type.Aggregate symbol ->
            if not (Symbol_table.owns_symbol table symbol) then
              Error
                (invalid_input
                   "bound call argument type belongs to another symbol table")
            else if not (symbol_in_scope symbol parent) then
              Error
                (invalid_input "bound call argument type has the wrong scope")
            else if
              not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type)
            then
              Error
                (invalid_input "bound call argument type is not an aggregate")
            else Ok ())
  | Top_level_bound_identifier_expression _ ->
      Error
        (invalid_input
           "function call input contains a top-level identifier binding")
  | Integer_literal
  | Float_literal
  | Character_literal
  | String_literal
  | Unresolved_expression _ -> Ok ()

let validate_callable table parent callable =
  let validate_type role type_ =
    match Type.base type_ with
    | Type.Primitive _ -> Ok ()
    | Type.Aggregate symbol ->
        if not (Symbol_table.owns_symbol table symbol) then
          Error (invalid_input (role ^ " belongs to another symbol table"))
        else if not (symbol_in_scope symbol parent) then
          Error (invalid_input (role ^ " has the wrong module scope"))
        else if
          not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type)
        then Error (invalid_input (role ^ " is not an aggregate type"))
        else Ok ()
  in
  let rec validate_signature signature_ =
    let rec parameters = function
      | [] -> Ok ()
      | parameter :: rest -> (
          match
            parameter |> Function_type_resolution.parameter_type_reference
            |> Type_reference.resolved_type
            |> validate_type "callback parameter type"
          with
          | Error _ as error -> error
          | Ok () -> (
              match
                Function_type_resolution.parameter_declarator_kind parameter
              with
              | Function_type_resolution.Object -> parameters rest
              | Function_type_resolution.Function_pointer pointer -> (
                  match
                    pointer
                    |> Function_type_resolution.function_pointer_signature
                    |> validate_signature
                  with
                  | Error _ as error -> error
                  | Ok () -> parameters rest)))
    in
    parameters (Function_type_resolution.signature_parameters signature_)
  in
  match
    callable |> callable_return_type |> Type_reference.resolved_type
    |> validate_type "callback return type"
  with
  | Error _ as error -> error
  | Ok () -> validate_signature (callable_signature callable)

let validate_argument_expressions table parent visible declarations
    compilation_mode calls =
  let rec arguments (values : argument list) =
    match values with
    | [] -> Ok ()
    | argument :: rest -> (
        match argument.expression with
        | None -> arguments rest
        | Some expression -> (
            match
              validate_argument_expression table parent visible declarations
                compilation_mode expression
            with
            | Error _ as error -> error
            | Ok () -> arguments rest))
  in
  let rec loop = function
    | [] -> Ok ()
    | (call : call) :: rest -> (
        match
          match call.computed_callee with
          | None -> Ok ()
          | Some expression ->
              validate_argument_expression table parent visible declarations
                compilation_mode expression
        with
        | Error _ as error -> error
        | Ok () -> (
            match
              match call.callable with
              | None -> Ok ()
              | Some callable -> validate_callable table parent callable
            with
            | Error _ as error -> error
            | Ok () -> (
                match arguments call.arguments with
                | Error _ as error -> error
                | Ok () -> loop rest)))
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
    | (call : call) :: rest -> (
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

let rec validate_bound_occurrences occurrence_by_index expression =
  match argument_expression_kind expression with
  | Parenthesized_expression grouped ->
      validate_bound_occurrences occurrence_by_index grouped
  | Prefix_expression prefix ->
      validate_bound_occurrences occurrence_by_index prefix.prefix_operand
  | Postfix_expression postfix ->
      validate_bound_occurrences occurrence_by_index postfix.postfix_operand
  | Binary_expression binary -> (
      match
        validate_bound_occurrences occurrence_by_index binary.binary_left
      with
      | Error _ as error -> error
      | Ok () ->
          validate_bound_occurrences occurrence_by_index binary.binary_right)
  | Index_expression index -> (
      match validate_bound_occurrences occurrence_by_index index.index_base with
      | Error _ as error -> error
      | Ok () ->
          validate_bound_occurrences occurrence_by_index index.index_value)
  | Member_access_expression member ->
      validate_bound_occurrences occurrence_by_index member.member_base
  | Postfix_cast_expression (operand, _) ->
      validate_bound_occurrences occurrence_by_index operand
  | Bound_identifier_expression identifier -> (
      let occurrence = bound_identifier_occurrence identifier in
      let index = Module_expression_binding.occurrence_index occurrence in
      match Int_map.find_opt index occurrence_by_index with
      | Some expected when expected == occurrence -> Ok ()
      | Some _ ->
          Error
            (invalid_input
               "bound call argument uses a different expression occurrence")
      | None ->
          Error
            (invalid_input
               "bound call argument occurrence does not belong to its function")
      )
  | Top_level_bound_identifier_expression _ ->
      Error
        (invalid_input
           "function call input contains a top-level identifier binding")
  | Integer_literal
  | Float_literal
  | Character_literal
  | String_literal
  | Unresolved_expression _ -> Ok ()

let validate_call_bound_occurrences calls occurrences =
  let occurrence_by_index =
    List.fold_left
      (fun map occurrence ->
        Int_map.add
          (Module_expression_binding.occurrence_index occurrence)
          occurrence map)
      Int_map.empty occurrences
  in
  let rec arguments (values : argument list) =
    match values with
    | [] -> Ok ()
    | argument :: rest -> (
        match argument.expression with
        | None -> arguments rest
        | Some expression -> (
            match validate_bound_occurrences occurrence_by_index expression with
            | Error _ as error -> error
            | Ok () -> arguments rest))
  in
  let rec loop = function
    | [] -> Ok ()
    | call :: rest -> (
        match
          match call.computed_callee with
          | None -> Ok ()
          | Some expression ->
              validate_bound_occurrences occurrence_by_index expression
        with
        | Error _ as error -> error
        | Ok () -> (
            match arguments call.arguments with
            | Error _ as error -> error
            | Ok () -> loop rest))
  in
  loop calls

let validate_returns table parent visible declarations compilation_mode returns
    occurrences =
  let occurrence_by_index =
    List.fold_left
      (fun map occurrence ->
        Int_map.add
          (Module_expression_binding.occurrence_index occurrence)
          occurrence map)
      Int_map.empty occurrences
  in
  let rec loop expected = function
    | [] -> Ok ()
    | (return_ : return_input) :: rest -> (
        if return_.index <> expected then
          Error (invalid_input "function return indexes are not contiguous")
        else
          match return_.expression with
          | None -> loop (expected + 1) rest
          | Some expression -> (
              match
                validate_argument_expression table parent visible declarations
                  compilation_mode expression
              with
              | Error _ as error -> error
              | Ok () -> (
                  match
                    validate_bound_occurrences occurrence_by_index expression
                  with
                  | Error _ as error -> error
                  | Ok () -> loop (expected + 1) rest)))
  in
  loop 0 returns

let validate_expression_statements table parent visible declarations
    compilation_mode statements occurrences =
  let occurrence_by_index =
    List.fold_left
      (fun map occurrence ->
        Int_map.add
          (Module_expression_binding.occurrence_index occurrence)
          occurrence map)
      Int_map.empty occurrences
  in
  let rec loop expected = function
    | [] -> Ok ()
    | (statement : expression_statement_input) :: rest -> (
        if statement.index <> expected then
          Error
            (invalid_input
               "function expression statement indexes are not contiguous")
        else
          match
            validate_argument_expression table parent visible declarations
              compilation_mode statement.expression
          with
          | Error _ as error -> error
          | Ok () -> (
              match
                validate_bound_occurrences occurrence_by_index
                  statement.expression
              with
              | Error _ as error -> error
              | Ok () -> loop (expected + 1) rest))
  in
  loop 0 statements

let validate_implicit_outputs table parent visible declarations compilation_mode
    outputs occurrences =
  let occurrence_by_index =
    List.fold_left
      (fun map occurrence ->
        Int_map.add
          (Module_expression_binding.occurrence_index occurrence)
          occurrence map)
      Int_map.empty occurrences
  in
  let validate_expression expression =
    match
      validate_argument_expression table parent visible declarations
        compilation_mode expression
    with
    | Error _ as error -> error
    | Ok () -> validate_bound_occurrences occurrence_by_index expression
  in
  let rec validate_arguments expected = function
    | [] -> Ok ()
    | (argument : implicit_output_argument) :: rest -> (
        if argument.index <> expected then
          Error
            (invalid_input "implicit output argument indexes are not contiguous")
        else
          match validate_expression argument.expression with
          | Error _ as error -> error
          | Ok () -> validate_arguments (expected + 1) rest)
  in
  let rec loop expected = function
    | [] -> Ok ()
    | (output : implicit_output_input) :: rest -> (
        if output.index <> expected then
          Error
            (invalid_input "function implicit output indexes are not contiguous")
        else if output.target = Put_chars_output && output.arguments <> [] then
          Error
            (invalid_input
               "implicit PutChars output cannot have variadic arguments")
        else
          match validate_expression output.fixed_expression with
          | Error _ as error -> error
          | Ok () -> (
              match validate_arguments 0 output.arguments with
              | Error _ as error -> error
              | Ok () -> loop (expected + 1) rest))
  in
  loop 0 outputs

let validate_conditions table parent visible declarations compilation_mode
    conditions occurrences =
  let occurrence_by_index =
    List.fold_left
      (fun map occurrence ->
        Int_map.add
          (Module_expression_binding.occurrence_index occurrence)
          occurrence map)
      Int_map.empty occurrences
  in
  let rec loop expected = function
    | [] -> Ok ()
    | (condition : condition_input) :: rest -> (
        if condition.index <> expected then
          Error (invalid_input "function condition indexes are not contiguous")
        else
          match
            validate_argument_expression table parent visible declarations
              compilation_mode condition.expression
          with
          | Error _ as error -> error
          | Ok () -> (
              match
                validate_bound_occurrences occurrence_by_index
                  condition.expression
              with
              | Error _ as error -> error
              | Ok () -> loop (expected + 1) rest))
  in
  loop 0 conditions

let validate_selectors table parent visible declarations compilation_mode
    selectors occurrences =
  let occurrence_by_index =
    List.fold_left
      (fun map occurrence ->
        Int_map.add
          (Module_expression_binding.occurrence_index occurrence)
          occurrence map)
      Int_map.empty occurrences
  in
  let rec loop expected = function
    | [] -> Ok ()
    | (selector : selector_input) :: rest -> (
        if selector.index <> expected then
          Error
            (invalid_input "function switch selector indexes are not contiguous")
        else
          match
            validate_argument_expression table parent visible declarations
              compilation_mode selector.expression
          with
          | Error _ as error -> error
          | Ok () -> (
              match
                validate_bound_occurrences occurrence_by_index
                  selector.expression
              with
              | Error _ as error -> error
              | Ok () -> loop (expected + 1) rest))
  in
  loop 0 selectors

let validate_switch_cases table parent visible declarations compilation_mode
    cases occurrences =
  let occurrence_by_index =
    List.fold_left
      (fun map occurrence ->
        Int_map.add
          (Module_expression_binding.occurrence_index occurrence)
          occurrence map)
      Int_map.empty occurrences
  in
  let validate_expression expression =
    match
      validate_argument_expression table parent visible declarations
        compilation_mode expression
    with
    | Error _ as error -> error
    | Ok () -> validate_bound_occurrences occurrence_by_index expression
  in
  let validate_pattern = function
    | Implicit_case -> Ok ()
    | Single_case expression -> validate_expression expression
    | Ranged_case { start_expression; end_expression; _ } -> (
        match validate_expression start_expression with
        | Error _ as error -> error
        | Ok () -> validate_expression end_expression)
  in
  let rec loop expected = function
    | [] -> Ok ()
    | (case_ : switch_case_input) :: rest -> (
        if case_.index <> expected then
          Error
            (invalid_input "function switch case indexes are not contiguous")
        else
          match validate_pattern case_.pattern with
          | Error _ as error -> error
          | Ok () -> loop (expected + 1) rest)
  in
  loop 0 cases

let validate_function_input table parent visible declarations compilation_mode
    expected (input : function_input) =
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
    match
      validate_argument_expressions table parent visible declarations
        compilation_mode input.calls
    with
    | Error _ as error -> error
    | Ok () -> (
        let occurrences =
          Module_expression_binding.function_occurrences expected
        in
        match validate_call_bound_occurrences input.calls occurrences with
        | Error _ as error -> error
        | Ok () -> (
            match validate_calls input.calls occurrences with
            | Error _ as error -> error
            | Ok () -> (
                match
                  validate_expression_statements table parent visible
                    declarations compilation_mode input.expression_statements
                    occurrences
                with
                | Error _ as error -> error
                | Ok () -> (
                    match
                      validate_implicit_outputs table parent visible
                        declarations compilation_mode input.implicit_outputs
                        occurrences
                    with
                    | Error _ as error -> error
                    | Ok () -> (
                        match
                          validate_conditions table parent visible declarations
                            compilation_mode input.conditions occurrences
                        with
                        | Error _ as error -> error
                        | Ok () -> (
                            match
                              validate_selectors table parent visible
                                declarations compilation_mode input.selectors
                                occurrences
                            with
                            | Error _ as error -> error
                            | Ok () -> (
                                match
                                  validate_switch_cases table parent visible
                                    declarations compilation_mode
                                    input.switch_cases occurrences
                                with
                                | Error _ as error -> error
                                | Ok () ->
                                    validate_returns table parent visible
                                      declarations compilation_mode
                                      input.returns occurrences)))))))

let validate_function_inputs table parent expressions declarations
    compilation_mode inputs =
  let rec pair visible publications expected inputs =
    match (expected, inputs) with
    | [], [] -> Ok ()
    | expected :: expected_rest, (input : function_input) :: input_rest -> (
        let visible, publications =
          publish_aggregates_before visible publications input.item_index
        in
        match
          validate_function_input table parent visible declarations
            compilation_mode expected input
        with
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

let bind_arguments call ~parameters ~is_variadic =
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
      match is_variadic with
      | false -> (
          match extras with
          | [] -> Ok (fixed_arguments, [], 0L)
          | argument :: _ ->
              Error
                (extra_fixed_argument call argument (List.length parameters)))
      | true ->
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

let bind_direct_arguments call header =
  let parameters =
    Function_type_resolution.function_signature header
    |> Function_type_resolution.signature_parameters
  in
  bind_arguments call ~parameters
    ~is_variadic:
      (Option.is_some
         (Function_type_resolution.function_variadic_bindings header))

let bind_indirect_arguments call callable =
  let signature = callable_signature callable in
  bind_arguments call
    ~parameters:(Function_type_resolution.signature_parameters signature)
    ~is_variadic:
      (Option.is_some
         (Function_type_resolution.signature_variadic_origin signature))

let same_publication_target publication declaration =
  function_declaration_matches_publication declaration publication

type computed_type = { type_ : Type.t; array_rank : int }

let rec computed_expression_type members ~before_item_index expression =
  let invalid origin message = Error (invalid_input_at origin message) in
  match argument_expression_kind expression with
  | Bound_identifier_expression identifier ->
      Ok
        {
          type_ = bound_identifier_type identifier;
          array_rank = bound_identifier_array_rank identifier;
        }
  | Parenthesized_expression grouped ->
      computed_expression_type members ~before_item_index grouped
  | Postfix_cast_expression (_, target) ->
      Ok { type_ = Type_reference.resolved_type target; array_rank = 0 }
  | Prefix_expression prefix -> (
      let origin = prefix_operator_origin prefix in
      match
        computed_expression_type members ~before_item_index
          (prefix_operand prefix)
      with
      | Error _ as error -> error
      | Ok source -> (
          match prefix_operator prefix with
          | Dereference -> (
              if source.array_rank > 0 then
                Ok { source with array_rank = source.array_rank - 1 }
              else
                match Type.dereference source.type_ with
                | Ok type_ -> Ok { type_; array_rank = 0 }
                | Error _ ->
                    invalid origin
                      "cannot dereference a nonpointer callback base")
          | Address_of -> (
              match Type.pointer_to source.type_ with
              | Ok type_ -> Ok { type_; array_rank = 0 }
              | Error message -> invalid origin message)
          | Unary_plus
          | Unary_minus
          | Logical_not
          | Bitwise_not
          | Pre_increment
          | Pre_decrement ->
              invalid origin
                "callback member base does not have a source type after this \
                 unary operator"))
  | Index_expression index -> (
      match
        computed_expression_type members ~before_item_index (index_base index)
      with
      | Error _ as error -> error
      | Ok source when source.array_rank > 0 ->
          Ok { source with array_rank = source.array_rank - 1 }
      | Ok source -> (
          match Type.dereference source.type_ with
          | Ok type_ -> Ok { type_; array_rank = 0 }
          | Error _ ->
              invalid
                (index_opening_origin index)
                "callback member index base is neither an array nor a pointer"))
  | Member_access_expression member -> (
      match resolve_computed_member members ~before_item_index member with
      | Error _ as error -> error
      | Ok lookup ->
          let indexed = Aggregate_member_index.lookup_member lookup in
          let layout = Aggregate_member_index.member_layout indexed in
          Ok
            {
              type_ = Aggregate_member_index.member_type indexed;
              array_rank = List.length layout.dimensions;
            })
  | Postfix_expression _
  | Binary_expression _
  | Top_level_bound_identifier_expression _
  | Integer_literal
  | Float_literal
  | Character_literal
  | String_literal
  | Unresolved_expression _ ->
      invalid
        (argument_expression_origin expression)
        "callback member base does not have a statically resolved source type"

and resolve_computed_member members ~before_item_index member =
  let operator_origin = member_operator_origin member in
  let member_origin = member_origin member in
  let invalid origin message = Error (invalid_input_at origin message) in
  match
    computed_expression_type members ~before_item_index (member_base member)
  with
  | Error _ as error -> error
  | Ok base when base.array_rank <> 0 ->
      invalid operator_origin "member access base is an array"
  | Ok base -> (
      let aggregate_type =
        match member_access_kind member with
        | Direct_member ->
            if Type.pointer_depth base.type_ = 0 then Ok base.type_
            else
              invalid operator_origin
                "direct member access requires an aggregate object, not a \
                 pointer"
        | Pointer_member -> (
            match Type.dereference base.type_ with
            | Error _ ->
                invalid operator_origin
                  "pointer member access requires a pointer to an aggregate"
            | Ok pointee when Type.pointer_depth pointee = 0 -> Ok pointee
            | Ok _ ->
                invalid operator_origin
                  "pointer member access leaves another pointer layer before \
                   the aggregate")
      in
      match aggregate_type with
      | Error _ as error -> error
      | Ok aggregate_type -> (
          match Type.base aggregate_type with
          | Type.Primitive _ ->
              invalid operator_origin "member access base is not an aggregate"
          | Type.Aggregate aggregate_symbol -> (
              match
                Aggregate_member_index.find_aggregate members aggregate_symbol
              with
              | None ->
                  invalid member_origin
                    (Printf.sprintf
                       "aggregate `%s` has no completed member index"
                       (Symbol.name aggregate_symbol))
              | Some aggregate
                when Aggregate_member_index.aggregate_item_index aggregate
                     >= before_item_index ->
                  invalid member_origin
                    (Printf.sprintf
                       "aggregate `%s` is not complete before this member \
                        access"
                       (Symbol.name aggregate_symbol))
              | Some _ -> (
                  match
                    Aggregate_member_index.lookup members
                      ~aggregate:aggregate_symbol ~name:(member_name member)
                  with
                  | Error error ->
                      invalid member_origin
                        (Aggregate_member_index.error_message error)
                  | Ok None ->
                      invalid member_origin
                        (Printf.sprintf "aggregate `%s` has no member `%s`"
                           (Symbol.name aggregate_symbol)
                           (member_name member))
                  | Ok (Some lookup) -> Ok lookup))))

let rec resolve_member_callable members ~before_item_index computed =
  match argument_expression_kind computed with
  | Member_access_expression member -> (
      match resolve_computed_member members ~before_item_index member with
      | Error _ as error -> error
      | Ok lookup -> (
          let indexed = Aggregate_member_index.lookup_member lookup in
          match Aggregate_member_index.member_function_pointer indexed with
          | None ->
              Error
                (invalid_input_at (member_origin member)
                   (Printf.sprintf "member `%s` is not callable"
                      (member_name member)))
          | Some function_pointer ->
              Ok
                ( lookup,
                  make_callable
                    ~return_type:
                      (Aggregate_member_index.member_type_reference indexed)
                    ~function_pointer )))
  | Parenthesized_expression grouped ->
      resolve_member_callable members ~before_item_index grouped
  | _ ->
      Error
        (invalid_input_at
           (argument_expression_origin computed)
           "member call callee is not a member access expression")

let resolve_call ?members ~before_item_index types declarations occurrence
    (call : call) =
  let indirect_or_deferred reason =
    match call.callable with
    | None -> Ok (Deferred_call { call; occurrence; reason })
    | Some callable -> (
        match bind_indirect_arguments call callable with
        | Error _ as error -> error
        | Ok (fixed_arguments, variadic_arguments, variadic_count) ->
            Ok
              (Indirect_call
                 {
                   source = call;
                   occurrence;
                   callable;
                   member_lookup = None;
                   fixed_arguments;
                   variadic_arguments;
                   variadic_count;
                 }))
  in
  if call.callee_form = Member_callee then
    match (call.computed_callee, members) with
    | Some computed, None ->
        Ok
          (Deferred_call
             { call; occurrence; reason = Computed_member_callee computed })
    | Some computed, Some members -> (
        match resolve_member_callable members ~before_item_index computed with
        | Error _ as error -> error
        | Ok (member_lookup, callable) -> (
            match bind_indirect_arguments call callable with
            | Error _ as error -> error
            | Ok (fixed_arguments, variadic_arguments, variadic_count) ->
                Ok
                  (Indirect_call
                     {
                       source = call;
                       occurrence;
                       callable;
                       member_lookup = Some member_lookup;
                       fixed_arguments;
                       variadic_arguments;
                       variadic_count;
                     })))
    | None, _ -> Error (invalid_input "member call has no computed callee")
  else
    match Module_expression_binding.occurrence_resolution occurrence with
    | Module_expression_binding.Local_binding binding ->
        indirect_or_deferred (Local_callee binding)
    | Module_expression_binding.Outer_candidate ->
        Ok (Deferred_call { call; occurrence; reason = Outer_callee })
    | Module_expression_binding.Module_binding publication -> (
        match Module_expression_binding.publication_kind publication with
        | Module_expression_binding.Global_variable ->
            indirect_or_deferred (Global_callee publication)
        | Module_expression_binding.Aggregate ->
            Ok
              (Deferred_call
                 { call; occurrence; reason = Aggregate_callee publication })
        | Module_expression_binding.Function -> (
            if Option.is_some call.callable then
              Error
                (invalid_input
                   "direct function call unexpectedly carries a callback header")
            else if call.callee_form <> Identifier_callee then
              Ok
                (Deferred_call
                   { call; occurrence; reason = Global_callee publication })
            else
              let source =
                Module_expression_binding.publication_source_symbol publication
              in
              let number = symbol_number source in
              match
                ( Int_map.find_opt number types,
                  Int_map.find_opt number declarations )
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
                       "function call publication disagrees with function \
                        identity resolution")
              | None, _ | _, None ->
                  Error
                    (invalid_input
                       "function call publication has no active typed header")))

let resolve_function ?members types declarations expected
    (input : function_input) =
  let occurrences = Module_expression_binding.function_occurrences expected in
  let occurrence_by_index =
    List.fold_left
      (fun map occurrence ->
        Int_map.add
          (Module_expression_binding.occurrence_index occurrence)
          occurrence map)
      Int_map.empty occurrences
  in
  let typed_function = Int_map.find_opt (symbol_number input.symbol) types in
  let rec calls rev = function
    | [] -> (
        match typed_function with
        | None ->
            Error
              (invalid_input
                 "function return input has no matching typed declaration")
        | Some typed ->
            Ok
              {
                symbol = input.symbol;
                scope = input.scope;
                item_index = input.item_index;
                return_type =
                  Function_type_resolution.function_return_type typed;
                calls = List.rev rev;
                expression_statements = input.expression_statements;
                implicit_outputs = input.implicit_outputs;
                conditions = input.conditions;
                selectors = input.selectors;
                switch_cases = input.switch_cases;
                returns = input.returns;
              })
    | call :: rest -> (
        match
          Int_map.find_opt call.callee_occurrence_index occurrence_by_index
        with
        | None ->
            Error
              (invalid_input
                 "function call lost its validated callee occurrence")
        | Some occurrence -> (
            match
              resolve_call ?members ~before_item_index:input.item_index types
                declarations occurrence call
            with
            | Error _ as error -> error
            | Ok call -> calls (call :: rev) rest))
  in
  calls [] input.calls

let resolve_validated ?members types declarations expressions inputs =
  let rec pair functions_rev by_symbol expected inputs =
    match (expected, inputs) with
    | [], [] -> Ok (List.rev functions_rev, by_symbol)
    | expected :: expected_rest, input :: input_rest -> (
        match resolve_function ?members types declarations expected input with
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

let resolve ~table ~parent ?members ~function_types ~functions ~expressions
    inputs =
  if not (Symbol_table.owns_scope table parent) then
    Error (invalid_input "function call parent belongs to another symbol table")
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error (invalid_input "function call resolution requires a module scope")
  else if not (Module_expression_binding.owns_table expressions table) then
    Error
      (invalid_input "function call expressions belong to another symbol table")
  else if
    match members with
    | None -> false
    | Some members -> not (Aggregate_member_index.owns_table members table)
  then
    Error
      (invalid_input "aggregate member index belongs to another symbol table")
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
            match
              validate_function_inputs table parent expressions declarations
                (Function_resolution.compilation_mode functions)
                inputs
            with
            | Error _ as error -> error
            | Ok () -> (
                match
                  resolve_validated ?members types declarations expressions
                    inputs
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
