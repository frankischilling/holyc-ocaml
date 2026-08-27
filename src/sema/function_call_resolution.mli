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
  | Postfix_cast_expression
  | Call_expression

type defined_operand_kind = Defined_name | Defined_non_name

type defined_operand_resolution =
  | Defined_non_name_false
  | Defined_function_query of Function_expression_binding.query
  | Defined_top_level_name

type defined_expression

type prefix_operator =
  | Unary_plus
  | Unary_minus
  | Logical_not
  | Bitwise_not
  | Dereference
  | Address_of
  | Pre_increment
  | Pre_decrement

type prefix_expression
type postfix_operator = Post_increment | Post_decrement
type postfix_expression
type binary_expression
type index_expression
type member_access_kind = Direct_member | Pointer_member
type member_expression

type identifier_value_shape =
  | Object_value
  | Array_value
  | Function_pointer_value
  | Direct_function_value

type identifier_value

type direct_function_address_path =
  | Jit_extern_slot
  | Jit_immediate
  | Aot_absolute
  | Reject_aot_extern
  | Reject_aot_import
  | Reject_internal

type bound_identifier
type top_level_bound_identifier

type argument_expression_kind =
  (* Literal payloads use the values consumed by TempleOS expression lowering:
      target [I64] values for integers and characters, raw IEEE-754 bits for
      [F64], and decoded bytes without storage or terminator policy for strings. *)
  | Integer_literal of int64
  | Float_literal of int64
  | Character_literal of int64
  | String_literal of string
  | Parenthesized_expression of argument_expression
  | Prefix_expression of prefix_expression
  | Postfix_expression of postfix_expression
  | Postfix_cast_expression of argument_expression * Type_reference.t
  | Binary_expression of binary_expression
  | Index_expression of index_expression
  | Member_access_expression of member_expression
  | Bound_identifier_expression of bound_identifier
  | Top_level_bound_identifier_expression of top_level_bound_identifier
  | Defined_expression of defined_expression
  | Unresolved_expression of unresolved_expression_kind

and argument_expression

type argument
type call
type callable

type condition_role =
  | If_condition
  | While_condition
  | Do_while_condition
  | For_condition

type condition_input
type selector_mode = Bounded_switch | No_bound_switch
type selector_input
type expression_statement_input
type implicit_output_target = Print_output | Put_chars_output

type implicit_output_fixed_source =
  | Marker_fixed_output
  | Following_expression_output

type implicit_output_argument
type implicit_output_input

type switch_case_pattern =
  | Implicit_case
  | Single_case of argument_expression
  | Ranged_case of {
      start_expression : argument_expression;
      ellipsis_origin : Symbol.origin;
      end_expression : argument_expression;
    }

type switch_case_input
type return_input
type function_input

val make_callable :
  return_type:Type_reference.t ->
  function_pointer:Function_type_resolution.function_pointer ->
  callable

val make_argument_expression :
  kind:argument_expression_kind -> origin:Symbol.origin -> argument_expression

val make_prefix_argument_expression :
  operator:prefix_operator ->
  operator_origin:Symbol.origin ->
  operand:argument_expression ->
  (argument_expression_kind, string) result

val make_postfix_argument_expression :
  operator:postfix_operator ->
  operator_origin:Symbol.origin ->
  operand:argument_expression ->
  (argument_expression_kind, string) result

val make_binary_argument_expression :
  operator:Generated.Intermediate_codes.t ->
  operator_origin:Symbol.origin ->
  left:argument_expression ->
  right:argument_expression ->
  (argument_expression_kind, string) result

val make_index_argument_expression :
  base:argument_expression ->
  opening_origin:Symbol.origin ->
  index:argument_expression ->
  closing_origin:Symbol.origin ->
  (argument_expression_kind, string) result

val make_member_argument_expression :
  base:argument_expression ->
  access_kind:member_access_kind ->
  operator_origin:Symbol.origin ->
  member_name:string ->
  member_origin:Symbol.origin ->
  (argument_expression_kind, string) result

val make_defined_argument_expression :
  operand_kind:defined_operand_kind ->
  operand_spelling:string ->
  operand_origin:Symbol.origin ->
  operand_resolution:defined_operand_resolution ->
  (argument_expression_kind, string) result
(** Retain the one source token inspected by ordinary HolyC [defined]. This
    constructor accepts source-ordered function-query evidence or an explicit
    top-level deferral; it does not perform a new symbol-table lookup. *)

val make_bound_identifier_argument_expression :
  occurrence:Module_expression_binding.occurrence ->
  resolved_type:Type.t ->
  shape:identifier_value_shape ->
  array_rank:int ->
  ?function_declaration:Function_resolution.resolved_declaration ->
  ?function_address_path:direct_function_address_path ->
  unit ->
  (argument_expression_kind, string) result

val make_top_level_bound_identifier_argument_expression :
  occurrence:Top_level_outer_expression_binding.occurrence ->
  (argument_expression_kind, string) result
(** Retain the exact module or outer binding selected for an identifier in an
    executable top-level statement. Type and value-shape analysis follows in a
    later pass. *)

val make_identifier_value :
  resolved_type:Type.t ->
  shape:identifier_value_shape ->
  array_rank:int ->
  ?function_declaration:Function_resolution.resolved_declaration ->
  ?function_address_path:direct_function_address_path ->
  unit ->
  (identifier_value, string) result

val global_identifier_value :
  Global_type_resolution.global -> (identifier_value, string) result

val direct_function_identifier_value :
  declaration:Function_resolution.resolved_declaration ->
  address_path:direct_function_address_path ->
  (identifier_value, string) result

val identifier_value_type : identifier_value -> Type.t
val identifier_value_shape : identifier_value -> identifier_value_shape
val identifier_value_array_rank : identifier_value -> int

val identifier_value_function_declaration :
  identifier_value -> Function_resolution.resolved_declaration option

val identifier_value_function_address_path :
  identifier_value -> direct_function_address_path option

val make_argument :
  index:int ->
  kind:argument_kind ->
  expression:argument_expression option ->
  origin:Symbol.origin ->
  (argument, string) result

val make_call :
  index:int ->
  callee_occurrence_index:int ->
  callee_name:string ->
  callee_origin:Symbol.origin ->
  ?callee_form:callee_form ->
  ?callable:callable ->
  ?computed_callee:argument_expression ->
  origin:Symbol.origin ->
  syntax:call_syntax ->
  argument list ->
  (call, string) result

val make_return :
  index:int ->
  keyword_origin:Symbol.origin ->
  expression:argument_expression option ->
  origin:Symbol.origin ->
  (return_input, string) result

val make_condition :
  index:int ->
  role:condition_role ->
  keyword_origin:Symbol.origin ->
  expression:argument_expression ->
  origin:Symbol.origin ->
  (condition_input, string) result

val make_selector :
  index:int ->
  mode:selector_mode ->
  keyword_origin:Symbol.origin ->
  expression:argument_expression ->
  origin:Symbol.origin ->
  (selector_input, string) result

val make_expression_statement :
  index:int ->
  expression:argument_expression ->
  origin:Symbol.origin ->
  (expression_statement_input, string) result

val make_implicit_output_argument :
  index:int ->
  leading_comma_origin:Symbol.origin ->
  expression:argument_expression ->
  origin:Symbol.origin ->
  (implicit_output_argument, string) result

val make_implicit_output :
  index:int ->
  target:implicit_output_target ->
  marker_origin:Symbol.origin ->
  fixed_source:implicit_output_fixed_source ->
  fixed_expression:argument_expression ->
  arguments:implicit_output_argument list ->
  origin:Symbol.origin ->
  (implicit_output_input, string) result

val make_ranged_case_pattern :
  start_expression:argument_expression ->
  ellipsis_origin:Symbol.origin ->
  end_expression:argument_expression ->
  (switch_case_pattern, string) result

val make_switch_case :
  index:int ->
  keyword_origin:Symbol.origin ->
  pattern:switch_case_pattern ->
  origin:Symbol.origin ->
  (switch_case_input, string) result

val make_function :
  symbol:Symbol.t ->
  scope:Symbol_table.scope ->
  item_index:int ->
  ?expression_statements:expression_statement_input list ->
  ?implicit_outputs:implicit_output_input list ->
  ?conditions:condition_input list ->
  ?selectors:selector_input list ->
  ?switch_cases:switch_case_input list ->
  ?returns:return_input list ->
  call list ->
  (function_input, string) result

type default_use

type fixed_value =
  | Provided_argument of argument
  | Declared_default of default_use

type fixed_argument
type direct_call
type indirect_call

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

type resolved_function
type t

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

type error

val resolve :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  ?members:Aggregate_member_index.t ->
  function_types:Function_type_resolution.t ->
  functions:Function_resolution.t ->
  expressions:Module_expression_binding.t ->
  function_input list ->
  (t, error) result
(** Resolve function-body calls. A module function receives the source header
    visible to the caller and its canonical joined identity. A supplied member
    index also resolves direct and pointer callbacks through their exact lookup
    and stored signature. Named aggregate types must carry the identity visible
    before the caller. Other computed callee categories remain explicit deferred
    calls. *)

val bind_direct_arguments :
  call ->
  Function_type_resolution.resolved_function ->
  (fixed_argument list * argument list * int64, error) result
(** Apply the checked [PrsFunCall] fixed-slot and variadic rules to one direct
    call and one source-visible header. This pure seam is shared by function
    bodies and executable top-level statements. *)

val bind_indirect_arguments :
  call ->
  callable ->
  (fixed_argument list * argument list * int64, error) result
(** Apply the checked [PrsFunCall] fixed-slot and variadic rules to one indirect
    call and its stored function-pointer signature. This pure seam is shared by
    function bodies and executable top-level statements. *)

val resolve_member_callable :
  Aggregate_member_index.t ->
  before_item_index:int ->
  argument_expression ->
  (Aggregate_member_index.lookup * callable, error) result
(** Resolve an aggregate callback member and require its callee to consume every
    declared array dimension exactly once. Parentheses do not change the member
    or subscript chain. *)

val validate_member_callable :
  argument_expression ->
  Aggregate_member_index.lookup ->
  (callable, error) result
(** Check an already resolved member lookup against the exact source subscript
    chain. This keeps function-body and executable top-level calls on the same
    array-dimension rule. *)

val member_callable_base_expression :
  argument_expression -> (argument_expression, error) result
(** Return the member expression at the bottom of an exact parenthesized and
    subscripted member-callee chain. *)

val functions : t -> resolved_function list
val expressions : t -> Module_expression_binding.t
val find_function : t -> Symbol.t -> resolved_function option
val compilation_mode : t -> Function_resolution.compilation_mode
val owns_table : t -> Symbol_table.t -> bool
val function_symbol : resolved_function -> Symbol.t
val function_scope : resolved_function -> Symbol_table.scope
val function_item_index : resolved_function -> int
val function_return_type : resolved_function -> Type_reference.t
val function_calls : resolved_function -> call_resolution list

val function_expression_statements :
  resolved_function -> expression_statement_input list

val function_implicit_outputs : resolved_function -> implicit_output_input list
val function_conditions : resolved_function -> condition_input list
val function_selectors : resolved_function -> selector_input list
val function_switch_cases : resolved_function -> switch_case_input list
val function_returns : resolved_function -> return_input list
val call_index : call -> int
val call_callee_occurrence_index : call -> int
val call_callee_name : call -> string
val call_callee_origin : call -> Symbol.origin
val call_callee_form : call -> callee_form
val call_callable : call -> callable option
val call_computed_callee : call -> argument_expression option
val call_origin : call -> Symbol.origin
val call_syntax : call -> call_syntax
val call_arguments : call -> argument list
val condition_index : condition_input -> int
val condition_role : condition_input -> condition_role
val condition_keyword_origin : condition_input -> Symbol.origin
val condition_expression : condition_input -> argument_expression
val condition_origin : condition_input -> Symbol.origin
val selector_index : selector_input -> int
val selector_mode : selector_input -> selector_mode
val selector_keyword_origin : selector_input -> Symbol.origin
val selector_expression : selector_input -> argument_expression
val selector_origin : selector_input -> Symbol.origin
val expression_statement_index : expression_statement_input -> int

val expression_statement_expression :
  expression_statement_input -> argument_expression

val expression_statement_origin : expression_statement_input -> Symbol.origin
val implicit_output_index : implicit_output_input -> int
val implicit_output_target : implicit_output_input -> implicit_output_target
val implicit_output_marker_origin : implicit_output_input -> Symbol.origin

val implicit_output_fixed_source :
  implicit_output_input -> implicit_output_fixed_source

val implicit_output_fixed_expression :
  implicit_output_input -> argument_expression

val implicit_output_arguments :
  implicit_output_input -> implicit_output_argument list

val implicit_output_origin : implicit_output_input -> Symbol.origin
val implicit_output_argument_index : implicit_output_argument -> int

val implicit_output_argument_leading_comma_origin :
  implicit_output_argument -> Symbol.origin

val implicit_output_argument_expression :
  implicit_output_argument -> argument_expression

val implicit_output_argument_origin : implicit_output_argument -> Symbol.origin
val switch_case_index : switch_case_input -> int
val switch_case_keyword_origin : switch_case_input -> Symbol.origin
val switch_case_pattern : switch_case_input -> switch_case_pattern
val switch_case_origin : switch_case_input -> Symbol.origin
val return_index : return_input -> int
val return_keyword_origin : return_input -> Symbol.origin
val return_expression : return_input -> argument_expression option
val return_origin : return_input -> Symbol.origin
val argument_index : argument -> int
val argument_kind : argument -> argument_kind
val argument_expression : argument -> argument_expression option
val argument_origin : argument -> Symbol.origin
val argument_expression_kind : argument_expression -> argument_expression_kind
val argument_expression_origin : argument_expression -> Symbol.origin
val defined_operand_kind : defined_expression -> defined_operand_kind
val defined_operand_spelling : defined_expression -> string
val defined_operand_origin : defined_expression -> Symbol.origin
val defined_operand_resolution : defined_expression -> defined_operand_resolution
val defined_known_value : defined_expression -> bool option
val prefix_operator : prefix_expression -> prefix_operator
val prefix_operator_origin : prefix_expression -> Symbol.origin
val prefix_operand : prefix_expression -> argument_expression
val postfix_operator : postfix_expression -> postfix_operator
val postfix_operator_origin : postfix_expression -> Symbol.origin
val postfix_operand : postfix_expression -> argument_expression
val binary_operator : binary_expression -> Generated.Intermediate_codes.t
val binary_operator_origin : binary_expression -> Symbol.origin
val binary_left : binary_expression -> argument_expression
val binary_right : binary_expression -> argument_expression
val index_base : index_expression -> argument_expression
val index_opening_origin : index_expression -> Symbol.origin
val index_value : index_expression -> argument_expression
val index_closing_origin : index_expression -> Symbol.origin
val member_base : member_expression -> argument_expression
val member_access_kind : member_expression -> member_access_kind
val member_operator_origin : member_expression -> Symbol.origin
val member_name : member_expression -> string
val member_origin : member_expression -> Symbol.origin

val bound_identifier_occurrence :
  bound_identifier -> Module_expression_binding.occurrence

val bound_identifier_type : bound_identifier -> Type.t
val bound_identifier_shape : bound_identifier -> identifier_value_shape
val bound_identifier_array_rank : bound_identifier -> int

val bound_identifier_function_declaration :
  bound_identifier -> Function_resolution.resolved_declaration option

val bound_identifier_function_address_path :
  bound_identifier -> direct_function_address_path option

val top_level_bound_identifier_occurrence :
  top_level_bound_identifier -> Top_level_outer_expression_binding.occurrence

val identifier_value_shape_name : identifier_value_shape -> string
val direct_function_address_path_name : direct_function_address_path -> string

val direct_function_address_path :
  Function_resolution.compilation_mode ->
  Function_resolution.resolved_declaration ->
  (direct_function_address_path, string) result
(** Classify the source-visible address path selected by [PrsExp] for one
    checked function declaration. *)

val default_parameter_default :
  default_use -> Function_type_resolution.parameter_default

val default_omission : default_use -> argument option
val fixed_parameter : fixed_argument -> Function_type_resolution.parameter
val fixed_value : fixed_argument -> fixed_value
val direct_source : direct_call -> call
val direct_occurrence : direct_call -> Module_expression_binding.occurrence

val direct_active_header :
  direct_call -> Function_type_resolution.resolved_function

val direct_target_symbol : direct_call -> Symbol.t
val direct_fixed_arguments : direct_call -> fixed_argument list
val direct_variadic_arguments : direct_call -> argument list
val direct_variadic_count : direct_call -> int64
val callable_return_type : callable -> Type_reference.t
val callable_pointer : callable -> Function_type_resolution.function_pointer
val callable_signature : callable -> Function_type_resolution.signature
val indirect_source : indirect_call -> call
val indirect_occurrence : indirect_call -> Module_expression_binding.occurrence
val indirect_callable : indirect_call -> callable

val indirect_member_lookup :
  indirect_call -> Aggregate_member_index.lookup option

val indirect_fixed_arguments : indirect_call -> fixed_argument list
val indirect_variadic_arguments : indirect_call -> argument list
val indirect_variadic_count : indirect_call -> int64
val call_syntax_name : call_syntax -> string
val callee_form_name : callee_form -> string
val argument_kind_name : argument_kind -> string
val prefix_operator_name : prefix_operator -> string
val postfix_operator_name : postfix_operator -> string
val member_access_kind_name : member_access_kind -> string
val implicit_output_target_name : implicit_output_target -> string
val implicit_output_fixed_source_name : implicit_output_fixed_source -> string
val binary_operator_name : Generated.Intermediate_codes.t -> string
val unresolved_expression_kind_name : unresolved_expression_kind -> string
val argument_expression_kind_name : argument_expression_kind -> string
val deferred_reason_name : deferred_reason -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
