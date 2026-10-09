type output = {
  ast : Ast.module_ option;
  diagnostics : Common.Diagnostic.t list;
}

val max_pointer_depth : int
val max_expression_depth : int
val max_block_depth : int
val max_conditional_depth : int
val max_loop_depth : int
val max_lock_depth : int
val max_try_depth : int
val max_switch_depth : int
val max_aggregate_depth : int
val max_initializer_depth : int

type command_context
type compiler_position_source
type suspension

val suspend_context : command_context -> (suspension, string) result
(** Capture the current stack position of a live parser context for one nested
    input. A suspended ancestor cannot issue a token while a child is active. *)

val suspension_enclosing_context :
  suspension -> (command_context, string) result
(** The unchanged enclosing compiler position saved by this active original
    [#exe] sequence. Ordinary inputs, inactive directives, advanced parents and
    consumed or foreign-domain tokens cannot supply saved compiler tables. *)

type command_start = private {
  command_context : command_context;
  command_ordinal : int;
  command_compiler_options : int64;
  command_predecessor : completed_command option;
}

and completed_command = private {
  command_start : command_start;
  command_ast : Ast.module_;
}

type command_position = private
  | Before_first_command of command_context
  | Reading_command of command_start
  | Awaiting_resume of completed_command

type completed_sequence = private {
  sequence_context : command_context;
  sequence_commands : completed_command list;
  sequence_ast : Ast.module_;
}

type command_event = private
  | Sequence_started of command_context
  | Command_started of command_start
  | Command_completed of completed_command
  | Command_resumed of completed_command
  | Sequence_completed of completed_sequence
  | Sequence_aborted of command_context

val context_sources : command_context -> Common.Source_manager.t
val context_source : command_context -> Common.Source_file.t
val context_environment : command_context -> Symbol_visibility.Environment.t
val context_mode : command_context -> Preprocessor.compilation_mode

val lexical_lookup_is_current :
  command_context -> Preprocessor.lexical_lookup -> bool
(** Whether this is an original current lexer read under the exact focused
    parser context, environment, mode and domain. This does not authorize a
    record mutation or advance a command, declaration or executable cursor. *)

val context_compiler_options : command_context -> (int64, string) result
(** Read the native option field of the original current control. A directive
    shares its enclosing allocation; a nested ordinary input copies the live
    caller's options before selecting its saved compiler tables. Suspended
    ancestors, closed inputs and other domains cannot operate on the control. *)

val context_get_option :
  command_context -> bit_index:int64 -> (bool, string) result

val context_set_option :
  command_context -> bit_index:int64 -> bool -> (bool, string) result
(** Set an original known compiler option and return its previous state, as
    [_BEQU] does. Invalid indices leave the control unchanged. These operations
    require the exact current parser context; a numeric mask grants no entry
    authority. *)

val context_emit_compiler_warning :
  command_context -> Common.Diagnostic.t -> (unit, string) result
(** Append a reached warning to the enclosing source diagnostic stream under the
    original focused control. Child compiler warnings retain their order and
    survive a later child or parent failure. *)

val context_warning_count : command_context -> (int64, string) result

val context_emit_counted_compiler_warning :
  command_context -> Common.Diagnostic.t -> (unit, string) result
(** Emit a warning and increment this original control's native warning_cnt.
    Directives share the counter; ordinary child controls start at zero.
    PrintWarn-only consumers use the uncounted emitter. *)

val context_parent : command_context -> command_position option
(** Exact input and environment ownership, with the parent's suspended parser
    phase at nested entry. Contexts from distinct parse calls remain distinct.
*)

val context_parent_in_environment :
  command_context ->
  environment:Symbol_visibility.Environment.t ->
  (command_position option, string) result
(** The nearest unchanged original ancestor using this exact environment.
    Intermediate saved-compiler inputs may use different tables. Every link must
    retain its live parser position and event count; inactive, advanced or
    unrelated contexts cannot connect declaration sequences. *)

val context_is_current : command_context -> observed_events:int -> bool
(** The parser still owns this live context and has issued exactly this many
    command checkpoints. A delayed or incomplete observer cannot use remembered
    lifecycle state to activate execution after parsing advances or ends. *)

val sequence_accepted : completed_sequence -> bool
(** Becomes true only after the sequence completion callback returns
    successfully. Rejected or exceptional completion never accepts the sequence.
    Later parent parsing or stream-generation failures do not revoke accepted
    child syntax. *)

type named_aggregate_selection = private {
  type_specifier : Ast.type_specifier;
  identifier : Ast.identifier;
  environment : Symbol_visibility.Environment.t;
  entry : Symbol_visibility.entry;
}
(** Exact visible aggregate selected when the original named type token was
    produced. [type_specifier] is the same AST node retained by the surrounding
    function source witness and [identifier] is its exact named child. The entry
    is a snapshot, not authority for a later lookup or parser phase. A captured
    Class selection takes precedence over a coincident public primitive
    spelling, matching the original token's selected hash entry. *)

type local_source = private
  | Local_parameter of Ast.function_parameter
  | Local_variable of {
      local_type_specifier : Ast.type_specifier;
      local_type_selection : named_aggregate_selection option;
      local_type_entry : Symbol_visibility.entry option;
      local_name : Ast.identifier;
      local_pointer_layers : Ast.pointer_layer list;
      local_array_dimensions : Ast.array_dimension list;
      local_function_pointer : Ast.function_pointer_declarator option;
    }
  | Variadic_count of Ast.variadic_marker
  | Variadic_vector of Ast.variadic_marker

type local_publication = private {
  local_environment : Symbol_visibility.Environment.t;
  local_command : command_start;
  local_spelling : string;
  local_source : local_source;
}

type reference_selection

val selected_identifier : reference_selection -> Ast.identifier

val selected_environment :
  reference_selection -> Symbol_visibility.Environment.t

val selected_lookup : reference_selection -> Symbol_visibility.lookup
(** Selection captured when the identifier token was produced, before later
    lookahead can execute a directive. Header completion promotes an unconsumed
    buffered token selecting that exact provisional function. Once delivered,
    the receipt remains fixed and belongs to this exact AST occurrence and
    environment. Selected absence and local shadowing also remain fixed. Retain
    entry objects, not environment-local numeric IDs. *)

val selected_local : reference_selection -> local_publication option
(** Original local publication captured when this identifier token was produced,
    before subsequent lookahead or generated input. This source record grants no
    semantic symbol or storage authority. *)

val selected_command : reference_selection -> command_start
val reference_selection_is_current : reference_selection -> bool

type call_activity

type call_start = private {
  call_reference : reference_selection;
  call_callee : Ast.expression;
  call_opening_parenthesis : Ast.location option;
  call_activity : call_activity;
}

type completed_call = private {
  call_start : call_start;
  call_expression : Ast.expression;
  emission_activity : call_activity;
}

val call_start_is_current : call_start -> bool

val call_emission_is_current : completed_call -> bool
(** True only during each receipt's original synchronous callback. The start
    retains the original identifier selection, including its command and
    environment, and the exact callee and opening location children. Emission
    retains the complete original call expression and argument children. *)

val claim_call_start : call_start -> bool

val claim_call_emission : completed_call -> bool
(** Claim one receipt during its original callback, at most once across all
    journals. Callers must finish journal preflight before claiming. *)

type implicit_output_selection

type implicit_call_sink = {
  arguments :
    implicit_output_selection ->
    ( Symbol_visibility.function_call_shape option,
      Common.Diagnostic.t list )
    result;
  emission :
    implicit_output_selection -> (unit, Common.Diagnostic.t list) result;
}

type direct_call_sink = {
  implicit : implicit_call_sink option;
  start :
    call_start ->
    ( Symbol_visibility.function_call_shape option,
      Common.Diagnostic.t list )
    result;
  emit : completed_call -> (unit, Common.Diagnostic.t list) result;
}

(** Direct function start follows name lookahead and precedes lookahead inside
    an opening parenthesis. A supplied shape fixes argument traversal at that
    phase; [None] preserves legacy grammar. Emission follows the existing tail
    lookahead after closing-parenthesis consumption (or the parenthesis-free
    arguments), before any subsequent expression processing. Neither callback
    adds a lexer read. The frontend retains source evidence only; checked
    metadata selection, template classification and runtime admission belong to
    the consumer. A provider requiring checked evidence must reject its absence
    instead of returning [None]. *)

val implicit_target : implicit_output_selection -> Ast.implicit_output_target
val implicit_marker : implicit_output_selection -> Ast.location

val implicit_environment :
  implicit_output_selection -> Symbol_visibility.Environment.t

val implicit_lookup :
  implicit_output_selection -> Symbol_visibility.entry option

val implicit_command : implicit_output_selection -> command_start

val implicit_statement :
  implicit_output_selection -> Ast.implicit_output_statement option

val implicit_selection_is_current : implicit_output_selection -> bool
val implicit_arguments_are_current : implicit_output_selection -> bool
val implicit_emission_is_current : implicit_output_selection -> bool
val claim_implicit_arguments : implicit_output_selection -> bool

val claim_implicit_emission : implicit_output_selection -> bool
(** Claims require the corresponding original active callback and succeed at
    most once across all journals. Failed preflight must not claim a receipt.
    Arguments follow empty-marker lookahead and precede argument-expression
    lookahead. Emission follows closing-parenthesis lookahead and precedes
    statement terminator validation.

    The function-only target lookup occurs at the original literal marker,
    before argument parsing or subsequent lookahead. The callback is current
    only while its original observer runs. A successful parse later attaches the
    exact completed statement to the same receipt. No ordinary identifier or
    call is synthesized. *)

type query_node =
  | Sizeof_target of Ast.identifier
  | Offset_target of Ast.identifier
  | Defined_target of Ast.defined_operand

type query_activity

type query_root = private {
  query_node : query_node;
  query_location : Ast.location;
  query_environment : Symbol_visibility.Environment.t;
  query_lookup : Symbol_visibility.lookup;
  query_local : local_publication option;
  query_present : bool;
  query_command : command_start;
  query_activity : query_activity;
}

val query_root_is_current : query_root -> bool
(** True only during the original root read in an active parser context. *)

type query_member_node =
  | Sizeof_member of Ast.sizeof_member
  | Offset_member of Ast.offset_member

type query_member_start = private {
  member_start_root : query_root;
  member_start_ordinal : int;
  member_start_dot : Ast.location;
}

type query_member = private {
  query_member_root : query_root;
  query_member_ordinal : int;
  query_member_node : query_member_node;
  query_member_start : query_member_start;
}

type completed_query = private {
  query_root : query_root;
  query_members : query_member list;
  query_expression : Ast.expression;
}

type query_event = private
  | Query_root of query_root
  | Query_member_started of query_member_start
  | Query_member of query_member
  | Query_completed of completed_query

(** Root, dot and member reads precede their respective subsequent lookahead.
    The dot receipt retains the same location child used by the completed member
    and allows class validation before the member token is produced. Presence
    tests the actual post-preprocessing native identifier token (including
    hosted Keyword tokens), independently of runtime admission. Member receipts
    describe reads through the retained root's class; their spelling does not
    authorize a fresh environment lookup. Completion associates the original AST
    expression with its exact root and ordered member receipts. Rejection stops
    parsing at that read point. *)

type binding_activity

type internal_binding_preparation = private {
  binding_command : command_start;
  binding_environment : Symbol_visibility.Environment.t;
  binding_ast : Ast.declaration_binding;
  binding_activity : binding_activity;
}

val internal_binding_is_current : internal_binding_preparation -> bool
(** Original expression lookahead has completed. Type validation and function
    name publication follow this callback; lookahead may already have read the
    type token. The receipt is active only during its callback. *)

type declaration_header = private {
  declaration_sources : Common.Source_manager.t;
  declaration_source : Common.Source_file.t;
  declaration_command : command_start;
  declaration_compiler_options : int64;
  modifiers : Ast.declaration_modifier list;
  binding : Ast.declaration_binding option;
  binding_preparation : internal_binding_preparation option;
  type_specifier : Ast.type_specifier;
  declaration_type_selection : named_aggregate_selection option;
}

type global_activity

type global_publication = private {
  global_activity : global_activity;
  global_header : declaration_header;
  global_environment : Symbol_visibility.Environment.t;
  global_entry : Symbol_visibility.entry;
  global_previous : Symbol_visibility.lookup;
  global_name : Ast.identifier;
  global_pointer_layers : Ast.pointer_layer list;
  global_function_pointer : Ast.function_pointer_declarator option;
  global_dimensions : Ast.array_dimension list;
}

val global_publication_is_current : global_publication -> bool

type initializer_activity

type global_initializer_start = private {
  initializer_owner : global_publication;
  initializer_equals : Ast.location;
  initializer_activity : initializer_activity;
}

type initializer_delimiter =
  | Initializer_open of Ast.location
  | Initializer_close of Ast.location
  | Initializer_comma of Ast.location

type completed_initializer_leaf = private {
  leaf_initializer : global_initializer_start;
  leaf_index : int;
  leaf_predecessor : completed_initializer_leaf option;
  leaf_path : int list;
  leaf_value : Ast.initial_value;
  leaf_delimiters : initializer_delimiter list;
  leaf_delimiter_predecessor : completed_initializer_delimiter option;
}

and completed_initializer_delimiter = private {
  delimiter_initializer : global_initializer_start;
  delimiter_index : int;
  delimiter_predecessor : completed_initializer_delimiter option;
  delimiter_leaf_predecessor : completed_initializer_leaf option;
  delimiter_value : initializer_delimiter;
}
(** Delimiters consumed since the preceding leaf, in original source order.
    Their locations are the same objects later retained in the complete AST. The
    first leaf includes any opening delimiters; expression-internal punctuation
    is excluded. This is source evidence, not execution authority. *)

val initializer_start_is_current : global_initializer_start -> bool

val initializer_leaf_is_current : completed_initializer_leaf -> bool
(** True only during the original synchronous declaration callback. Remembered
    events cannot be observed later, even while their command remains open. *)

val initializer_delimiter_is_current : completed_initializer_delimiter -> bool
(** Original delimiter callback, before requesting the following token. Both
    predecessor chains retain ordering with leaves and other delimiters. *)

type function_activity
type join_lookup

val join_lookup_environment : join_lookup -> Symbol_visibility.Environment.t
val join_lookup_mode : join_lookup -> Preprocessor.compilation_mode
val join_lookup_scope : join_lookup -> Symbol_visibility.table_scope
val join_lookup_kind : join_lookup -> Symbol_visibility.kind
val join_lookup_name : join_lookup -> Ast.identifier
val join_lookup_selection : join_lookup -> Symbol_visibility.entry option

val join_lookup_is_current : join_lookup -> bool
(** Original kind-filtered declaration lookup before publication and parameter
    or aggregate-body input. JIT selects the current writer's table; AOT also
    searches visible baseline entries. This records selection before extern or
    import filtering by a native record consumer. It is read-only and current
    only in the original focused declaration callback. *)

type function_publication = private {
  function_activity : function_activity;
  function_header : declaration_header;
  function_return_selection : named_aggregate_selection option;
  function_environment : Symbol_visibility.Environment.t;
  function_entry : Symbol_visibility.entry;
  function_previous : Symbol_visibility.lookup;
  function_join_lookup : join_lookup;
  function_name : Ast.identifier;
  function_pointer_layers : Ast.pointer_layer list;
  function_opening_parenthesis : Ast.location;
}

val function_publication_is_current : function_publication -> bool
(** True only during the original function-declaration callback. A present
    [function_return_selection] is the exact named aggregate selected for
    [function_header.type_specifier] before later parameter/body lookahead. *)

type function_parameter_activity
type parameter_completion_activity

type function_parameter_publication = private {
  parameter_function : function_publication;
  parameter_index : int;
  parameter_predecessor : completed_function_parameter option;
  parameter_register_qualifiers : Ast.register_qualifier list;
  parameter_type_specifier : Ast.type_specifier;
  parameter_type_selection : named_aggregate_selection option;
  parameter_pointer_layers : Ast.pointer_layer list;
  parameter_name : Ast.identifier option;
  parameter_function_pointer : Ast.function_pointer_declarator option;
  parameter_activity : function_parameter_activity;
}

and completed_function_parameter = private {
  parameter_publication : function_parameter_publication;
  parameter_ast : Ast.function_parameter;
  parameter_completion_activity : parameter_completion_activity;
}

val function_parameter_is_current : function_parameter_publication -> bool
(** Original named-function member head, after type lookahead and before default
    input. The predecessor is the exact preceding accepted parameter completion.
    A present [parameter_type_selection] selects the exact
    [parameter_type_specifier] node. Recursive callback signature children
    remain attached to the original head but do not gain aggregate-selection
    receipts. *)

val function_parameter_completion_is_current :
  completed_function_parameter -> bool
(** Exact completed parameter, including its original default and delimiter,
    before lookahead beyond the parameter delimiter or closing parenthesis. *)

type function_variadic_activity
type function_position_activity

type function_local_allocation = private {
  allocation_function : function_publication;
  allocation_local : local_publication;
  allocation_storage : Ast.local_storage;
  allocation_first_in_declaration : bool;
  allocation_lookahead : Ast.location;
  allocation_initializer_equals : Ast.location option;
  allocation_predecessor : function_local_allocation option;
  allocation_activity : function_position_activity;
}

val function_local_allocation_is_current : function_local_allocation -> bool
(** Original local declaration after type/dimension lookahead and before its
    initializer. The predecessor belongs to this function's parser cursor. The
    first-declarator flag and current lookahead are captured by the parser; they
    cannot be supplied by consumers. Only the first automatic declarator enters
    MemberAdd's duplicate-base-type index in the pinned source. *)

type static_initializer_activity

type static_initializer_start = private {
  static_start_allocation : function_local_allocation;
  static_equals : Ast.location;
  static_start_activity : static_initializer_activity;
}

type static_initializer_preparation = private {
  static_initializer : static_initializer_start;
  static_allocation : function_local_allocation;
  static_leaf_index : int;
  static_leaf_predecessor : static_initializer_preparation option;
  static_leaf_path : int list;
  static_leaf_value : Ast.initial_value;
  static_leaf_delimiters : initializer_delimiter list;
  static_activity : static_initializer_activity;
}

type completed_static_initializer = private {
  static_completed_start : static_initializer_start;
  static_preparation : static_initializer_preparation option;
  static_declarator : Ast.local_declarator;
}

val static_initializer_is_current : static_initializer_preparation -> bool

val static_initializer_completion_is_current :
  completed_static_initializer -> bool

val static_initializer_completed_declarator :
  static_initializer_preparation -> Ast.local_declarator option

val static_initializer_leaf_location :
  static_initializer_preparation -> Ast.location

val static_initializer_allocation :
  static_initializer_preparation -> function_local_allocation
(** Each preparation is one exact original initializer leaf. It follows that
    leaf's expression lookahead and precedes parent delimiter validation, so
    earlier successful callbacks remain observable when a later leaf or closing
    delimiter fails. [static_leaf_predecessor] and [static_leaf_index] retain
    source order, while [static_leaf_path] and [static_leaf_value] bind the
    callback to the AST later retained by the completed declarator. Completion
    records the last leaf (or [None] for an empty initializer) only after the
    declarator delimiter is consumed and the entire retained root is verified.
    Callback-free parsing emits neither event. *)

type function_position_write = private {
  position_function : function_publication;
  position_source : compiler_position_source;
  position_predecessor : completed_function_parameter option;
  position_is_local : bool;
  position_local_predecessor : function_local_allocation option;
  position_activity : function_position_activity;
}

val function_position_is_current : function_position_write -> bool
(** Original PrsVarLst iteration write after opening/delimiter lookahead, before
    parameter or local type input. This identifies a write without supplying its
    numeric value or runtime authority. *)

type function_variadic_publication = private {
  variadic_function : function_publication;
  variadic_marker : Ast.variadic_marker;
  variadic_parameter_predecessor : completed_function_parameter option;
  variadic_activity : function_variadic_activity;
}

val function_variadic_start_is_current : function_variadic_publication -> bool

val function_variadic_completion_is_current :
  function_variadic_publication -> bool
(** The same original ellipsis receipt first publishes the native flag before
    lookahead past the marker, then synthetic-member availability after that
    lookahead and before consuming an actual closing parenthesis. Each predicate
    is true only during its original synchronous callback. *)

type parameter_default_activity

type default_position_source =
  | Class_default_position of compiler_position_source option
  | Instruction_default_position

type completed_parameter_default = private {
  default_function : function_publication;
  default_parameter : function_parameter_publication;
  default_parameter_index : int;
  default_predecessor : completed_parameter_default option;
  default_register_qualifiers : Ast.register_qualifier list;
  default_type_specifier : Ast.type_specifier;
  default_pointer_layers : Ast.pointer_layer list;
  default_parameter_name : Ast.identifier option;
  default_function_pointer : Ast.function_pointer_declarator option;
  default_position_reads : (Ast.expression * default_position_source) list;
  default_ast : Ast.parameter_default;
  default_activity : parameter_default_activity;
}

val parameter_default_is_current : completed_parameter_default -> bool
(** Original named-function default after expression lookahead and before the
    parameter delimiter is consumed. Only its synchronous callback is current;
    the receipt alone grants no evaluation or call-materialization authority. *)

type callback_signature_activity

type callback_signature_publication = private {
  callback_command : command_start;
  callback_return_type_specifier : Ast.type_specifier;
  callback_return_selection : named_aggregate_selection option;
  callback_return_pointer_layers : Ast.pointer_layer list;
  callback_opening : Ast.location;
  callback_indirection_layers : Ast.pointer_layer list;
  callback_activity : callback_signature_activity;
}

type callback_parameter_publication = private {
  callback_parameter_signature : callback_signature_publication;
  callback_parameter_index : int;
  callback_parameter_predecessor : completed_callback_parameter option;
  callback_parameter_register_qualifiers : Ast.register_qualifier list;
  callback_parameter_type_specifier : Ast.type_specifier;
  callback_parameter_type_selection : named_aggregate_selection option;
  callback_parameter_pointer_layers : Ast.pointer_layer list;
  callback_parameter_name : Ast.identifier option;
  callback_parameter_function_pointer : Ast.function_pointer_declarator option;
  callback_parameter_activity : function_parameter_activity;
}

and completed_callback_parameter = private {
  callback_parameter_publication : callback_parameter_publication;
  callback_parameter_ast : Ast.function_parameter;
  callback_parameter_completion_activity : parameter_completion_activity;
}

type callback_position_write = private {
  callback_position_signature : callback_signature_publication;
  callback_position_source : compiler_position_source;
  callback_position_predecessor : completed_callback_parameter option;
  callback_position_activity : function_position_activity;
}

type completed_callback_default = private {
  callback_default_signature : callback_signature_publication;
  callback_default_parameter : callback_parameter_publication;
  callback_default_index : int;
  callback_default_predecessor : completed_callback_default option;
  callback_default_position_reads :
    (Ast.expression * default_position_source) list;
  callback_default_ast : Ast.parameter_default;
  callback_default_activity : parameter_default_activity;
}

type completed_callback_signature = private {
  callback_signature_publication : callback_signature_publication;
  callback_pointer : Ast.function_pointer_declarator;
  callback_parameters : completed_callback_parameter list;
  callback_defaults : completed_callback_default list;
  callback_completion_activity : callback_signature_activity;
}

val callback_signature_is_current : callback_signature_publication -> bool
val callback_parameter_is_current : callback_parameter_publication -> bool
val callback_default_is_current : completed_callback_default -> bool

val callback_parameter_completion_is_current :
  completed_callback_parameter -> bool

val callback_signature_completion_is_current :
  completed_callback_signature -> bool

type function_header_activity

type completed_function_header = private {
  function_publication : function_publication;
  header_compiler_options : int64;
  completed_entry : Symbol_visibility.entry;
  parameters : Ast.function_parameter list;
  parameter_completions : completed_function_parameter list;
  empty_parameter_entries : Ast.empty_parameter_entry list;
  variadic : Ast.variadic_marker option;
  variadic_publication : function_variadic_publication option;
  closing_parenthesis : Ast.location option;
  header_activity : function_header_activity;
}

val function_header_is_current : completed_function_header -> bool
(** True only during this exact header's original completion callback. The
    callback follows closing-parenthesis lookahead and precedes body parsing. *)

val function_body_completion_is_current :
  completed_function_header -> Ast.function_definition -> bool
(** Only the exact original body during its completion callback is current. *)

val function_body_compiler_options :
  completed_function_header -> Ast.function_definition -> (int64, string) result
(** Immutable options reached after parsing this exact original body. The
    receipt survives callback expiry; copies and substituted bodies reject. This
    source evidence grants no execution authority. *)

type function_return_step =
  | Enter_function_body
  | Check_bare_return
  | Check_value_return
  | Value_return_parsed
  | Check_function_body_return

type function_return_activity

type function_return_phase = private {
  return_header : completed_function_header;
  return_step : function_return_step;
  return_location : Ast.location;
  return_activity : function_return_activity;
}

val function_return_phase_is_current : function_return_phase -> bool

val consume_function_return_phase :
  function_return_phase -> (bool, string) result
(** Consume once during the original callback, returning the previous native
    [CCF_HAS_RETURN]. Entry clears it; a parsed value sets it. Other phases read
    it without changing it. This supplies no return type or executable
    authority. *)

type array_dimensions_owner = private {
  dimensions_command : command_start;
  dimensions_environment : Symbol_visibility.Environment.t;
  dimensions_name : Ast.identifier;
}

type dimension_activity

type array_dimension_preparation = private {
  dimension_owner : array_dimensions_owner;
  dimension_index : int;
  dimension_predecessor : completed_array_dimension option;
  dimension_opening : Ast.location;
  dimension_expression : Ast.expression option;
  dimension_activity : dimension_activity;
}

and completed_array_dimension = private {
  dimension_preparation : array_dimension_preparation;
  dimension_ast : Ast.array_dimension;
}

val dimension_preparation_is_current : array_dimension_preparation -> bool
val dimension_completion_is_current : completed_array_dimension -> bool

type switch_activity

type switch_owner = private {
  switch_command : command_start;
  switch_environment : Symbol_visibility.Environment.t;
  switch_mode : Ast.switch_mode;
  switch_keyword : Ast.location;
  switch_expression : Ast.expression;
  switch_opening_brace : Ast.location;
  switch_activity : switch_activity;
}

type switch_case_endpoint = Switch_case_start | Switch_case_end

type switch_case_preparation = private {
  switch_owner : switch_owner;
  switch_case_index : int;
  switch_case_predecessor : completed_switch_case option;
  switch_case_keyword : Ast.location;
  switch_case_endpoint : switch_case_endpoint;
  switch_case_expression : Ast.expression;
  switch_case_endpoint_predecessor : switch_case_preparation option;
  switch_case_activity : switch_activity;
}

and completed_switch_case = private {
  completed_case_owner : switch_owner;
  completed_case_index : int;
  completed_case_predecessor : completed_switch_case option;
  switch_case_start_preparation : switch_case_preparation option;
  switch_case_end_preparation : switch_case_preparation option;
  switch_case_ast : Ast.switch_case_label;
  completed_case_activity : switch_activity;
}

type completed_switch = private {
  switch_owner : switch_owner;
  switch_cases : completed_switch_case list;
  switch_ast : Ast.switch_statement;
  switch_activity : switch_activity;
}

val switch_owner_is_current : switch_owner -> bool
val switch_case_preparation_is_current : switch_case_preparation -> bool
val switch_case_completion_is_current : completed_switch_case -> bool

val switch_completion_is_current : completed_switch -> bool
(** Case preparation runs after the exact endpoint expression has completed and
    before validating its following ':' or range ellipsis. Case completion runs
    after the label colon's following lookahead. Switch completion runs after
    the closing brace's following lookahead. These receipts are source witnesses
    only; they carry no numeric or execution authority. *)

type aggregate_activity

type aggregate_base_selection = private {
  base_ast : Ast.aggregate_base;
  base_environment : Symbol_visibility.Environment.t;
  base_entry : Symbol_visibility.entry;
}
(** The original class entry selected before the base name's following
    lookahead. Its layout is read only at [Aggregate_base_attached], after that
    lookahead and before validating the opening brace. *)

type aggregate_step =
  | Aggregate_base_attached of aggregate_base_selection
  | Aggregate_body_started of Ast.aggregate_base option
  | Aggregate_member_prepared of {
      member_type : Ast.type_specifier;
      member_name : Ast.identifier;
      member_pointers : Ast.pointer_layer list;
      member_callback : Ast.function_pointer_declarator option;
      member_dimensions : Ast.array_dimension list;
    }
  | Aggregate_union_entered
  | Aggregate_union_left
  | Aggregate_offset_reached of Ast.expression
  | Aggregate_body_finished
  | Aggregate_position_reset

type aggregate_publication = private {
  aggregate_header : declaration_header;
  aggregate_environment : Symbol_visibility.Environment.t;
  aggregate_entry : Symbol_visibility.entry;
  aggregate_previous : Symbol_visibility.entry option;
  aggregate_join_lookup : join_lookup option;
  aggregate_name : Ast.identifier;
  aggregate_kind : Ast.aggregate_kind;
  aggregate_activity : aggregate_activity;
}
(** [aggregate_previous] is the class-filtered entry selected in the original
    parser environment immediately before [aggregate_entry] is published.
    Same-name entries of other kinds do not mask it. [aggregate_join_lookup]
    records the separate declaration lookup; extern forward declarations have
    none because the original compiler publishes a fresh class directly. *)

type aggregate_phase = private {
  phase_aggregate : aggregate_publication;
  phase_predecessor : aggregate_phase option;
  phase_step : aggregate_step;
  phase_location : Ast.location;
  phase_activity : aggregate_activity;
  phase_written_position : compiler_position_source option;
  phase_position_reads : (Ast.expression * compiler_position_source option) list;
}
(** Position reads retain their exact original nodes and the last shared-cell
    write. None records an unmodeled function/frame write, not numeric zero. *)

val aggregate_phase_is_current : aggregate_phase -> bool
(** Body entry precedes opening-brace lookahead. Member placement follows
    type/dimension lookahead and precedes metadata. Union exit follows the inner
    closing-brace lookahead. The exact predecessor is parser-owned. *)

type completed_aggregate = private {
  aggregate_publication : aggregate_publication;
  aggregate_item : Ast.item;
  aggregate_final_phase : aggregate_phase option;
  aggregate_completion_activity : aggregate_activity;
}

val aggregate_publication_is_current : aggregate_publication -> bool
val aggregate_completion_is_current : completed_aggregate -> bool

type declaration_event = private
  | Internal_binding_preparing of internal_binding_preparation
  | Aggregate_declared of aggregate_publication
  | Aggregate_advanced of aggregate_phase
  | Aggregate_completed of completed_aggregate
  | Array_dimension_preparing of array_dimension_preparation
  | Array_dimension_completed of completed_array_dimension
  | Switch_case_preparing of switch_case_preparation
  | Switch_case_completed of completed_switch_case
  | Switch_completed of completed_switch
  | Global_declared of global_publication
  | Global_initializer_started of global_initializer_start
  | Global_initializer_leaf_completed of completed_initializer_leaf
  | Global_initializer_delimiter_completed of completed_initializer_delimiter
  | Global_completed of global_publication * Ast.global_declarator
  | Function_declared of function_publication
  | Function_position_written of function_position_write
  | Function_local_allocated of function_local_allocation
  | Static_initializer_preparing of static_initializer_preparation
  | Static_initializer_completed of completed_static_initializer
  | Callback_position_written of callback_position_write
  | Callback_signature_started of callback_signature_publication
  | Callback_parameter_declared of callback_parameter_publication
  | Callback_default_completed of completed_callback_default
  | Callback_parameter_completed of completed_callback_parameter
  | Callback_signature_completed of completed_callback_signature
  | Function_parameter_declared of function_parameter_publication
  | Parameter_default_completed of completed_parameter_default
  | Function_parameter_completed of completed_function_parameter
  | Function_variadic_started of function_variadic_publication
  | Function_variadic_completed of function_variadic_publication
  | Function_header_completed of completed_function_header
  | Function_return_phase of function_return_phase
  | Function_body_completed of
      completed_function_header * Ast.function_definition
      (** Parser-owned source witnesses. Array preparation follows expression
          lookahead and precedes closing-bracket validation. Completion reuses
          the original opening and expression children and precedes Lex beyond
          the closing bracket. The prospective owner retains the actual
          declarator name before publication. Empty first dimensions retain
          [None]; these witnesses do not establish evaluated values or
          initializer inference.

          A global is declared after its dimensions, before its initializer.
          Initializer start precedes reading the first value. Each leaf retains
          its original scalar node after expression lookahead and before parent
          delimiter validation or the next leaf's lexer reads. Adjacent strings
          remain one expression and one leaf. Paths describe the original syntax
          tree, not checked destination indices or byte offsets. These callbacks
          also describe unsupported initializer shapes; they grant no layout or
          execution authority. Global completion precedes lookahead past its
          delimiter. A function is provisional before parameter parsing. Header
          completion follows the first lookahead past ')' or an unterminated
          variadic marker; body completion follows body parsing and its
          terminating lookahead, including a native empty body at EOF.
          Completion records reuse exact source nodes and their declaration
          witness. Runtime validation, installation and replay admission remain
          the consumer's work. *)

type source_observation =
  | Command of command_event
  | Declaration of declaration_event
  | Reference of reference_selection
  | Call_start of call_start
  | Call_emission of completed_call
  | Implicit_output of implicit_output_selection
  | Implicit_arguments of implicit_output_selection
  | Implicit_emission of implicit_output_selection

val source_observations_match :
  command_context -> events_rev:source_observation list -> bool option

val source_observation_count : command_context -> int option
(** Call-aware contexts retain the original ordered observations of enabled
    consumers. Equality requires the exact event payloads. [None] denotes a
    legacy context without a call sink and never establishes call authority.
    Observations are recorded before each consumer is invoked. *)

type command_sink = {
  lexical_lookup :
    (command_context ->
    Preprocessor.lexical_lookup ->
    (unit, Common.Diagnostic.t list) result)
    option;
  checkpoint :
    (command_event -> (unit, Common.Diagnostic.t list) result) option;
  reference :
    (reference_selection -> (unit, Common.Diagnostic.t list) result) option;
  call : direct_call_sink option;
  implicit_output :
    (implicit_output_selection -> (unit, Common.Diagnostic.t list) result)
    option;
  query : (query_event -> (unit, Common.Diagnostic.t list) result) option;
  declaration :
    (declaration_event -> (unit, Common.Diagnostic.t list) result) option;
  dimension_count :
    (completed_array_dimension ->
    ( (completed_array_dimension * int64) option,
      Common.Diagnostic.t list )
    result)
    option;
  command : Ast.item -> (unit, Common.Diagnostic.t list) result;
  resume : unit -> (unit, Common.Diagnostic.t list) result;
}
(** [checkpoint] receives private parser lifecycle witnesses. Complete commands
    and successful sequences own exact AST views; they do not authorize runtime
    admission. A sequence starts before initial lookahead. Completion precedes
    [command]; resumption follows the next lookahead and precedes [resume]. On
    failure or exception, an abort checkpoint releases the context. A failed
    sequence has no successful sequence view. Declarations and references retain
    their exact command start, including across nested parsing.

    [lexical_lookup] consumes original raw lexer reads under this focused
    command context, including before the first command and during lookahead. A
    directive selects its own service; [None] masks the suspended parent's
    service. Errors stop parsing and retain reached diagnostics. Reads do not
    advance lifecycle observation counts. The optional parser inspection
    observer still sees the whole input after its scoped consumer.

    [dimension_count] requires [declaration]. After that observer accepts a
    completed dimension, before the next lexer read, this service may return its
    cached count with the exact same receipt. A foreign receipt rejects. Each
    cursor caches the projection by the original AST dimension solely to consume
    unbraced initializer elements. No AST is rewritten and this count supplies
    no semantic or runtime authority. [None] preserves the literal-only grammar
    path for analysis or callback-free parsing. A supplied source/runtime
    service must reject missing checked evidence rather than return [None].

    [command] receives a completed syntax command. [resume] runs after the next
    command's initial lookahead, including any #exe reached during that
    lookahead. The sink owns pending execution and must tolerate [resume] with
    no pending command. A reported error stops further command delivery. *)

type stream_execution = {
  definitions : Definition.Environment.t;
  symbols : Symbol_visibility.Environment.t;
  commands : command_sink;
  finish : unit -> (string, Common.Diagnostic.t list) result;
  abort : unit -> unit;
}
(** One active generation buffer and its task environment. The parser consumes
    ordinary command grammar in temporary JIT mode with no outer function-local
    context. [finish] supplies generated bytes only after a closing brace and
    successful commands. [abort] releases the buffer on any earlier failure,
    including an exception. Successful generation is inserted at the current
    lexer position after restoring the outer environment. *)

val parse :
  ?commands:command_sink ->
  ?lexical_lookup:(Preprocessor.lexical_lookup -> unit) ->
  ?execute_stream:
    (Common.Span.t -> (stream_execution, Common.Diagnostic.t list) result) ->
  sources:Common.Source_manager.t ->
  definitions:Definition.Environment.t ->
  symbols:Symbol_visibility.Environment.t ->
  config:Preprocessor.Config.t ->
  Common.Source_file.t ->
  output

(** [lexical_lookup] observes original raw lexer reads throughout this input,
    including directives consumed before a syntax token is returned. A [#exe]
    body shares the stream and observer while selecting its own JIT writer.
    Supplying an observer does not enable a declaration or execution sink. *)

val has_errors : output -> bool

val parse_suspended :
  suspension ->
  ?commands:command_sink ->
  ?lexical_lookup:(Preprocessor.lexical_lookup -> unit) ->
  ?execute_stream:
    (Common.Span.t -> (stream_execution, Common.Diagnostic.t list) result) ->
  sources:Common.Source_manager.t ->
  definitions:Definition.Environment.t ->
  symbols:Symbol_visibility.Environment.t ->
  config:Preprocessor.Config.t ->
  Common.Source_file.t ->
  (output, string) result

(** Parse one input at the original suspension, requiring the same source
    manager, symbol environment, compilation mode, stack position and event
    count. Rejected preflight does not consume the token. Once parsing starts,
    success, failure and exceptions consume it and restore the parent stack.
    Definitions and other config options describe the new input; the task
    adapter supplies its own original environments and config. *)

val suspension_owns_sequence : suspension -> completed_sequence -> bool
(** Only the exact accepted nested sequence belongs to a consumed token. This
    establishes syntax ownership; runtime admission remains separate. *)

val parse_suspended_enclosing :
  suspension ->
  enclosing:command_context ->
  ?commands:command_sink ->
  ?lexical_lookup:(Preprocessor.lexical_lookup -> unit) ->
  ?execute_stream:
    (Common.Span.t -> (stream_execution, Common.Diagnostic.t list) result) ->
  sources:Common.Source_manager.t ->
  definitions:Definition.Environment.t ->
  symbols:Symbol_visibility.Environment.t ->
  config:Preprocessor.Config.t ->
  Common.Source_file.t ->
  (output, string) result
(** Execute the parser part of [StreamExePrint] at its original directive
    suspension with the exact saved enclosing symbols and source manager.
    Original enclosing lexical local shadows are restored for child parsing; the
    directive caller's locals supply no compiler-local frame authority. The
    child starts in JIT mode, independently of the enclosing mode, and has no
    inherited [#exe] permission. The original token and accepted sequence retain
    the same consumption and completion rules as [parse_suspended]. *)

val callback_position_is_current : callback_position_write -> bool
