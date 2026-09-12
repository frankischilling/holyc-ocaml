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
type suspension

val suspend_context : command_context -> (suspension, string) result
(** Capture the current stack position of a live parser context for one nested
    input. A suspended ancestor cannot issue a token while a child is active. *)

type command_start = private {
  command_context : command_context;
  command_ordinal : int;
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

val context_parent : command_context -> command_position option
(** Exact input and environment ownership, with the parent's suspended parser
    phase at nested entry. Contexts from distinct parse calls remain distinct.
*)

val context_is_current : command_context -> observed_events:int -> bool
(** The parser still owns this live context and has issued exactly this many
    command checkpoints. A delayed or incomplete observer cannot use remembered
    lifecycle state to activate execution after parsing advances or ends. *)

val sequence_accepted : completed_sequence -> bool
(** Becomes true only after the sequence completion callback returns
    successfully. Rejected or exceptional completion never accepts the sequence.
    Later parent parsing or stream-generation failures do not revoke accepted
    child syntax. *)

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

type local_source = private
  | Local_parameter of Ast.function_parameter
  | Local_variable of {
      local_type_specifier : Ast.type_specifier;
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

type query_node =
  | Sizeof_target of Ast.identifier
  | Offset_target of Ast.identifier
  | Defined_target of Ast.defined_operand

type query_root = private {
  query_node : query_node;
  query_location : Ast.location;
  query_environment : Symbol_visibility.Environment.t;
  query_lookup : Symbol_visibility.lookup;
  query_local : local_publication option;
  query_present : bool;
  query_command : command_start;
}

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

type declaration_header = private {
  declaration_sources : Common.Source_manager.t;
  declaration_source : Common.Source_file.t;
  declaration_command : command_start;
  modifiers : Ast.declaration_modifier list;
  binding : Ast.declaration_binding option;
  type_specifier : Ast.type_specifier;
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

type function_publication = private {
  function_activity : function_activity;
  function_header : declaration_header;
  function_environment : Symbol_visibility.Environment.t;
  function_entry : Symbol_visibility.entry;
  function_previous : Symbol_visibility.lookup;
  function_name : Ast.identifier;
  function_pointer_layers : Ast.pointer_layer list;
  function_opening_parenthesis : Ast.location;
}

val function_publication_is_current : function_publication -> bool
(** True only during the original function-declaration callback. *)

type function_parameter_activity
type parameter_completion_activity

type function_parameter_publication = private {
  parameter_function : function_publication;
  parameter_index : int;
  parameter_predecessor : completed_function_parameter option;
  parameter_register_qualifiers : Ast.register_qualifier list;
  parameter_type_specifier : Ast.type_specifier;
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
    Recursive callback signature children remain attached to the original head.
*)

val function_parameter_completion_is_current :
  completed_function_parameter -> bool
(** Exact completed parameter, including its original default and delimiter,
    before lookahead beyond the parameter delimiter or closing parenthesis. *)

type function_variadic_activity

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
  default_ast : Ast.parameter_default;
  default_activity : parameter_default_activity;
}

val parameter_default_is_current : completed_parameter_default -> bool
(** Original named-function default after expression lookahead and before the
    parameter delimiter is consumed. Only its synchronous callback is current;
    the receipt alone grants no evaluation or call-materialization authority. *)

type function_header_activity

type completed_function_header = private {
  function_publication : function_publication;
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

type declaration_event = private
  | Array_dimension_preparing of array_dimension_preparation
  | Array_dimension_completed of completed_array_dimension
  | Global_declared of global_publication
  | Global_initializer_started of global_initializer_start
  | Global_initializer_leaf_completed of completed_initializer_leaf
  | Global_initializer_delimiter_completed of completed_initializer_delimiter
  | Global_completed of global_publication * Ast.global_declarator
  | Function_declared of function_publication
  | Function_parameter_declared of function_parameter_publication
  | Parameter_default_completed of completed_parameter_default
  | Function_parameter_completed of completed_function_parameter
  | Function_variadic_started of function_variadic_publication
  | Function_variadic_completed of function_variadic_publication
  | Function_header_completed of completed_function_header
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
  ?execute_stream:
    (Common.Span.t -> (stream_execution, Common.Diagnostic.t list) result) ->
  sources:Common.Source_manager.t ->
  definitions:Definition.Environment.t ->
  symbols:Symbol_visibility.Environment.t ->
  config:Preprocessor.Config.t ->
  Common.Source_file.t ->
  output

val has_errors : output -> bool

val parse_suspended :
  suspension ->
  ?commands:command_sink ->
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
