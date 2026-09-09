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

type global_publication = private {
  global_header : declaration_header;
  global_environment : Symbol_visibility.Environment.t;
  global_entry : Symbol_visibility.entry;
  global_previous : Symbol_visibility.lookup;
  global_name : Ast.identifier;
  global_pointer_layers : Ast.pointer_layer list;
  global_function_pointer : Ast.function_pointer_declarator option;
  global_dimensions : Ast.array_dimension list;
}

type function_publication = private {
  function_header : declaration_header;
  function_environment : Symbol_visibility.Environment.t;
  function_entry : Symbol_visibility.entry;
  function_previous : Symbol_visibility.lookup;
  function_name : Ast.identifier;
  function_pointer_layers : Ast.pointer_layer list;
  function_opening_parenthesis : Ast.location;
}

type completed_function_header = private {
  function_publication : function_publication;
  completed_entry : Symbol_visibility.entry;
  parameters : Ast.function_parameter list;
  empty_parameter_entries : Ast.empty_parameter_entry list;
  variadic : Ast.variadic_marker option;
  closing_parenthesis : Ast.location;
}

type array_dimensions_owner = private {
  dimensions_command : command_start;
  dimensions_environment : Symbol_visibility.Environment.t;
  dimensions_name : Ast.identifier;
}

type array_dimension_preparation = private {
  dimension_owner : array_dimensions_owner;
  dimension_index : int;
  dimension_predecessor : completed_array_dimension option;
  dimension_opening : Ast.location;
  dimension_expression : Ast.expression option;
}

and completed_array_dimension = private {
  dimension_preparation : array_dimension_preparation;
  dimension_ast : Ast.array_dimension;
}

type declaration_event = private
  | Array_dimension_preparing of array_dimension_preparation
  | Array_dimension_completed of completed_array_dimension
  | Global_declared of global_publication
  | Global_completed of global_publication * Ast.global_declarator
  | Function_declared of function_publication
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

          A global is declared after its dimensions, before its initializer;
          completion precedes lookahead past its delimiter. A function is
          provisional before parameter parsing. Header completion follows the
          first lookahead past ')'; body completion follows body parsing and its
          terminating lookahead, including a native empty body at EOF.
          Completion records reuse exact source nodes and their declaration
          witness. Runtime validation, installation and replay admission remain
          the consumer's work. *)

type command_sink = {
  checkpoint :
    (command_event -> (unit, Common.Diagnostic.t list) result) option;
  reference :
    (reference_selection -> (unit, Common.Diagnostic.t list) result) option;
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
