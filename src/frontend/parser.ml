type output = {
  ast : Ast.module_ option;
  diagnostics : Common.Diagnostic.t list;
}

type command_context = {
  context_sources : Common.Source_manager.t;
  context_source : Common.Source_file.t;
  context_environment : Symbol_visibility.Environment.t;
  context_mode : Preprocessor.compilation_mode;
  context_parent : command_position option;
  mutable context_active : bool;
  mutable context_event_count : int;
  mutable context_accepted_ast : Ast.module_ option;
  context_observation_id : int;
  context_stack : command_position ref list ref;
  mutable context_position : command_position ref option;
}

and command_start = {
  command_context : command_context;
  command_ordinal : int;
  command_predecessor : completed_command option;
}

and completed_command = {
  command_start : command_start;
  command_ast : Ast.module_;
}

and command_position =
  | Before_first_command of command_context
  | Reading_command of command_start
  | Awaiting_resume of completed_command

type suspension = {
  suspended_context : command_context;
  suspended_position : command_position;
  suspended_ref : command_position ref;
  suspended_events : int;
  mutable suspension_consumed : bool;
  mutable suspended_ast : Ast.module_ option;
}

let suspend_context context =
  match (context.context_position, !(context.context_stack)) with
  | Some position, active :: _ when context.context_active && active == position
    ->
      Ok
        {
          suspended_context = context;
          suspended_position = !position;
          suspended_ref = position;
          suspended_events = context.context_event_count;
          suspension_consumed = false;
          suspended_ast = None;
        }
  | _ -> Error "parser suspension requires its current active context"

type completed_sequence = {
  sequence_context : command_context;
  sequence_commands : completed_command list;
  sequence_ast : Ast.module_;
}

type command_event =
  | Sequence_started of command_context
  | Command_started of command_start
  | Command_completed of completed_command
  | Command_resumed of completed_command
  | Sequence_completed of completed_sequence
  | Sequence_aborted of command_context

let context_sources context = context.context_sources
let context_source context = context.context_source
let context_environment context = context.context_environment
let context_mode context = context.context_mode
let context_parent context = context.context_parent

let context_is_current context ~observed_events =
  context.context_active && observed_events = context.context_event_count

let sequence_accepted sequence =
  match sequence.sequence_context.context_accepted_ast with
  | Some ast -> ast == sequence.sequence_ast
  | None -> false

type reference_selection = {
  identifier : Ast.identifier;
  environment : Symbol_visibility.Environment.t;
  lookup : Symbol_visibility.lookup;
  selected_command : command_start;
  mutable reference_active : bool;
}

let selected_identifier selection = selection.identifier
let selected_environment selection = selection.environment
let selected_lookup selection = selection.lookup
let selected_command selection = selection.selected_command
let reference_selection_is_current selection = selection.reference_active

type call_activity = {
  mutable call_active : bool;
  mutable call_captured : bool;
}

type call_start = {
  call_reference : reference_selection;
  call_callee : Ast.expression;
  call_opening_parenthesis : Ast.location option;
  call_activity : call_activity;
}

type completed_call = {
  call_start : call_start;
  call_expression : Ast.expression;
  emission_activity : call_activity;
}

let call_start_is_current receipt = receipt.call_activity.call_active
let call_emission_is_current receipt = receipt.emission_activity.call_active

let claim_call_activity activity =
  if (not activity.call_active) || activity.call_captured then false
  else (
    activity.call_captured <- true;
    true)

let claim_call_start receipt = claim_call_activity receipt.call_activity
let claim_call_emission receipt = claim_call_activity receipt.emission_activity

type implicit_output_selection = {
  output_target : Ast.implicit_output_target;
  output_marker : Ast.location;
  output_environment : Symbol_visibility.Environment.t;
  output_lookup : Symbol_visibility.entry option;
  output_command : command_start;
  mutable output_active : bool;
  mutable output_statement : Ast.implicit_output_statement option;
  output_arguments : call_activity;
  output_emission : call_activity;
}

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

let implicit_target selection = selection.output_target
let implicit_marker selection = selection.output_marker
let implicit_environment selection = selection.output_environment
let implicit_lookup selection = selection.output_lookup
let implicit_command selection = selection.output_command
let implicit_statement selection = selection.output_statement
let implicit_selection_is_current selection = selection.output_active

let implicit_arguments_are_current selection =
  selection.output_arguments.call_active

let implicit_emission_is_current selection =
  selection.output_emission.call_active

let claim_implicit_arguments selection =
  claim_call_activity selection.output_arguments

let claim_implicit_emission selection =
  claim_call_activity selection.output_emission

type local_source =
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

type local_publication = {
  local_environment : Symbol_visibility.Environment.t;
  local_command : command_start;
  local_spelling : string;
  local_source : local_source;
}

type query_node =
  | Sizeof_target of Ast.identifier
  | Offset_target of Ast.identifier
  | Defined_target of Ast.defined_operand

type query_root = {
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

type query_member_start = {
  member_start_root : query_root;
  member_start_ordinal : int;
  member_start_dot : Ast.location;
}

type query_member = {
  query_member_root : query_root;
  query_member_ordinal : int;
  query_member_node : query_member_node;
  query_member_start : query_member_start;
}

type completed_query = {
  query_root : query_root;
  query_members : query_member list;
  query_expression : Ast.expression;
}

type query_event =
  | Query_root of query_root
  | Query_member_started of query_member_start
  | Query_member of query_member
  | Query_completed of completed_query

type declaration_header = {
  declaration_sources : Common.Source_manager.t;
  declaration_source : Common.Source_file.t;
  declaration_command : command_start;
  modifiers : Ast.declaration_modifier list;
  binding : Ast.declaration_binding option;
  type_specifier : Ast.type_specifier;
}

type global_activity = { mutable global_active : bool }

type global_publication = {
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

let global_publication_is_current publication =
  publication.global_activity.global_active
  && publication.global_header.declaration_command.command_context
       .context_active

type initializer_phase =
  | Starting_initializer
  | Completing_leaf of int
  | Completing_delimiter of int

type initializer_activity = {
  mutable initializer_phase : initializer_phase option;
}

type global_initializer_start = {
  initializer_owner : global_publication;
  initializer_equals : Ast.location;
  initializer_activity : initializer_activity;
}

type initializer_delimiter =
  | Initializer_open of Ast.location
  | Initializer_close of Ast.location
  | Initializer_comma of Ast.location

type completed_initializer_leaf = {
  leaf_initializer : global_initializer_start;
  leaf_index : int;
  leaf_predecessor : completed_initializer_leaf option;
  leaf_path : int list;
  leaf_value : Ast.initial_value;
  leaf_delimiters : initializer_delimiter list;
  leaf_delimiter_predecessor : completed_initializer_delimiter option;
}

and completed_initializer_delimiter = {
  delimiter_initializer : global_initializer_start;
  delimiter_index : int;
  delimiter_predecessor : completed_initializer_delimiter option;
  delimiter_leaf_predecessor : completed_initializer_leaf option;
  delimiter_value : initializer_delimiter;
}

let initializer_start_is_current start =
  start.initializer_activity.initializer_phase = Some Starting_initializer
  && start.initializer_owner.global_header.declaration_command.command_context
       .context_active

let initializer_leaf_is_current leaf =
  leaf.leaf_initializer.initializer_activity.initializer_phase
  = Some (Completing_leaf leaf.leaf_index)
  && leaf.leaf_initializer.initializer_owner.global_header.declaration_command
       .command_context
       .context_active

let initializer_delimiter_is_current delimiter =
  delimiter.delimiter_initializer.initializer_activity.initializer_phase
  = Some (Completing_delimiter delimiter.delimiter_index)
  && delimiter.delimiter_initializer.initializer_owner.global_header
       .declaration_command
       .command_context
       .context_active

type function_activity = { mutable function_active : bool }

type function_publication = {
  function_activity : function_activity;
  function_header : declaration_header;
  function_environment : Symbol_visibility.Environment.t;
  function_entry : Symbol_visibility.entry;
  function_previous : Symbol_visibility.lookup;
  function_name : Ast.identifier;
  function_pointer_layers : Ast.pointer_layer list;
  function_opening_parenthesis : Ast.location;
}

let function_publication_is_current receipt =
  receipt.function_activity.function_active
  && receipt.function_header.declaration_command.command_context.context_active

type function_parameter_activity = { mutable function_parameter_active : bool }

type parameter_completion_activity = {
  mutable parameter_completion_active : bool;
}

type function_parameter_publication = {
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

and completed_function_parameter = {
  parameter_publication : function_parameter_publication;
  parameter_ast : Ast.function_parameter;
  parameter_completion_activity : parameter_completion_activity;
}

let function_parameter_is_current receipt =
  receipt.parameter_activity.function_parameter_active
  && receipt.parameter_function.function_header.declaration_command
       .command_context
       .context_active

let function_parameter_completion_is_current receipt =
  receipt.parameter_completion_activity.parameter_completion_active
  && receipt.parameter_publication.parameter_function.function_header
       .declaration_command
       .command_context
       .context_active

type function_variadic_activity = {
  mutable function_variadic_start_active : bool;
  mutable function_variadic_completion_active : bool;
}

type function_variadic_publication = {
  variadic_function : function_publication;
  variadic_marker : Ast.variadic_marker;
  variadic_parameter_predecessor : completed_function_parameter option;
  variadic_activity : function_variadic_activity;
}

let function_variadic_start_is_current receipt =
  receipt.variadic_activity.function_variadic_start_active
  && receipt.variadic_function.function_header.declaration_command
       .command_context
       .context_active

let function_variadic_completion_is_current receipt =
  receipt.variadic_activity.function_variadic_completion_active
  && receipt.variadic_function.function_header.declaration_command
       .command_context
       .context_active

type parameter_default_activity = { mutable parameter_default_active : bool }

type completed_parameter_default = {
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

let parameter_default_is_current receipt =
  receipt.default_activity.parameter_default_active
  && receipt.default_function.function_header.declaration_command
       .command_context
       .context_active

type function_header_activity = {
  mutable function_header_active : bool;
  mutable function_body_active : Ast.function_definition option;
}

type completed_function_header = {
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

let function_header_is_current receipt =
  receipt.header_activity.function_header_active
  && receipt.function_publication.function_header.declaration_command
       .command_context
       .context_active

let function_body_completion_is_current receipt definition =
  Option.fold ~none:false ~some:(( == ) definition)
    receipt.header_activity.function_body_active
  && receipt.function_publication.function_header.declaration_command
       .command_context
       .context_active

type array_dimensions_owner = {
  dimensions_command : command_start;
  dimensions_environment : Symbol_visibility.Environment.t;
  dimensions_name : Ast.identifier;
}

type dimension_activity = {
  mutable dimension_active : bool;
  mutable completion_active : bool;
}

type array_dimension_preparation = {
  dimension_owner : array_dimensions_owner;
  dimension_index : int;
  dimension_predecessor : completed_array_dimension option;
  dimension_opening : Ast.location;
  dimension_expression : Ast.expression option;
  dimension_activity : dimension_activity;
}

and completed_array_dimension = {
  dimension_preparation : array_dimension_preparation;
  dimension_ast : Ast.array_dimension;
}

let dimension_preparation_is_current preparation =
  preparation.dimension_activity.dimension_active
  && preparation.dimension_owner.dimensions_command.command_context
       .context_active

let dimension_completion_is_current receipt =
  let preparation = receipt.dimension_preparation in
  preparation.dimension_activity.completion_active
  && preparation.dimension_owner.dimensions_command.command_context
       .context_active

type declaration_event =
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

type source_observation =
  | Command of command_event
  | Declaration of declaration_event
  | Reference of reference_selection
  | Call_start of call_start
  | Call_emission of completed_call
  | Implicit_output of implicit_output_selection
  | Implicit_arguments of implicit_output_selection
  | Implicit_emission of implicit_output_selection

module Context_observations = Ephemeron.K1.Make (struct
  type t = command_context

  let equal left right = left == right
  let hash context = context.context_observation_id
end)

type observations = {
  mutable events_rev : source_observation list;
  mutable count : int;
}

let context_observations = Context_observations.create 16
let next_observation_id = ref 0

let fresh_observation_id () =
  incr next_observation_id;
  !next_observation_id

let record_observation context event =
  match Context_observations.find_opt context_observations context with
  | None -> ()
  | Some observations ->
      observations.events_rev <- event :: observations.events_rev;
      observations.count <- observations.count + 1

let same_observation left right =
  match (left, right) with
  | Command left, Command right -> left == right
  | Declaration left, Declaration right -> left == right
  | Reference left, Reference right -> left == right
  | Call_start left, Call_start right -> left == right
  | Call_emission left, Call_emission right -> left == right
  | Implicit_output left, Implicit_output right -> left == right
  | Implicit_arguments left, Implicit_arguments right -> left == right
  | Implicit_emission left, Implicit_emission right -> left == right
  | _ -> false

let source_observations_match context ~events_rev =
  Option.map
    (fun observations ->
      List.length events_rev = observations.count
      && List.for_all2 same_observation events_rev observations.events_rev)
    (Context_observations.find_opt context_observations context)

let source_observation_count context =
  Option.map
    (fun observations -> observations.count)
    (Context_observations.find_opt context_observations context)

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

type stream_execution = {
  definitions : Definition.Environment.t;
  symbols : Symbol_visibility.Environment.t;
  commands : command_sink;
  finish : unit -> (string, Common.Diagnostic.t list) result;
  abort : unit -> unit;
}

type located_token = {
  token : Token.t;
  context : Preprocessor.diagnostic_context;
  selection :
    (Symbol_visibility.Environment.t * Symbol_visibility.lookup) option;
  local_selection : local_publication option;
}

module Identifier_table = Hashtbl.Make (struct
  type t = Ast.identifier

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

module Dimension_table = Hashtbl.Make (struct
  type t = Ast.array_dimension

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

type cursor = {
  command_stack : command_position ref list ref;
  mutable current_command : command_start option;
  stream : Preprocessor.t;
  sources : Common.Source_manager.t;
  source : Common.Source_file.t;
  symbols : Symbol_visibility.Environment.t;
  compilation_mode : Preprocessor.compilation_mode;
  stop_on_error : bool;
  reference :
    (reference_selection -> (unit, Common.Diagnostic.t list) result) option;
  references : reference_selection Identifier_table.t;
  call : direct_call_sink option;
  mutable pending_calls : completed_call list;
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
  dimension_counts : int64 Dimension_table.t;
  mutable lookahead : located_token list;
  mutable diagnostics_rev : Common.Diagnostic.t list;
  mutable local_context : Symbol_visibility.Environment.local_context option;
  mutable local_publications : local_publication list;
}

type parsed_declarator = { node : Ast.global_declarator; tokens : Token.t list }

exception Stop_command

type parsed_declarator_list = {
  declarators : parsed_declarator list;
  trailing_semicolon : located_token option;
}

type parsed_declarator_prefix = {
  pointer_layers : Ast.pointer_layer list;
  name : Ast.identifier;
  function_pointer : Ast.function_pointer_declarator option;
  tokens : Token.t list;
  definition_trace : Common.Diagnostic.related list;
  name_selection :
    (Symbol_visibility.Environment.t * Symbol_visibility.lookup) option;
}

type parsed_parameter = { node : Ast.function_parameter; tokens : Token.t list }
type parsed_expression = { node : Ast.expression; tokens : Token.t list }
type parsed_statement = { node : Ast.statement; tokens : Token.t list }

type parsed_inline_assembly_operand = {
  node : Ast.inline_assembly_operand;
  tokens : Token.t list;
}

type direct_inline_assembly_directive_form =
  | Direct_inline_import
  | Direct_inline_data of int
  | Direct_inline_binfile
  | Direct_inline_list
  | Direct_inline_nolist
  | Direct_inline_use of int
  | Direct_inline_forbidden_in_function
  | Direct_inline_operand_prefix
  | Direct_inline_invalid_standalone

type parsed_local_declarator = {
  node : Ast.local_declarator;
  tokens : Token.t list;
}

type parsed_switch_element = {
  node : Ast.switch_element;
  tokens : Token.t list;
}

type switch_region_end =
  | Switch_region_brace of located_token
  | Switch_region_end_label of located_token * located_token

type parsed_switch_region = {
  region_elements : Ast.switch_element list;
  region_tokens : Token.t list;
  region_end : switch_region_end;
  region_had_error : bool;
}

type statement_boundary =
  | Top_level_boundary
  | Block_boundary
  | Switch_boundary
  | For_update_boundary of statement_boundary

type parsed_array_dimension = {
  node : Ast.array_dimension;
  tokens : Token.t list;
}

type parsed_initializer = { node : Ast.initial_value; tokens : Token.t list }

type initializer_declarator_context =
  | Global_initializer_declarator
  | Static_local_initializer_declarator of statement_boundary

type parsed_aggregate_member_declarator = {
  node : Ast.aggregate_member_declarator;
  tokens : Token.t list;
}

type parsed_aggregate_member_metadata = {
  metadata_nodes : Ast.aggregate_member_metadata list;
  metadata_tokens : Token.t list;
}

type parsed_aggregate_member = {
  node : Ast.aggregate_member;
  tokens : Token.t list;
}

type parsed_aggregate_members = {
  members : Ast.aggregate_member list;
  tokens : Token.t list;
  closing_brace : Ast.location;
}

type parsed_aggregate_backing = {
  node : Ast.aggregate_backing;
  tokens : Token.t list;
}

type parsed_aggregate_base = {
  node : Ast.aggregate_base;
  tokens : Token.t list;
}

type aggregate_parse_failure = { recovery_depth : int }

type expression_context =
  | Default_expression
  | Array_dimension_expression
  | Intern_binding_expression
  | Call_argument_expression
  | Index_expression
  | Implicit_output_argument_expression
  | Return_expression
  | Do_while_condition_expression
  | For_condition_expression
  | If_condition_expression
  | Switch_expression
  | Switch_case_expression
  | While_condition_expression
  | Local_initializer_expression
  | Global_initializer_expression
  | Aggregate_offset_expression
  | Aggregate_member_metadata_expression
  | Inline_assembly_operand_expression
  | Statement_expression

type direct_function_resolution =
  | Not_a_direct_function
  | Direct_function_without_shape of Symbol_visibility.entry
  | Direct_function_with_shape of Symbol_visibility.function_call_shape

type parsed_parameter_default = {
  node : Ast.parameter_default;
  tokens : Token.t list;
}

type parsed_function_pointer = {
  node : Ast.function_pointer_declarator;
  name : Ast.identifier option;
  name_selection :
    (Symbol_visibility.Environment.t * Symbol_visibility.lookup) option;
  tokens : Token.t list;
}

type parsed_register_qualifiers = {
  nodes : Ast.register_qualifier list;
  tokens : Token.t list;
}

type parsed_parameter_list = {
  parameters : Ast.function_parameter list;
  parameter_completions : completed_function_parameter list;
  empty_parameter_entries : Ast.empty_parameter_entry list;
  variadic : Ast.variadic_marker option;
  variadic_publication : function_variadic_publication option;
  tokens : Token.t list;
  closing_parenthesis : Ast.location option;
}

type function_pointer_declarator_context =
  | Function_parameter_declarator
  | Global_variable_declarator
  | Aggregate_member_declarator
  | Local_variable_declarator of statement_boundary

type parsed_modifier = { node : Ast.declaration_modifier; item : located_token }

type parsed_binding = {
  node : Ast.declaration_binding;
  keyword : located_token;
  tokens : Token.t list;
}

type binding_parse =
  | No_binding
  | Parsed_binding of parsed_binding
  | Bad_binding

let max_pointer_depth = 4
let max_parser_lookahead = max_pointer_depth + 3
let max_function_pointer_depth = 32
let max_expression_depth = 256
let max_block_depth = 256
let max_conditional_depth = 256
let max_loop_depth = 256
let max_lock_depth = 256
let max_try_depth = 256
let max_switch_depth = 256
let max_aggregate_depth = 256
let max_initializer_depth = 256
let max_unbraced_initializer_elements = 1_000_000

let resolve_assembly_opcode token =
  match token.Token.kind with
  | Token_kind.Identifier -> Asm.Opcode.resolve token.raw
  | _ -> None

let resolve_assembly_directive token =
  match token.Token.kind with
  | Token_kind.Identifier -> Asm.Directive.find token.raw
  | _ -> None

let resolve_visible_assembly_opcode cursor token =
  match token.Token.kind with
  | Token_kind.Identifier -> (
      match
        Symbol_visibility.Environment.find_preprocessor cursor.symbols token.raw
      with
      | Symbol_visibility.Present entry
        when Symbol_visibility.kind entry = Symbol_visibility.Opcode ->
          resolve_assembly_opcode token
      | Symbol_visibility.Absent
      | Symbol_visibility.Shadowed_by_local
      | Symbol_visibility.Present _ -> None)
  | _ -> None

let resolve_visible_assembly_directive cursor token =
  match token.Token.kind with
  | Token_kind.Identifier -> (
      match
        Symbol_visibility.Environment.find_preprocessor cursor.symbols token.raw
      with
      | Symbol_visibility.Present entry
        when Symbol_visibility.kind entry = Symbol_visibility.Assembly_keyword
        -> resolve_assembly_directive token
      | Symbol_visibility.Absent
      | Symbol_visibility.Shadowed_by_local
      | Symbol_visibility.Present _ -> None)
  | _ -> None

let direct_inline_assembly_directive_form directive =
  match Asm.Directive.templeos_id directive with
  | 64 | 65 -> Direct_inline_forbidden_in_function
  | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 ->
      Direct_inline_operand_prefix
  | 77 -> Direct_inline_data 1
  | 78 -> Direct_inline_data 2
  | 79 -> Direct_inline_data 4
  | 80 -> Direct_inline_data 8
  | 81 -> Direct_inline_invalid_standalone
  | 82 -> Direct_inline_use 16
  | 83 -> Direct_inline_use 32
  | 84 -> Direct_inline_use 64
  | 85 -> Direct_inline_import
  | 86 -> Direct_inline_list
  | 87 -> Direct_inline_nolist
  | 88 -> Direct_inline_binfile
  | templeos_id ->
      invalid_arg
        (Printf.sprintf "unknown checked assembler-keyword ID %d" templeos_id)

let token_starts_inline_assembly cursor token =
  Option.is_some (resolve_visible_assembly_opcode cursor token)
  ||
  match resolve_visible_assembly_directive cursor token with
  | Some directive ->
      direct_inline_assembly_directive_form directive
      <> Direct_inline_operand_prefix
  | None -> false

let assembly_token_kind token =
  match token.Token.kind with
  | Token_kind.Identifier -> (
      match resolve_assembly_opcode token with
      | Some resolved ->
          let opcode = Asm.Opcode.resolved_opcode resolved in
          Ast.Assembly_opcode_token
            {
              canonical_spelling = Asm.Opcode.spelling opcode;
              source_is_alias = Asm.Opcode.resolved_is_alias resolved;
            }
      | None -> (
          match resolve_assembly_directive token with
          | Some directive ->
              Ast.Assembly_directive_token
                { templeos_id = Asm.Directive.templeos_id directive }
          | None -> (
              match Asm.Register.find token.raw with
              | Some register ->
                  Ast.Assembly_register_token
                    {
                      register_kind = Asm.Register.kind register;
                      register_number = Asm.Register.number register;
                    }
              | None -> Ast.Assembly_identifier_token)))
  | Token_kind.Keyword _ -> Ast.Assembly_keyword_token
  | Token_kind.Integer -> Ast.Assembly_integer_token
  | Token_kind.Float -> Ast.Assembly_float_token
  | Token_kind.String -> Ast.Assembly_string_token
  | Token_kind.Inserted_binary -> Ast.Assembly_string_token
  | Token_kind.Inserted_binary_size -> Ast.Assembly_integer_token
  | Token_kind.Character -> Ast.Assembly_character_token
  | Token_kind.Operator _ -> Ast.Assembly_operator_token
  | Token_kind.Punctuation _ -> Ast.Assembly_punctuation_token
  | Token_kind.Newline -> Ast.Assembly_newline_token
  | Token_kind.Eof ->
      invalid_arg "end-of-input cannot be an assembly body token"

let has_error diagnostics =
  List.exists
    (fun diagnostic ->
      diagnostic.Common.Diagnostic.severity = Common.Diagnostic.Error)
    diagnostics

let has_errors output = has_error output.diagnostics

let rec pull cursor =
  match Preprocessor.next cursor.stream with
  | Lexer.Diagnostic diagnostic ->
      cursor.diagnostics_rev <- diagnostic :: cursor.diagnostics_rev;
      if
        cursor.stop_on_error
        && diagnostic.Common.Diagnostic.severity = Common.Diagnostic.Error
      then (
        cursor.diagnostics_rev <-
          List.rev_append
            (Preprocessor.take_pending_diagnostics cursor.stream)
            cursor.diagnostics_rev;
        raise Stop_command);
      pull cursor
  | Lexer.Token token ->
      let selection =
        if cursor.stop_on_error then
          Some
            ( cursor.symbols,
              Symbol_visibility.Environment.find_preprocessor cursor.symbols
                token.raw )
        else None
      in
      let local_selection =
        match selection with
        | Some (_, Symbol_visibility.Shadowed_by_local) ->
            List.find_opt
              (fun publication -> publication.local_spelling = token.raw)
              cursor.local_publications
        | _ -> None
      in
      {
        token;
        context = Preprocessor.diagnostic_context cursor.stream;
        selection;
        local_selection;
      }

let rec ensure_lookahead cursor count =
  if List.length cursor.lookahead >= count then ()
  else (
    cursor.lookahead <- cursor.lookahead @ [ pull cursor ];
    ensure_lookahead cursor count)

let peek_n cursor offset =
  if offset < 0 then invalid_arg "parser lookahead offset cannot be negative";
  if offset >= max_parser_lookahead then
    invalid_arg
      (Printf.sprintf "parser lookahead is limited to %d tokens"
         max_parser_lookahead);
  ensure_lookahead cursor (offset + 1);
  List.nth cursor.lookahead offset

let peek cursor = peek_n cursor 0

let take cursor =
  let item = peek cursor in
  cursor.lookahead <- List.tl cursor.lookahead;
  item

let token_segments token =
  match token.Token.source_segments with
  | [] -> [ token.span ]
  | segments -> segments

let token_location token =
  Ast.make_location ?generated_from:token.Token.origin.generated_from
    ?defined_at:token.origin.defined_at ~span:token.Token.span
    ~source_segments:(token_segments token) ()

let location_before_token token =
  let location = token_location token in
  let empty_span (span : Common.Span.t) =
    Common.Span.unsafe_make ~source:span.source ~start:span.start
      ~stop:span.start
  in
  Ast.make_location ?generated_from:location.generated_from
    ?defined_at:location.defined_at ~span:(empty_span location.span)
    ~source_segments:(List.map empty_span location.source_segments)
    ()

let location_after_location (location : Ast.location) =
  let empty_at_stop (span : Common.Span.t) =
    Common.Span.unsafe_make ~source:span.source ~start:span.stop ~stop:span.stop
  in
  let source_segments =
    match List.rev location.source_segments with
    | last :: _ -> [ empty_at_stop last ]
    | [] -> [ empty_at_stop location.span ]
  in
  Ast.make_location ?generated_from:location.generated_from
    ?defined_at:location.defined_at
    ~span:(empty_at_stop location.span)
    ~source_segments ()

let location_from_tokens = function
  | [] -> invalid_arg "a syntax location needs at least one token"
  | first_token :: _ as tokens ->
      let last_token = List.hd (List.rev tokens) in
      let segments = List.concat_map token_segments tokens in
      let all_in_primary_source =
        List.for_all
          (fun segment ->
            Common.Source_id.equal segment.Common.Span.source
              first_token.Token.span.source)
          segments
      in
      let span =
        if all_in_primary_source then
          Common.Span.unsafe_make ~source:first_token.span.source
            ~start:first_token.span.start ~stop:last_token.span.stop
        else first_token.span
      in
      Ast.make_location ~span ~source_segments:segments ()

let location_from_expression_tokens = function
  | [] -> invalid_arg "an expression location needs at least one token"
  | first_token :: _ as tokens ->
      let base = location_from_tokens tokens in
      Ast.make_location ?generated_from:first_token.Token.origin.generated_from
        ?defined_at:first_token.origin.defined_at ~span:base.span
        ~source_segments:base.source_segments ()

let location_from_locations (locations : Ast.location list) =
  match locations with
  | [] -> invalid_arg "a syntax location needs at least one child location"
  | (first : Ast.location) :: _ ->
      let last : Ast.location = List.hd (List.rev locations) in
      let all_in_primary_source =
        List.for_all
          (fun (location : Ast.location) ->
            Common.Source_id.equal location.Ast.span.source
              first.Ast.span.source)
          locations
      in
      let span =
        if all_in_primary_source then
          Common.Span.unsafe_make ~source:first.span.source
            ~start:first.span.start ~stop:last.span.stop
        else first.span
      in
      let source_segments =
        List.concat_map
          (fun (location : Ast.location) -> location.Ast.source_segments)
          locations
      in
      Ast.make_location ?generated_from:first.generated_from
        ?defined_at:first.defined_at ~span ~source_segments ()

let token_text token =
  match token.Token.value with
  | Token.Text text -> text
  | _ -> token.raw

let token_description token =
  match token.Token.kind with
  | Token_kind.Eof -> "end of input"
  | Token_kind.Identifier -> Printf.sprintf "identifier %S" (token_text token)
  | _ when String.length token.raw > 0 -> Printf.sprintf "%S" token.raw
  | kind -> Token_kind.name kind

let same_related (left : Common.Diagnostic.related)
    (right : Common.Diagnostic.related) =
  String.equal left.message right.message
  && Common.Span.compare left.span right.span = 0

let append_unique_related items additions =
  List.fold_left
    (fun result item ->
      if List.exists (same_related item) result then result
      else result @ [ item ])
    items additions

let report ?(secondary = []) cursor item ~code ~message =
  let secondary =
    append_unique_related item.context.definition_trace secondary
  in
  let diagnostic =
    Common.Diagnostic.make ~secondary ~include_stack:item.context.include_stack
      ~code ~severity:Common.Diagnostic.Error ~message ~primary:item.token.span
      ()
  in
  cursor.diagnostics_rev <- diagnostic :: cursor.diagnostics_rev;
  if cursor.stop_on_error then raise Stop_command

let publish_declaration cursor at event =
  Option.iter
    (fun consume ->
      record_observation (Option.get cursor.current_command).command_context
        (Declaration event);
      match consume event with
      | Ok () -> ()
      | Error diagnostics ->
          cursor.diagnostics_rev <-
            List.rev_append diagnostics cursor.diagnostics_rev;
          if not (has_error diagnostics) then
            report cursor at ~code:"HCPARSE0161"
              ~message:"declaration consumer failed without an error diagnostic";
          raise Stop_command)
    cursor.declaration

let cache_dimension_count cursor at receipt =
  Option.iter
    (fun read ->
      match read receipt with
      | Ok None -> ()
      | Ok (Some (original, count)) ->
          if original != receipt then (
            report cursor at ~code:"HCPARSE0161"
              ~message:"array count reader returned another completed dimension";
            raise Stop_command);
          Dimension_table.add cursor.dimension_counts receipt.dimension_ast
            count
      | Error diagnostics ->
          cursor.diagnostics_rev <-
            List.rev_append diagnostics cursor.diagnostics_rev;
          if not (has_error diagnostics) then
            report cursor at ~code:"HCPARSE0161"
              ~message:"array count reader failed without an error diagnostic";
          raise Stop_command)
    cursor.dimension_count

let expression_identifier cursor item =
  let identifier =
    Ast.make_identifier ~spelling:item.token.raw
      ~location:(token_location item.token)
  in
  Option.iter
    (fun (environment, lookup) ->
      let selection =
        {
          identifier;
          environment;
          lookup;
          selected_command = Option.get cursor.current_command;
          reference_active = false;
        }
      in
      Identifier_table.add cursor.references identifier selection;
      Option.iter
        (fun reference ->
          record_observation selection.selected_command.command_context
            (Reference selection);
          selection.reference_active <- true;
          match
            Fun.protect
              ~finally:(fun () -> selection.reference_active <- false)
              (fun () -> reference selection)
          with
          | Ok () -> ()
          | Error diagnostics ->
              cursor.diagnostics_rev <-
                List.rev_append diagnostics cursor.diagnostics_rev;
              if not (has_error diagnostics) then
                report cursor item ~code:"HCPARSE0161"
                  ~message:
                    "reference consumer failed without an error diagnostic";
              raise Stop_command)
        cursor.reference)
    item.selection;
  Ast.Identifier_expression identifier

let identifier_lookup cursor identifier =
  match Identifier_table.find_opt cursor.references identifier with
  | Some selection -> selection.lookup
  | None ->
      Symbol_visibility.Environment.find_preprocessor cursor.symbols
        identifier.Ast.spelling

let call_result cursor item = function
  | Ok value -> value
  | Error diagnostics ->
      cursor.diagnostics_rev <-
        List.rev_append diagnostics cursor.diagnostics_rev;
      if not (has_error diagnostics) then
        report cursor item ~code:"HCPARSE0161"
          ~message:"call consumer failed without an error diagnostic";
      raise Stop_command

let start_direct_call cursor item callee opening =
  match (cursor.call, callee) with
  | Some consume, Ast.Identifier_expression identifier -> (
      match Identifier_table.find_opt cursor.references identifier with
      | Some reference ->
          let receipt =
            {
              call_reference = reference;
              call_callee = callee;
              call_opening_parenthesis = opening;
              call_activity = { call_active = true; call_captured = false };
            }
          in
          let shape =
            Fun.protect
              ~finally:(fun () -> receipt.call_activity.call_active <- false)
              (fun () ->
                record_observation reference.selected_command.command_context
                  (Call_start receipt);
                call_result cursor item (consume.start receipt))
          in
          (Some receipt, shape)
      | None -> (None, None))
  | _ -> (None, None)

let retain_direct_call cursor start expression =
  Option.iter
    (fun call_start ->
      cursor.pending_calls <-
        {
          call_start;
          call_expression = expression;
          emission_activity = { call_active = false; call_captured = false };
        }
        :: cursor.pending_calls)
    start

let emit_direct_call cursor item expression =
  match
    List.find_opt
      (fun receipt -> receipt.call_expression == expression)
      cursor.pending_calls
  with
  | None -> ()
  | Some receipt ->
      cursor.pending_calls <-
        List.filter (fun pending -> pending != receipt) cursor.pending_calls;
      receipt.emission_activity.call_active <- true;
      Fun.protect
        ~finally:(fun () -> receipt.emission_activity.call_active <- false)
        (fun () ->
          let consume = Option.get cursor.call in
          record_observation
            receipt.call_start.call_reference.selected_command.command_context
            (Call_emission receipt);
          call_result cursor item (consume.emit receipt))

let publish_query cursor item event =
  Option.iter
    (fun consume ->
      match consume event with
      | Ok () -> ()
      | Error diagnostics ->
          cursor.diagnostics_rev <-
            List.rev_append diagnostics cursor.diagnostics_rev;
          if not (has_error diagnostics) then
            report cursor item ~code:"HCPARSE0161"
              ~message:"query consumer failed without an error diagnostic";
          raise Stop_command)
    cursor.query

let start_query cursor keyword item query_node =
  Option.map
    (fun (query_environment, query_lookup) ->
      let query_present =
        match (item.token.kind, query_lookup) with
        | ( (Token_kind.Identifier | Token_kind.Keyword _),
            (Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local)
          ) -> true
        | _ -> false
      in
      let root =
        {
          query_node;
          query_location = token_location keyword.token;
          query_environment;
          query_lookup;
          query_local = item.local_selection;
          query_present;
          query_command = Option.get cursor.current_command;
        }
      in
      publish_query cursor item (Query_root root);
      (root, ref []))
    item.selection

let start_query_member cursor item query member_start_dot =
  Option.map
    (fun (query_member_root, members) ->
      let query_member_ordinal =
        match !members with
        | [] -> 0
        | previous :: _ -> previous.query_member_ordinal + 1
      in
      let start =
        {
          member_start_root = query_member_root;
          member_start_ordinal = query_member_ordinal;
          member_start_dot;
        }
      in
      publish_query cursor item (Query_member_started start);
      start)
    query

let query_member cursor item query start query_member_node =
  Option.iter
    (fun (query_member_root, members) ->
      let query_member_start = Option.get start in
      let query_member_ordinal = query_member_start.member_start_ordinal in
      let member =
        {
          query_member_root;
          query_member_ordinal;
          query_member_node;
          query_member_start;
        }
      in
      publish_query cursor item (Query_member member);
      members := member :: !members)
    query

let complete_query cursor item query query_expression =
  Option.iter
    (fun (query_root, members) ->
      publish_query cursor item
        (Query_completed
           { query_root; query_members = List.rev !members; query_expression }))
    query

let rec recover_declaration cursor =
  let item = peek cursor in
  match item.token.Token.kind with
  | Token_kind.Eof -> ()
  | Token_kind.Punctuation ';' -> ignore (take cursor)
  | _ ->
      ignore (take cursor);
      recover_declaration cursor

let recover_aggregate_declaration cursor ~depth =
  let rec skip depth =
    let item = peek cursor in
    match item.token.Token.kind with
    | Token_kind.Eof -> ()
    | Token_kind.Punctuation '{' ->
        ignore (take cursor);
        skip (depth + 1)
    | Token_kind.Punctuation '}' when depth > 0 ->
        ignore (take cursor);
        if depth = 1 then (
          if (peek cursor).token.kind = Token_kind.Punctuation ';' then
            ignore (take cursor))
        else skip (depth - 1)
    | Token_kind.Punctuation ';' when depth = 0 -> ignore (take cursor)
    | _ ->
        ignore (take cursor);
        skip depth
  in
  skip depth

let rec statement_boundary_stops_at_closing_brace = function
  | Block_boundary | Switch_boundary -> true
  | For_update_boundary boundary ->
      statement_boundary_stops_at_closing_brace boundary
  | Top_level_boundary -> false

let rec statement_boundary_is_switch = function
  | Switch_boundary -> true
  | For_update_boundary boundary -> statement_boundary_is_switch boundary
  | Top_level_boundary | Block_boundary -> false

let token_is_switch_boundary token =
  match token.Token.kind with
  | Token_kind.Keyword
      (Keyword.Case | Keyword.Default | Keyword.Start | Keyword.End) -> true
  | _ -> false

let rec recover_statement cursor ~boundary =
  let item = peek cursor in
  match item.token.Token.kind with
  | Token_kind.Eof -> ()
  | Token_kind.Punctuation '}'
    when statement_boundary_stops_at_closing_brace boundary -> ()
  | _
    when statement_boundary_is_switch boundary
         && token_is_switch_boundary item.token -> ()
  | Token_kind.Punctuation ')' -> (
      match boundary with
      | For_update_boundary _ -> ()
      | Top_level_boundary | Block_boundary | Switch_boundary ->
          ignore (take cursor);
          recover_statement cursor ~boundary)
  | Token_kind.Punctuation ';' -> ignore (take cursor)
  | _ ->
      ignore (take cursor);
      recover_statement cursor ~boundary

let recover_static_initializer cursor ~boundary ~open_braces =
  let rec skip open_braces =
    if open_braces = 0 then recover_statement cursor ~boundary
    else
      let item = peek cursor in
      match item.token.kind with
      | Token_kind.Eof -> ()
      | Token_kind.Punctuation '{' ->
          ignore (take cursor);
          skip (open_braces + 1)
      | Token_kind.Punctuation '}' ->
          ignore (take cursor);
          skip (open_braces - 1)
      | _ ->
          ignore (take cursor);
          skip open_braces
  in
  skip open_braces

let recover_switch_tail cursor ~boundary =
  let rec skip_body depth =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Eof -> ()
    | Token_kind.Punctuation '{' ->
        ignore (take cursor);
        skip_body (depth + 1)
    | Token_kind.Punctuation '}' when depth > 1 ->
        ignore (take cursor);
        skip_body (depth - 1)
    | Token_kind.Punctuation '}' -> ignore (take cursor)
    | _ ->
        ignore (take cursor);
        skip_body depth
  in
  let rec seek_body () =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Eof -> ()
    | Token_kind.Punctuation '{' ->
        ignore (take cursor);
        skip_body 1
    | Token_kind.Punctuation ';' -> ignore (take cursor)
    | Token_kind.Punctuation '}'
      when statement_boundary_stops_at_closing_brace boundary -> ()
    | _
      when statement_boundary_is_switch boundary
           && token_is_switch_boundary item.token -> ()
    | _ ->
        ignore (take cursor);
        seek_body ()
  in
  seek_body ()

let recover_for_header cursor ~boundary =
  let rec skip nested_parentheses nested_braces =
    let item = peek cursor in
    match item.token.Token.kind with
    | Token_kind.Eof -> ()
    | Token_kind.Punctuation '(' ->
        ignore (take cursor);
        skip (nested_parentheses + 1) nested_braces
    | Token_kind.Punctuation ')' ->
        ignore (take cursor);
        if nested_parentheses > 0 then
          skip (nested_parentheses - 1) nested_braces
        else if nested_braces > 0 then skip nested_parentheses nested_braces
    | Token_kind.Punctuation '{' ->
        ignore (take cursor);
        skip nested_parentheses (nested_braces + 1)
    | Token_kind.Punctuation '}' when nested_braces > 0 ->
        ignore (take cursor);
        skip nested_parentheses (nested_braces - 1)
    | Token_kind.Punctuation '}'
      when statement_boundary_stops_at_closing_brace boundary -> ()
    | _ ->
        ignore (take cursor);
        skip nested_parentheses nested_braces
  in
  skip 0 0

let rec statement_body_boundary = function
  | For_update_boundary boundary -> statement_body_boundary boundary
  | boundary -> boundary

let is_for_update_boundary = function
  | For_update_boundary _ -> true
  | Top_level_boundary | Block_boundary | Switch_boundary -> false

let recover_compound_statement cursor =
  let rec skip depth =
    let item = peek cursor in
    match item.token.Token.kind with
    | Token_kind.Eof -> ()
    | Token_kind.Punctuation '{' ->
        ignore (take cursor);
        skip (depth + 1)
    | Token_kind.Punctuation '}' ->
        ignore (take cursor);
        if depth > 1 then skip (depth - 1)
    | _ ->
        ignore (take cursor);
        skip depth
  in
  skip 0

let rec parse_pointer_layers_with_recovery cursor ~recover depth layers_rev
    items_rev =
  let item = peek cursor in
  match item.token.Token.kind with
  | Token_kind.Punctuation '*' ->
      if depth = max_pointer_depth then (
        report cursor item ~code:"HCPARSE0004"
          ~message:
            (Printf.sprintf
               "HolyC types may have at most %d pointer stars; this star \
                exceeds that limit"
               max_pointer_depth);
        recover cursor;
        None)
      else
        let item = take cursor in
        let depth = depth + 1 in
        let layer =
          Ast.make_pointer_layer ~depth ~spelling:item.token.raw
            ~location:(token_location item.token)
        in
        parse_pointer_layers_with_recovery cursor ~recover depth
          (layer :: layers_rev) (item :: items_rev)
  | _ -> Some (List.rev layers_rev, List.rev items_rev)

let parse_pointer_layers cursor depth layers_rev items_rev =
  parse_pointer_layers_with_recovery cursor ~recover:recover_declaration depth
    layers_rev items_rev

let parse_aggregate_backing_pointer_layers cursor =
  parse_pointer_layers_with_recovery cursor
    ~recover:(fun cursor -> recover_aggregate_declaration cursor ~depth:0)
    0 [] []

let pointer_definition_trace pointer_items =
  List.fold_left
    (fun trace item ->
      append_unique_related trace item.context.definition_trace)
    [] pointer_items

let type_spelling base_spelling pointer_layers =
  base_spelling ^ String.make (List.length pointer_layers) '*'

let primitive_type_of_token token =
  match token.Token.kind with
  | Token_kind.Identifier ->
      Common.Primitive_type.of_spelling (token_text token)
  | _ -> None

let internal_type_of_token cursor token =
  match token.Token.kind with
  | Token_kind.Identifier -> (
      match
        Symbol_visibility.Environment.find_preprocessor cursor.symbols
          (token_text token)
      with
      | Symbol_visibility.Present entry
        when Symbol_visibility.kind entry = Symbol_visibility.Internal_type ->
          Common.Primitive_type.of_storage_spelling (token_text token)
      | Symbol_visibility.Present _
      | Symbol_visibility.Absent
      | Symbol_visibility.Shadowed_by_local -> None)
  | _ -> None

let token_is_named_type cursor token =
  match token.Token.kind with
  | Token_kind.Identifier | Token_kind.Keyword _ -> (
      match
        Symbol_visibility.Environment.find_preprocessor cursor.symbols
          (token_text token)
      with
      | Symbol_visibility.Present entry ->
          Symbol_visibility.kind entry = Symbol_visibility.Class
      | Symbol_visibility.Absent | Symbol_visibility.Shadowed_by_local -> false)
  | _ -> false

let type_specifier_of_item cursor item =
  match primitive_type_of_token item.token with
  | Some primitive ->
      Some
        (Ast.Primitive_type_specifier
           (Ast.make_primitive_type ~primitive ~spelling:item.token.raw
              ~location:(token_location item.token)))
  | None -> (
      match internal_type_of_token cursor item.token with
      | Some primitive ->
          Some
            (Ast.Internal_type_specifier
               (Ast.make_internal_type ~primitive ~spelling:item.token.raw
                  ~location:(token_location item.token)))
      | None when token_is_named_type cursor item.token ->
          Some
            (Ast.Named_type_specifier
               (Ast.make_identifier ~spelling:item.token.raw
                  ~location:(token_location item.token)))
      | None -> None)

let symbol_source_origin (location : Ast.location) =
  Symbol_visibility.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let publish_global cursor (name : Ast.identifier) =
  Symbol_visibility.Environment.add cursor.symbols ~name:name.spelling
    ~kind:Symbol_visibility.Global_variable
    ~origin:(symbol_source_origin name.location)
    ()

let publish_class cursor (name : Ast.identifier) =
  ignore
    (Symbol_visibility.Environment.add cursor.symbols ~name:name.spelling
       ~kind:Symbol_visibility.Class
       ~origin:(symbol_source_origin name.location)
       ())

let function_call_shape parameters variadic =
  let function_call_shape : Symbol_visibility.function_call_shape =
    {
      parameters =
        List.map
          (fun (parameter : Ast.function_parameter) ->
            {
              Symbol_visibility.parameter_name =
                Option.map
                  (fun (name : Ast.identifier) -> name.spelling)
                  parameter.name;
              has_default = Option.is_some parameter.default;
            })
          parameters;
      variadic = Option.is_some variadic;
    }
  in
  function_call_shape

let publish_function cursor (name : Ast.identifier) parameters variadic =
  let function_call_shape = function_call_shape parameters variadic in
  ignore
    (Symbol_visibility.Environment.add cursor.symbols ~name:name.spelling
       ~kind:Symbol_visibility.Function ~function_call_shape
       ~origin:(symbol_source_origin name.location)
       ())

let declaration_header cursor ~modifiers ~binding ~type_specifier =
  {
    declaration_sources = cursor.sources;
    declaration_source = cursor.source;
    declaration_command = Option.get cursor.current_command;
    modifiers;
    binding;
    type_specifier;
  }

let declare_function cursor header (prefix : parsed_declarator_prefix) opening =
  if not cursor.stop_on_error then None
  else
    let function_previous =
      match
        Symbol_visibility.Environment.find_function cursor.symbols
          prefix.name.spelling
      with
      | Some entry -> Symbol_visibility.Present entry
      | None -> Symbol_visibility.Absent
    in
    let function_entry =
      Symbol_visibility.Environment.add cursor.symbols
        ~name:prefix.name.spelling ~kind:Symbol_visibility.Function
        ~origin:(symbol_source_origin prefix.name.location)
        ()
    in
    let publication =
      {
        function_activity = { function_active = true };
        function_header = header;
        function_environment = cursor.symbols;
        function_entry;
        function_previous;
        function_name = prefix.name;
        function_pointer_layers = prefix.pointer_layers;
        function_opening_parenthesis = token_location opening.token;
      }
    in
    Fun.protect
      ~finally:(fun () ->
        publication.function_activity.function_active <- false)
      (fun () ->
        publish_declaration cursor opening (Function_declared publication));
    Some publication

let complete_function_header cursor at publication
    (parsed : parsed_parameter_list) =
  Option.map
    (fun function_publication ->
      let completed_entry =
        Symbol_visibility.Environment.complete_function_header cursor.symbols
          ~entry:function_publication.function_entry
          ~function_call_shape:
            (function_call_shape parsed.parameters parsed.variadic)
        |> function
        | Ok entry -> entry
        | Error message -> invalid_arg message
      in
      (* Native Lex retains the hash object across the lookahead after ')' or
         an unterminated variadic marker.
       Finish only this cursor's unconsumed selections of that exact object;
       already delivered references and nested cursors retain their snapshots. *)
      cursor.lookahead <-
        List.map
          (fun item ->
            match item.selection with
            | Some (environment, Symbol_visibility.Present entry)
              when environment == cursor.symbols
                   && entry == function_publication.function_entry ->
                {
                  item with
                  selection =
                    Some (environment, Symbol_visibility.Present completed_entry);
                }
            | _ -> item)
          cursor.lookahead;
      let completed =
        {
          function_publication;
          completed_entry;
          parameters = parsed.parameters;
          parameter_completions = parsed.parameter_completions;
          empty_parameter_entries = parsed.empty_parameter_entries;
          variadic = parsed.variadic;
          variadic_publication = parsed.variadic_publication;
          closing_parenthesis = parsed.closing_parenthesis;
          header_activity =
            { function_header_active = true; function_body_active = None };
        }
      in
      Fun.protect
        ~finally:(fun () ->
          completed.header_activity.function_header_active <- false)
        (fun () ->
          publish_declaration cursor at (Function_header_completed completed));
      completed)
    publication

let publish_local cursor ~spelling source =
  match cursor.local_context with
  | None -> invalid_arg "local declaration parsed outside a function context"
  | Some context -> (
      match
        Symbol_visibility.Environment.add_local cursor.symbols context
          ~name:spelling
      with
      | Ok () ->
          Option.iter
            (fun local_command ->
              cursor.local_publications <-
                {
                  local_environment = cursor.symbols;
                  local_command;
                  local_spelling = spelling;
                  local_source = source;
                }
                :: cursor.local_publications)
            cursor.current_command
      | Error message -> invalid_arg message)

let with_function_local_context cursor parameters variadic run =
  if Option.is_some cursor.local_context then
    invalid_arg "function local contexts cannot be nested";
  let context =
    Symbol_visibility.Environment.begin_local_context cursor.symbols
  in
  cursor.local_context <- Some context;
  List.iter
    (fun (parameter : Ast.function_parameter) ->
      Option.iter
        (fun (name : Ast.identifier) ->
          publish_local cursor ~spelling:name.spelling
            (Local_parameter parameter))
        parameter.name)
    parameters;
  Option.iter
    (fun marker ->
      publish_local cursor ~spelling:"argc" (Variadic_count marker);
      publish_local cursor ~spelling:"argv" (Variadic_vector marker))
    variadic;
  Fun.protect run ~finally:(fun () ->
      cursor.local_context <- None;
      cursor.local_publications <- [];
      match
        Symbol_visibility.Environment.end_local_context cursor.symbols context
      with
      | Ok () -> ()
      | Error message -> invalid_arg message)

let delimiter_kind token =
  match token.Token.kind with
  | Token_kind.Punctuation ',' -> Some Ast.Comma
  | Token_kind.Punctuation ';' -> Some Ast.Semicolon
  | _ -> None

(* TempleOS has no keyword tokens. [Lex] hands back [TK_IDENT] for every word,
   and [PrsKeyWord] (Compiler/PrsLib.HC:31-38) is what turns one into a keyword
   at the sites that ask. Name positions never ask, so a keyword spelling is an
   ordinary name in a declarator or after a member dot. *)
let token_is_name_position_identifier token =
  match token.Token.kind with
  | Token_kind.Identifier | Token_kind.Keyword _ -> true
  | _ -> false

let token_is_contextual_identifier_operand cursor token =
  match token.Token.kind with
  | Token_kind.Identifier | Token_kind.Keyword _ -> (
      match
        Symbol_visibility.Environment.find_preprocessor cursor.symbols
          (token_text token)
      with
      | Symbol_visibility.Shadowed_by_local -> true
      | Symbol_visibility.Present entry -> (
          match Symbol_visibility.kind entry with
          | Symbol_visibility.Global_variable | Symbol_visibility.Function ->
              true
          | Symbol_visibility.Export_system_symbol
          | Symbol_visibility.Import_system_symbol
          | Symbol_visibility.Definition
          | Symbol_visibility.Class
          | Symbol_visibility.Internal_type
          | Symbol_visibility.Word
          | Symbol_visibility.Dictionary_word
          | Symbol_visibility.Keyword
          | Symbol_visibility.Assembly_keyword
          | Symbol_visibility.Opcode
          | Symbol_visibility.Register
          | Symbol_visibility.File
          | Symbol_visibility.Module
          | Symbol_visibility.Help_file
          | Symbol_visibility.Frame_pointer -> false)
      | Symbol_visibility.Absent -> false)
  | _ -> false

let declaration_modifier_kind token =
  match token.Token.kind with
  | Token_kind.Keyword Keyword.Public -> Some Ast.Public
  | Token_kind.Keyword Keyword.Static -> Some Ast.Static
  | Token_kind.Keyword Keyword.Interrupt -> Some Ast.Interrupt
  | Token_kind.Keyword Keyword.Haserrcode -> Some Ast.Has_error_code
  | Token_kind.Keyword Keyword.Argpop -> Some Ast.Argument_pop
  | Token_kind.Keyword Keyword.Noargpop -> Some Ast.No_argument_pop
  | _ -> None

let rec parse_modifiers cursor (modifiers_rev : parsed_modifier list) =
  let item = peek cursor in
  match declaration_modifier_kind item.token with
  | None -> List.rev modifiers_rev
  | Some kind ->
      let item = take cursor in
      let node =
        Ast.make_declaration_modifier ~kind ~spelling:item.token.raw
          ~location:(token_location item.token)
      in
      parse_modifiers cursor ({ node; item } :: modifiers_rev)

let parse_declarator_prefix cursor base_spelling ~parse_function_pointer =
  match parse_pointer_layers cursor 0 [] [] with
  | None -> None
  | Some (pointer_layers, pointer_items) ->
      let pointer_tokens = List.map (fun item -> item.token) pointer_items in
      let pointer_trace = pointer_definition_trace pointer_items in
      let name_item = peek cursor in
      if name_item.token.kind = Token_kind.Punctuation '(' then
        Option.map
          (fun (parsed : parsed_function_pointer) ->
            let name =
              match parsed.name with
              | Some name -> name
              | None ->
                  invalid_arg
                    "global function-pointer parser returned an unnamed \
                     declarator"
            in
            {
              pointer_layers;
              name;
              function_pointer = Some parsed.node;
              tokens = pointer_tokens @ parsed.tokens;
              definition_trace = pointer_trace;
              name_selection = parsed.name_selection;
            })
          (parse_function_pointer ())
      else if not (token_is_name_position_identifier name_item.token) then (
        report ~secondary:pointer_trace cursor name_item ~code:"HCPARSE0002"
          ~message:
            (Printf.sprintf "expected an identifier after type %S, but found %s"
               (type_spelling base_spelling pointer_layers)
               (token_description name_item.token));
        recover_declaration cursor;
        None)
      else
        let name_item = take cursor in
        let name =
          Ast.make_identifier ~spelling:name_item.token.raw
            ~location:(token_location name_item.token)
        in
        Some
          {
            pointer_layers;
            name;
            function_pointer = None;
            tokens = pointer_tokens @ [ name_item.token ];
            definition_trace = pointer_trace;
            name_selection = name_item.selection;
          }

let declaration_failure ?(secondary = []) cursor item ~code ~message =
  report ~secondary cursor item ~code ~message;
  recover_declaration cursor;
  None

let local_declaration_failure cursor ~boundary item ~code ~message =
  report cursor item ~code ~message;
  recover_statement cursor ~boundary;
  None

let function_pointer_declaration_failure cursor ~declarator_context item ~code
    ~message =
  match declarator_context with
  | Local_variable_declarator boundary ->
      local_declaration_failure cursor ~boundary item ~code ~message
  | Function_parameter_declarator
  | Global_variable_declarator
  | Aggregate_member_declarator ->
      declaration_failure cursor item ~code ~message

let recover_function_pointer_declaration cursor = function
  | Local_variable_declarator boundary -> recover_statement cursor ~boundary
  | Function_parameter_declarator
  | Global_variable_declarator
  | Aggregate_member_declarator -> recover_declaration cursor

let unsupported_parameter_form cursor item ~code description =
  declaration_failure cursor item ~code
    ~message:
      (Printf.sprintf "%s are not implemented in function prototypes"
         description)

let rec parse_register_qualifiers cursor ~position nodes_rev tokens_rev =
  let item = peek cursor in
  let kind =
    match item.token.kind with
    | Token_kind.Keyword Keyword.Reg -> Some Ast.Reg
    | Token_kind.Keyword Keyword.Noreg -> Some Ast.Noreg
    | _ -> None
  in
  match kind with
  | None -> { nodes = List.rev nodes_rev; tokens = List.rev tokens_rev }
  | Some kind ->
      let keyword_item = take cursor in
      let tokens_rev = keyword_item.token :: tokens_rev in
      let explicit_item =
        match kind with
        | Ast.Noreg -> None
        | Ast.Reg ->
            let candidate = peek cursor in
            if
              candidate.token.kind = Token_kind.Identifier
              && Common.Canonical_registers.is_canonical_u64_register
                   (token_text candidate.token)
            then Some (take cursor)
            else None
      in
      let tokens_rev =
        match explicit_item with
        | None -> tokens_rev
        | Some explicit_item -> explicit_item.token :: tokens_rev
      in
      let explicit_register =
        Option.map
          (fun explicit_item ->
            Ast.make_identifier ~spelling:explicit_item.token.raw
              ~location:(token_location explicit_item.token))
          explicit_item
      in
      let node =
        Ast.make_register_qualifier ~kind ~position
          ~spelling:keyword_item.token.raw ~explicit_register
          ~location:(token_location keyword_item.token)
      in
      parse_register_qualifiers cursor ~position (node :: nodes_rev) tokens_rev

let unary_operator_kind token =
  match token.Token.kind with
  | Token_kind.Punctuation '+' -> Some Ast.Unary_plus
  | Token_kind.Punctuation '-' -> Some Ast.Unary_minus
  | Token_kind.Punctuation '!' -> Some Ast.Logical_not
  | Token_kind.Punctuation '~' -> Some Ast.Bitwise_not
  | Token_kind.Punctuation '*' -> Some Ast.Dereference
  | Token_kind.Punctuation '&' -> Some Ast.Address_of
  | Token_kind.Operator Operator.Increment -> Some Ast.Pre_increment
  | Token_kind.Operator Operator.Decrement -> Some Ast.Pre_decrement
  | _ -> None

let postfix_operator_kind token =
  match token.Token.kind with
  | Token_kind.Operator Operator.Increment -> Some Ast.Post_increment
  | Token_kind.Operator Operator.Decrement -> Some Ast.Post_decrement
  | _ -> None

let is_postfix_continuation token =
  match token.Token.kind with
  | Token_kind.Punctuation ('(' | '[' | '.')
  | Token_kind.Operator
      (Operator.Arrow | Operator.Increment | Operator.Decrement) -> true
  | _ -> false

let restricted_modifier_term = function
  | Ast.Sizeof_expression _ -> Some ("sizeof", "HCPARSE0034")
  | Ast.Offset_expression _ -> Some ("offset", "HCPARSE0039")
  | Ast.Defined_expression _ -> Some ("defined", "HCPARSE0042")
  | _ -> None

let binary_operator token =
  match token.Token.kind with
  | Token_kind.Punctuation _ | Token_kind.Operator _ ->
      Operator.find_binary token.raw
  | _ -> None

let binary_binding_power (operator : Operator.binary_operator) =
  0x100 - operator.precedence_value

let make_expression_operator token =
  Ast.make_expression_operator ~spelling:token.Token.raw
    ~location:(token_location token)

let make_literal ?origin token value constructor =
  let origin = Option.value origin ~default:Ast.Source_literal in
  constructor
    (Ast.make_expression_literal ~origin ~spelling:token.Token.raw ~value
       ~location:(token_location token))

let take_string_literal_sequence cursor : parsed_expression =
  let rec take_segments segments_rev tokens_rev values_rev spellings_rev =
    let item = peek cursor in
    match (item.token.Token.kind, item.token.value) with
    | Token_kind.String, Token.Bytes value ->
        let item = take cursor in
        let segment =
          Ast.make_expression_literal_segment ~spelling:item.token.raw ~value
            ~location:(token_location item.token)
        in
        take_segments (segment :: segments_rev) (item.token :: tokens_rev)
          (value :: values_rev)
          (item.token.raw :: spellings_rev)
    | _ ->
        ( List.rev segments_rev,
          List.rev tokens_rev,
          String.concat "" (List.rev values_rev),
          String.concat "" (List.rev spellings_rev) )
  in
  let segments, tokens, value, spelling = take_segments [] [] [] [] in
  match tokens with
  | [] -> invalid_arg "a string literal sequence needs at least one token"
  | [ token ] ->
      {
        node =
          make_literal token (Ast.Bytes_value value) (fun literal ->
              Ast.String_literal literal);
        tokens;
      }
  | _ ->
      let literal =
        Ast.make_segmented_expression_literal ~segments ~spelling
          ~value:(Ast.Bytes_value value)
          ~location:(location_from_expression_tokens tokens)
      in
      { node = Ast.String_literal literal; tokens }

let rebuild_prefix prefix operand =
  let location =
    location_from_locations
      [
        prefix.Ast.prefix_operator.operator_location;
        Ast.expression_location operand;
      ]
  in
  Ast.Prefix_expression
    (Ast.make_prefix_expression ~operator_kind:prefix.prefix_operator_kind
       ~operator:prefix.prefix_operator ~operand ~location)

let rec split_power_sensitive_minus expression =
  match expression with
  | Ast.Prefix_expression prefix
    when prefix.prefix_operator_kind = Ast.Unary_minus ->
      Some (prefix.prefix_operand, fun operand -> rebuild_prefix prefix operand)
  | Ast.Prefix_expression prefix -> (
      match split_power_sensitive_minus prefix.prefix_operand with
      | None -> None
      | Some (base, wrap) ->
          Some (base, fun operand -> rebuild_prefix prefix (wrap operand)))
  | _ -> None

let make_binary_expression left operator_item operator_spec right =
  let operator = make_expression_operator operator_item.token in
  let location =
    location_from_locations
      [
        Ast.expression_location left;
        operator.operator_location;
        Ast.expression_location right;
      ]
  in
  Ast.Binary_expression
    (Ast.make_binary_expression ~left ~operator ~operator_spec ~right ~location)

let combine_binary_expression left operator_item operator_spec right =
  if String.equal operator_spec.Operator.ic_name "IC_POWER" then
    match split_power_sensitive_minus left with
    | None -> make_binary_expression left operator_item operator_spec right
    | Some (base, wrap) ->
        wrap (make_binary_expression base operator_item operator_spec right)
  else make_binary_expression left operator_item operator_spec right

let expression_failure ?(secondary = []) cursor item ~code ~message =
  declaration_failure ~secondary cursor item ~code ~message

let expression_context_name = function
  | Default_expression -> "default expression"
  | Array_dimension_expression -> "array dimension expression"
  | Intern_binding_expression -> "_intern target expression"
  | Call_argument_expression -> "call argument expression"
  | Index_expression -> "index expression"
  | Implicit_output_argument_expression -> "implicit output argument"
  | Return_expression -> "return expression"
  | Do_while_condition_expression -> "do-while condition expression"
  | For_condition_expression -> "for condition expression"
  | If_condition_expression -> "if condition expression"
  | Switch_expression -> "switch expression"
  | Switch_case_expression -> "switch case expression"
  | While_condition_expression -> "while condition expression"
  | Local_initializer_expression -> "local initializer expression"
  | Global_initializer_expression -> "global initializer expression"
  | Aggregate_offset_expression -> "aggregate offset expression"
  | Aggregate_member_metadata_expression ->
      "aggregate member metadata expression"
  | Inline_assembly_operand_expression -> "inline assembly operand expression"
  | Statement_expression -> "statement expression"

let expression_operand_name = function
  | Default_expression -> "a default expression operand"
  | Array_dimension_expression -> "an array dimension expression operand"
  | Intern_binding_expression -> "an _intern target expression operand"
  | Call_argument_expression -> "a call argument expression operand"
  | Index_expression -> "an index expression operand"
  | Implicit_output_argument_expression -> "an implicit output argument"
  | Return_expression -> "a return expression operand"
  | Do_while_condition_expression -> "a do-while condition expression operand"
  | For_condition_expression -> "a for condition expression operand"
  | If_condition_expression -> "an if condition expression operand"
  | Switch_expression -> "a switch expression operand"
  | Switch_case_expression -> "a switch case expression operand"
  | While_condition_expression -> "a while condition expression operand"
  | Local_initializer_expression -> "a local initializer expression operand"
  | Global_initializer_expression -> "a global initializer expression operand"
  | Aggregate_offset_expression -> "an aggregate offset expression operand"
  | Aggregate_member_metadata_expression ->
      "an aggregate member metadata expression operand"
  | Inline_assembly_operand_expression ->
      "an inline assembly operand expression"
  | Statement_expression -> "a statement expression operand"

let rec parse_expression ?(allow_parenthesis_free_call = true) cursor ~context
    ~depth ~minimum_binding_power : parsed_expression option =
  let item = peek cursor in
  if depth >= max_expression_depth then
    expression_failure cursor item ~code:"HCPARSE0021"
      ~message:
        (Printf.sprintf "%s nesting exceeds the hosted limit of %d"
           (expression_context_name context)
           max_expression_depth)
  else
    match
      parse_expression_prefix cursor ~context ~depth
        ~allow_parenthesis_free_call
    with
    | None -> None
    | Some left ->
        parse_expression_tail cursor ~context ~depth ~minimum_binding_power
          ~allow_parenthesis_free_call left

and parse_expression_prefix cursor ~context ~depth ~allow_parenthesis_free_call
    : parsed_expression option =
  let item = peek cursor in
  match unary_operator_kind item.token with
  | Some operator_kind -> (
      let operator_item = take cursor in
      let operator = make_expression_operator operator_item.token in
      let allow_parenthesis_free_call =
        allow_parenthesis_free_call && operator_kind <> Ast.Address_of
      in
      let direct_function_address =
        operator_kind = Ast.Address_of
        && depth + 1 < max_expression_depth
        &&
        let operand = peek cursor in
        match operand.token.kind with
        | Token_kind.Identifier | Token_kind.Keyword _ -> (
            let lookup =
              match operand.selection with
              | Some (_, lookup) -> lookup
              | None ->
                  Symbol_visibility.Environment.find_preprocessor cursor.symbols
                    operand.token.raw
            in
            match lookup with
            | Symbol_visibility.Present entry ->
                Symbol_visibility.kind entry = Symbol_visibility.Function
            | Symbol_visibility.Absent | Symbol_visibility.Shadowed_by_local ->
                false)
        | _ -> false
      in
      let operand =
        if direct_function_address then
          (* PrsExp.HC:621-654 returns the function address before ordinary
             modifiers. In particular, a following cast applies to that address,
             and does not enter the direct function-call grammar. *)
          parse_expression_atom cursor ~context ~depth:(depth + 1)
        else
          parse_expression ~allow_parenthesis_free_call cursor ~context
            ~depth:(depth + 1) ~minimum_binding_power:max_int
      in
      match operand with
      | None -> None
      | Some (operand : parsed_expression) ->
          let tokens = operator_item.token :: operand.tokens in
          let location = location_from_expression_tokens tokens in
          let node =
            Ast.Prefix_expression
              (Ast.make_prefix_expression ~operator_kind ~operator
                 ~operand:operand.node ~location)
          in
          Some { node; tokens })
  | None -> parse_expression_atom cursor ~context ~depth

and parse_expression_atom cursor ~context ~depth : parsed_expression option =
  let item = peek cursor in
  let take_literal ?origin value
      (constructor : Ast.expression_literal -> Ast.expression) :
      parsed_expression option =
    let item = take cursor in
    Some
      ({
         node = make_literal ?origin item.token value constructor;
         tokens = [ item.token ];
       }
        : parsed_expression)
  in
  match (item.token.Token.kind, item.token.value) with
  | Token_kind.Integer, Token.Int64 value ->
      take_literal (Ast.Integer_value value) (fun literal ->
          Ast.Integer_literal literal)
  | Token_kind.Float, Token.Float64 value ->
      take_literal (Ast.Float_value value) (fun literal ->
          Ast.Float_literal literal)
  | Token_kind.Character, Token.Int64 value ->
      take_literal (Ast.Integer_value value) (fun literal ->
          Ast.Character_literal literal)
  | Token_kind.String, Token.Bytes _ ->
      Some (take_string_literal_sequence cursor)
  | Token_kind.Inserted_binary, Token.Bytes value ->
      let record = Option.get item.token.binary_record in
      let origin : Ast.inserted_binary_origin =
        {
          record_number = record.number;
          declared_size = record.declared_size;
          payload_complete = record.payload_complete;
        }
      in
      take_literal ~origin:(Ast.Inserted_binary_literal origin)
        (Ast.Bytes_value value) (fun literal -> Ast.String_literal literal)
  | Token_kind.Inserted_binary_size, Token.Int64 value ->
      let record = Option.get item.token.binary_record in
      let origin : Ast.inserted_binary_origin =
        {
          record_number = record.number;
          declared_size = record.declared_size;
          payload_complete = record.payload_complete;
        }
      in
      take_literal ~origin:(Ast.Inserted_binary_size_literal origin)
        (Ast.Integer_value value) (fun literal -> Ast.Integer_literal literal)
  | (Token_kind.Identifier | Token_kind.Keyword _), _
    when token_is_contextual_identifier_operand cursor item.token ->
      let item = take cursor in
      let node = expression_identifier cursor item in
      Some { node; tokens = [ item.token ] }
  | Token_kind.Identifier, _ ->
      let item = take cursor in
      let node = expression_identifier cursor item in
      Some { node; tokens = [ item.token ] }
  | Token_kind.Operator Operator.Current_position, _ ->
      let item = take cursor in
      let node =
        Ast.Current_position_expression (make_expression_operator item.token)
      in
      Some { node; tokens = [ item.token ] }
  | Token_kind.Keyword Keyword.Sizeof, _ ->
      parse_sizeof_expression cursor ~context
  | Token_kind.Keyword Keyword.Offset, _ ->
      parse_offset_expression cursor ~context
  | Token_kind.Keyword Keyword.Defined, _ ->
      parse_defined_expression cursor ~context
  | Token_kind.Punctuation '(', _ -> (
      let opening = take cursor in
      let first = peek cursor in
      match type_specifier_of_item cursor first with
      | Some _ ->
          expression_failure cursor first ~code:"HCPARSE0029"
            ~message:
              (Printf.sprintf
                 "C-style cast syntax is not valid HolyC in %s; write the cast \
                  after its operand, for example value(%s)"
                 (expression_context_name context)
                 (token_text first.token))
      | None -> (
          match
            parse_expression cursor ~context ~depth:(depth + 1)
              ~minimum_binding_power:0
          with
          | None -> None
          | Some expression ->
              let closing = peek cursor in
              if closing.token.kind <> Token_kind.Punctuation ')' then
                expression_failure cursor closing ~code:"HCPARSE0019"
                  ~message:
                    (Printf.sprintf "expected ')' to close %s, but found %s"
                       (expression_context_name context)
                       (token_description closing.token))
              else
                let closing = take cursor in
                let tokens =
                  (opening.token :: expression.tokens) @ [ closing.token ]
                in
                let location = location_from_expression_tokens tokens in
                let node =
                  Ast.Parenthesized_expression
                    (Ast.make_parenthesized_expression
                       ~opening_parenthesis:(token_location opening.token)
                       ~expression:expression.node
                       ~closing_parenthesis:(token_location closing.token)
                       ~location)
                in
                Some { node; tokens }))
  | (Token_kind.Punctuation (',' | ')' | ']' | ';') | Token_kind.Eof), _ ->
      expression_failure cursor item ~code:"HCPARSE0018"
        ~message:
          (Printf.sprintf "expected %s, but found %s"
             (expression_operand_name context)
             (token_description item.token))
  | _ ->
      expression_failure cursor item ~code:"HCPARSE0020"
        ~message:
          (Printf.sprintf "%s form starting with %s is not implemented"
             (expression_context_name context)
             (token_description item.token))

and parse_sizeof_expression cursor ~context : parsed_expression option =
  let keyword_item = take cursor in
  let rec take_opening_parentheses items_rev =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Punctuation '(' ->
        take_opening_parentheses (take cursor :: items_rev)
    | _ -> List.rev items_rev
  in
  let opening_items = take_opening_parentheses [] in
  let target_item = peek cursor in
  if target_item.token.kind <> Token_kind.Identifier then
    expression_failure cursor target_item ~code:"HCPARSE0031"
      ~message:
        (Printf.sprintf "expected a named sizeof target in %s, but found %s"
           (expression_context_name context)
           (token_description target_item.token))
  else
    let target_item = take cursor in
    let target =
      Ast.make_identifier ~spelling:target_item.token.raw
        ~location:(token_location target_item.token)
    in
    let query =
      start_query cursor keyword_item target_item (Sizeof_target target)
    in
    let rec take_members members_rev items_rev =
      let item = peek cursor in
      match item.token.kind with
      | Token_kind.Punctuation '.' ->
          let dot_item = take cursor in
          let dot = token_location dot_item.token in
          let member_start = start_query_member cursor dot_item query dot in
          let name_item = peek cursor in
          if not (token_is_name_position_identifier name_item.token) then
            expression_failure ~secondary:dot_item.context.definition_trace
              cursor name_item ~code:"HCPARSE0032"
              ~message:
                (Printf.sprintf
                   "expected a member name after '.' in sizeof target, but \
                    found %s"
                   (token_description name_item.token))
          else
            let name_item = take cursor in
            let name =
              Ast.make_identifier ~spelling:name_item.token.raw
                ~location:(token_location name_item.token)
            in
            let member =
              Ast.make_sizeof_member ~dot ~name
                ~location:
                  (location_from_expression_tokens
                     [ dot_item.token; name_item.token ])
            in
            query_member cursor name_item query member_start
              (Sizeof_member member);
            take_members (member :: members_rev)
              (name_item :: dot_item :: items_rev)
      | _ -> Some (List.rev members_rev, List.rev items_rev)
    in
    match take_members [] [] with
    | None -> None
    | Some (members, member_items) -> (
        let rec take_pointer_layers depth layers_rev items_rev =
          let item = peek cursor in
          match item.token.kind with
          | Token_kind.Punctuation '*' ->
              let item = take cursor in
              let depth = depth + 1 in
              let layer =
                Ast.make_pointer_layer ~depth ~spelling:item.token.raw
                  ~location:(token_location item.token)
              in
              take_pointer_layers depth (layer :: layers_rev) (item :: items_rev)
          | _ -> (List.rev layers_rev, List.rev items_rev)
        in
        let pointer_layers, pointer_items = take_pointer_layers 0 [] [] in
        let items_before_closing =
          (keyword_item :: (opening_items @ (target_item :: member_items)))
          @ pointer_items
        in
        let definition_trace =
          List.fold_left
            (fun trace item ->
              append_unique_related trace item.context.definition_trace)
            [] items_before_closing
        in
        let rec take_closing_parentheses remaining items_rev =
          if remaining = 0 then Some (List.rev items_rev)
          else
            let item = peek cursor in
            match item.token.kind with
            | Token_kind.Punctuation ')' ->
                take_closing_parentheses (remaining - 1)
                  (take cursor :: items_rev)
            | _ ->
                let remaining_text =
                  if remaining = 1 then "one wrapper parenthesis remains"
                  else Printf.sprintf "%d wrapper parentheses remain" remaining
                in
                expression_failure ~secondary:definition_trace cursor item
                  ~code:"HCPARSE0033"
                  ~message:
                    (Printf.sprintf
                       "expected ')' to close sizeof target in %s; %s, but \
                        found %s"
                       (expression_context_name context)
                       remaining_text
                       (token_description item.token))
        in
        match take_closing_parentheses (List.length opening_items) [] with
        | None -> None
        | Some closing_items ->
            let tokens =
              List.map
                (fun item -> item.token)
                (items_before_closing @ closing_items)
            in
            let node =
              Ast.Sizeof_expression
                (Ast.make_sizeof_expression
                   ~keyword_spelling:keyword_item.token.raw
                   ~keyword_location:(token_location keyword_item.token)
                   ~opening_parentheses:
                     (List.map
                        (fun item -> token_location item.token)
                        opening_items)
                   ~target ~members ~pointer_layers
                   ~closing_parentheses:
                     (List.map
                        (fun item -> token_location item.token)
                        closing_items)
                   ~location:(location_from_expression_tokens tokens))
            in
            complete_query cursor target_item query node;
            Some { node; tokens })

and parse_offset_expression cursor ~context : parsed_expression option =
  let keyword_item = take cursor in
  let rec take_opening_parentheses items_rev =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Punctuation '(' ->
        take_opening_parentheses (take cursor :: items_rev)
    | _ -> List.rev items_rev
  in
  let opening_items = take_opening_parentheses [] in
  let target_item = peek cursor in
  if target_item.token.kind <> Token_kind.Identifier then
    expression_failure cursor target_item ~code:"HCPARSE0035"
      ~message:
        (Printf.sprintf "expected a named offset target in %s, but found %s"
           (expression_context_name context)
           (token_description target_item.token))
  else
    let target_item = take cursor in
    let target =
      Ast.make_identifier ~spelling:target_item.token.raw
        ~location:(token_location target_item.token)
    in
    let query =
      start_query cursor keyword_item target_item (Offset_target target)
    in
    let first_dot = peek cursor in
    if first_dot.token.kind <> Token_kind.Punctuation '.' then
      expression_failure cursor first_dot ~code:"HCPARSE0036"
        ~message:
          (Printf.sprintf
             "expected '.' after named offset target %S in %s, but found %s"
             target.spelling
             (expression_context_name context)
             (token_description first_dot.token))
    else
      let rec take_members members_rev items_rev =
        let dot_item = take cursor in
        let dot = token_location dot_item.token in
        let member_start = start_query_member cursor dot_item query dot in
        let name_item = peek cursor in
        if not (token_is_name_position_identifier name_item.token) then
          expression_failure ~secondary:dot_item.context.definition_trace cursor
            name_item ~code:"HCPARSE0037"
            ~message:
              (Printf.sprintf
                 "expected a member name after '.' in offset target, but found \
                  %s"
                 (token_description name_item.token))
        else
          let name_item = take cursor in
          let name =
            Ast.make_identifier ~spelling:name_item.token.raw
              ~location:(token_location name_item.token)
          in
          let member =
            Ast.make_offset_member ~dot ~name
              ~location:
                (location_from_expression_tokens
                   [ dot_item.token; name_item.token ])
          in
          query_member cursor name_item query member_start
            (Offset_member member);
          let members_rev = member :: members_rev in
          let items_rev = name_item :: dot_item :: items_rev in
          let following = peek cursor in
          match following.token.kind with
          | Token_kind.Punctuation '.' -> take_members members_rev items_rev
          | _ -> Some (List.rev members_rev, List.rev items_rev)
      in
      match take_members [] [] with
      | None -> None
      | Some (members, member_items) -> (
          let items_before_closing =
            keyword_item :: (opening_items @ (target_item :: member_items))
          in
          let definition_trace =
            List.fold_left
              (fun trace item ->
                append_unique_related trace item.context.definition_trace)
              [] items_before_closing
          in
          let rec take_closing_parentheses remaining items_rev =
            if remaining = 0 then Some (List.rev items_rev)
            else
              let item = peek cursor in
              match item.token.kind with
              | Token_kind.Punctuation ')' ->
                  take_closing_parentheses (remaining - 1)
                    (take cursor :: items_rev)
              | _ ->
                  let remaining_text =
                    if remaining = 1 then "one wrapper parenthesis remains"
                    else
                      Printf.sprintf "%d wrapper parentheses remain" remaining
                  in
                  expression_failure ~secondary:definition_trace cursor item
                    ~code:"HCPARSE0038"
                    ~message:
                      (Printf.sprintf
                         "expected ')' to close offset target in %s; %s, but \
                          found %s"
                         (expression_context_name context)
                         remaining_text
                         (token_description item.token))
          in
          match take_closing_parentheses (List.length opening_items) [] with
          | None -> None
          | Some closing_items ->
              let tokens =
                List.map
                  (fun item -> item.token)
                  (items_before_closing @ closing_items)
              in
              let node =
                Ast.Offset_expression
                  (Ast.make_offset_expression
                     ~keyword_spelling:keyword_item.token.raw
                     ~keyword_location:(token_location keyword_item.token)
                     ~opening_parentheses:
                       (List.map
                          (fun item -> token_location item.token)
                          opening_items)
                     ~target ~members
                     ~closing_parentheses:
                       (List.map
                          (fun item -> token_location item.token)
                          closing_items)
                     ~location:(location_from_expression_tokens tokens))
              in
              complete_query cursor target_item query node;
              Some { node; tokens })

and parse_defined_expression cursor ~context : parsed_expression option =
  let keyword_item = take cursor in
  let rec take_opening_parentheses items_rev =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Punctuation '(' ->
        take_opening_parentheses (take cursor :: items_rev)
    | _ -> List.rev items_rev
  in
  let opening_items = take_opening_parentheses [] in
  let operand_item = peek cursor in
  if operand_item.token.kind = Token_kind.Eof then
    let definition_trace =
      List.fold_left
        (fun trace item ->
          append_unique_related trace item.context.definition_trace)
        keyword_item.context.definition_trace opening_items
    in
    expression_failure ~secondary:definition_trace cursor operand_item
      ~code:"HCPARSE0040"
      ~message:
        (Printf.sprintf
           "expected one token after defined in %s, but reached end of input"
           (expression_context_name context))
  else
    let operand_item = take cursor in
    let operand_kind =
      match operand_item.token.kind with
      | Token_kind.Identifier | Token_kind.Keyword _ -> Ast.Defined_name
      | _ -> Ast.Defined_non_name
    in
    let operand =
      Ast.make_defined_operand ~kind:operand_kind
        ~spelling:operand_item.token.raw
        ~location:(token_location operand_item.token)
    in
    let query =
      start_query cursor keyword_item operand_item (Defined_target operand)
    in
    let items_before_closing =
      keyword_item :: (opening_items @ [ operand_item ])
    in
    let definition_trace =
      List.fold_left
        (fun trace item ->
          append_unique_related trace item.context.definition_trace)
        [] items_before_closing
    in
    let rec take_closing_parentheses remaining items_rev =
      if remaining = 0 then Some (List.rev items_rev)
      else
        let item = peek cursor in
        match item.token.kind with
        | Token_kind.Punctuation ')' ->
            take_closing_parentheses (remaining - 1) (take cursor :: items_rev)
        | _ ->
            let remaining_text =
              if remaining = 1 then "one wrapper parenthesis remains"
              else Printf.sprintf "%d wrapper parentheses remain" remaining
            in
            expression_failure ~secondary:definition_trace cursor item
              ~code:"HCPARSE0041"
              ~message:
                (Printf.sprintf
                   "expected ')' to close defined target in %s; %s, but found \
                    %s"
                   (expression_context_name context)
                   remaining_text
                   (token_description item.token))
    in
    match take_closing_parentheses (List.length opening_items) [] with
    | None -> None
    | Some closing_items ->
        let tokens =
          List.map
            (fun item -> item.token)
            (items_before_closing @ closing_items)
        in
        let node =
          Ast.Defined_expression
            (Ast.make_defined_expression
               ~keyword_spelling:keyword_item.token.raw
               ~keyword_location:(token_location keyword_item.token)
               ~opening_parentheses:
                 (List.map
                    (fun item -> token_location item.token)
                    opening_items)
               ~operand
               ~closing_parentheses:
                 (List.map
                    (fun item -> token_location item.token)
                    closing_items)
               ~location:(location_from_expression_tokens tokens))
        in
        complete_query cursor operand_item query node;
        Some { node; tokens }

and parse_parenthesis_free_call ?start cursor ~depth
    (callee : parsed_expression) (shape : Symbol_visibility.function_call_shape)
    : parsed_expression option =
  let callee_name =
    match callee.node with
    | Ast.Identifier_expression identifier -> identifier.spelling
    | _ -> invalid_arg "a parenthesis-free call needs a direct function name"
  in
  let token_cannot_start_argument token =
    match token.Token.kind with
    | Token_kind.Punctuation (',' | ')' | ']' | '}' | ';') | Token_kind.Eof ->
        true
    | _ -> false
  in
  let missing_argument_message index parameter item =
    let name =
      match parameter.Symbol_visibility.parameter_name with
      | None -> ""
      | Some name -> Printf.sprintf " (%s)" name
    in
    Printf.sprintf
      "parenthesis-free call to %S requires argument %d%s, but found %s"
      callee_name index name
      (token_description item.token)
  in
  let rec collect index arguments_rev tokens_rev insertion_location = function
    | [] ->
        let tokens = List.rev tokens_rev in
        let node =
          Ast.Call_expression
            (Ast.make_call_expression ~callee:callee.node
               ~syntax:Ast.Parenthesis_free_call
               ~arguments:(List.rev arguments_rev)
               ~location:(location_from_expression_tokens tokens))
        in
        retain_direct_call cursor start node;
        Some ({ node; tokens } : parsed_expression)
    | parameter :: parameters -> (
        if parameter.Symbol_visibility.has_default then
          let argument =
            Ast.make_call_argument ~value:Ast.Omitted_call_argument
              ~following_comma:None
              ~location:(location_after_location insertion_location)
          in
          collect (index + 1)
            (argument :: arguments_rev)
            tokens_rev insertion_location parameters
        else
          let item = peek cursor in
          if token_cannot_start_argument item.token then
            expression_failure cursor item ~code:"HCPARSE0105"
              ~message:(missing_argument_message index parameter item)
          else
            match
              parse_expression cursor ~context:Call_argument_expression
                ~depth:(depth + 1) ~minimum_binding_power:0
            with
            | None -> None
            | Some (expression : parsed_expression) ->
                let argument =
                  Ast.make_call_argument
                    ~value:(Ast.Provided_call_argument expression.node)
                    ~following_comma:None
                    ~location:(Ast.expression_location expression.node)
                in
                collect (index + 1)
                  (argument :: arguments_rev)
                  (List.rev_append expression.tokens tokens_rev)
                  (Ast.expression_location expression.node)
                  parameters)
  in
  collect 1 [] (List.rev callee.tokens)
    (Ast.expression_location callee.node)
    shape.parameters

and parse_call_suffix ?start ?shape cursor ~context ~depth
    (callee : parsed_expression) opening ~opening_location :
    parsed_expression option =
  let build arguments_rev interior_tokens_rev closing =
    let suffix_tokens = List.rev (closing.token :: interior_tokens_rev) in
    let tokens = callee.tokens @ (opening.token :: suffix_tokens) in
    let node =
      Ast.Call_expression
        (Ast.make_call_expression ~callee:callee.node
           ~syntax:
             (Ast.Parenthesized_call
                {
                  opening_parenthesis = opening_location;
                  closing_parenthesis = token_location closing.token;
                })
           ~arguments:(List.rev arguments_rev)
           ~location:(location_from_expression_tokens tokens))
    in
    retain_direct_call cursor start node;
    Some ({ node; tokens } : parsed_expression)
  in
  let omitted_argument delimiter =
    Ast.make_call_argument ~value:Ast.Omitted_call_argument
      ~following_comma:None
      ~location:(location_before_token delimiter.token)
  in
  let parse_supplied_shape (shape : Symbol_visibility.function_call_shape) =
    let missing_close item =
      expression_failure cursor item ~code:"HCPARSE0025"
        ~message:
          (Printf.sprintf "expected ')' to close a call in %s, but found %s"
             (expression_context_name context)
             (token_description item.token))
    in
    let close arguments_rev tokens_rev =
      let item = peek cursor in
      if item.token.kind = Token_kind.Punctuation ')' then
        build arguments_rev tokens_rev (take cursor)
      else missing_close item
    in
    let add_comma argument comma =
      Ast.make_call_argument ~value:argument.Ast.call_argument_value
        ~following_comma:(Some (token_location comma.token))
        ~location:argument.call_argument_location
    in
    let missing_comma item =
      expression_failure cursor item ~code:"HCPARSE0024"
        ~message:
          (Printf.sprintf "expected ',' after a call argument, but found %s"
             (token_description item.token))
    in
    let rec variadic arguments_rev tokens_rev =
      match
        parse_expression cursor ~context:Call_argument_expression
          ~depth:(depth + 1) ~minimum_binding_power:0
      with
      | None -> None
      | Some (expression : parsed_expression) ->
          let argument =
            Ast.make_call_argument
              ~value:(Ast.Provided_call_argument expression.node)
              ~following_comma:None
              ~location:(Ast.expression_location expression.node)
          in
          let tokens_rev = List.rev_append expression.tokens tokens_rev in
          let following = peek cursor in
          if following.token.kind = Token_kind.Punctuation ',' then
            let comma = take cursor in
            variadic
              (add_comma argument comma :: arguments_rev)
              (comma.token :: tokens_rev)
          else close (argument :: arguments_rev) tokens_rev
    in
    let rec fixed arguments_rev tokens_rev = function
      | [] ->
          let item = peek cursor in
          if shape.variadic && item.token.kind <> Token_kind.Punctuation ')'
          then variadic arguments_rev tokens_rev
          else close arguments_rev tokens_rev
      | parameter :: remaining -> (
          let item = peek cursor in
          let argument =
            if
              parameter.Symbol_visibility.has_default
              && (item.token.kind = Token_kind.Punctuation ')'
                 || item.token.kind = Token_kind.Punctuation ',')
            then Some (omitted_argument item, tokens_rev)
            else
              Option.map
                (fun (expression : parsed_expression) ->
                  ( Ast.make_call_argument
                      ~value:(Ast.Provided_call_argument expression.node)
                      ~following_comma:None
                      ~location:(Ast.expression_location expression.node),
                    List.rev_append expression.tokens tokens_rev ))
                (parse_expression cursor ~context:Call_argument_expression
                   ~depth:(depth + 1) ~minimum_binding_power:0)
          in
          match argument with
          | None -> None
          | Some (argument, tokens_rev) ->
              let following = peek cursor in
              if remaining <> [] then
                if following.token.kind = Token_kind.Punctuation ',' then
                  let comma = take cursor in
                  fixed
                    (add_comma argument comma :: arguments_rev)
                    (comma.token :: tokens_rev)
                    remaining
                else if following.token.kind = Token_kind.Punctuation ')' then
                  fixed (argument :: arguments_rev) tokens_rev remaining
                else missing_comma following
              else if
                shape.variadic
                && following.token.kind <> Token_kind.Punctuation ')'
              then
                if following.token.kind = Token_kind.Punctuation ',' then
                  let comma = take cursor in
                  variadic
                    (add_comma argument comma :: arguments_rev)
                    (comma.token :: tokens_rev)
                else missing_comma following
              else close (argument :: arguments_rev) tokens_rev)
    in
    fixed [] [] shape.parameters
  in
  let rec parse_arguments arguments_rev interior_tokens_rev after_comma =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Punctuation ')' ->
        let closing = take cursor in
        let arguments_rev =
          if after_comma then omitted_argument closing :: arguments_rev
          else arguments_rev
        in
        build arguments_rev interior_tokens_rev closing
    | Token_kind.Punctuation ',' ->
        let comma = take cursor in
        let argument =
          Ast.make_call_argument ~value:Ast.Omitted_call_argument
            ~following_comma:(Some (token_location comma.token))
            ~location:(location_before_token comma.token)
        in
        parse_arguments
          (argument :: arguments_rev)
          (comma.token :: interior_tokens_rev)
          true
    | Token_kind.Eof ->
        expression_failure cursor item ~code:"HCPARSE0025"
          ~message:
            (Printf.sprintf "expected ')' to close a call in %s"
               (expression_context_name context))
    | _ -> (
        match
          parse_expression cursor ~context:Call_argument_expression
            ~depth:(depth + 1) ~minimum_binding_power:0
        with
        | None -> None
        | Some (expression : parsed_expression) -> (
            let following = peek cursor in
            let expression_tokens_rev =
              List.rev_append expression.tokens interior_tokens_rev
            in
            match following.token.kind with
            | Token_kind.Punctuation ',' ->
                let comma = take cursor in
                let argument =
                  Ast.make_call_argument
                    ~value:(Ast.Provided_call_argument expression.node)
                    ~following_comma:(Some (token_location comma.token))
                    ~location:(Ast.expression_location expression.node)
                in
                parse_arguments
                  (argument :: arguments_rev)
                  (comma.token :: expression_tokens_rev)
                  true
            | Token_kind.Punctuation ')' ->
                let closing = take cursor in
                let argument =
                  Ast.make_call_argument
                    ~value:(Ast.Provided_call_argument expression.node)
                    ~following_comma:None
                    ~location:(Ast.expression_location expression.node)
                in
                build (argument :: arguments_rev) expression_tokens_rev closing
            | Token_kind.Eof ->
                expression_failure cursor following ~code:"HCPARSE0025"
                  ~message:
                    (Printf.sprintf "expected ')' to close a call in %s"
                       (expression_context_name context))
            | _ ->
                expression_failure cursor following ~code:"HCPARSE0024"
                  ~message:
                    (Printf.sprintf
                       "expected ',' or ')' after a call argument, but found %s"
                       (token_description following.token))))
  in
  match shape with
  | Some shape -> parse_supplied_shape shape
  | None -> parse_arguments [] [] false

and parse_postfix_cast_suffix cursor ~context (operand : parsed_expression)
    opening type_specifier : parsed_expression option =
  let type_item = take cursor in
  match parse_pointer_layers cursor 0 [] [] with
  | None -> None
  | Some (pointer_layers, pointer_items) ->
      let pointer_tokens = List.map (fun item -> item.token) pointer_items in
      let definition_trace =
        append_unique_related opening.context.definition_trace
          type_item.context.definition_trace
        |> fun trace ->
        append_unique_related trace (pointer_definition_trace pointer_items)
      in
      let closing = peek cursor in
      if closing.token.kind <> Token_kind.Punctuation ')' then
        expression_failure ~secondary:definition_trace cursor closing
          ~code:"HCPARSE0030"
          ~message:
            (Printf.sprintf
               "expected ')' to close postfix cast to %S in %s, but found %s"
               (type_spelling (token_text type_item.token) pointer_layers)
               (expression_context_name context)
               (token_description closing.token))
      else
        let closing = take cursor in
        let tokens =
          operand.tokens
          @ (opening.token :: type_item.token :: pointer_tokens)
          @ [ closing.token ]
        in
        let node =
          Ast.Postfix_cast_expression
            (Ast.make_postfix_cast_expression ~operand:operand.node
               ~opening_parenthesis:(token_location opening.token)
               ~type_specifier ~pointer_layers
               ~closing_parenthesis:(token_location closing.token)
               ~location:(location_from_expression_tokens tokens))
        in
        Some { node; tokens }

and parse_index_suffix cursor ~context ~depth (base : parsed_expression) :
    parsed_expression option =
  let opening = take cursor in
  match
    parse_expression cursor ~context:Index_expression ~depth:(depth + 1)
      ~minimum_binding_power:0
  with
  | None -> None
  | Some (index : parsed_expression) ->
      let closing = peek cursor in
      if closing.token.kind <> Token_kind.Punctuation ']' then
        expression_failure cursor closing ~code:"HCPARSE0026"
          ~message:
            (Printf.sprintf
               "expected ']' to close an index expression in %s, but found %s"
               (expression_context_name context)
               (token_description closing.token))
      else
        let closing = take cursor in
        let tokens =
          base.tokens @ (opening.token :: index.tokens) @ [ closing.token ]
        in
        let node =
          Ast.Index_expression
            (Ast.make_index_expression ~base:base.node
               ~opening_bracket:(token_location opening.token)
               ~index:index.node
               ~closing_bracket:(token_location closing.token)
               ~location:(location_from_expression_tokens tokens))
        in
        Some { node; tokens }

and parse_member_suffix cursor ~context (base : parsed_expression) access_kind :
    parsed_expression option =
  let operator_item = take cursor in
  let member_item = peek cursor in
  if not (token_is_name_position_identifier member_item.token) then
    expression_failure cursor member_item ~code:"HCPARSE0027"
      ~message:
        (Printf.sprintf "expected a member name after %S in %s, but found %s"
           operator_item.token.raw
           (expression_context_name context)
           (token_description member_item.token))
  else
    let member_item = take cursor in
    let operator = make_expression_operator operator_item.token in
    let member =
      Ast.make_identifier ~spelling:member_item.token.raw
        ~location:(token_location member_item.token)
    in
    let tokens = base.tokens @ [ operator_item.token; member_item.token ] in
    let node =
      Ast.Member_expression
        (Ast.make_member_expression ~base:base.node ~access_kind ~operator
           ~member
           ~location:(location_from_expression_tokens tokens))
    in
    Some { node; tokens }

and parse_postfix_update_suffix cursor ~context (operand : parsed_expression)
    operator_kind : parsed_expression option =
  let operator_item = take cursor in
  let operator = make_expression_operator operator_item.token in
  let tokens = operand.tokens @ [ operator_item.token ] in
  let node =
    Ast.Postfix_expression
      (Ast.make_postfix_expression ~operand:operand.node ~operator_kind
         ~operator
         ~location:(location_from_expression_tokens tokens))
  in
  let following = peek cursor in
  if is_postfix_continuation following.token then
    expression_failure cursor following ~code:"HCPARSE0028"
      ~message:
        (Printf.sprintf
           "postfix update %S must end the postfix chain in %s, but found %s"
           operator_item.token.raw
           (expression_context_name context)
           (token_description following.token))
  else Some { node; tokens }

and parse_expression_tail cursor ~context ~depth ~minimum_binding_power
    ~allow_parenthesis_free_call (left : parsed_expression) :
    parsed_expression option =
  let item = peek cursor in
  emit_direct_call cursor item left.node;
  let direct_function =
    match left.node with
    | Ast.Identifier_expression identifier -> (
        match identifier_lookup cursor identifier with
        | Symbol_visibility.Present entry
          when Symbol_visibility.kind entry = Symbol_visibility.Function -> (
            match Symbol_visibility.function_call_shape entry with
            | Some shape -> Direct_function_with_shape shape
            | None -> Direct_function_without_shape entry)
        | Symbol_visibility.Absent
        | Symbol_visibility.Shadowed_by_local
        | Symbol_visibility.Present _ -> Not_a_direct_function)
    | _ -> Not_a_direct_function
  in
  if allow_parenthesis_free_call then
    match direct_function with
    | (Direct_function_without_shape _ | Direct_function_with_shape _)
      when item.token.kind <> Token_kind.Punctuation '(' -> (
        let start, supplied = start_direct_call cursor item left.node None in
        let shape =
          match (supplied, direct_function) with
          | Some shape, _ | None, Direct_function_with_shape shape -> Some shape
          | _ -> None
        in
        match shape with
        | None ->
            let entry =
              match direct_function with
              | Direct_function_without_shape entry -> entry
              | _ -> assert false
            in
            expression_failure cursor item ~code:"HCPARSE0106"
              ~message:
                (Printf.sprintf
                   "cannot parse direct call to %S because its fixed-parameter \
                    shape is unavailable"
                   (Symbol_visibility.name entry))
        | Some shape -> (
            match
              parse_parenthesis_free_call ?start cursor ~depth left shape
            with
            | None -> None
            | Some call ->
                parse_expression_tail cursor ~context ~depth
                  ~minimum_binding_power ~allow_parenthesis_free_call call))
    | Not_a_direct_function
    | Direct_function_without_shape _
    | Direct_function_with_shape _ ->
        parse_expression_modifiers cursor ~context ~depth ~minimum_binding_power
          ~allow_parenthesis_free_call left
  else
    parse_expression_modifiers cursor ~context ~depth ~minimum_binding_power
      ~allow_parenthesis_free_call left

and parse_expression_modifiers cursor ~context ~depth ~minimum_binding_power
    ~allow_parenthesis_free_call (left : parsed_expression) :
    parsed_expression option =
  let item = peek cursor in
  let is_direct_function =
    match left.node with
    | Ast.Identifier_expression identifier -> (
        match identifier_lookup cursor identifier with
        | Symbol_visibility.Present entry
          when Symbol_visibility.kind entry = Symbol_visibility.Function -> true
        | Symbol_visibility.Absent
        | Symbol_visibility.Shadowed_by_local
        | Symbol_visibility.Present _ -> false)
    | _ -> false
  in
  let restricted_term = restricted_modifier_term left.node in
  let invalid_direct_restricted_suffix =
    match (restricted_term, item.token.kind) with
    | ( Some _,
        ( Token_kind.Punctuation ('[' | '.')
        | Token_kind.Operator
            (Operator.Arrow | Operator.Increment | Operator.Decrement) ) ) ->
        true
    | _ -> false
  in
  if invalid_direct_restricted_suffix then
    let term_name, code = Option.get restricted_term in
    expression_failure cursor item ~code
      ~message:
        (Printf.sprintf
           "%s result cannot be followed directly by %s in %s; apply a postfix \
            cast before this suffix"
           term_name
           (token_description item.token)
           (expression_context_name context))
  else
    match item.token.kind with
    | Token_kind.Punctuation '(' -> (
        let opening = take cursor in
        let opening_location = token_location opening.token in
        let suffix =
          if is_direct_function then
            let start, shape =
              start_direct_call cursor item left.node (Some opening_location)
            in
            parse_call_suffix ?start ?shape cursor ~context ~depth left opening
              ~opening_location
          else
            let first = peek cursor in
            match type_specifier_of_item cursor first with
            | Some type_specifier ->
                parse_postfix_cast_suffix cursor ~context left opening
                  type_specifier
            | None -> (
                match restricted_term with
                | Some (term_name, code) ->
                    if first.token.kind = Token_kind.Identifier then
                      expression_failure
                        ~secondary:opening.context.definition_trace cursor first
                        ~code:"HCPARSE0020"
                        ~message:
                          (Printf.sprintf
                             "postfix cast target %s after %s is not a visible \
                              type"
                             (token_description first.token)
                             term_name)
                    else
                      expression_failure
                        ~secondary:opening.context.definition_trace cursor first
                        ~code
                        ~message:
                          (Printf.sprintf
                             "expected a postfix cast target after %s in %s, \
                              but found %s"
                             term_name
                             (expression_context_name context)
                             (token_description first.token))
                | None ->
                    parse_call_suffix cursor ~context ~depth left opening
                      ~opening_location)
        in
        match suffix with
        | None -> None
        | Some expression ->
            parse_expression_tail cursor ~context ~depth ~minimum_binding_power
              ~allow_parenthesis_free_call expression)
    | Token_kind.Punctuation '[' -> (
        match parse_index_suffix cursor ~context ~depth left with
        | None -> None
        | Some index ->
            parse_expression_tail cursor ~context ~depth ~minimum_binding_power
              ~allow_parenthesis_free_call index)
    | Token_kind.Punctuation '.' -> (
        match parse_member_suffix cursor ~context left Ast.Direct_member with
        | None -> None
        | Some member ->
            parse_expression_tail cursor ~context ~depth ~minimum_binding_power
              ~allow_parenthesis_free_call member)
    | Token_kind.Operator Operator.Arrow -> (
        match parse_member_suffix cursor ~context left Ast.Pointer_member with
        | None -> None
        | Some member ->
            parse_expression_tail cursor ~context ~depth ~minimum_binding_power
              ~allow_parenthesis_free_call member)
    | Token_kind.Operator (Operator.Increment | Operator.Decrement) -> (
        match postfix_operator_kind item.token with
        | None -> assert false
        | Some operator_kind -> (
            match
              parse_postfix_update_suffix cursor ~context left operator_kind
            with
            | None -> None
            | Some postfix ->
                parse_expression_binary_tail cursor ~context ~depth
                  ~minimum_binding_power postfix))
    | _ ->
        parse_expression_binary_tail cursor ~context ~depth
          ~minimum_binding_power left

and parse_expression_binary_tail cursor ~context ~depth ~minimum_binding_power
    (left : parsed_expression) : parsed_expression option =
  let item = peek cursor in
  match binary_operator item.token with
  | Some operator_spec
    when binary_binding_power operator_spec >= minimum_binding_power -> (
      let operator_item = take cursor in
      let binding_power = binary_binding_power operator_spec in
      let right_minimum =
        match operator_spec.association with
        | Operator.Right -> binding_power
        | Operator.Left | Operator.Unspecified -> binding_power + 1
      in
      match
        parse_expression cursor ~context ~depth:(depth + 1)
          ~minimum_binding_power:right_minimum
      with
      | None -> None
      | Some (right : parsed_expression) ->
          let node =
            combine_binary_expression left.node operator_item operator_spec
              right.node
          in
          let left : parsed_expression =
            {
              node;
              tokens = left.tokens @ (operator_item.token :: right.tokens);
            }
          in
          parse_expression_binary_tail cursor ~context ~depth
            ~minimum_binding_power left)
  | _ -> Some left

let parse_parameter_default cursor =
  let equals = take cursor in
  let item = peek cursor in
  match item.token.kind with
  | Token_kind.Keyword Keyword.Lastclass ->
      let keyword = take cursor in
      let tokens = [ equals.token; keyword.token ] in
      let lastclass =
        Ast.make_lastclass_default ~spelling:keyword.token.raw
          ~location:(token_location keyword.token)
      in
      let node =
        Ast.make_parameter_default
          ~equals:(token_location equals.token)
          ~value:(Ast.Lastclass_default lastclass)
          ~location:(location_from_expression_tokens tokens)
      in
      Some ({ node; tokens } : parsed_parameter_default)
  | _ -> (
      match
        parse_expression cursor ~context:Default_expression ~depth:0
          ~minimum_binding_power:0
      with
      | None -> None
      | Some (expression : parsed_expression) ->
          let tokens = equals.token :: expression.tokens in
          let node =
            Ast.make_parameter_default
              ~equals:(token_location equals.token)
              ~value:(Ast.Expression_default expression.node)
              ~location:(location_from_expression_tokens tokens)
          in
          Some ({ node; tokens } : parsed_parameter_default))

let declaration_binding_kind token =
  match token.Token.kind with
  | Token_kind.Keyword Keyword.Extern -> Some (Ast.Extern, false)
  | Token_kind.Keyword Keyword.Import -> Some (Ast.Import, false)
  | Token_kind.Keyword Keyword.Underscore_extern -> Some (Ast.Extern, true)
  | Token_kind.Keyword Keyword.Underscore_import -> Some (Ast.Import, true)
  | _ -> None

let aggregate_kind_of_token token =
  match token.Token.kind with
  | Token_kind.Keyword Keyword.Class -> Some Ast.Class_aggregate
  | Token_kind.Keyword Keyword.Union -> Some Ast.Union_aggregate
  | _ -> None

let aggregate_kind_after_backing cursor ~offset =
  let rec inspect offset pointer_count =
    let token = (peek_n cursor offset).token in
    match token.kind with
    | Token_kind.Punctuation '*' when pointer_count <= max_pointer_depth ->
        inspect (offset + 1) (pointer_count + 1)
    | _ -> aggregate_kind_of_token token
  in
  inspect offset 0

let parse_aggregate_backing cursor type_item type_specifier =
  match parse_aggregate_backing_pointer_layers cursor with
  | None -> None
  | Some (pointer_layers, pointer_items) ->
      let tokens =
        type_item.token
        :: List.map (fun (item : located_token) -> item.token) pointer_items
      in
      let node =
        Ast.make_aggregate_backing ~type_specifier ~pointer_layers
          ~location:(location_from_expression_tokens tokens)
      in
      Some ({ node; tokens } : parsed_aggregate_backing)

let parse_aggregate_base cursor =
  let colon_item = peek cursor in
  if colon_item.token.kind <> Token_kind.Punctuation ':' then Some None
  else
    let colon_item = take cursor in
    let base_item = peek cursor in
    if
      (not (token_is_name_position_identifier base_item.token))
      || not (token_is_named_type cursor base_item.token)
    then (
      let message =
        if token_is_name_position_identifier base_item.token then
          Printf.sprintf
            "%S is not a visible class or union and cannot be used as a base"
            base_item.token.raw
        else
          Printf.sprintf
            "expected a visible class or union after ':', but found %s"
            (token_description base_item.token)
      in
      report cursor base_item ~code:"HCPARSE0121" ~message;
      recover_aggregate_declaration cursor ~depth:0;
      None)
    else
      let base_item = take cursor in
      let base_name =
        Ast.make_identifier ~spelling:base_item.token.raw
          ~location:(token_location base_item.token)
      in
      let following_item = peek cursor in
      if following_item.token.kind = Token_kind.Punctuation ',' then (
        report cursor following_item ~code:"HCPARSE0126"
          ~message:
            (Printf.sprintf
               "aggregate definition names a second base after %S; HolyC \
                allows one base class"
               base_name.spelling);
        recover_aggregate_declaration cursor ~depth:0;
        None)
      else
        let tokens = [ colon_item.token; base_item.token ] in
        let node =
          Ast.make_aggregate_base ~colon_spelling:colon_item.token.raw
            ~colon_location:(token_location colon_item.token)
            ~name:base_name
            ~location:(location_from_expression_tokens tokens)
        in
        Some (Some ({ node; tokens } : parsed_aggregate_base))

let parse_binding cursor =
  let item = peek cursor in
  match item.token.kind with
  | Token_kind.Keyword Keyword.Underscore_intern -> (
      let keyword = take cursor in
      let parsed_expression =
        parse_expression cursor ~context:Intern_binding_expression ~depth:0
          ~minimum_binding_power:0
      in
      match parsed_expression with
      | None -> Bad_binding
      | Some expression ->
          let node =
            Ast.make_declaration_binding ~kind:Ast.Intern
              ~spelling:keyword.token.raw
              ~location:(token_location keyword.token)
              ~target:(Ast.Expression_binding_target expression.node)
          in
          Parsed_binding
            { node; keyword; tokens = keyword.token :: expression.tokens })
  | _ -> (
      match declaration_binding_kind item.token with
      | None -> No_binding
      | Some (kind, false) ->
          let item = take cursor in
          let node =
            Ast.make_declaration_binding ~kind ~spelling:item.token.raw
              ~location:(token_location item.token)
              ~target:Ast.No_binding_target
          in
          Parsed_binding { node; keyword = item; tokens = [ item.token ] }
      | Some (kind, true) ->
          let keyword = take cursor in
          let target_item = peek cursor in
          if target_item.token.kind <> Token_kind.Identifier then (
            report cursor target_item ~code:"HCPARSE0007"
              ~message:
                (Printf.sprintf
                   "expected a target symbol after declaration binding %S, but \
                    found %s"
                   keyword.token.raw
                   (token_description target_item.token));
            recover_declaration cursor;
            Bad_binding)
          else
            let target_item = take cursor in
            let target =
              Ast.make_identifier ~spelling:target_item.token.raw
                ~location:(token_location target_item.token)
            in
            let node =
              Ast.make_declaration_binding ~kind ~spelling:keyword.token.raw
                ~location:(token_location keyword.token)
                ~target:(Ast.Symbol_binding_target target)
            in
            Parsed_binding
              { node; keyword; tokens = [ keyword.token; target_item.token ] })

let parse_array_dimension cursor ~owner ~predecessor ~index =
  let opening = take cursor in
  let opening_bracket = token_location opening.token in
  let prepare dimension_expression =
    Option.map
      (fun dimension_owner ->
        let preparation =
          {
            dimension_owner;
            dimension_index = index;
            dimension_predecessor = predecessor;
            dimension_opening = opening_bracket;
            dimension_expression;
            dimension_activity =
              { dimension_active = true; completion_active = false };
          }
        in
        Fun.protect
          ~finally:(fun () ->
            preparation.dimension_activity.dimension_active <- false)
          (fun () ->
            publish_declaration cursor opening
              (Array_dimension_preparing preparation));
        preparation)
      owner
  in
  let complete preparation closing tokens dimension_expression =
    let node =
      Ast.make_array_dimension ~opening_bracket ~dimension_expression
        ~closing_bracket:(token_location closing.token)
        ~location:(location_from_expression_tokens tokens)
    in
    let completed =
      Option.map
        (fun dimension_preparation ->
          let receipt = { dimension_preparation; dimension_ast = node } in
          dimension_preparation.dimension_activity.completion_active <- true;
          Fun.protect
            ~finally:(fun () ->
              dimension_preparation.dimension_activity.completion_active <-
                false)
            (fun () ->
              publish_declaration cursor closing
                (Array_dimension_completed receipt));
          cache_dimension_count cursor closing receipt;
          receipt)
        preparation
    in
    Some (({ node; tokens } : parsed_array_dimension), completed)
  in
  let next_item = peek cursor in
  if next_item.token.kind = Token_kind.Punctuation ']' then
    if index = 0 then
      let preparation = prepare None in
      let closing = take cursor in
      let tokens = [ opening.token; closing.token ] in
      complete preparation closing tokens None
    else
      expression_failure cursor next_item ~code:"HCPARSE0022"
        ~message:"only the first array dimension may be empty"
  else
    match
      parse_expression cursor ~context:Array_dimension_expression ~depth:0
        ~minimum_binding_power:0
    with
    | None -> None
    | Some (expression : parsed_expression) ->
        let preparation = prepare (Some expression.node) in
        let closing = peek cursor in
        if closing.token.kind <> Token_kind.Punctuation ']' then
          expression_failure cursor closing ~code:"HCPARSE0023"
            ~message:
              (Printf.sprintf
                 "expected ']' to close array dimension, but found %s"
                 (token_description closing.token))
        else
          let closing = take cursor in
          let tokens =
            (opening.token :: expression.tokens) @ [ closing.token ]
          in
          complete preparation closing tokens (Some expression.node)

let parse_array_dimensions cursor ~name =
  let owner =
    Option.map
      (fun _ ->
        {
          dimensions_command = Option.get cursor.current_command;
          dimensions_environment = cursor.symbols;
          dimensions_name = name;
        })
      cursor.declaration
  in
  let rec loop predecessor index dimensions_rev token_groups_rev =
    let item = peek cursor in
    if item.token.kind <> Token_kind.Punctuation '[' then
      Some (List.rev dimensions_rev, token_groups_rev |> List.rev |> List.concat)
    else
      match parse_array_dimension cursor ~owner ~predecessor ~index with
      | None -> None
      | Some (dimension, completed) ->
          loop completed (index + 1)
            (dimension.node :: dimensions_rev)
            (dimension.tokens :: token_groups_rev)
  in
  loop None 0 [] []

let initializer_failure ?(secondary = []) ?(local_open_braces = 0) cursor
    ~declarator_context item ~global_code ~local_code ~global_message
    ~local_message =
  match declarator_context with
  | Global_initializer_declarator ->
      declaration_failure ~secondary cursor item ~code:global_code
        ~message:global_message
  | Static_local_initializer_declarator boundary ->
      report ~secondary cursor item ~code:local_code ~message:local_message;
      recover_static_initializer cursor ~boundary ~open_braces:local_open_braces;
      None

type live_initializer = {
  start : global_initializer_start;
  mutable previous_leaf : completed_initializer_leaf option;
  mutable delimiters_rev : initializer_delimiter list;
  mutable previous_delimiter : completed_initializer_delimiter option;
}

let publish_initializer_phase cursor at start phase event =
  start.initializer_activity.initializer_phase <- Some phase;
  Fun.protect
    ~finally:(fun () -> start.initializer_activity.initializer_phase <- None)
    (fun () -> publish_declaration cursor at event)

let publish_initializer_delimiter cursor at live delimiter_value =
  Option.iter
    (fun (state, _) ->
      let delimiter_index =
        Option.fold ~none:0
          ~some:(fun previous -> previous.delimiter_index + 1)
          state.previous_delimiter
      in
      let receipt =
        {
          delimiter_initializer = state.start;
          delimiter_index;
          delimiter_predecessor = state.previous_delimiter;
          delimiter_leaf_predecessor = state.previous_leaf;
          delimiter_value;
        }
      in
      publish_initializer_phase cursor at state.start
        (Completing_delimiter delimiter_index)
        (Global_initializer_delimiter_completed receipt);
      state.previous_delimiter <- Some receipt;
      state.delimiters_rev <- delimiter_value :: state.delimiters_rev)
    live

let publish_initializer_leaf cursor live node =
  Option.iter
    (fun (state, path_rev) ->
      let leaf_index =
        match state.previous_leaf with
        | None -> 0
        | Some previous -> previous.leaf_index + 1
      in
      let leaf =
        {
          leaf_initializer = state.start;
          leaf_index;
          leaf_predecessor = state.previous_leaf;
          leaf_path = List.rev path_rev;
          leaf_value = node;
          leaf_delimiters = List.rev state.delimiters_rev;
          leaf_delimiter_predecessor = state.previous_delimiter;
        }
      in
      publish_initializer_phase cursor (peek cursor) state.start
        (Completing_leaf leaf_index) (Global_initializer_leaf_completed leaf);
      state.previous_leaf <- Some leaf;
      state.delimiters_rev <- [])
    live

let initializer_child live index =
  Option.map (fun (state, path_rev) -> (state, index :: path_rev)) live

let rec parse_initializer_value ?live cursor ~declarator_context ~depth :
    parsed_initializer option =
  let item = peek cursor in
  if depth >= max_initializer_depth then
    initializer_failure cursor ~declarator_context item ~local_open_braces:depth
      ~global_code:"HCPARSE0130" ~local_code:"HCPARSE0141"
      ~global_message:
        (Printf.sprintf
           "global initializer nesting exceeds the hosted limit of %d"
           max_initializer_depth)
      ~local_message:
        (Printf.sprintf
           "static local initializer nesting exceeds the hosted limit of %d"
           max_initializer_depth)
  else
    match item.token.kind with
    | Token_kind.Punctuation '{' ->
        parse_braced_initializer ?live cursor ~declarator_context ~depth
    | Token_kind.Punctuation (';' | ',' | '}') | Token_kind.Eof ->
        initializer_failure cursor ~declarator_context item
          ~local_open_braces:depth ~global_code:"HCPARSE0127"
          ~local_code:"HCPARSE0138"
          ~global_message:
            (Printf.sprintf "expected a global initializer value, but found %s"
               (token_description item.token))
          ~local_message:
            (Printf.sprintf
               "expected a static local initializer value, but found %s"
               (token_description item.token))
    | _ -> (
        let expression_context =
          match declarator_context with
          | Global_initializer_declarator -> Global_initializer_expression
          | Static_local_initializer_declarator _ ->
              Local_initializer_expression
        in
        match
          parse_expression cursor ~context:expression_context ~depth:0
            ~minimum_binding_power:0
        with
        | None -> None
        | Some expression ->
            let node = Ast.Scalar_initializer expression.node in
            publish_initializer_leaf cursor live node;
            Some { node; tokens = expression.tokens })

and parse_braced_initializer ?live cursor ~declarator_context ~depth :
    parsed_initializer option =
  let opening_item = take cursor in
  let opening_brace = token_location opening_item.token in
  publish_initializer_delimiter cursor opening_item live
    (Initializer_open opening_brace);
  let rec parse_elements index elements_rev token_groups_rev :
      parsed_initializer option =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Punctuation '}' ->
        let closing_item = take cursor in
        let closing_brace = token_location closing_item.token in
        publish_initializer_delimiter cursor closing_item live
          (Initializer_close closing_brace);
        let tokens =
          (opening_item.token :: (List.rev token_groups_rev |> List.concat))
          @ [ closing_item.token ]
        in
        let node =
          Ast.make_braced_initializer ~opening_brace
            ~elements:(List.rev elements_rev) ~closing_brace
            ~location:(location_from_expression_tokens tokens)
          |> fun braced -> Ast.Braced_initializer braced
        in
        Some ({ node; tokens } : parsed_initializer)
    | Token_kind.Eof ->
        initializer_failure cursor ~declarator_context item
          ~local_open_braces:(depth + 1) ~global_code:"HCPARSE0129"
          ~local_code:"HCPARSE0140"
          ~secondary:
            [
              ({
                 Common.Diagnostic.span = opening_brace.span;
                 message = "initializer list starts here";
               }
                : Common.Diagnostic.related);
            ]
          ~global_message:"expected '}' to close the global initializer list"
          ~local_message:
            "expected '}' to close the static local initializer list"
    | _ -> (
        match
          parse_initializer_value
            ?live:(initializer_child live index)
            cursor ~declarator_context ~depth:(depth + 1)
        with
        | None -> None
        | Some value ->
            let following_item = peek cursor in
            let comma, element_tokens =
              match following_item.token.kind with
              | Token_kind.Punctuation ',' ->
                  let comma_item = take cursor in
                  let comma = token_location comma_item.token in
                  publish_initializer_delimiter cursor comma_item live
                    (Initializer_comma comma);
                  (Some comma, value.tokens @ [ comma_item.token ])
              | Token_kind.Punctuation '}' -> (None, value.tokens)
              | _ -> (None, [])
            in
            if element_tokens = [] then
              initializer_failure cursor ~declarator_context following_item
                ~local_open_braces:(depth + 1) ~global_code:"HCPARSE0128"
                ~local_code:"HCPARSE0139"
                ~global_message:
                  (Printf.sprintf
                     "expected ',' or '}' after a global initializer element, \
                      but found %s"
                     (token_description following_item.token))
                ~local_message:
                  (Printf.sprintf
                     "expected ',' or '}' after a static local initializer \
                      element, but found %s"
                     (token_description following_item.token))
            else
              let element =
                Ast.make_initializer_element ~value:value.node ~comma
                  ~location:(location_from_expression_tokens element_tokens)
              in
              parse_elements (index + 1) (element :: elements_rev)
                (element_tokens :: token_groups_rev))
  in
  parse_elements 0 [] []

and parse_unbraced_array_initializer ?live cursor ~declarator_context ~depth
    ~allow_closing_brace ~dimensions : parsed_initializer option =
  let item = peek cursor in
  let count =
    match dimensions with
    | [] -> None
    | (dimension : Ast.array_dimension) :: _ -> (
        let value =
          match Dimension_table.find_opt cursor.dimension_counts dimension with
          | Some count -> Some count
          | None -> (
              match dimension.dimension_expression with
              | Some
                  (Ast.Integer_literal
                     { literal_value = Ast.Integer_value value; _ }) ->
                  Some value
              | None | Some _ -> None)
        in
        match value with
        | Some value
          when Int64.compare value 0L > 0
               && Int64.compare value
                    (Int64.of_int max_unbraced_initializer_elements)
                  <= 0 -> Some (Int64.to_int value)
        | None | Some _ -> None)
  in
  match count with
  | None ->
      let bound_kind =
        match dimensions with
        | dimension :: _
          when Dimension_table.mem cursor.dimension_counts dimension ->
            "checked"
        | _ -> "definition-expanded literal"
      in
      initializer_failure cursor ~declarator_context item
        ~local_open_braces:depth ~global_code:"HCPARSE0159"
        ~local_code:"HCPARSE0159"
        ~global_message:
          (Printf.sprintf
             "an unbraced global array initializer requires a positive, %s \
              bound no larger than %d"
             bound_kind max_unbraced_initializer_elements)
        ~local_message:
          "unbraced static local array initializers are not implemented"
  | Some count ->
      let remaining_dimensions = List.tl dimensions in
      let rec parse_elements index elements_rev token_groups_rev =
        if index = count then
          let closing_brace, closing_tokens =
            let closing_item = peek cursor in
            if
              allow_closing_brace
              && closing_item.token.kind = Token_kind.Punctuation '}'
            then (
              let closing_item = take cursor in
              let closing = token_location closing_item.token in
              publish_initializer_delimiter cursor closing_item live
                (Initializer_close closing);
              (Some closing, [ closing_item.token ]))
            else (None, [])
          in
          let element_tokens = List.rev token_groups_rev |> List.concat in
          let tokens = element_tokens @ closing_tokens in
          let node =
            Ast.make_unbraced_array_initializer
              ~elements:(List.rev elements_rev) ~closing_brace
              ~location:(location_from_expression_tokens tokens)
            |> fun unbraced -> Ast.Unbraced_array_initializer unbraced
          in
          Some ({ node; tokens } : parsed_initializer)
        else
          let parsed_value =
            let live = initializer_child live index in
            match remaining_dimensions with
            | [] ->
                parse_initializer_value ?live cursor ~declarator_context
                  ~depth:(depth + 1)
            | dimensions ->
                let next_item = peek cursor in
                if next_item.token.kind = Token_kind.Punctuation '{' then
                  parse_braced_initializer ?live cursor ~declarator_context
                    ~depth:(depth + 1)
                else
                  parse_unbraced_array_initializer ?live cursor
                    ~declarator_context ~depth:(depth + 1)
                    ~allow_closing_brace:false ~dimensions
          in
          match parsed_value with
          | None -> None
          | Some value ->
              let needs_comma = index + 1 < count in
              let following_item = peek cursor in
              let comma, element_tokens =
                if needs_comma then
                  if following_item.token.kind = Token_kind.Punctuation ',' then (
                    let comma_item = take cursor in
                    let comma = token_location comma_item.token in
                    publish_initializer_delimiter cursor comma_item live
                      (Initializer_comma comma);
                    (Some comma, value.tokens @ [ comma_item.token ]))
                  else (None, [])
                else (None, value.tokens)
              in
              if element_tokens = [] then
                initializer_failure cursor ~declarator_context following_item
                  ~local_open_braces:depth ~global_code:"HCPARSE0160"
                  ~local_code:"HCPARSE0160"
                  ~global_message:
                    (Printf.sprintf
                       "expected ',' after element %d of an unbraced global \
                        array initializer, but found %s"
                       (index + 1)
                       (token_description following_item.token))
                  ~local_message:
                    "unbraced static local array initializers are not \
                     implemented"
              else
                let element =
                  Ast.make_initializer_element ~value:value.node ~comma
                    ~location:(location_from_expression_tokens element_tokens)
                in
                parse_elements (index + 1) (element :: elements_rev)
                  (element_tokens :: token_groups_rev)
      in
      parse_elements 0 [] []

let parse_global_initializer ?publication cursor ~array_dimensions =
  let equals_item = peek cursor in
  if equals_item.token.kind <> Token_kind.Punctuation '=' then Some (None, [])
  else
    let equals_item = take cursor in
    let equals = token_location equals_item.token in
    let live =
      Option.map
        (fun initializer_owner ->
          let start =
            {
              initializer_owner;
              initializer_equals = equals;
              initializer_activity = { initializer_phase = None };
            }
          in
          publish_initializer_phase cursor equals_item start
            Starting_initializer (Global_initializer_started start);
          ( {
              start;
              previous_leaf = None;
              delimiters_rev = [];
              previous_delimiter = None;
            },
            [] ))
        publication
    in
    let value =
      let first_item = peek cursor in
      if
        array_dimensions <> []
        && first_item.token.kind <> Token_kind.Punctuation '{'
        && first_item.token.kind <> Token_kind.String
      then
        parse_unbraced_array_initializer ?live cursor
          ~declarator_context:Global_initializer_declarator ~depth:0
          ~allow_closing_brace:true ~dimensions:array_dimensions
      else
        parse_initializer_value ?live cursor
          ~declarator_context:Global_initializer_declarator ~depth:0
    in
    match value with
    | None -> None
    | Some value ->
        let tokens = equals_item.token :: value.tokens in
        let initial_value =
          Ast.make_global_initializer ~equals ~value:value.node
            ~location:(location_from_expression_tokens tokens)
        in
        Some (Some initial_value, tokens)

let parse_variable_declarator_suffix ?header cursor
    (prefix : parsed_declarator_prefix) =
  match parse_array_dimensions cursor ~name:prefix.name with
  | None -> None
  | Some (array_dimensions, array_tokens) ->
      let publication =
        if not cursor.stop_on_error then None
        else
          let global_entry = publish_global cursor prefix.name in
          Option.map
            (fun global_header ->
              let global_previous =
                match prefix.name_selection with
                | Some (environment, selected)
                  when environment == cursor.symbols -> selected
                | _ -> Symbol_visibility.Absent
              in
              let publication =
                {
                  global_activity = { global_active = true };
                  global_header;
                  global_environment = cursor.symbols;
                  global_entry;
                  global_previous;
                  global_name = prefix.name;
                  global_pointer_layers = prefix.pointer_layers;
                  global_function_pointer = prefix.function_pointer;
                  global_dimensions = array_dimensions;
                }
              in
              Fun.protect
                ~finally:(fun () ->
                  publication.global_activity.global_active <- false)
                (fun () ->
                  publish_declaration cursor (peek cursor)
                    (Global_declared publication));
              publication)
            header
      in
      Option.bind
        (parse_global_initializer ?publication cursor ~array_dimensions)
        (fun (initial_value, initializer_tokens) ->
          let delimiter_item = peek cursor in
          match delimiter_kind delimiter_item.token with
          | None ->
              report ~secondary:prefix.definition_trace cursor delimiter_item
                ~code:"HCPARSE0003"
                ~message:
                  (Printf.sprintf
                     "expected ',' or ';' after global variable %S, but found \
                      %s"
                     prefix.name.spelling
                     (token_description delimiter_item.token));
              recover_declaration cursor;
              None
          | Some kind ->
              let delimiter_item = take cursor in
              let delimiter =
                Ast.make_declaration_delimiter ~kind
                  ~spelling:delimiter_item.token.raw
                  ~location:(token_location delimiter_item.token)
              in
              let tokens =
                prefix.tokens @ array_tokens @ initializer_tokens
                @ [ delimiter_item.token ]
              in
              let node =
                Ast.make_global_declarator ~pointer_layers:prefix.pointer_layers
                  ~name:prefix.name ~function_pointer:prefix.function_pointer
                  ~array_dimensions ~initial_value ~delimiter
                  ~location:(location_from_tokens tokens)
              in
              if not cursor.stop_on_error then
                ignore (publish_global cursor prefix.name);
              Option.iter
                (fun publication ->
                  publish_declaration cursor delimiter_item
                    (Global_completed (publication, node)))
                publication;
              Some ({ node; tokens } : parsed_declarator))

let parse_declarator ?header cursor base_spelling ~parse_function_pointer =
  match
    parse_declarator_prefix cursor base_spelling ~parse_function_pointer
  with
  | None -> None
  | Some prefix -> parse_variable_declarator_suffix ?header cursor prefix

let rec parse_declarators ?header cursor base_spelling ~parse_function_pointer
    declarators_rev =
  let item = peek cursor in
  if item.token.kind = Token_kind.Punctuation ';' then
    Some
      {
        declarators = List.rev declarators_rev;
        trailing_semicolon = Some (take cursor);
      }
  else
    match
      parse_declarator ?header cursor base_spelling ~parse_function_pointer
    with
    | None -> None
    | Some declarator -> (
        let declarators_rev = declarator :: declarators_rev in
        match declarator.node.delimiter.kind with
        | Ast.Semicolon ->
            Some
              {
                declarators = List.rev declarators_rev;
                trailing_semicolon = None;
              }
        | Ast.Comma ->
            parse_declarators ?header cursor base_spelling
              ~parse_function_pointer declarators_rev)

let aggregate_member_failure cursor item ~recovery_depth ~code ~message =
  report cursor item ~code ~message;
  Error { recovery_depth }

let rec parse_aggregate_members cursor ~(opening_brace : Ast.location) ~depth
    ~parse_member_function_pointer members_rev tokens_rev :
    (parsed_aggregate_members, aggregate_parse_failure) result =
  let item = peek cursor in
  match item.token.kind with
  | Token_kind.Punctuation '}' ->
      let closing_item = take cursor in
      Ok
        {
          members = List.rev members_rev;
          tokens = List.rev (closing_item.token :: tokens_rev);
          closing_brace = token_location closing_item.token;
        }
  | Token_kind.Eof ->
      report cursor item ~code:"HCPARSE0111"
        ~secondary:
          [
            ({
               Common.Diagnostic.span = opening_brace.Ast.span;
               message = "aggregate body starts here";
             }
              : Common.Diagnostic.related);
          ]
        ~message:"expected '}' to close the aggregate body";
      Error { recovery_depth = depth + 1 }
  | Token_kind.Punctuation ';' ->
      let semicolon_item = take cursor in
      parse_aggregate_members cursor ~opening_brace ~depth
        ~parse_member_function_pointer
        (Ast.Empty_aggregate_member (token_location semicolon_item.token)
        :: members_rev)
        (semicolon_item.token :: tokens_rev)
  | Token_kind.Keyword Keyword.Union -> (
      match
        parse_anonymous_union_member cursor ~depth
          ~parse_member_function_pointer
      with
      | Error failure -> Error failure
      | Ok member ->
          parse_aggregate_members cursor ~opening_brace ~depth
            ~parse_member_function_pointer
            (member.node :: members_rev)
            (List.rev_append member.tokens tokens_rev))
  | Token_kind.Keyword Keyword.Class ->
      aggregate_member_failure cursor item ~recovery_depth:(depth + 1)
        ~code:"HCPARSE0116"
        ~message:
          "nested named class definitions are not implemented in aggregate \
           bodies"
  | Token_kind.Operator Operator.Current_position -> (
      match
        parse_aggregate_offset_directive cursor ~recovery_depth:(depth + 1)
      with
      | Error failure -> Error failure
      | Ok member ->
          parse_aggregate_members cursor ~opening_brace ~depth
            ~parse_member_function_pointer
            (member.node :: members_rev)
            (List.rev_append member.tokens tokens_rev))
  | _ -> (
      match
        parse_aggregate_member_declaration cursor ~recovery_depth:(depth + 1)
          ~parse_member_function_pointer
      with
      | Error failure -> Error failure
      | Ok member ->
          parse_aggregate_members cursor ~opening_brace ~depth
            ~parse_member_function_pointer
            (member.node :: members_rev)
            (List.rev_append member.tokens tokens_rev))

and parse_aggregate_offset_directive cursor ~recovery_depth :
    (parsed_aggregate_member, aggregate_parse_failure) result =
  let marker_item = take cursor in
  let marker = make_expression_operator marker_item.token in
  let equals_item = peek cursor in
  if equals_item.token.kind <> Token_kind.Punctuation '=' then
    aggregate_member_failure cursor equals_item ~recovery_depth
      ~code:"HCPARSE0142"
      ~message:
        (Printf.sprintf
           "expected '=' after the '$$' aggregate offset marker, but found %s"
           (token_description equals_item.token))
  else
    let equals_item = take cursor in
    match
      parse_expression cursor ~context:Aggregate_offset_expression ~depth:0
        ~minimum_binding_power:0
    with
    | None -> Error { recovery_depth }
    | Some expression ->
        let semicolon_item = peek cursor in
        if semicolon_item.token.kind <> Token_kind.Punctuation ';' then
          aggregate_member_failure cursor semicolon_item ~recovery_depth
            ~code:"HCPARSE0143"
            ~message:
              (Printf.sprintf
                 "expected ';' after the aggregate offset expression, but \
                  found %s"
                 (token_description semicolon_item.token))
        else
          let semicolon_item = take cursor in
          let tokens =
            (marker_item.token :: equals_item.token :: expression.tokens)
            @ [ semicolon_item.token ]
          in
          let node =
            Ast.make_aggregate_offset_directive ~marker
              ~equals:(token_location equals_item.token)
              ~expression:expression.node
              ~semicolon:(token_location semicolon_item.token)
              ~location:(location_from_expression_tokens tokens)
          in
          Ok { node = Ast.Aggregate_offset_directive node; tokens }

and parse_anonymous_union_member cursor ~depth ~parse_member_function_pointer :
    (parsed_aggregate_member, aggregate_parse_failure) result =
  let keyword_item = take cursor in
  if depth >= max_aggregate_depth then
    aggregate_member_failure cursor keyword_item ~recovery_depth:(depth + 1)
      ~code:"HCPARSE0122"
      ~message:
        (Printf.sprintf "anonymous-union nesting exceeds the hosted limit of %d"
           max_aggregate_depth)
  else
    let opening_item = peek cursor in
    if opening_item.token.kind <> Token_kind.Punctuation '{' then
      aggregate_member_failure cursor opening_item ~recovery_depth:(depth + 1)
        ~code:"HCPARSE0116"
        ~message:
          (Printf.sprintf
             "expected '{' after anonymous union keyword, but found %s; nested \
              named aggregate definitions are not implemented"
             (token_description opening_item.token))
    else
      let opening_item = take cursor in
      let opening_brace = token_location opening_item.token in
      match
        parse_aggregate_members cursor ~opening_brace ~depth:(depth + 1)
          ~parse_member_function_pointer [] []
      with
      | Error failure -> Error failure
      | Ok parsed_members ->
          let semicolon_item =
            if (peek cursor).token.kind = Token_kind.Punctuation ';' then
              Some (take cursor)
            else None
          in
          let semicolon =
            Option.map (fun item -> token_location item.token) semicolon_item
          in
          let tokens =
            [ keyword_item.token; opening_item.token ]
            @ parsed_members.tokens
            @ Option.to_list
                (Option.map (fun item -> item.token) semicolon_item)
          in
          let node =
            Ast.make_anonymous_union_member
              ~keyword_spelling:keyword_item.token.raw
              ~keyword_location:(token_location keyword_item.token)
              ~opening_brace ~members:parsed_members.members
              ~closing_brace:parsed_members.closing_brace ~semicolon
              ~location:(location_from_expression_tokens tokens)
          in
          Ok { node = Ast.Anonymous_union_member node; tokens }

and parse_aggregate_member_declaration cursor ~recovery_depth
    ~parse_member_function_pointer :
    (parsed_aggregate_member, aggregate_parse_failure) result =
  let type_item = peek cursor in
  match type_specifier_of_item cursor type_item with
  | None ->
      aggregate_member_failure cursor type_item ~recovery_depth
        ~code:"HCPARSE0112"
        ~message:
          (Printf.sprintf
             "expected a primitive, class, or union member type, but found %s"
             (token_description type_item.token))
  | Some type_specifier ->
      let type_item = take cursor in
      let base_spelling = Ast.type_specifier_spelling type_specifier in
      let rec collect declarators_rev tokens_rev :
          (parsed_aggregate_member, aggregate_parse_failure) result =
        match
          parse_aggregate_member_declarator cursor ~base_spelling
            ~recovery_depth ~parse_member_function_pointer
        with
        | Error failure -> Error failure
        | Ok declarator -> (
            let declarators_rev = declarator.node :: declarators_rev in
            let tokens_rev = List.rev_append declarator.tokens tokens_rev in
            match declarator.node.member_delimiter.kind with
            | Ast.Comma -> collect declarators_rev tokens_rev
            | Ast.Semicolon ->
                let tokens = type_item.token :: List.rev tokens_rev in
                let declaration =
                  Ast.make_aggregate_member_declaration ~type_specifier
                    ~declarators:(List.rev declarators_rev)
                    ~location:(location_from_expression_tokens tokens)
                in
                Ok
                  ({
                     node = Ast.Aggregate_member_declaration declaration;
                     tokens;
                   }
                    : parsed_aggregate_member))
      in
      collect [] []

and parse_aggregate_member_declarator cursor ~base_spelling ~recovery_depth
    ~parse_member_function_pointer :
    (parsed_aggregate_member_declarator, aggregate_parse_failure) result =
  match parse_pointer_layers cursor 0 [] [] with
  | None -> Error { recovery_depth }
  | Some (pointer_layers, pointer_items) -> (
      let pointer_tokens = List.map (fun item -> item.token) pointer_items in
      let name_item = peek cursor in
      let parsed_core =
        if name_item.token.kind = Token_kind.Punctuation '(' then
          match parse_member_function_pointer () with
          | None -> None
          | Some (parsed : parsed_function_pointer) ->
              Option.map
                (fun name ->
                  (name, Some parsed.node, pointer_tokens @ parsed.tokens))
                parsed.name
        else if not (token_is_name_position_identifier name_item.token) then (
          report cursor name_item ~code:"HCPARSE0113"
            ~message:
              (Printf.sprintf
                 "expected a member name after type %S, but found %s"
                 (type_spelling base_spelling pointer_layers)
                 (token_description name_item.token));
          None)
        else
          let name_item = take cursor in
          let name =
            Ast.make_identifier ~spelling:name_item.token.raw
              ~location:(token_location name_item.token)
          in
          Some (name, None, pointer_tokens @ [ name_item.token ])
      in
      match parsed_core with
      | None -> Error { recovery_depth }
      | Some (name, function_pointer, core_tokens) -> (
          match parse_array_dimensions cursor ~name with
          | None -> Error { recovery_depth }
          | Some (array_dimensions, array_tokens) -> (
              match parse_aggregate_member_metadata cursor ~recovery_depth with
              | Error failure -> Error failure
              | Ok metadata -> (
                  let delimiter_item = peek cursor in
                  match delimiter_kind delimiter_item.token with
                  | None ->
                      aggregate_member_failure cursor delimiter_item
                        ~recovery_depth ~code:"HCPARSE0114"
                        ~message:
                          (Printf.sprintf
                             "expected ',' or ';' after aggregate member %S, \
                              but found %s"
                             name.spelling
                             (token_description delimiter_item.token))
                  | Some kind ->
                      let delimiter_item = take cursor in
                      let delimiter =
                        Ast.make_declaration_delimiter ~kind
                          ~spelling:delimiter_item.token.raw
                          ~location:(token_location delimiter_item.token)
                      in
                      let tokens =
                        core_tokens @ array_tokens @ metadata.metadata_tokens
                        @ [ delimiter_item.token ]
                      in
                      let node =
                        Ast.make_aggregate_member_declarator ~pointer_layers
                          ~name ~function_pointer ~array_dimensions
                          ~metadata:metadata.metadata_nodes ~delimiter
                          ~location:(location_from_expression_tokens tokens)
                      in
                      Ok { node; tokens }))))

and parse_aggregate_member_metadata cursor ~recovery_depth :
    (parsed_aggregate_member_metadata, aggregate_parse_failure) result =
  let rec collect nodes_rev tokens_rev =
    let name_item = peek cursor in
    if not (token_is_name_position_identifier name_item.token) then
      Ok
        {
          metadata_nodes = List.rev nodes_rev;
          metadata_tokens = List.rev tokens_rev;
        }
    else
      let name_item = take cursor in
      let name =
        Ast.make_identifier ~spelling:name_item.token.raw
          ~location:(token_location name_item.token)
      in
      let finish value value_tokens =
        let tokens = name_item.token :: value_tokens in
        let node =
          Ast.make_aggregate_member_metadata ~name ~value
            ~location:(location_from_expression_tokens tokens)
        in
        collect (node :: nodes_rev) (List.rev_append tokens tokens_rev)
      in
      let value_item = peek cursor in
      match (value_item.token.kind, value_item.token.value) with
      | Token_kind.String, Token.Bytes _ ->
          let rec take_segments segments_rev items_rev values_rev =
            let item = peek cursor in
            match (item.token.kind, item.token.value) with
            | Token_kind.String, Token.Bytes value ->
                let item = take cursor in
                let segment =
                  Ast.make_expression_literal ~origin:Ast.Source_literal
                    ~spelling:item.token.raw ~value:(Ast.Bytes_value value)
                    ~location:(token_location item.token)
                in
                take_segments (segment :: segments_rev) (item :: items_rev)
                  (value :: values_rev)
            | _ ->
                ( List.rev segments_rev,
                  List.rev items_rev,
                  String.concat "" (List.rev values_rev) )
          in
          let segments, items, value = take_segments [] [] [] in
          let value_tokens = List.map (fun item -> item.token) items in
          let string =
            Ast.make_aggregate_member_metadata_string ~segments ~value
              ~location:(location_from_expression_tokens value_tokens)
          in
          finish (Ast.Member_metadata_string string) value_tokens
      | ( (Token_kind.Punctuation (',' | ')' | ']' | ';' | '}') | Token_kind.Eof),
          _ ) ->
          aggregate_member_failure cursor value_item ~recovery_depth
            ~code:"HCPARSE0144"
            ~message:
              (Printf.sprintf
                 "expected a string or expression value after aggregate member \
                  metadata name %S, but found %s"
                 name.spelling
                 (token_description value_item.token))
      | _ -> (
          match
            parse_expression cursor
              ~context:Aggregate_member_metadata_expression ~depth:0
              ~minimum_binding_power:0
          with
          | None -> Error { recovery_depth }
          | Some expression ->
              finish (Ast.Member_metadata_expression expression.node)
                expression.tokens)
  in
  collect [] []

let parse_aggregate_definition cursor ~modifier_tokens ~modifiers ~backing
    ~aggregate_kind ~parse_function_pointer ~parse_member_function_pointer =
  let aggregate_item = take cursor in
  let name_item = peek cursor in
  if not (token_is_name_position_identifier name_item.token) then (
    report cursor name_item ~code:"HCPARSE0109"
      ~message:
        (Printf.sprintf "expected a name after %S, but found %s"
           aggregate_item.token.raw
           (token_description name_item.token));
    recover_aggregate_declaration cursor ~depth:0;
    None)
  else
    let name_item = take cursor in
    let name =
      Ast.make_identifier ~spelling:name_item.token.raw
        ~location:(token_location name_item.token)
    in
    publish_class cursor name;
    match parse_aggregate_base cursor with
    | None -> None
    | Some base -> (
        let opening_item = peek cursor in
        if opening_item.token.kind <> Token_kind.Punctuation '{' then (
          report cursor opening_item ~code:"HCPARSE0110"
            ~message:
              (Printf.sprintf "expected '{' after %s name %S, but found %s"
                 aggregate_item.token.raw name.spelling
                 (token_description opening_item.token));
          recover_aggregate_declaration cursor ~depth:0;
          None)
        else
          let opening_item = take cursor in
          let opening_brace = token_location opening_item.token in
          match
            parse_aggregate_members cursor ~opening_brace ~depth:0
              ~parse_member_function_pointer [] []
          with
          | Error failure ->
              recover_aggregate_declaration cursor ~depth:failure.recovery_depth;
              None
          | Ok parsed_members ->
              let following_item = peek cursor in
              let parsed_tail =
                match following_item.token.kind with
                | Token_kind.Punctuation ';' ->
                    let semicolon_item = take cursor in
                    Some
                      ( [],
                        [ semicolon_item.token ],
                        Some (token_location semicolon_item.token) )
                | Token_kind.Keyword (Keyword.Class | Keyword.Union) ->
                    Some ([], [], None)
                | Token_kind.Identifier | Token_kind.Punctuation ('*' | '(')
                  -> (
                    match
                      parse_declarators
                        ~header:
                          (declaration_header cursor ~modifiers ~binding:None
                             ~type_specifier:(Ast.Named_type_specifier name))
                        cursor name.spelling ~parse_function_pointer []
                    with
                    | None -> None
                    | Some parsed_declarators ->
                        let declarators = parsed_declarators.declarators in
                        let last = List.hd (List.rev declarators) in
                        let trailing_tokens =
                          Option.to_list
                            (Option.map
                               (fun item -> item.token)
                               parsed_declarators.trailing_semicolon)
                        in
                        let semicolon =
                          match parsed_declarators.trailing_semicolon with
                          | Some item -> Some (token_location item.token)
                          | None -> Some last.node.delimiter.location
                        in
                        Some
                          ( List.map
                              (fun (declarator : parsed_declarator) ->
                                declarator.node)
                              declarators,
                            List.concat_map
                              (fun (declarator : parsed_declarator) ->
                                declarator.tokens)
                              declarators
                            @ trailing_tokens,
                            semicolon ))
                | _ ->
                    report cursor following_item ~code:"HCPARSE0115"
                      ~message:
                        (Printf.sprintf
                           "expected ';' or a global declarator after \
                            aggregate definition %S, but found %s"
                           name.spelling
                           (token_description following_item.token));
                    None
              in
              Option.map
                (fun (attached_declarators, tail_tokens, semicolon) ->
                  let tokens =
                    modifier_tokens
                    @ (match backing with
                      | None -> []
                      | Some (backing : parsed_aggregate_backing) ->
                          backing.tokens)
                    @ [ aggregate_item.token; name_item.token ]
                    @ (match base with
                      | None -> []
                      | Some (base : parsed_aggregate_base) -> base.tokens)
                    @ (opening_item.token :: parsed_members.tokens)
                    @ tail_tokens
                  in
                  let definition =
                    Ast.make_aggregate_definition ~modifiers
                      ~backing:
                        (Option.map
                           (fun (backing : parsed_aggregate_backing) ->
                             backing.node)
                           backing)
                      ~aggregate_kind
                      ~aggregate_keyword_spelling:aggregate_item.token.raw
                      ~aggregate_keyword_location:
                        (token_location aggregate_item.token)
                      ~name
                      ~base:
                        (Option.map
                           (fun (base : parsed_aggregate_base) -> base.node)
                           base)
                      ~opening_brace ~members:parsed_members.members
                      ~closing_brace:parsed_members.closing_brace
                      ~attached_declarators ~semicolon
                      ~location:(location_from_expression_tokens tokens)
                  in
                  Ast.Aggregate_definition definition)
                parsed_tail)

let finish_function_parameter ?default_context cursor ~register_qualifiers
    ~type_specifier ~pointer_layers ~name ~function_pointer ~tokens =
  (* PrsType leaves the following token current. Native MemberAdd precedes
     default input, including any directive reached by Lex beyond '='. *)
  let following_head = peek cursor in
  let publication =
    Option.map
      (fun (parameter_function, parameter_index, _, completions) ->
        let publication =
          {
            parameter_function;
            parameter_index;
            parameter_predecessor = List.nth_opt !completions 0;
            parameter_register_qualifiers = register_qualifiers;
            parameter_type_specifier = type_specifier;
            parameter_pointer_layers = pointer_layers;
            parameter_name = name;
            parameter_function_pointer = function_pointer;
            parameter_activity = { function_parameter_active = true };
          }
        in
        Fun.protect
          ~finally:(fun () ->
            publication.parameter_activity.function_parameter_active <- false)
          (fun () ->
            publish_declaration cursor following_head
              (Function_parameter_declared publication));
        publication)
      default_context
  in
  let parsed_default =
    if following_head.token.kind = Token_kind.Punctuation '=' then
      Option.map (fun parsed -> Some parsed) (parse_parameter_default cursor)
    else Some None
  in
  (match (default_context, publication, parsed_default) with
  | ( Some (default_function, default_parameter_index, previous, _),
      Some default_parameter,
      Some (Some parsed) ) ->
      let receipt =
        {
          default_function;
          default_parameter;
          default_parameter_index;
          default_predecessor = !previous;
          default_register_qualifiers = register_qualifiers;
          default_type_specifier = type_specifier;
          default_pointer_layers = pointer_layers;
          default_parameter_name = name;
          default_function_pointer = function_pointer;
          default_ast = parsed.node;
          default_activity = { parameter_default_active = true };
        }
      in
      Fun.protect
        ~finally:(fun () ->
          receipt.default_activity.parameter_default_active <- false)
        (fun () ->
          publish_declaration cursor (peek cursor)
            (Parameter_default_completed receipt));
      previous := Some receipt
  | _ -> ());
  match parsed_default with
  | None -> None
  | Some parsed_default -> (
      let following_item = peek cursor in
      let fail_special_form () =
        match following_item.token.kind with
        | Token_kind.Punctuation '[' when Option.is_none parsed_default ->
            unsupported_parameter_form cursor following_item ~code:"HCPARSE0011"
              "array parameters"
        | Token_kind.Keyword Keyword.Reg | Token_kind.Keyword Keyword.Noreg ->
            declaration_failure cursor following_item ~code:"HCPARSE0013"
              ~message:
                "register qualifier must appear before a parameter type or \
                 immediately after it"
        | _ ->
            declaration_failure cursor following_item ~code:"HCPARSE0010"
              ~message:
                (Printf.sprintf
                   "expected ',', ';', or ')' after function parameter, but \
                    found %s"
                   (token_description following_item.token))
      in
      match following_item.token.kind with
      | Token_kind.Punctuation (',' | ';') | Token_kind.Punctuation ')' ->
          let delimiter_item =
            match following_item.token.kind with
            | Token_kind.Punctuation (',' | ';') -> Some (take cursor)
            | _ -> None
          in
          let delimiter =
            Option.map
              (fun item ->
                let kind =
                  match item.token.kind with
                  | Token_kind.Punctuation ',' -> Ast.Comma
                  | Token_kind.Punctuation ';' -> Ast.Semicolon
                  | _ -> invalid_arg "function parameter delimiter"
                in
                Ast.make_declaration_delimiter ~kind ~spelling:item.token.raw
                  ~location:(token_location item.token))
              delimiter_item
          in
          let default_tokens =
            match parsed_default with
            | None -> []
            | Some (parsed : parsed_parameter_default) -> parsed.tokens
          in
          let tokens =
            tokens @ default_tokens
            @ Option.to_list
                (Option.map (fun item -> item.token) delimiter_item)
          in
          let node =
            Ast.make_function_parameter ~register_qualifiers ~type_specifier
              ~pointer_layers ~name ~function_pointer
              ~default:
                (Option.map
                   (fun (parsed : parsed_parameter_default) -> parsed.node)
                   parsed_default)
              ~delimiter
              ~location:(location_from_tokens tokens)
          in
          (match (publication, default_context) with
          | Some parameter_publication, Some (_, _, _, completions) ->
              let completed =
                {
                  parameter_publication;
                  parameter_ast = node;
                  parameter_completion_activity =
                    { parameter_completion_active = true };
                }
              in
              Fun.protect
                ~finally:(fun () ->
                  completed.parameter_completion_activity.parameter_completion_active <-
                    false)
                (fun () ->
                  publish_declaration cursor following_item
                    (Function_parameter_completed completed));
              completions := completed :: !completions
          | _ -> ());
          Some ({ node; tokens } : parsed_parameter)
      | _ -> fail_special_form ())

let rec parse_function_parameter ?default_context cursor ~prefix_qualifiers
    ~prefix_tokens ~function_pointer_depth =
  let type_item = peek cursor in
  match type_specifier_of_item cursor type_item with
  | Some type_specifier -> (
      let type_item = take cursor in
      let suffix =
        parse_register_qualifiers cursor ~position:Ast.After_type [] []
      in
      let register_qualifiers = prefix_qualifiers @ suffix.nodes in
      match parse_pointer_layers cursor 0 [] [] with
      | None -> None
      | Some (pointer_layers, pointer_items) ->
          let pointer_tokens =
            List.map (fun item -> item.token) pointer_items
          in
          let next_item = peek cursor in
          let leading_tokens =
            prefix_tokens @ [ type_item.token ] @ suffix.tokens @ pointer_tokens
          in
          if next_item.token.kind = Token_kind.Punctuation '(' then
            match
              parse_function_pointer_declarator cursor ~function_pointer_depth
                ~declarator_context:Function_parameter_declarator
            with
            | None -> None
            | Some parsed ->
                finish_function_parameter ?default_context cursor
                  ~register_qualifiers ~type_specifier ~pointer_layers
                  ~name:parsed.name ~function_pointer:(Some parsed.node)
                  ~tokens:(leading_tokens @ parsed.tokens)
          else if token_is_name_position_identifier next_item.token then
            let name_item = take cursor in
            let name =
              Ast.make_identifier ~spelling:name_item.token.raw
                ~location:(token_location name_item.token)
            in
            finish_function_parameter ?default_context cursor
              ~register_qualifiers ~type_specifier ~pointer_layers
              ~name:(Some name) ~function_pointer:None
              ~tokens:(leading_tokens @ [ name_item.token ])
          else
            finish_function_parameter ?default_context cursor
              ~register_qualifiers ~type_specifier ~pointer_layers ~name:None
              ~function_pointer:None ~tokens:leading_tokens)
  | _ ->
      declaration_failure cursor type_item ~code:"HCPARSE0009"
        ~message:
          (Printf.sprintf
             "expected a primitive, class, or union parameter type, but found \
              %s"
             (token_description type_item.token))

and parse_function_pointer_declarator cursor ~function_pointer_depth
    ~declarator_context =
  let opening_item = peek cursor in
  if function_pointer_depth >= max_function_pointer_depth then
    function_pointer_declaration_failure cursor ~declarator_context opening_item
      ~code:"HCPARSE0017"
      ~message:
        (Printf.sprintf
           "function-pointer type nesting exceeds the hosted limit of %d"
           max_function_pointer_depth)
  else
    let opening_item = take cursor in
    let first_star = peek cursor in
    if first_star.token.kind <> Token_kind.Punctuation '*' then
      function_pointer_declaration_failure cursor ~declarator_context first_star
        ~code:
          (match declarator_context with
          | Function_parameter_declarator -> "HCPARSE0014"
          | Global_variable_declarator -> "HCPARSE0131"
          | Aggregate_member_declarator -> "HCPARSE0133"
          | Local_variable_declarator _ -> "HCPARSE0135")
        ~message:
          (match declarator_context with
          | Function_parameter_declarator ->
              Printf.sprintf
                "expected '*' after '(' in function-pointer parameter, but \
                 found %s"
                (token_description first_star.token)
          | Global_variable_declarator ->
              Printf.sprintf
                "expected '*' after '(' in global function-pointer declarator, \
                 but found %s"
                (token_description first_star.token)
          | Aggregate_member_declarator ->
              Printf.sprintf
                "expected '*' after '(' in aggregate function-pointer member, \
                 but found %s"
                (token_description first_star.token)
          | Local_variable_declarator _ ->
              Printf.sprintf
                "expected '*' after '(' in local function-pointer declarator, \
                 but found %s"
                (token_description first_star.token))
    else
      match
        parse_pointer_layers_with_recovery cursor
          ~recover:(fun cursor ->
            recover_function_pointer_declaration cursor declarator_context)
          0 [] []
      with
      | None -> None
      | Some (pointer_layers, pointer_items) -> (
          let pointer_tokens =
            List.map (fun item -> item.token) pointer_items
          in
          let name_item = peek cursor in
          let name_is_identifier =
            token_is_name_position_identifier name_item.token
          in
          let parsed_name =
            if name_is_identifier then
              let name_item = take cursor in
              Some
                ( Ast.make_identifier ~spelling:name_item.token.raw
                    ~location:(token_location name_item.token),
                  name_item.token )
            else
              match name_item.token.kind with
              | Token_kind.Punctuation ')'
                when declarator_context = Function_parameter_declarator -> None
              | _ ->
                  report cursor name_item
                    ~code:
                      (match declarator_context with
                      | Function_parameter_declarator -> "HCPARSE0014"
                      | Global_variable_declarator -> "HCPARSE0132"
                      | Aggregate_member_declarator -> "HCPARSE0134"
                      | Local_variable_declarator _ -> "HCPARSE0136")
                    ~message:
                      (match declarator_context with
                      | Function_parameter_declarator ->
                          Printf.sprintf
                            "expected a function-pointer name or ')' after \
                             pointer stars, but found %s"
                            (token_description name_item.token)
                      | Global_variable_declarator ->
                          Printf.sprintf
                            "expected a global function-pointer name after \
                             pointer stars, but found %s"
                            (token_description name_item.token)
                      | Aggregate_member_declarator ->
                          Printf.sprintf
                            "expected an aggregate member name after \
                             function-pointer stars, but found %s"
                            (token_description name_item.token)
                      | Local_variable_declarator _ ->
                          Printf.sprintf
                            "expected a local variable name after \
                             function-pointer stars, but found %s"
                            (token_description name_item.token));
                  recover_function_pointer_declaration cursor declarator_context;
                  None
          in
          if
            (not name_is_identifier)
            && not
                 (declarator_context = Function_parameter_declarator
                 && name_item.token.kind = Token_kind.Punctuation ')')
          then None
          else
            let name = Option.map fst parsed_name in
            let name_tokens = Option.to_list (Option.map snd parsed_name) in
            let closing_item = peek cursor in
            if closing_item.token.kind <> Token_kind.Punctuation ')' then
              function_pointer_declaration_failure cursor ~declarator_context
                closing_item
                ~code:
                  (match declarator_context with
                  | Function_parameter_declarator -> "HCPARSE0014"
                  | Global_variable_declarator -> "HCPARSE0131"
                  | Aggregate_member_declarator -> "HCPARSE0133"
                  | Local_variable_declarator _ -> "HCPARSE0135")
                ~message:
                  (Printf.sprintf
                     "expected ')' after function-pointer name, but found %s"
                     (token_description closing_item.token))
            else
              let closing_item = take cursor in
              let signature_opening = peek cursor in
              if signature_opening.token.kind <> Token_kind.Punctuation '(' then
                function_pointer_declaration_failure cursor ~declarator_context
                  signature_opening
                  ~code:
                    (match declarator_context with
                    | Function_parameter_declarator -> "HCPARSE0014"
                    | Global_variable_declarator -> "HCPARSE0131"
                    | Aggregate_member_declarator -> "HCPARSE0133"
                    | Local_variable_declarator _ -> "HCPARSE0135")
                  ~message:
                    (Printf.sprintf
                       "expected '(' for function-pointer signature, but found \
                        %s"
                       (token_description signature_opening.token))
              else
                let signature_opening = take cursor in
                match
                  parse_function_parameters cursor [] [] []
                    ~function_pointer_depth:(function_pointer_depth + 1)
                with
                | None -> None
                | Some parsed_parameters ->
                    let tokens =
                      [ opening_item.token ] @ pointer_tokens @ name_tokens
                      @ [ closing_item.token; signature_opening.token ]
                      @ parsed_parameters.tokens
                    in
                    let node =
                      Ast.make_function_pointer_declarator
                        ~declarator_opening_parenthesis:
                          (token_location opening_item.token)
                        ~indirection_layers:pointer_layers
                        ~declarator_closing_parenthesis:
                          (token_location closing_item.token)
                        ~signature_opening_parenthesis:
                          (token_location signature_opening.token)
                        ~signature_parameters:parsed_parameters.parameters
                        ~signature_empty_parameter_entries:
                          parsed_parameters.empty_parameter_entries
                        ~signature_variadic:parsed_parameters.variadic
                        ~signature_closing_parenthesis:
                          parsed_parameters.closing_parenthesis
                        ~function_pointer_location:(location_from_tokens tokens)
                    in
                    Some
                      {
                        node;
                        name;
                        tokens;
                        name_selection =
                          (if name_is_identifier then name_item.selection
                           else None);
                      })

and parse_function_parameters ?default_owner cursor parameters_rev
    empty_entries_rev tokens_rev ~function_pointer_depth :
    parsed_parameter_list option =
  let parameter_completions () =
    match default_owner with
    | None -> []
    | Some (_, _, completions) -> List.rev !completions
  in
  let prefix =
    parse_register_qualifiers cursor ~position:Ast.Before_type [] []
  in
  let item = peek cursor in
  match item.token.kind with
  | Token_kind.Punctuation ')' when prefix.nodes = [] ->
      let closing = take cursor in
      Some
        {
          parameters = List.rev parameters_rev;
          parameter_completions = parameter_completions ();
          empty_parameter_entries = List.rev empty_entries_rev;
          variadic = None;
          variadic_publication = None;
          tokens = List.rev (closing.token :: tokens_rev);
          closing_parenthesis = Some (token_location closing.token);
        }
  | Token_kind.Operator Operator.Ellipsis ->
      let ellipsis = take cursor in
      let variadic =
        Ast.make_variadic_marker ~register_qualifiers:prefix.nodes
          ~spelling:ellipsis.token.raw
          ~location:(location_from_tokens (prefix.tokens @ [ ellipsis.token ]))
      in
      let variadic_publication =
        Option.map
          (fun (variadic_function, _, completions) ->
            let publication =
              {
                variadic_function;
                variadic_marker = variadic;
                variadic_parameter_predecessor = List.nth_opt !completions 0;
                variadic_activity =
                  {
                    function_variadic_start_active = true;
                    function_variadic_completion_active = false;
                  };
              }
            in
            Fun.protect
              ~finally:(fun () ->
                publication.variadic_activity.function_variadic_start_active <-
                  false)
              (fun () ->
                publish_declaration cursor ellipsis
                  (Function_variadic_started publication));
            publication)
          default_owner
      in
      (* PrsDotDotDot sets its flag before Lex, then adds argc and argv before
         the optional ')' triggers another Lex. *)
      let following = peek cursor in
      Option.iter
        (fun publication ->
          publication.variadic_activity.function_variadic_completion_active <-
            true;
          Fun.protect
            ~finally:(fun () ->
              publication.variadic_activity.function_variadic_completion_active <-
                false)
            (fun () ->
              publish_declaration cursor following
                (Function_variadic_completed publication)))
        variadic_publication;
      let closing =
        if following.token.kind = Token_kind.Punctuation ')' then
          Some (take cursor)
        else None
      in
      let tokens_rev =
        ellipsis.token :: List.rev_append prefix.tokens tokens_rev
      in
      let tokens_rev =
        match closing with
        | Some closing -> closing.token :: tokens_rev
        | None -> tokens_rev
      in
      Some
        {
          parameters = List.rev parameters_rev;
          parameter_completions = parameter_completions ();
          empty_parameter_entries = List.rev empty_entries_rev;
          variadic = Some variadic;
          variadic_publication;
          tokens = List.rev tokens_rev;
          closing_parenthesis =
            Option.map (fun closing -> token_location closing.token) closing;
        }
  | Token_kind.Punctuation ';' when prefix.nodes = [] ->
      let semicolon = take cursor in
      let delimiter =
        Ast.make_declaration_delimiter ~kind:Ast.Semicolon
          ~spelling:semicolon.token.raw
          ~location:(token_location semicolon.token)
      in
      let empty_entry =
        Ast.make_empty_parameter_entry
          ~preceding_parameter_count:(List.length parameters_rev)
          ~delimiter
      in
      parse_function_parameters ?default_owner cursor parameters_rev
        (empty_entry :: empty_entries_rev)
        (semicolon.token :: tokens_rev)
        ~function_pointer_depth
  | Token_kind.Punctuation ')' ->
      declaration_failure cursor item ~code:"HCPARSE0009"
        ~message:
          "expected a primitive, class, or union parameter type after register \
           qualifier, but found ')'"
  | _ -> (
      match
        parse_function_parameter
          ?default_context:
            (Option.map
               (fun (owner, previous, completions) ->
                 (owner, List.length parameters_rev, previous, completions))
               default_owner)
          cursor ~prefix_qualifiers:prefix.nodes ~prefix_tokens:prefix.tokens
          ~function_pointer_depth
      with
      | None -> None
      | Some parameter ->
          let tokens_rev = List.rev_append parameter.tokens tokens_rev in
          let parameters_rev = parameter.node :: parameters_rev in
          parse_function_parameters ?default_owner cursor parameters_rev
            empty_entries_rev tokens_rev ~function_pointer_depth)

let parse_function_prototype cursor ~modifier_tokens ~modifiers ~binding_tokens
    ~binding ~type_item ~return_type (prefix : parsed_declarator_prefix) =
  let provisional =
    declare_function cursor
      (declaration_header cursor ~modifiers ~binding:(Some binding)
         ~type_specifier:return_type)
      prefix (peek cursor)
  in
  let opening = take cursor in
  let opening_parenthesis =
    match provisional with
    | Some publication -> publication.function_opening_parenthesis
    | None -> token_location opening.token
  in
  match
    parse_function_parameters
      ?default_owner:
        (Option.map (fun owner -> (owner, ref None, ref [])) provisional)
      cursor [] [] [] ~function_pointer_depth:0
  with
  | None -> None
  | Some parsed_parameters ->
      let semicolon_item = peek cursor in
      ignore
        (complete_function_header cursor semicolon_item provisional
           parsed_parameters);
      let semicolon, semicolon_tokens =
        if semicolon_item.token.kind = Token_kind.Punctuation ';' then
          let semicolon_item = take cursor in
          (Some (token_location semicolon_item.token), [ semicolon_item.token ])
        else (None, [])
      in
      let declaration_tokens =
        modifier_tokens @ binding_tokens
        @ (type_item.token :: prefix.tokens)
        @ (opening.token :: parsed_parameters.tokens)
        @ semicolon_tokens
      in
      let prototype =
        Ast.make_function_prototype ~modifiers ~binding ~return_type
          ~return_pointer_layers:prefix.pointer_layers ~name:prefix.name
          ~opening_parenthesis ~parameters:parsed_parameters.parameters
          ~empty_parameter_entries:parsed_parameters.empty_parameter_entries
          ~variadic:parsed_parameters.variadic
          ~closing_parenthesis:parsed_parameters.closing_parenthesis ~semicolon
          ~location:(location_from_tokens declaration_tokens)
      in
      if not cursor.stop_on_error then
        publish_function cursor prefix.name parsed_parameters.parameters
          parsed_parameters.variadic;
      Some (Ast.Function_prototype prototype)

let parse_global cursor ~parse_function_definition =
  let parse_global_function_pointer () =
    parse_function_pointer_declarator cursor ~function_pointer_depth:0
      ~declarator_context:Global_variable_declarator
  in
  let parse_member_function_pointer () =
    parse_function_pointer_declarator cursor ~function_pointer_depth:0
      ~declarator_context:Aggregate_member_declarator
  in
  let parsed_modifiers = parse_modifiers cursor [] in
  let modifiers =
    List.map
      (fun (modifier : parsed_modifier) -> modifier.node)
      parsed_modifiers
  in
  let modifier_tokens =
    List.map
      (fun (modifier : parsed_modifier) -> modifier.item.token)
      parsed_modifiers
  in
  let aggregate_forward_kind =
    let binding_item = peek cursor in
    match binding_item.token.kind with
    | Token_kind.Keyword Keyword.Extern ->
        aggregate_kind_of_token (peek_n cursor 1).token
    | _ -> None
  in
  match aggregate_forward_kind with
  | Some aggregate_kind ->
      let binding_item = take cursor in
      let aggregate_item = take cursor in
      let binding =
        Ast.make_declaration_binding ~kind:Ast.Extern
          ~spelling:binding_item.token.raw
          ~location:(token_location binding_item.token)
          ~target:Ast.No_binding_target
      in
      let name_item = peek cursor in
      if not (token_is_name_position_identifier name_item.token) then (
        report cursor name_item ~code:"HCPARSE0107"
          ~message:
            (Printf.sprintf "expected a name after %S %S, but found %s"
               binding_item.token.raw aggregate_item.token.raw
               (token_description name_item.token));
        recover_declaration cursor;
        None)
      else
        let name_item = take cursor in
        let name =
          Ast.make_identifier ~spelling:name_item.token.raw
            ~location:(token_location name_item.token)
        in
        let semicolon_item = peek cursor in
        if semicolon_item.token.kind <> Token_kind.Punctuation ';' then (
          report cursor semicolon_item ~code:"HCPARSE0108"
            ~message:
              (Printf.sprintf
                 "expected ';' after %s forward declaration %S, but found %s"
                 aggregate_item.token.raw name.spelling
                 (token_description semicolon_item.token));
          let rec recover nested_braces =
            let item = peek cursor in
            match item.token.kind with
            | Token_kind.Eof -> ()
            | Token_kind.Punctuation ';' when nested_braces = 0 ->
                ignore (take cursor)
            | Token_kind.Punctuation '{' ->
                ignore (take cursor);
                recover (nested_braces + 1)
            | Token_kind.Punctuation '}' when nested_braces > 0 ->
                ignore (take cursor);
                recover (nested_braces - 1)
            | _ ->
                ignore (take cursor);
                recover nested_braces
          in
          recover 0;
          None)
        else
          let semicolon_item = take cursor in
          let declaration_tokens =
            modifier_tokens
            @ [
                binding_item.token;
                aggregate_item.token;
                name_item.token;
                semicolon_item.token;
              ]
          in
          let base_location = location_from_tokens declaration_tokens in
          let first_token = List.hd declaration_tokens in
          let location =
            Ast.make_location ?generated_from:first_token.origin.generated_from
              ?defined_at:first_token.origin.defined_at ~span:base_location.span
              ~source_segments:base_location.source_segments ()
          in
          let declaration =
            Ast.make_aggregate_forward_declaration ~modifiers ~binding
              ~aggregate_kind
              ~aggregate_keyword_spelling:aggregate_item.token.raw
              ~aggregate_keyword_location:(token_location aggregate_item.token)
              ~name
              ~semicolon:(token_location semicolon_item.token)
              ~location
          in
          publish_class cursor name;
          Some (Ast.Aggregate_forward_declaration declaration)
  | None -> (
      match aggregate_kind_of_token (peek cursor).token with
      | Some aggregate_kind ->
          parse_aggregate_definition cursor ~modifier_tokens ~modifiers
            ~backing:None ~aggregate_kind
            ~parse_function_pointer:parse_global_function_pointer
            ~parse_member_function_pointer
      | None -> (
          match parse_binding cursor with
          | Bad_binding -> None
          | Parsed_binding binding
            when binding.node.kind = Ast.Import
                 && cursor.compilation_mode = Preprocessor.Jit ->
              report cursor binding.keyword ~code:"HCPARSE0006"
                ~message:
                  "import declarations require AOT mode; select AOT mode \
                   before parsing this declaration";
              recover_declaration cursor;
              None
          | binding_parse -> (
              let binding, binding_tokens =
                match binding_parse with
                | No_binding -> (None, [])
                | Parsed_binding binding -> (Some binding.node, binding.tokens)
                | Bad_binding -> assert false
              in
              let type_item = peek cursor in
              match type_specifier_of_item cursor type_item with
              | Some type_specifier -> (
                  let type_item = take cursor in
                  match aggregate_kind_after_backing cursor ~offset:0 with
                  | Some aggregate_kind -> (
                      match binding with
                      | Some binding ->
                          report cursor type_item ~code:"HCPARSE0125"
                            ~message:
                              (Printf.sprintf
                                 "declaration binding %S cannot introduce a \
                                  type-backed aggregate definition"
                                 binding.Ast.spelling);
                          recover_aggregate_declaration cursor ~depth:0;
                          None
                      | None -> (
                          match
                            parse_aggregate_backing cursor type_item
                              type_specifier
                          with
                          | None -> None
                          | Some backing ->
                              parse_aggregate_definition cursor ~modifier_tokens
                                ~modifiers ~backing:(Some backing)
                                ~aggregate_kind
                                ~parse_function_pointer:
                                  parse_global_function_pointer
                                ~parse_member_function_pointer))
                  | None -> (
                      let spelling =
                        Ast.type_specifier_spelling type_specifier
                      in
                      match
                        parse_declarator_prefix cursor spelling
                          ~parse_function_pointer:parse_global_function_pointer
                      with
                      | None -> None
                      | Some first_prefix -> (
                          let next_item = peek cursor in
                          match (next_item.token.kind, binding) with
                          | Token_kind.Punctuation '(', Some binding ->
                              parse_function_prototype cursor ~modifier_tokens
                                ~modifiers ~binding_tokens ~binding ~type_item
                                ~return_type:type_specifier first_prefix
                          | Token_kind.Punctuation '(', None ->
                              parse_function_definition cursor ~modifier_tokens
                                ~modifiers ~type_item
                                ~return_type:type_specifier first_prefix
                          | _ -> (
                              match
                                parse_variable_declarator_suffix
                                  ~header:
                                    (declaration_header cursor ~modifiers
                                       ~binding ~type_specifier)
                                  cursor first_prefix
                              with
                              | None -> None
                              | Some first_declarator -> (
                                  let parsed_declarators =
                                    match
                                      first_declarator.node.delimiter.kind
                                    with
                                    | Ast.Semicolon ->
                                        Some
                                          {
                                            declarators = [ first_declarator ];
                                            trailing_semicolon = None;
                                          }
                                    | Ast.Comma ->
                                        parse_declarators
                                          ~header:
                                            (declaration_header cursor
                                               ~modifiers ~binding
                                               ~type_specifier)
                                          cursor spelling
                                          ~parse_function_pointer:
                                            parse_global_function_pointer
                                          [ first_declarator ]
                                  in
                                  match parsed_declarators with
                                  | None -> None
                                  | Some parsed_declarators -> (
                                      let declarators =
                                        parsed_declarators.declarators
                                      in
                                      let trailing_tokens =
                                        Option.to_list
                                          (Option.map
                                             (fun item -> item.token)
                                             parsed_declarators
                                               .trailing_semicolon)
                                      in
                                      let declaration_tokens =
                                        modifier_tokens @ binding_tokens
                                        @ type_item.token
                                          :: List.concat_map
                                               (fun (item : parsed_declarator)
                                                  -> item.tokens)
                                               declarators
                                        @ trailing_tokens
                                      in
                                      match declarators with
                                      | [ declarator ]
                                        when Option.is_none
                                               parsed_declarators
                                                 .trailing_semicolon
                                             && Option.is_none
                                                  declarator.node
                                                    .global_initial_value
                                             && Option.is_none
                                                  declarator.node
                                                    .function_pointer ->
                                          let variable =
                                            Ast.make_global_variable ~modifiers
                                              ~binding ~type_specifier
                                              ~pointer_layers:
                                                declarator.node.pointer_layers
                                              ~name:declarator.node.name
                                              ~array_dimensions:
                                                declarator.node.array_dimensions
                                              ~semicolon:
                                                declarator.node.delimiter
                                                  .location
                                                  .span
                                              ~location:
                                                (location_from_tokens
                                                   declaration_tokens)
                                          in
                                          Some (Ast.Global_variable variable)
                                      | _ ->
                                          let declaration =
                                            Ast.make_global_declaration
                                              ~modifiers ~binding
                                              ~type_specifier
                                              ~declarators:
                                                (List.map
                                                   (fun (item :
                                                          parsed_declarator) ->
                                                     item.node)
                                                   declarators)
                                              ~trailing_semicolon:
                                                (Option.map
                                                   (fun item ->
                                                     token_location item.token)
                                                   parsed_declarators
                                                     .trailing_semicolon)
                                              ~location:
                                                (location_from_tokens
                                                   declaration_tokens)
                                          in
                                          Some
                                            (Ast.Global_declaration declaration)
                                      ))))))
              | _
                when Option.is_some
                       (aggregate_kind_after_backing cursor ~offset:1) ->
                  report cursor type_item ~code:"HCPARSE0124"
                    ~message:
                      (Printf.sprintf
                         "%S is not a visible HolyC type and cannot back this \
                          aggregate definition"
                         type_item.token.raw);
                  recover_aggregate_declaration cursor ~depth:0;
                  None
              | _ ->
                  let prefix =
                    match binding with
                    | Some (binding : Ast.declaration_binding) ->
                        Printf.sprintf "after declaration binding %S"
                          binding.spelling
                    | None -> (
                        match modifiers with
                        | [] -> "at the start of a global declaration"
                        | _ ->
                            Printf.sprintf "after declaration modifier%s %S"
                              (if List.length modifiers = 1 then "" else "s")
                              (modifiers
                              |> List.map
                                   (fun (modifier : Ast.declaration_modifier) ->
                                     modifier.spelling)
                              |> String.concat " "))
                  in
                  report cursor type_item ~code:"HCPARSE0001"
                    ~message:
                      (Printf.sprintf
                         "expected a primitive, class, or union type %s, but \
                          found %s"
                         prefix
                         (token_description type_item.token));
                  recover_declaration cursor;
                  None)))

let parse_implicit_output_statement cursor ~boundary : parsed_statement option =
  let marker_item = peek cursor in
  let target =
    match (marker_item.token.Token.kind, marker_item.token.value) with
    | Token_kind.String, Token.Bytes _ -> Ast.Print_target
    | Token_kind.Character, Token.Int64 _ -> Ast.Put_chars_target
    | _ -> invalid_arg "an implicit output statement needs a literal marker"
  in
  let selection =
    {
      output_target = target;
      output_marker = token_location marker_item.token;
      output_environment = cursor.symbols;
      output_lookup =
        Symbol_visibility.Environment.find_function cursor.symbols
          (match target with
          | Ast.Print_target -> "Print"
          | Ast.Put_chars_target -> "PutChars");
      output_command = Option.get cursor.current_command;
      output_active = true;
      output_statement = None;
      output_arguments = { call_active = false; call_captured = false };
      output_emission = { call_active = false; call_captured = false };
    }
  in
  Fun.protect
    ~finally:(fun () -> selection.output_active <- false)
    (fun () ->
      Option.iter
        (fun observe ->
          record_observation selection.output_command.command_context
            (Implicit_output selection);
          match observe selection with
          | Ok () -> ()
          | Error diagnostics ->
              cursor.diagnostics_rev <-
                List.rev_append diagnostics cursor.diagnostics_rev;
              if not (has_error diagnostics) then
                report cursor marker_item ~code:"HCPARSE0161"
                  ~message:
                    "implicit output consumer failed without an error \
                     diagnostic";
              raise Stop_command)
        cursor.implicit_output);
  let marker_empty =
    match marker_item.token.value with
    | Token.Bytes value ->
        String.length value = 0 || Char.equal value.[0] '\000'
    | Token.Int64 value -> Int64.equal value 0L
    | _ -> false
  in
  let consumed_marker =
    if marker_empty then (
      let item = take cursor in
      ignore (peek cursor);
      Some item)
    else None
  in
  let implicit_sink = Option.bind cursor.call (fun sink -> sink.implicit) in
  let observe_phase activity event callback =
    activity.call_active <- true;
    Fun.protect
      ~finally:(fun () -> activity.call_active <- false)
      (fun () ->
        record_observation selection.output_command.command_context event;
        match callback selection with
        | Ok value -> value
        | Error diagnostics ->
            cursor.diagnostics_rev <-
              List.rev_append diagnostics cursor.diagnostics_rev;
            if not (has_error diagnostics) then
              report cursor marker_item ~code:"HCPARSE0161"
                ~message:
                  "implicit call consumer failed without an error diagnostic";
            raise Stop_command)
  in
  let supplied_shape =
    match implicit_sink with
    | None -> None
    | Some sink ->
        observe_phase selection.output_arguments (Implicit_arguments selection)
          sink.arguments
  in
  let selected_shape =
    match supplied_shape with
    | Some _ -> supplied_shape
    | None ->
        Option.bind selection.output_lookup
          Symbol_visibility.function_call_shape
  in
  let selected_parameter index =
    Option.bind selected_shape (fun shape ->
        List.nth_opt shape.Symbol_visibility.parameters index)
  in
  let selected_default index =
    selected_parameter index
    |> Option.fold ~none:false ~some:(fun parameter ->
        parameter.Symbol_visibility.has_default)
  in
  let reject_unconsumed_default item =
    report cursor item ~code:"HCPARSE0164"
      ~message:"implicit output default leaves this argument unconsumed";
    raise Stop_command
  in
  let putchars_later_required =
    target = Ast.Put_chars_target
    && Option.fold ~none:false
         ~some:(fun shape ->
           List.exists
             (fun (index, parameter) ->
               index > 0 && not parameter.Symbol_visibility.has_default)
             (List.mapi
                (fun index parameter -> (index, parameter))
                shape.Symbol_visibility.parameters))
         selected_shape
  in
  let deferred_marker =
    (not marker_empty) && selected_default 0 && putchars_later_required
  in
  if (not marker_empty) && selected_default 0 && not deferred_marker then
    reject_unconsumed_default marker_item;
  let marker_expression : parsed_expression =
    match (marker_item.token.Token.kind, marker_item.token.value) with
    | Token_kind.String, Token.Bytes value when marker_empty ->
        let item = Option.get consumed_marker in
        {
          node =
            make_literal item.token (Ast.Bytes_value value) (fun literal ->
                Ast.String_literal literal);
          tokens = [ item.token ];
        }
    | Token_kind.String, Token.Bytes _ -> take_string_literal_sequence cursor
    | Token_kind.Character, Token.Int64 value ->
        let item =
          match consumed_marker with
          | Some item -> item
          | None -> take cursor
        in
        {
          node =
            make_literal item.token (Ast.Integer_value value) (fun literal ->
                Ast.Character_literal literal);
          tokens = [ item.token ];
        }
    | _ -> invalid_arg "an implicit output statement needs a literal marker"
  in
  let marker =
    match marker_expression.node with
    | Ast.String_literal literal | Ast.Character_literal literal -> literal
    | _ -> invalid_arg "an implicit output marker must remain a literal"
  in
  let empty_marker =
    match marker.literal_value with
    | Ast.Bytes_value value ->
        String.length value = 0 || Char.equal value.[0] '\000'
    | Ast.Integer_value value -> Int64.equal value 0L
    | Ast.Float_value _ -> false
  in
  let opening_parenthesis =
    if empty_marker && (peek cursor).token.kind = Token_kind.Punctuation '('
    then Some (take cursor)
    else None
  in
  let fixed_prefix =
    (if deferred_marker then [] else marker_expression.tokens)
    @
    match opening_parenthesis with
    | None -> []
    | Some item -> [ item.token ]
  in
  let initial_item = if deferred_marker then marker_item else peek cursor in
  let omitted_initial =
    (empty_marker || deferred_marker)
    && selected_default 0
    &&
    match opening_parenthesis with
    | Some _ ->
        initial_item.token.kind = Token_kind.Punctuation ','
        || initial_item.token.kind = Token_kind.Punctuation ')'
    | None -> true
  in
  let no_values =
    empty_marker
    && Option.fold ~none:false
         ~some:(fun shape ->
           shape.Symbol_visibility.parameters = []
           && ((not shape.variadic)
              || target = Ast.Put_chars_target
                 && (opening_parenthesis = None
                    || initial_item.token.kind = Token_kind.Punctuation ')')
              || target = Ast.Print_target
                 && initial_item.token.kind = Token_kind.Punctuation ';'))
         selected_shape
  in
  if
    (omitted_initial || no_values)
    && Option.is_none opening_parenthesis
    && (not putchars_later_required)
    && initial_item.token.kind <> Token_kind.Punctuation ';'
    && initial_item.token.kind <> Token_kind.Punctuation ','
  then reject_unconsumed_default initial_item;
  let initial_omissions =
    if omitted_initial then
      [
        Ast.make_implicit_output_omission ~parameter_index:0 ~leading_comma:None
          ~lookahead:(token_location initial_item.token);
      ]
    else []
  in
  let fixed_argument =
    if omitted_initial || no_values then
      Some (Ast.Absent_fixed_argument, fixed_prefix)
    else if empty_marker then (
      let next_item = peek cursor in
      if selected_default 0 && Option.is_none opening_parenthesis then
        reject_unconsumed_default next_item;
      match next_item.token.kind with
      | Token_kind.Punctuation (';' | ',') | Token_kind.Eof ->
          let target_name =
            match target with
            | Ast.Print_target -> "format"
            | Ast.Put_chars_target -> "character"
          in
          report cursor next_item ~code:"HCPARSE0043"
            ~message:
              (Printf.sprintf
                 "empty %s output marker must be followed by a %s expression"
                 (if target = Ast.Print_target then "string" else "character")
                 target_name);
          None
      | _ ->
          parse_expression cursor ~context:Implicit_output_argument_expression
            ~depth:0 ~minimum_binding_power:0
          |> Option.map (fun (expression : parsed_expression) ->
              ( Ast.Expression_fixed_argument expression.node,
                fixed_prefix @ expression.tokens )))
    else
      parse_expression_tail cursor ~context:Implicit_output_argument_expression
        ~depth:0 ~minimum_binding_power:0 ~allow_parenthesis_free_call:true
        marker_expression
      |> Option.map (fun (expression : parsed_expression) ->
          (Ast.Marker_fixed_argument expression.node, expression.tokens))
  in
  match fixed_argument with
  | None ->
      recover_statement cursor ~boundary;
      None
  | Some (fixed_argument, fixed_tokens) -> (
      let completed_fixed_call =
        (omitted_initial || no_values)
        && Option.fold ~none:false
             ~some:(fun shape -> not shape.Symbol_visibility.variadic)
             selected_shape
      in
      let rec parse_print_arguments position arguments_rev omissions_rev
          tokens_rev =
        let item = peek cursor in
        let parameter = selected_parameter position in
        if completed_fixed_call && Option.is_none parameter then
          Some
            (List.rev arguments_rev, List.rev omissions_rev, List.rev tokens_rev)
        else
          let comma =
            match item.token.kind with
            | Token_kind.Punctuation ',' -> Some (take cursor)
            | _ -> None
          in
          let argument_item = peek cursor in
          let tokens_rev =
            match comma with
            | None -> tokens_rev
            | Some comma -> comma.token :: tokens_rev
          in
          match parameter with
          | Some parameter when parameter.Symbol_visibility.has_default ->
              if
                item.token.kind <> Token_kind.Punctuation ','
                && item.token.kind <> Token_kind.Punctuation ';'
              then reject_unconsumed_default item;
              if
                argument_item.token.kind <> Token_kind.Punctuation ','
                && argument_item.token.kind <> Token_kind.Punctuation ';'
              then reject_unconsumed_default argument_item;
              let omission =
                Ast.make_implicit_output_omission ~parameter_index:position
                  ~leading_comma:
                    (Option.map (fun item -> token_location item.token) comma)
                  ~lookahead:(token_location argument_item.token)
              in
              parse_print_arguments (position + 1) arguments_rev
                (omission :: omissions_rev)
                tokens_rev
          | Some _ | None -> (
              match (comma, parameter, argument_item.token.kind) with
              | None, None, _ ->
                  Some
                    ( List.rev arguments_rev,
                      List.rev omissions_rev,
                      List.rev tokens_rev )
              | _, Some _, Token_kind.Punctuation (';' | ',') | None, Some _, _
                ->
                  report cursor argument_item ~code:"HCPARSE0165"
                    ~message:"implicit output is missing a required argument";
                  raise Stop_command
              | Some _, _, (Token_kind.Punctuation (';' | ',') | Token_kind.Eof)
                ->
                  report cursor argument_item ~code:"HCPARSE0044"
                    ~message:"expected a Print argument expression after ','";
                  None
              | Some comma, _, _ -> (
                  match
                    parse_expression cursor
                      ~context:Implicit_output_argument_expression ~depth:0
                      ~minimum_binding_power:0
                  with
                  | None -> None
                  | Some (expression : parsed_expression) ->
                      let argument =
                        Ast.make_implicit_output_argument
                          ~leading_comma:(token_location comma.token)
                          ~value:expression.node
                          ~location:
                            (location_from_expression_tokens
                               (comma.token :: expression.tokens))
                      in
                      parse_print_arguments (position + 1)
                        (argument :: arguments_rev)
                        omissions_rev
                        (List.rev_append expression.tokens tokens_rev)))
      in
      let parse_parenthesized_arguments () =
        let syntax_error item message =
          report cursor item ~code:"HCPARSE0167" ~message;
          raise Stop_command
        in
        let rec supplied position comma arguments_rev omissions_rev tokens_rev
            next =
          let item = peek cursor in
          match item.token.kind with
          | Token_kind.Punctuation (',' | ')' | ';') | Token_kind.Eof ->
              report cursor item ~code:"HCPARSE0165"
                ~message:"implicit output is missing a required argument";
              raise Stop_command
          | _ -> (
              match
                parse_expression cursor
                  ~context:Implicit_output_argument_expression ~depth:0
                  ~minimum_binding_power:0
              with
              | None -> None
              | Some (expression : parsed_expression) ->
                  let argument =
                    Ast.make_implicit_output_argument
                      ~leading_comma:(token_location comma.token)
                      ~value:expression.node
                      ~location:
                        (location_from_expression_tokens
                           (comma.token :: expression.tokens))
                  in
                  next (position + 1)
                    (argument :: arguments_rev)
                    omissions_rev
                    (List.rev_append expression.tokens
                       (comma.token :: tokens_rev)))
        and fixed position arguments_rev omissions_rev tokens_rev =
          match selected_parameter position with
          | None ->
              let variadic =
                Option.fold ~none:true
                  ~some:(fun shape -> shape.Symbol_visibility.variadic)
                  selected_shape
              in
              if variadic then
                let already_variadic =
                  Option.fold ~none:true
                    ~some:(fun shape -> shape.Symbol_visibility.parameters = [])
                    selected_shape
                in
                variadic_tail already_variadic position arguments_rev
                  omissions_rev tokens_rev
              else finish arguments_rev omissions_rev tokens_rev
          | Some parameter -> (
              let item = peek cursor in
              let comma =
                match item.token.kind with
                | Token_kind.Punctuation ',' -> Some (take cursor)
                | Token_kind.Punctuation ';' when target = Ast.Print_target ->
                    None
                | Token_kind.Punctuation ')' when target = Ast.Put_chars_target
                  -> None
                | _ ->
                    syntax_error item
                      "expected ',' before the next implicit argument"
              in
              let item = peek cursor in
              if
                parameter.Symbol_visibility.has_default
                && (item.token.kind = Token_kind.Punctuation ','
                   || item.token.kind = Token_kind.Punctuation ')')
              then
                let omission =
                  Ast.make_implicit_output_omission ~parameter_index:position
                    ~leading_comma:
                      (Option.map (fun item -> token_location item.token) comma)
                    ~lookahead:(token_location item.token)
                in
                let tokens_rev =
                  match comma with
                  | None -> tokens_rev
                  | Some item -> item.token :: tokens_rev
                in
                fixed (position + 1) arguments_rev
                  (omission :: omissions_rev)
                  tokens_rev
              else
                match comma with
                | Some comma ->
                    supplied position comma arguments_rev omissions_rev
                      tokens_rev fixed
                | None ->
                    report cursor item ~code:"HCPARSE0165"
                      ~message:"implicit output is missing a required argument";
                    raise Stop_command)
        and variadic_tail started position arguments_rev omissions_rev
            tokens_rev =
          let item = peek cursor in
          match item.token.kind with
          | Token_kind.Punctuation ',' ->
              let comma = take cursor in
              supplied position comma arguments_rev omissions_rev tokens_rev
                (variadic_tail true)
          | _
            when started
                 || target = Ast.Put_chars_target
                 || item.token.kind = Token_kind.Punctuation ';' ->
              finish arguments_rev omissions_rev tokens_rev
          | _ ->
              syntax_error item
                "expected ',' before an implicit Print variadic argument"
        and finish arguments_rev omissions_rev tokens_rev =
          Some
            (List.rev arguments_rev, List.rev omissions_rev, List.rev tokens_rev)
        in
        fixed 1 [] [] []
      in
      let rec parse_putchars_arguments position pending_marker arguments_rev
          omissions_rev tokens_rev =
        match selected_parameter position with
        | None ->
            Some
              ( List.rev arguments_rev,
                List.rev omissions_rev,
                List.rev tokens_rev )
        | Some parameter ->
            let item =
              if Option.is_some pending_marker then marker_item else peek cursor
            in
            if parameter.Symbol_visibility.has_default then
              let omission =
                Ast.make_implicit_output_omission ~parameter_index:position
                  ~leading_comma:None
                  ~lookahead:(token_location item.token)
              in
              parse_putchars_arguments (position + 1) pending_marker
                arguments_rev
                (omission :: omissions_rev)
                tokens_rev
            else (
              (match item.token.kind with
              | Token_kind.Punctuation (',' | ';' | ')' | '}') | Token_kind.Eof
                ->
                  report cursor item ~code:"HCPARSE0165"
                    ~message:"implicit output is missing a required argument";
                  raise Stop_command
              | _ -> ());
              let parsed =
                match pending_marker with
                | Some marker ->
                    parse_expression_tail cursor
                      ~context:Implicit_output_argument_expression ~depth:0
                      ~minimum_binding_power:0 ~allow_parenthesis_free_call:true
                      marker
                | None ->
                    parse_expression cursor
                      ~context:Implicit_output_argument_expression ~depth:0
                      ~minimum_binding_power:0
              in
              match parsed with
              | None -> None
              | Some (expression : parsed_expression) ->
                  let argument =
                    Ast.make_implicit_output_argument_with_separator
                      ~leading_comma:None ~value:expression.node
                      ~location:
                        (location_from_expression_tokens expression.tokens)
                  in
                  parse_putchars_arguments (position + 1) None
                    (argument :: arguments_rev)
                    omissions_rev
                    (List.rev_append expression.tokens tokens_rev))
      in
      let parsed_arguments =
        match (opening_parenthesis, target) with
        | Some _, _ -> parse_parenthesized_arguments ()
        | None, Ast.Print_target -> parse_print_arguments 1 [] [] []
        | None, Ast.Put_chars_target ->
            parse_putchars_arguments 1
              (if deferred_marker then Some marker_expression else None)
              [] [] []
      in
      match parsed_arguments with
      | None ->
          recover_statement cursor ~boundary;
          None
      | Some (arguments, omissions, argument_tokens) -> (
          let omissions = initial_omissions @ omissions in
          let call_parentheses, closing_tokens =
            match opening_parenthesis with
            | None -> (None, [])
            | Some opening ->
                let closing = peek cursor in
                if closing.token.kind <> Token_kind.Punctuation ')' then (
                  report cursor closing ~code:"HCPARSE0167"
                    ~message:"expected ')' after implicit call arguments";
                  raise Stop_command);
                let closing = take cursor in
                ( Some
                    (token_location opening.token, token_location closing.token),
                  [ closing.token ] )
          in
          let terminator_item = peek cursor in
          Option.iter
            (fun sink ->
              observe_phase selection.output_emission
                (Implicit_emission selection) sink.emission)
            implicit_sink;
          let terminator =
            match (boundary, terminator_item.token.kind) with
            | For_update_boundary _, _ -> Some (None, [])
            | _, Token_kind.Punctuation ';' ->
                let semicolon_item = take cursor in
                Some
                  ( Some (token_location semicolon_item.token),
                    [ semicolon_item.token ] )
            | _, Token_kind.Punctuation ','
              when target = Ast.Put_chars_target
                   || completed_fixed_call
                   || Option.is_some opening_parenthesis -> Some (None, [])
            | _ ->
                report cursor terminator_item ~code:"HCPARSE0046"
                  ~message:
                    (Printf.sprintf
                       "expected ';' after implicit %s statement, but found %s"
                       (match target with
                       | Ast.Print_target -> "Print"
                       | Ast.Put_chars_target -> "PutChars")
                       (token_description terminator_item.token));
                None
          in
          match terminator with
          | None ->
              recover_statement cursor ~boundary;
              None
          | Some (semicolon, terminator_tokens) ->
              let tokens =
                fixed_tokens @ argument_tokens @ closing_tokens
                @ terminator_tokens
              in
              let statement =
                Ast.make_implicit_output_statement_with_syntax ~target ~marker
                  ~fixed_argument ~arguments ~omissions ~call_parentheses
                  ~semicolon
                  ~location:(location_from_expression_tokens tokens)
              in
              selection.output_statement <- Some statement;
              Some { node = Ast.Implicit_output_statement statement; tokens }))

let rec take_statement_commas cursor items_rev =
  let item = peek cursor in
  match item.token.kind with
  | Token_kind.Punctuation ',' ->
      take_statement_commas cursor (take cursor :: items_rev)
  | _ -> List.rev items_rev

let statement_symbol_is_expression cursor name =
  match Symbol_visibility.Environment.find_preprocessor cursor.symbols name with
  | Symbol_visibility.Absent -> false
  | Symbol_visibility.Shadowed_by_local -> true
  | Symbol_visibility.Present entry -> (
      match Symbol_visibility.kind entry with
      | Symbol_visibility.Export_system_symbol
      | Symbol_visibility.Global_variable
      | Symbol_visibility.Function
      | Symbol_visibility.Word
      | Symbol_visibility.Dictionary_word
      | Symbol_visibility.Frame_pointer -> true
      | Symbol_visibility.Import_system_symbol
      | Symbol_visibility.Definition
      | Symbol_visibility.Class
      | Symbol_visibility.Internal_type
      | Symbol_visibility.Keyword
      | Symbol_visibility.Assembly_keyword
      | Symbol_visibility.Opcode
      | Symbol_visibility.Register
      | Symbol_visibility.File
      | Symbol_visibility.Module
      | Symbol_visibility.Help_file -> false)

let token_starts_function_label cursor token =
  match token.Token.kind with
  | Token_kind.Identifier -> (
      match
        Symbol_visibility.Environment.find_preprocessor cursor.symbols
          (token_text token)
      with
      | Symbol_visibility.Absent ->
          (peek_n cursor 1).token.kind = Token_kind.Punctuation ':'
      | Symbol_visibility.Shadowed_by_local | Symbol_visibility.Present _ ->
          false)
  | _ -> false

let token_starts_statement_expression cursor token =
  match token.Token.kind with
  | Token_kind.Integer | Token_kind.Float -> true
  | Token_kind.Identifier ->
      statement_symbol_is_expression cursor (token_text token)
  | Token_kind.Punctuation ('(' | '+' | '-' | '!' | '~' | '*' | '&') -> true
  | Token_kind.Operator
      (Operator.Increment | Operator.Decrement | Operator.Current_position) ->
      true
  | Token_kind.Keyword (Keyword.Sizeof | Keyword.Offset | Keyword.Defined) ->
      true
  | _ -> false

let token_starts_global_declaration cursor token =
  Option.is_some (primitive_type_of_token token)
  || Option.is_some (internal_type_of_token cursor token)
  || token_is_named_type cursor token
  || Option.is_some (aggregate_kind_of_token token)
  || Option.is_some (declaration_modifier_kind token)
  || Option.is_some (declaration_binding_kind token)
  || token.kind = Token_kind.Keyword Keyword.Underscore_intern
  ||
  match token.kind with
  | Token_kind.Identifier ->
      not (statement_symbol_is_expression cursor (token_text token))
  | _ -> false

let parse_empty_statement cursor : parsed_statement =
  let semicolon_item = take cursor in
  let location = token_location semicolon_item.token in
  {
    node =
      Ast.Empty_statement
        (Ast.make_empty_statement ~semicolon:location ~location);
    tokens = [ semicolon_item.token ];
  }

let parse_break_statement cursor ~boundary : parsed_statement option =
  let keyword_item = take cursor in
  let terminator_item = peek cursor in
  let terminator =
    match (boundary, terminator_item.token.kind) with
    | For_update_boundary _, _ -> Some (None, [])
    | _, Token_kind.Punctuation ';' ->
        let semicolon_item = take cursor in
        Some
          (Some (token_location semicolon_item.token), [ semicolon_item.token ])
    | _, Token_kind.Punctuation ',' -> Some (None, [])
    | _ ->
        report cursor terminator_item ~code:"HCPARSE0072"
          ~message:
            (Printf.sprintf "expected ';' or ',' after 'break', but found %s"
               (token_description terminator_item.token));
        None
  in
  match terminator with
  | None ->
      recover_statement cursor ~boundary;
      None
  | Some (semicolon, terminator_tokens) ->
      let tokens = keyword_item.token :: terminator_tokens in
      let statement =
        Ast.make_break_statement
          ~keyword:(token_location keyword_item.token)
          ~semicolon
          ~location:(location_from_expression_tokens tokens)
      in
      Some { node = Ast.Break_statement statement; tokens }

let parse_goto_statement cursor ~boundary : parsed_statement option =
  let keyword_item = take cursor in
  let target_item = peek cursor in
  if target_item.token.kind <> Token_kind.Identifier then (
    report cursor target_item ~code:"HCPARSE0075"
      ~message:
        (Printf.sprintf "expected a label name after 'goto', but found %s"
           (token_description target_item.token));
    recover_statement cursor ~boundary;
    None)
  else
    let target_item = take cursor in
    let terminator_item = peek cursor in
    let terminator =
      match (boundary, terminator_item.token.kind) with
      | For_update_boundary _, _ -> Some (None, [])
      | _, Token_kind.Punctuation ';' ->
          let semicolon_item = take cursor in
          Some
            ( Some (token_location semicolon_item.token),
              [ semicolon_item.token ] )
      | _, Token_kind.Punctuation ',' -> Some (None, [])
      | _ ->
          report cursor terminator_item ~code:"HCPARSE0076"
            ~message:
              (Printf.sprintf
                 "expected ';' or ',' after goto target %S, but found %s"
                 (token_text target_item.token)
                 (token_description terminator_item.token));
          None
    in
    match terminator with
    | None ->
        recover_statement cursor ~boundary;
        None
    | Some (semicolon, terminator_tokens) ->
        let tokens =
          keyword_item.token :: target_item.token :: terminator_tokens
        in
        let target =
          Ast.make_identifier
            ~spelling:(token_text target_item.token)
            ~location:(token_location target_item.token)
        in
        let statement =
          Ast.make_goto_statement
            ~keyword:(token_location keyword_item.token)
            ~target ~semicolon
            ~location:(location_from_expression_tokens tokens)
        in
        Some { node = Ast.Goto_statement statement; tokens }

let parse_no_warn_statement cursor ~boundary : parsed_statement option =
  let keyword_item = take cursor in
  let target_is_visible name =
    match
      Symbol_visibility.Environment.find_preprocessor cursor.symbols name
    with
    | Symbol_visibility.Shadowed_by_local -> true
    | Symbol_visibility.Absent | Symbol_visibility.Present _ -> false
  in
  let build targets_rev tokens_rev semicolon terminator_tokens :
      parsed_statement option =
    let tokens =
      keyword_item.token :: (List.rev tokens_rev @ terminator_tokens)
    in
    let statement =
      Ast.make_no_warn_statement
        ~keyword:(token_location keyword_item.token)
        ~targets:(List.rev targets_rev) ~semicolon
        ~location:(location_from_expression_tokens tokens)
    in
    Some ({ node = Ast.No_warn_statement statement; tokens } : parsed_statement)
  in
  let rec collect targets_rev tokens_rev =
    let target_item = peek cursor in
    match target_item.token.kind with
    | Token_kind.Punctuation ';' ->
        let semicolon_item = take cursor in
        build targets_rev tokens_rev
          (Some (token_location semicolon_item.token))
          [ semicolon_item.token ]
    | Token_kind.Punctuation ',' -> build targets_rev tokens_rev None []
    | Token_kind.Punctuation ')' when is_for_update_boundary boundary ->
        build targets_rev tokens_rev None []
    | Token_kind.Identifier ->
        let name = token_text target_item.token in
        if not (target_is_visible name) then (
          report cursor target_item ~code:"HCPARSE0157"
            ~message:
              (Printf.sprintf
                 "no_warn target %S is not a visible parameter or local" name);
          recover_statement cursor ~boundary;
          None)
        else
          let target_item = take cursor in
          let next_item = peek cursor in
          let following_comma, tokens_rev =
            match next_item.token.kind with
            | Token_kind.Punctuation ',' ->
                let comma_item = take cursor in
                ( Some (token_location comma_item.token),
                  comma_item.token :: target_item.token :: tokens_rev )
            | _ -> (None, target_item.token :: tokens_rev)
          in
          let target_tokens =
            match following_comma with
            | None -> [ target_item.token ]
            | Some _ -> [ target_item.token; next_item.token ]
          in
          let target =
            Ast.make_no_warn_target
              ~name:
                (Ast.make_identifier ~spelling:name
                   ~location:(token_location target_item.token))
              ~following_comma
              ~location:(location_from_expression_tokens target_tokens)
          in
          if Option.is_some following_comma then
            collect (target :: targets_rev) tokens_rev
          else if next_item.token.kind = Token_kind.Punctuation ';' then
            collect (target :: targets_rev) tokens_rev
          else (
            report cursor next_item ~code:"HCPARSE0158"
              ~message:
                (Printf.sprintf
                   "expected ',' or ';' after no_warn target %S, but found %s"
                   name
                   (token_description next_item.token));
            recover_statement cursor ~boundary;
            None)
    | _ when targets_rev <> [] ->
        report cursor target_item ~code:"HCPARSE0158"
          ~message:
            (Printf.sprintf
               "expected another no_warn target or a statement boundary after \
                ',', but found %s"
               (token_description target_item.token));
        recover_statement cursor ~boundary;
        None
    | _ ->
        report cursor target_item ~code:"HCPARSE0157"
          ~message:
            (Printf.sprintf
               "expected a visible parameter or local after 'no_warn', but \
                found %s"
               (token_description target_item.token));
        recover_statement cursor ~boundary;
        None
  in
  collect [] []

let parse_label_statement cursor : parsed_statement option =
  let name_item = take cursor in
  let colon_item = take cursor in
  let tokens = [ name_item.token; colon_item.token ] in
  let name =
    Ast.make_identifier
      ~spelling:(token_text name_item.token)
      ~location:(token_location name_item.token)
  in
  let statement =
    Ast.make_label_statement ~name
      ~colon:(token_location colon_item.token)
      ~location:(location_from_expression_tokens tokens)
  in
  Some { node = Ast.Label_statement statement; tokens }

let parse_return_statement cursor ~boundary : parsed_statement option =
  let keyword_item = take cursor in
  let build value value_tokens semicolon terminator_tokens :
      parsed_statement option =
    let tokens = keyword_item.token :: (value_tokens @ terminator_tokens) in
    let statement =
      Ast.make_return_statement
        ~keyword:(token_location keyword_item.token)
        ~value ~semicolon
        ~location:(location_from_expression_tokens tokens)
    in
    Some { node = Ast.Return_statement statement; tokens }
  in
  let first_item = peek cursor in
  match (boundary, first_item.token.kind) with
  | For_update_boundary _, Token_kind.Punctuation ';' ->
      report cursor first_item ~code:"HCPARSE0070"
        ~message:"expected ')' after the for update, but found ';'";
      recover_statement cursor ~boundary;
      None
  | _, Token_kind.Punctuation ';' ->
      let semicolon_item = take cursor in
      build None []
        (Some (token_location semicolon_item.token))
        [ semicolon_item.token ]
  | _, (Token_kind.Punctuation (',' | ')' | ']' | '}') | Token_kind.Eof) ->
      report cursor first_item ~code:"HCPARSE0074"
        ~message:
          (Printf.sprintf "expected a return expression or ';', but found %s"
             (token_description first_item.token));
      recover_statement cursor ~boundary;
      None
  | _ -> (
      match
        parse_expression cursor ~context:Return_expression ~depth:0
          ~minimum_binding_power:0
      with
      | None ->
          recover_statement cursor ~boundary;
          None
      | Some (value : parsed_expression) -> (
          let terminator_item = peek cursor in
          let terminator =
            match (boundary, terminator_item.token.kind) with
            | For_update_boundary _, _ -> Some (None, [])
            | _, Token_kind.Punctuation ';' ->
                let semicolon_item = take cursor in
                Some
                  ( Some (token_location semicolon_item.token),
                    [ semicolon_item.token ] )
            | _, Token_kind.Punctuation ',' -> Some (None, [])
            | _ ->
                report cursor terminator_item ~code:"HCPARSE0073"
                  ~message:
                    (Printf.sprintf
                       "expected ';' or ',' after a return expression, but \
                        found %s"
                       (token_description terminator_item.token));
                None
          in
          match terminator with
          | None ->
              recover_statement cursor ~boundary;
              None
          | Some (semicolon, terminator_tokens) ->
              build (Some value.node) value.tokens semicolon terminator_tokens))

let parse_expression_statement cursor ~boundary : parsed_statement option =
  match
    parse_expression cursor ~context:Statement_expression ~depth:0
      ~minimum_binding_power:0
  with
  | None -> None
  | Some (expression : parsed_expression) -> (
      let terminator_item = peek cursor in
      let terminator =
        match (boundary, terminator_item.token.kind) with
        | For_update_boundary _, _ -> Some (None, [])
        | _, Token_kind.Punctuation ';' ->
            let semicolon_item = take cursor in
            Some
              ( Some (token_location semicolon_item.token),
                [ semicolon_item.token ] )
        | _, Token_kind.Punctuation ',' -> Some (None, [])
        | _ ->
            report cursor terminator_item ~code:"HCPARSE0047"
              ~message:
                (Printf.sprintf
                   "expected ';' or ',' after statement expression, but found \
                    %s"
                   (token_description terminator_item.token));
            None
      in
      match terminator with
      | None ->
          recover_statement cursor ~boundary;
          None
      | Some (semicolon, terminator_tokens) ->
          let tokens = expression.tokens @ terminator_tokens in
          let statement =
            Ast.make_expression_statement ~expression:expression.node ~semicolon
              ~location:(location_from_expression_tokens tokens)
          in
          Some { node = Ast.Expression_statement statement; tokens })

let rec take_static_local_modifiers cursor nodes_rev tokens_rev =
  let item = peek cursor in
  match item.token.kind with
  | Token_kind.Keyword Keyword.Static ->
      let item = take cursor in
      let node =
        Ast.make_declaration_modifier ~kind:Ast.Static ~spelling:item.token.raw
          ~location:(token_location item.token)
      in
      take_static_local_modifiers cursor (node :: nodes_rev)
        (item.token :: tokens_rev)
  | _ -> (List.rev nodes_rev, List.rev tokens_rev)

let parse_local_declarator cursor ~boundary ~storage ~base_spelling
    ~type_specifier ~register_qualifiers ~qualifier_tokens :
    parsed_local_declarator option =
  match
    parse_pointer_layers_with_recovery cursor
      ~recover:(fun cursor -> recover_statement cursor ~boundary)
      0 [] []
  with
  | None -> None
  | Some (pointer_layers, pointer_items) ->
      let pointer_tokens = List.map (fun item -> item.token) pointer_items in
      let name_item = peek cursor in
      let parsed_name =
        if name_item.token.kind = Token_kind.Punctuation '(' then
          Option.map
            (fun parsed ->
              let name =
                match parsed.name with
                | Some name -> name
                | None ->
                    invalid_arg
                      "local function-pointer parser returned an unnamed \
                       declarator"
              in
              (name, Some parsed.node, parsed.tokens))
            (parse_function_pointer_declarator cursor ~function_pointer_depth:0
               ~declarator_context:(Local_variable_declarator boundary))
        else if token_is_name_position_identifier name_item.token then
          let name_item = take cursor in
          let name =
            Ast.make_identifier ~spelling:name_item.token.raw
              ~location:(token_location name_item.token)
          in
          Some (name, None, [ name_item.token ])
        else
          local_declaration_failure cursor ~boundary name_item
            ~code:"HCPARSE0100"
            ~message:
              (Printf.sprintf
                 "expected a local variable name after type %S, but found %s"
                 (type_spelling base_spelling pointer_layers)
                 (token_description name_item.token))
      in
      Option.bind parsed_name
        (fun (name, function_pointer, declarator_tokens) ->
          match parse_array_dimensions cursor ~name with
          | None -> None
          | Some (array_dimensions, array_tokens) ->
              publish_local cursor ~spelling:name.spelling
                (Local_variable
                   {
                     local_type_specifier = type_specifier;
                     local_name = name;
                     local_pointer_layers = pointer_layers;
                     local_array_dimensions = array_dimensions;
                     local_function_pointer = function_pointer;
                   });
              let equals_item = peek cursor in
              let parsed_initializer =
                if equals_item.token.kind <> Token_kind.Punctuation '=' then
                  Some (None, [])
                else if Option.is_some function_pointer then
                  local_declaration_failure cursor ~boundary equals_item
                    ~code:"HCPARSE0137"
                    ~message:
                      (Printf.sprintf
                         "function-pointer local %S cannot be initialized in \
                          its declaration; declare it first and assign it in a \
                          following statement"
                         name.spelling)
                else if storage = Ast.Static_local then
                  let equals_item = take cursor in
                  Option.map
                    (fun (value : parsed_initializer) ->
                      let tokens = equals_item.token :: value.tokens in
                      let initial_value =
                        Ast.make_local_initializer
                          ~equals:(token_location equals_item.token)
                          ~value:value.node
                          ~location:(location_from_expression_tokens tokens)
                      in
                      (Some initial_value, tokens))
                    (parse_initializer_value cursor
                       ~declarator_context:
                         (Static_local_initializer_declarator boundary) ~depth:0)
                else
                  let equals_item = take cursor in
                  let value_item = peek cursor in
                  match value_item.token.kind with
                  | Token_kind.Punctuation (';' | ',')
                  | Token_kind.Punctuation '}'
                  | Token_kind.Eof ->
                      local_declaration_failure cursor ~boundary value_item
                        ~code:"HCPARSE0101"
                        ~message:
                          (Printf.sprintf
                             "expected a scalar initializer for local variable \
                              %S, but found %s"
                             name.spelling
                             (token_description value_item.token))
                  | Token_kind.Punctuation '{' ->
                      local_declaration_failure cursor ~boundary value_item
                        ~code:"HCPARSE0104"
                        ~message:
                          "braced initializers are not accepted on automatic \
                           local declarations by the pinned parser"
                  | _ -> (
                      match
                        parse_expression cursor
                          ~context:Local_initializer_expression ~depth:0
                          ~minimum_binding_power:0
                      with
                      | None -> None
                      | Some (value : parsed_expression) ->
                          let tokens = equals_item.token :: value.tokens in
                          let initial_value =
                            Ast.make_local_initializer
                              ~equals:(token_location equals_item.token)
                              ~value:(Ast.Scalar_initializer value.node)
                              ~location:(location_from_expression_tokens tokens)
                          in
                          Some (Some initial_value, tokens))
              in
              Option.bind parsed_initializer
                (fun (initial_value, initializer_tokens) ->
                  let delimiter_item = peek cursor in
                  match delimiter_kind delimiter_item.token with
                  | None ->
                      local_declaration_failure cursor ~boundary delimiter_item
                        ~code:"HCPARSE0102"
                        ~message:
                          (Printf.sprintf
                             "expected ',' or ';' after local variable %S, but \
                              found %s"
                             name.spelling
                             (token_description delimiter_item.token))
                  | Some kind ->
                      let delimiter_item = take cursor in
                      let delimiter =
                        Ast.make_declaration_delimiter ~kind
                          ~spelling:delimiter_item.token.raw
                          ~location:(token_location delimiter_item.token)
                      in
                      let tokens =
                        qualifier_tokens @ pointer_tokens @ declarator_tokens
                        @ array_tokens @ initializer_tokens
                        @ [ delimiter_item.token ]
                      in
                      let node =
                        Ast.make_local_declarator ~register_qualifiers
                          ~pointer_layers ~name ~function_pointer
                          ~array_dimensions ~initial_value ~delimiter
                          ~location:(location_from_expression_tokens tokens)
                      in
                      Some ({ node; tokens } : parsed_local_declarator)))

let parse_local_declaration cursor ~boundary : parsed_statement option =
  let first_item = peek cursor in
  let storage, modifiers, modifier_tokens =
    match first_item.token.kind with
    | Token_kind.Keyword Keyword.Static ->
        let modifiers, tokens = take_static_local_modifiers cursor [] [] in
        (Ast.Static_local, modifiers, tokens)
    | _ -> (Ast.Automatic_local, [], [])
  in
  let type_item = peek cursor in
  match type_specifier_of_item cursor type_item with
  | Some type_specifier ->
      let type_item = take cursor in
      let spelling = Ast.type_specifier_spelling type_specifier in
      let rec parse_declarators declarators_rev =
        let qualifiers =
          match storage with
          | Ast.Automatic_local ->
              parse_register_qualifiers cursor ~position:Ast.After_type [] []
          | Ast.Static_local -> { nodes = []; tokens = [] }
        in
        let qualifier_item = peek cursor in
        if
          storage = Ast.Static_local
          &&
          match qualifier_item.token.kind with
          | Token_kind.Keyword (Keyword.Reg | Keyword.Noreg) -> true
          | _ -> false
        then
          local_declaration_failure cursor ~boundary qualifier_item
            ~code:"HCPARSE0099"
            ~message:
              "register qualifiers are not accepted on static local \
               declarations by the pinned parser"
        else
          match
            parse_local_declarator cursor ~boundary ~storage
              ~base_spelling:spelling ~type_specifier
              ~register_qualifiers:qualifiers.nodes
              ~qualifier_tokens:qualifiers.tokens
          with
          | None -> None
          | Some declarator -> (
              let declarators_rev = declarator :: declarators_rev in
              match declarator.node.local_delimiter.kind with
              | Ast.Semicolon -> Some (List.rev declarators_rev)
              | Ast.Comma -> parse_declarators declarators_rev)
      in
      Option.map
        (fun declarators ->
          let tokens =
            modifier_tokens @ [ type_item.token ]
            @ List.concat_map
                (fun (declarator : parsed_local_declarator) ->
                  declarator.tokens)
                declarators
          in
          let declaration =
            Ast.make_local_declaration ~storage ~modifiers ~type_specifier
              ~declarators:
                (List.map
                   (fun (declarator : parsed_local_declarator) ->
                     declarator.node)
                   declarators)
              ~location:(location_from_expression_tokens tokens)
          in
          ({ node = Ast.Local_declaration_statement declaration; tokens }
            : parsed_statement))
        (parse_declarators [])
  | _ ->
      let code, message =
        match (storage, type_item.token.kind) with
        | Ast.Static_local, Token_kind.Keyword (Keyword.Reg | Keyword.Noreg) ->
            ( "HCPARSE0099",
              "register qualifiers are not accepted before a static local's \
               type by the pinned parser" )
        | _ ->
            ( "HCPARSE0098",
              Printf.sprintf
                "expected a primitive, class, or union type after a local \
                 declaration modifier, but found %s"
                (token_description type_item.token) )
      in
      local_declaration_failure cursor ~boundary type_item ~code ~message

let source_line_of_token cursor token =
  match Common.Source_manager.find cursor.sources token.Token.span.source with
  | None -> invalid_arg "assembly token source is not registered"
  | Some source -> (
      match Common.Source_file.position source token.span.start with
      | Ok position -> position.line
      | Error message -> invalid_arg message)

let token_is_assembly_label_delimiter token =
  match token.Token.kind with
  | Token_kind.Punctuation ':' -> true
  | Token_kind.Operator Operator.Double_colon -> true
  | _ -> false

let assembly_label_kind name delimiter =
  if String.length name >= 2 && String.sub name 0 2 = "@@" then
    Ast.Assembly_local_label
  else
    match delimiter.Token.kind with
    | Token_kind.Operator Operator.Double_colon ->
        Ast.Assembly_exported_global_label
    | Token_kind.Punctuation ':' -> Ast.Assembly_global_label
    | _ -> invalid_arg "assembly label delimiter was not a colon"

let assembly_labels tokens =
  let rec collect labels_rev = function
    | name_token :: delimiter_token :: remaining
      when name_token.Token.kind = Token_kind.Identifier
           && token_is_assembly_label_delimiter delimiter_token ->
        let name =
          Ast.make_identifier ~spelling:name_token.raw
            ~location:(token_location name_token)
        in
        let label_tokens = [ name_token; delimiter_token ] in
        let label =
          Ast.make_assembly_label
            ~kind:(assembly_label_kind name_token.raw delimiter_token)
            ~name ~delimiter_spelling:delimiter_token.raw
            ~delimiter:(token_location delimiter_token)
            ~location:(location_from_expression_tokens label_tokens)
        in
        collect (label :: labels_rev) remaining
    | _ -> List.rev labels_rev
  in
  collect [] tokens

let assembly_lines cursor tokens =
  let make_line source_line line_tokens =
    let classified_tokens =
      List.map
        (fun source_token ->
          Ast.make_assembly_token
            ~kind:(assembly_token_kind source_token)
            ~source_token
            ~location:(token_location source_token))
        line_tokens
    in
    Ast.make_assembly_line ~source_line
      ~labels:(assembly_labels line_tokens)
      ~tokens:classified_tokens
      ~location:(location_from_expression_tokens line_tokens)
  in
  let finish lines_rev current_line current_tokens_rev =
    match current_line with
    | None -> List.rev lines_rev
    | Some source_line ->
        List.rev
          (make_line source_line (List.rev current_tokens_rev) :: lines_rev)
  in
  let rec collect lines_rev current_source current_line current_tokens_rev =
    function
    | [] -> finish lines_rev current_line current_tokens_rev
    | token :: remaining ->
        let source = token.Token.span.source in
        let source_line = source_line_of_token cursor token in
        if
          Option.equal Common.Source_id.equal current_source (Some source)
          && current_line = Some source_line
        then
          collect lines_rev current_source current_line
            (token :: current_tokens_rev)
            remaining
        else
          let lines_rev =
            match current_line with
            | None -> lines_rev
            | Some line ->
                make_line line (List.rev current_tokens_rev) :: lines_rev
          in
          collect lines_rev (Some source) (Some source_line) [ token ] remaining
  in
  collect [] None None [] tokens

let classified_assembly_token source_token =
  Ast.make_assembly_token
    ~kind:(assembly_token_kind source_token)
    ~source_token
    ~location:(token_location source_token)

let resolve_visible_assembly_register cursor token =
  match token.Token.kind with
  | Token_kind.Identifier -> (
      match
        Symbol_visibility.Environment.find_preprocessor cursor.symbols token.raw
      with
      | Symbol_visibility.Present entry
        when Symbol_visibility.kind entry = Symbol_visibility.Register ->
          Asm.Register.find token.raw
      | Symbol_visibility.Absent
      | Symbol_visibility.Shadowed_by_local
      | Symbol_visibility.Present _ -> None)
  | _ -> None

let inline_assembly_size_spellings =
  [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ]

let token_is_visible_inline_assembly_size cursor token =
  List.exists (String.equal token.Token.raw) inline_assembly_size_spellings
  &&
  match
    Symbol_visibility.Environment.find_preprocessor cursor.symbols token.raw
  with
  | Symbol_visibility.Present entry -> (
      match Symbol_visibility.kind entry with
      | Symbol_visibility.Class
      | Symbol_visibility.Internal_type
      | Symbol_visibility.Assembly_keyword -> true
      | Symbol_visibility.Export_system_symbol
      | Symbol_visibility.Import_system_symbol
      | Symbol_visibility.Definition
      | Symbol_visibility.Global_variable
      | Symbol_visibility.Function
      | Symbol_visibility.Word
      | Symbol_visibility.Dictionary_word
      | Symbol_visibility.Keyword
      | Symbol_visibility.Opcode
      | Symbol_visibility.Register
      | Symbol_visibility.File
      | Symbol_visibility.Module
      | Symbol_visibility.Help_file
      | Symbol_visibility.Frame_pointer -> false)
  | Symbol_visibility.Absent | Symbol_visibility.Shadowed_by_local -> false

let token_cannot_start_inline_assembly_operand cursor token =
  match token.Token.kind with
  | Token_kind.Eof | Token_kind.Punctuation (',' | ';' | '}' | ')') -> true
  | Token_kind.Identifier -> token_starts_inline_assembly cursor token
  | Token_kind.Keyword _
  | Token_kind.Integer
  | Token_kind.Float
  | Token_kind.String
  | Token_kind.Inserted_binary
  | Token_kind.Inserted_binary_size
  | Token_kind.Character
  | Token_kind.Operator _
  | Token_kind.Punctuation _
  | Token_kind.Newline -> false

let parse_inline_assembly_bracket cursor opcode_item operand_index =
  let malformed item message =
    report cursor item ~code:"HCPARSE0151"
      ~message:
        (Printf.sprintf "%s in operand %d of inline assembly opcode %S" message
           operand_index opcode_item.token.raw);
    None
  in
  let rec collect expected_closers tokens_rev =
    let item = peek cursor in
    match item.token.Token.kind with
    | Token_kind.Eof | Token_kind.Punctuation (';' | '}') ->
        let expected =
          match expected_closers with
          | closer :: _ ->
              Printf.sprintf "expected %C before the statement boundary" closer
          | [] -> "expected an address expression before the statement boundary"
        in
        malformed item expected
    | Token_kind.Punctuation '[' ->
        let item = take cursor in
        collect (']' :: expected_closers) (item.token :: tokens_rev)
    | Token_kind.Punctuation '(' ->
        let item = take cursor in
        collect (')' :: expected_closers) (item.token :: tokens_rev)
    | Token_kind.Punctuation ((']' | ')') as closer) -> (
        match expected_closers with
        | expected :: remaining when Char.equal closer expected ->
            let item = take cursor in
            let tokens_rev = item.token :: tokens_rev in
            if remaining = [] then Some (List.rev tokens_rev)
            else collect remaining tokens_rev
        | expected :: _ ->
            malformed item
              (Printf.sprintf "found %C while waiting for %C" closer expected)
        | [] -> malformed item (Printf.sprintf "found unmatched %C" closer))
    | _ ->
        let item = take cursor in
        collect expected_closers (item.token :: tokens_rev)
  in
  collect [] []

let parse_inline_assembly_operand cursor opcode_item operand_index =
  let missing item =
    report cursor item ~code:"HCPARSE0149"
      ~message:
        (Printf.sprintf "expected operand %d for inline assembly opcode %S"
           operand_index opcode_item.token.raw);
    None
  in
  let make_operand kind size_prefix segment_prefix tokens =
    let classified_tokens = List.map classified_assembly_token tokens in
    let node =
      Ast.make_inline_assembly_operand ~kind
        ~size_prefix:(Option.map classified_assembly_token size_prefix)
        ~segment_prefix:(Option.map classified_assembly_token segment_prefix)
        ~tokens:classified_tokens
        ~location:(location_from_expression_tokens tokens)
    in
    Some ({ node; tokens } : parsed_inline_assembly_operand)
  in
  let rec parse_prefix prefix_tokens size_prefix segment_prefix =
    let item = peek cursor in
    if token_cannot_start_inline_assembly_operand cursor item.token then
      missing item
    else if token_is_visible_inline_assembly_size cursor item.token then
      let item = take cursor in
      parse_prefix
        (prefix_tokens @ [ item.token ])
        (Some item.token) segment_prefix
    else
      match resolve_visible_assembly_register cursor item.token with
      | Some register
        when Asm.Register.kind register = Asm.Register.Segment
             && (peek_n cursor 1).token.kind = Token_kind.Punctuation ':' ->
          let register_item = take cursor in
          let colon_item = take cursor in
          parse_prefix
            (prefix_tokens @ [ register_item.token; colon_item.token ])
            size_prefix (Some register_item.token)
      | Some _ ->
          let register_item = take cursor in
          make_operand Ast.Inline_assembly_register_operand size_prefix
            segment_prefix
            (prefix_tokens @ [ register_item.token ])
      | None -> (
          match item.token.kind with
          | Token_kind.Punctuation '[' -> (
              match
                parse_inline_assembly_bracket cursor opcode_item operand_index
              with
              | None -> None
              | Some bracket_tokens ->
                  make_operand Ast.Inline_assembly_memory_operand size_prefix
                    segment_prefix
                    (prefix_tokens @ bracket_tokens))
          | _ -> (
              match
                parse_expression ~allow_parenthesis_free_call:false cursor
                  ~context:Inline_assembly_operand_expression ~depth:0
                  ~minimum_binding_power:0
              with
              | None -> None
              | Some expression ->
                  let kind =
                    if
                      List.exists
                        (fun token ->
                          token.Token.kind = Token_kind.Punctuation '[')
                        expression.tokens
                    then Ast.Inline_assembly_memory_operand
                    else Ast.Inline_assembly_immediate_operand
                  in
                  make_operand kind size_prefix segment_prefix
                    (prefix_tokens @ expression.tokens)))
  in
  parse_prefix [] None None

let parse_assembly_block_statement cursor ~boundary : parsed_statement option =
  let keyword_item = take cursor in
  let opening_item = peek cursor in
  if opening_item.token.kind <> Token_kind.Punctuation '{' then (
    report cursor opening_item ~code:"HCPARSE0145"
      ~message:
        (Printf.sprintf "expected '{' after 'asm', but found %s"
           (token_description opening_item.token));
    recover_statement cursor ~boundary;
    None)
  else
    let opening_item = take cursor in
    let rec take_body body_tokens_rev =
      let item = peek cursor in
      match item.token.kind with
      | Token_kind.Eof ->
          report cursor item ~code:"HCPARSE0146"
            ~message:"expected '}' to close the assembly block";
          None
      | Token_kind.Punctuation '}' ->
          let closing_item = take cursor in
          let body_tokens = List.rev body_tokens_rev in
          let tokens =
            (keyword_item.token :: opening_item.token :: body_tokens)
            @ [ closing_item.token ]
          in
          let statement =
            Ast.make_assembly_block_statement
              ~keyword:(token_location keyword_item.token)
              ~opening_brace:(token_location opening_item.token)
              ~lines:(assembly_lines cursor body_tokens)
              ~closing_brace:(token_location closing_item.token)
              ~location:(location_from_expression_tokens tokens)
          in
          Some
            ({ node = Ast.Assembly_block_statement statement; tokens }
              : parsed_statement)
      | _ ->
          let item = take cursor in
          take_body (item.token :: body_tokens_rev)
    in
    take_body []

let parse_inline_assembly_statement cursor ~boundary : parsed_statement option =
  let first_item = peek cursor in
  if Option.is_none cursor.local_context then (
    report cursor first_item ~code:"HCPARSE0147"
      ~message:
        (Printf.sprintf
           "inline assembly item %S requires a function body; use an 'asm { \
            ... }' block at top level"
           first_item.token.raw);
    recover_statement cursor ~boundary;
    None)
  else
    let optional_semicolon () =
      let item = peek cursor in
      if item.token.kind = Token_kind.Punctuation ';' then Some (take cursor)
      else None
    in
    let make_directive directive_item ~kind ~arguments ~separators
        ~semicolon_item =
      let directive_tokens =
        (directive_item.token :: arguments)
        @
        match semicolon_item with
        | None -> []
        | Some semicolon -> [ semicolon.token ]
      in
      let directive =
        Ast.make_inline_assembly_directive ~kind
          ~directive:(classified_assembly_token directive_item.token)
          ~arguments:(List.map classified_assembly_token arguments)
          ~separators
          ~semicolon:
            (Option.map
               (fun semicolon -> token_location semicolon.token)
               semicolon_item)
          ~location:(location_from_expression_tokens directive_tokens)
      in
      Some (Ast.Inline_assembly_directive directive, directive_tokens)
    in
    let parse_operation item resolved =
      let opcode = Asm.Opcode.resolved_opcode resolved in
      let argument_count = Asm.Opcode.first_form_argument_count opcode in
      let opcode_item = take cursor in
      let parse_operand operand_index =
        parse_inline_assembly_operand cursor opcode_item operand_index
      in
      let parsed_operands =
        match argument_count with
        | 0 -> Some ([], None, [])
        | 1 ->
            Option.map
              (fun (operand : parsed_inline_assembly_operand) ->
                ([ operand.node ], None, operand.tokens))
              (parse_operand 1)
        | 2 -> (
            match parse_operand 1 with
            | None -> None
            | Some first_operand ->
                let separator_item = peek cursor in
                if separator_item.token.kind <> Token_kind.Punctuation ',' then (
                  report cursor separator_item ~code:"HCPARSE0150"
                    ~message:
                      (Printf.sprintf
                         "expected ',' between the two operands of inline \
                          assembly opcode %S, but found %s"
                         item.token.raw
                         (token_description separator_item.token));
                  None)
                else
                  let separator_item = take cursor in
                  Option.map
                    (fun (second_operand : parsed_inline_assembly_operand) ->
                      ( [ first_operand.node; second_operand.node ],
                        Some separator_item,
                        first_operand.tokens
                        @ (separator_item.token :: second_operand.tokens) ))
                    (parse_operand 2))
        | count ->
            invalid_arg
              (Printf.sprintf
                 "checked opcode %S has unsupported first-form arity %d"
                 item.token.raw count)
      in
      match parsed_operands with
      | None -> None
      | Some (operands, separator_item, operand_tokens) ->
          let following_item = peek cursor in
          if following_item.token.kind = Token_kind.Punctuation ',' then (
            report cursor following_item ~code:"HCPARSE0152"
              ~message:
                (Printf.sprintf
                   "inline assembly opcode %S has %d operand%s in its first \
                    checked form; another comma starts an extra operand"
                   item.token.raw argument_count
                   (if argument_count = 1 then "" else "s"));
            None)
          else
            let semicolon_item = optional_semicolon () in
            let operation_tokens =
              [ opcode_item.token ] @ operand_tokens
              @
              match semicolon_item with
              | None -> []
              | Some semicolon -> [ semicolon.token ]
            in
            let operation =
              Ast.make_inline_assembly_operation
                ~opcode:(classified_assembly_token opcode_item.token)
                ~operands
                ~separator:
                  (Option.map
                     (fun separator -> token_location separator.token)
                     separator_item)
                ~semicolon:
                  (Option.map
                     (fun semicolon -> token_location semicolon.token)
                     semicolon_item)
                ~location:(location_from_expression_tokens operation_tokens)
            in
            Some (Ast.Inline_assembly_instruction operation, operation_tokens)
    in
    let parse_import directive_item =
      let rec collect arguments_rev separators_rev =
        let item = peek cursor in
        match item.token.kind with
        | Token_kind.Punctuation ';' ->
            let semicolon_item = take cursor in
            make_directive directive_item
              ~kind:Ast.Inline_assembly_import_directive
              ~arguments:(List.rev arguments_rev)
              ~separators:(List.rev separators_rev)
              ~semicolon_item:(Some semicolon_item)
        | Token_kind.Identifier ->
            let name_item = take cursor in
            let arguments_rev = name_item.token :: arguments_rev in
            let comma_item = peek cursor in
            if comma_item.token.kind = Token_kind.Punctuation ',' then
              let comma_item = take cursor in
              collect
                (comma_item.token :: arguments_rev)
                (token_location comma_item.token :: separators_rev)
            else collect arguments_rev separators_rev
        | Token_kind.Eof | Token_kind.Punctuation '}' ->
            report cursor item ~code:"HCPARSE0154"
              ~message:
                (Printf.sprintf
                   "expected ';' to finish inline assembly IMPORT, but found %s"
                   (token_description item.token));
            None
        | _ ->
            report cursor item ~code:"HCPARSE0154"
              ~message:
                (Printf.sprintf
                   "expected an imported symbol name or ';' after inline \
                    assembly IMPORT, but found %s"
                   (token_description item.token));
            None
      in
      collect [] []
    in
    let parse_data directive_item element_width_bytes =
      let dup_directive token =
        match resolve_visible_assembly_directive cursor token with
        | Some directive -> Asm.Directive.templeos_id directive = 81
        | None -> false
      in
      let parse_dup_suffix value_tokens =
        let dup_item = peek cursor in
        if not (dup_directive dup_item.token) then Some value_tokens
        else
          let dup_item = take cursor in
          let opening_item = peek cursor in
          if opening_item.token.kind <> Token_kind.Punctuation '(' then (
            report cursor opening_item ~code:"HCPARSE0155"
              ~message:
                (Printf.sprintf
                   "expected '(' after DUP in inline assembly %S data, but \
                    found %s"
                   directive_item.token.raw
                   (token_description opening_item.token));
            None)
          else
            let opening_item = take cursor in
            let count_item = peek cursor in
            if count_item.token.kind = Token_kind.Punctuation ')' then (
              report cursor count_item ~code:"HCPARSE0155"
                ~message:
                  (Printf.sprintf
                     "expected a repeated value inside DUP(...) for inline \
                      assembly %S data"
                     directive_item.token.raw);
              None)
            else
              match
                parse_expression ~allow_parenthesis_free_call:false cursor
                  ~context:Inline_assembly_operand_expression ~depth:0
                  ~minimum_binding_power:0
              with
              | None -> None
              | Some count_expression ->
                  let closing_item = peek cursor in
                  if closing_item.token.kind <> Token_kind.Punctuation ')' then (
                    report cursor closing_item ~code:"HCPARSE0155"
                      ~message:
                        (Printf.sprintf
                           "expected ')' after the DUP value for inline \
                            assembly %S data, but found %s"
                           directive_item.token.raw
                           (token_description closing_item.token));
                    None)
                  else
                    let closing_item = take cursor in
                    Some
                      (value_tokens
                      @ dup_item.token :: opening_item.token
                        :: count_expression.tokens
                      @ [ closing_item.token ])
      in
      let parse_value () =
        let item = peek cursor in
        match item.token.kind with
        | Token_kind.String ->
            let item = take cursor in
            Some [ item.token ]
        | Token_kind.Eof | Token_kind.Punctuation (',' | ';' | '}') ->
            report cursor item ~code:"HCPARSE0155"
              ~message:
                (Printf.sprintf
                   "expected a value in inline assembly %S data, but found %s"
                   directive_item.token.raw
                   (token_description item.token));
            None
        | _ when token_starts_inline_assembly cursor item.token ->
            report cursor item ~code:"HCPARSE0155"
              ~message:
                (Printf.sprintf
                   "expected ';' before inline assembly item %S after %S data"
                   item.token.raw directive_item.token.raw);
            None
        | _ -> (
            match
              parse_expression ~allow_parenthesis_free_call:false cursor
                ~context:Inline_assembly_operand_expression ~depth:0
                ~minimum_binding_power:0
            with
            | None -> None
            | Some expression -> parse_dup_suffix expression.tokens)
      in
      let rec collect arguments_rev separators_rev =
        let item = peek cursor in
        match item.token.kind with
        | Token_kind.Punctuation ';' ->
            let semicolon_item = take cursor in
            make_directive directive_item
              ~kind:(Ast.Inline_assembly_data_directive { element_width_bytes })
              ~arguments:(List.rev arguments_rev)
              ~separators:(List.rev separators_rev)
              ~semicolon_item:(Some semicolon_item)
        | Token_kind.Eof | Token_kind.Punctuation '}' ->
            report cursor item ~code:"HCPARSE0155"
              ~message:
                (Printf.sprintf
                   "expected ';' to finish inline assembly %S data, but found \
                    %s"
                   directive_item.token.raw
                   (token_description item.token));
            None
        | _ -> (
            match parse_value () with
            | None -> None
            | Some value_tokens ->
                let arguments_rev =
                  List.rev_append value_tokens arguments_rev
                in
                let comma_item = peek cursor in
                if comma_item.token.kind = Token_kind.Punctuation ',' then
                  let comma_item = take cursor in
                  collect
                    (comma_item.token :: arguments_rev)
                    (token_location comma_item.token :: separators_rev)
                else collect arguments_rev separators_rev)
      in
      collect [] []
    in
    let parse_binfile directive_item =
      let argument_item = peek cursor in
      if argument_item.token.kind <> Token_kind.String then (
        report cursor argument_item ~code:"HCPARSE0156"
          ~message:
            (Printf.sprintf
               "expected a file-name string after inline assembly BINFILE, but \
                found %s"
               (token_description argument_item.token));
        None)
      else
        let argument_item = take cursor in
        let semicolon_item = peek cursor in
        if semicolon_item.token.kind <> Token_kind.Punctuation ';' then (
          report cursor semicolon_item ~code:"HCPARSE0156"
            ~message:
              (Printf.sprintf
                 "expected ';' after the inline assembly BINFILE string, but \
                  found %s"
                 (token_description semicolon_item.token));
          None)
        else
          let semicolon_item = take cursor in
          make_directive directive_item
            ~kind:Ast.Inline_assembly_binfile_directive
            ~arguments:[ argument_item.token ] ~separators:[]
            ~semicolon_item:(Some semicolon_item)
    in
    let parse_directive item directive =
      match direct_inline_assembly_directive_form directive with
      | Direct_inline_import ->
          let directive_item = take cursor in
          parse_import directive_item
      | Direct_inline_data element_width_bytes ->
          let directive_item = take cursor in
          parse_data directive_item element_width_bytes
      | Direct_inline_binfile ->
          let directive_item = take cursor in
          parse_binfile directive_item
      | (Direct_inline_list | Direct_inline_nolist | Direct_inline_use _) as
        form ->
          let directive_item = take cursor in
          let kind =
            match form with
            | Direct_inline_list -> Ast.Inline_assembly_list_directive
            | Direct_inline_nolist -> Ast.Inline_assembly_nolist_directive
            | Direct_inline_use segment_width_bits ->
                Ast.Inline_assembly_use_directive { segment_width_bits }
            | Direct_inline_import
            | Direct_inline_data _
            | Direct_inline_binfile
            | Direct_inline_forbidden_in_function
            | Direct_inline_operand_prefix
            | Direct_inline_invalid_standalone -> assert false
          in
          make_directive directive_item ~kind ~arguments:[] ~separators:[]
            ~semicolon_item:(optional_semicolon ())
      | Direct_inline_forbidden_in_function ->
          report cursor item ~code:"HCPARSE0153"
            ~message:
              (Printf.sprintf
                 "inline assembly directive %S is not allowed in a function \
                  body; use a top-level 'asm { ... }' block"
                 item.token.raw);
          None
      | Direct_inline_operand_prefix ->
          invalid_arg
            "an inline assembly operand prefix cannot start a direct item"
      | Direct_inline_invalid_standalone ->
          report cursor item ~code:"HCPARSE0153"
            ~message:
              (Printf.sprintf
                 "assembler keyword %S cannot start a direct function-body \
                  assembly item"
                 item.token.raw);
          None
    in
    let rec collect items_rev tokens_rev =
      let item = peek cursor in
      match resolve_visible_assembly_opcode cursor item.token with
      | Some resolved -> (
          match parse_operation item resolved with
          | None ->
              recover_statement cursor ~boundary;
              None
          | Some (parsed_item, item_tokens) ->
              collect (parsed_item :: items_rev)
                (List.rev_append item_tokens tokens_rev))
      | None -> (
          match resolve_visible_assembly_directive cursor item.token with
          | Some directive
            when direct_inline_assembly_directive_form directive
                 <> Direct_inline_operand_prefix -> (
              match parse_directive item directive with
              | None ->
                  recover_statement cursor ~boundary;
                  None
              | Some (parsed_item, item_tokens) ->
                  collect (parsed_item :: items_rev)
                    (List.rev_append item_tokens tokens_rev))
          | Some _ | None ->
              let items = List.rev items_rev in
              let tokens = List.rev tokens_rev in
              if items = [] then
                invalid_arg "inline assembly parser needs an assembly item";
              let statement =
                Ast.make_inline_assembly_statement ~items
                  ~location:(location_from_expression_tokens tokens)
              in
              Some
                ({ node = Ast.Inline_assembly_statement statement; tokens }
                  : parsed_statement))
    in
    collect [] []

let rec parse_statement_atom cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let item = peek cursor in
  match item.token.kind with
  | (Token_kind.Identifier | Token_kind.Keyword _)
    when Option.is_some cursor.local_context
         && token_is_named_type cursor item.token ->
      parse_local_declaration cursor ~boundary
  | Token_kind.Keyword _
    when token_is_contextual_identifier_operand cursor item.token ->
      parse_expression_statement cursor ~boundary
  | Token_kind.Identifier when token_starts_inline_assembly cursor item.token ->
      parse_inline_assembly_statement cursor ~boundary
  | Token_kind.Keyword Keyword.Asm ->
      parse_assembly_block_statement cursor ~boundary
  | Token_kind.Punctuation '{' ->
      parse_block_statement cursor ~block_depth ~conditional_depth ~loop_depth
        ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.Break -> parse_break_statement cursor ~boundary
  | Token_kind.Keyword Keyword.Do ->
      parse_do_while_statement cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.For ->
      parse_for_statement cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.Goto -> parse_goto_statement cursor ~boundary
  | Token_kind.Keyword Keyword.If ->
      parse_if_statement cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.Lock ->
      parse_lock_statement cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.No_warn ->
      parse_no_warn_statement cursor ~boundary
  | Token_kind.Keyword Keyword.Static when Option.is_some cursor.local_context
    -> parse_local_declaration cursor ~boundary
  | Token_kind.Keyword (Keyword.Reg | Keyword.Noreg)
    when Option.is_some cursor.local_context ->
      local_declaration_failure cursor ~boundary item ~code:"HCPARSE0099"
        ~message:
          "a local register qualifier must follow its declared type in the \
           pinned parser"
  | Token_kind.Keyword Keyword.Return -> parse_return_statement cursor ~boundary
  | Token_kind.Keyword Keyword.Switch ->
      parse_switch_statement cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.Try ->
      parse_try_catch_statement cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.While ->
      parse_while_statement cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.Catch ->
      report cursor item ~code:"HCPARSE0082"
        ~message:"found 'catch' without a matching 'try'";
      recover_statement cursor ~boundary;
      None
  | Token_kind.Keyword Keyword.Else ->
      report cursor item ~code:"HCPARSE0055"
        ~message:"found 'else' without a matching 'if'";
      recover_statement cursor ~boundary;
      None
  | Token_kind.String | Token_kind.Character ->
      parse_implicit_output_statement cursor ~boundary
  | Token_kind.Punctuation ';' when is_for_update_boundary boundary ->
      report cursor item ~code:"HCPARSE0070"
        ~message:"expected ')' after the for update, but found ';'";
      recover_statement cursor ~boundary;
      None
  | Token_kind.Punctuation ';' -> Some (parse_empty_statement cursor)
  | Token_kind.Identifier when token_starts_function_label cursor item.token ->
      parse_label_statement cursor
  | Token_kind.Identifier
    when Option.is_some cursor.local_context
         && (Option.is_some (primitive_type_of_token item.token)
            || Option.is_some (internal_type_of_token cursor item.token)
            || token_is_named_type cursor item.token) ->
      parse_local_declaration cursor ~boundary
  | _ when token_starts_statement_expression cursor item.token ->
      parse_expression_statement cursor ~boundary
  | _ ->
      let code, message =
        match item.token.kind with
        | Token_kind.Punctuation '}' when boundary = Top_level_boundary ->
            ("HCPARSE0050", "found '}' without a matching '{'")
        | Token_kind.Punctuation '}' ->
            ( "HCPARSE0048",
              "expected another statement after ',', but found '}'" )
        | Token_kind.Identifier
          when not
                 (statement_symbol_is_expression cursor (token_text item.token))
          ->
            ( "HCPARSE0048",
              Printf.sprintf
                "label or declaration syntax for unresolved identifier %S \
                 after a statement comma is not implemented"
                (token_text item.token) )
        | _ when token_starts_global_declaration cursor item.token ->
            ( "HCPARSE0048",
              "a declaration after a statement comma is not implemented" )
        | _ ->
            ( "HCPARSE0048",
              Printf.sprintf
                "statement form beginning with %s is not implemented"
                (token_description item.token) )
      in
      report cursor item ~code ~message;
      recover_statement cursor ~boundary;
      None

and parse_do_while_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let do_item = peek cursor in
  if loop_depth >= max_loop_depth then (
    report cursor do_item ~code:"HCPARSE0061"
      ~message:
        (Printf.sprintf "loop-statement nesting exceeds the hosted limit of %d"
           max_loop_depth);
    recover_statement cursor ~boundary;
    None)
  else
    let do_item = take cursor in
    match
      parse_required_statement cursor
        ~boundary:(statement_body_boundary boundary)
        ~block_depth ~conditional_depth ~loop_depth:(loop_depth + 1) ~lock_depth
        ~try_depth ~switch_depth ~code:"HCPARSE0062"
        ~description:"a statement after 'do'"
    with
    | None -> None
    | Some body -> (
        let while_item = peek cursor in
        if while_item.token.kind <> Token_kind.Keyword Keyword.While then (
          report cursor while_item ~code:"HCPARSE0063"
            ~message:
              (Printf.sprintf
                 "expected 'while' after the do-while body, but found %s"
                 (token_description while_item.token));
          recover_statement cursor ~boundary;
          None)
        else
          let while_item = take cursor in
          let opening_item = peek cursor in
          if opening_item.token.kind <> Token_kind.Punctuation '(' then (
            report cursor opening_item ~code:"HCPARSE0064"
              ~message:
                (Printf.sprintf
                   "expected '(' after the do-while keyword, but found %s"
                   (token_description opening_item.token));
            recover_statement cursor ~boundary;
            None)
          else
            let opening_item = take cursor in
            match
              parse_expression cursor ~context:Do_while_condition_expression
                ~depth:0 ~minimum_binding_power:0
            with
            | None ->
                recover_statement cursor ~boundary;
                None
            | Some (condition : parsed_expression) ->
                let closing_item = peek cursor in
                if closing_item.token.kind <> Token_kind.Punctuation ')' then (
                  report cursor closing_item ~code:"HCPARSE0065"
                    ~message:
                      (Printf.sprintf
                         "expected ')' after the do-while condition, but found \
                          %s"
                         (token_description closing_item.token));
                  recover_statement cursor ~boundary;
                  None)
                else
                  let closing_item = take cursor in
                  let semicolon_item = peek cursor in
                  if semicolon_item.token.kind <> Token_kind.Punctuation ';'
                  then (
                    report cursor semicolon_item ~code:"HCPARSE0066"
                      ~message:
                        (Printf.sprintf
                           "expected ';' after the do-while condition, but \
                            found %s"
                           (token_description semicolon_item.token));
                    recover_statement cursor ~boundary;
                    None)
                  else
                    let semicolon_item = take cursor in
                    let tokens =
                      (do_item.token :: body.tokens)
                      @ while_item.token :: opening_item.token
                        :: condition.tokens
                      @ [ closing_item.token; semicolon_item.token ]
                    in
                    let statement =
                      Ast.make_do_while_statement
                        ~do_keyword:(token_location do_item.token)
                        ~body:body.node
                        ~while_keyword:(token_location while_item.token)
                        ~opening_parenthesis:(token_location opening_item.token)
                        ~condition:condition.node
                        ~closing_parenthesis:(token_location closing_item.token)
                        ~semicolon:(token_location semicolon_item.token)
                        ~location:(location_from_expression_tokens tokens)
                    in
                    Some { node = Ast.Do_while_statement statement; tokens })

and parse_for_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let keyword_item = peek cursor in
  if loop_depth >= max_loop_depth then (
    report cursor keyword_item ~code:"HCPARSE0061"
      ~message:
        (Printf.sprintf "loop-statement nesting exceeds the hosted limit of %d"
           max_loop_depth);
    recover_statement cursor ~boundary;
    None)
  else
    let keyword_item = take cursor in
    let statement_boundary = statement_body_boundary boundary in
    let opening_item = peek cursor in
    if opening_item.token.kind <> Token_kind.Punctuation '(' then (
      report cursor opening_item ~code:"HCPARSE0067"
        ~message:
          (Printf.sprintf "expected '(' after 'for', but found %s"
             (token_description opening_item.token));
      recover_statement cursor ~boundary;
      None)
    else
      let opening_item = take cursor in
      let initialization_item = peek cursor in
      if initialization_item.token.kind = Token_kind.Punctuation ')' then (
        report cursor initialization_item ~code:"HCPARSE0068"
          ~message:
            "expected an initializer statement in the for header, but found ')'";
        recover_for_header cursor ~boundary:statement_boundary;
        None)
      else
        match
          parse_required_statement cursor ~boundary:statement_boundary
            ~block_depth ~conditional_depth ~loop_depth:(loop_depth + 1)
            ~lock_depth ~try_depth ~switch_depth ~code:"HCPARSE0068"
            ~description:"an initializer statement in the for header"
        with
        | None ->
            recover_for_header cursor ~boundary:statement_boundary;
            None
        | Some initialization -> (
            match
              parse_expression cursor ~context:For_condition_expression ~depth:0
                ~minimum_binding_power:0
            with
            | None ->
                recover_for_header cursor ~boundary:statement_boundary;
                None
            | Some (condition : parsed_expression) -> (
                let condition_semicolon_item = peek cursor in
                if
                  condition_semicolon_item.token.kind
                  <> Token_kind.Punctuation ';'
                then (
                  report cursor condition_semicolon_item ~code:"HCPARSE0069"
                    ~message:
                      (Printf.sprintf
                         "expected ';' after the for condition, but found %s"
                         (token_description condition_semicolon_item.token));
                  recover_for_header cursor ~boundary:statement_boundary;
                  None)
                else
                  let condition_semicolon_item = take cursor in
                  let update_item = peek cursor in
                  let parsed_update =
                    if update_item.token.kind = Token_kind.Punctuation ')' then
                      Some None
                    else
                      Option.map
                        (fun update -> Some update)
                        (parse_required_statement cursor
                           ~boundary:(For_update_boundary statement_boundary)
                           ~block_depth ~conditional_depth
                           ~loop_depth:(loop_depth + 1) ~lock_depth ~try_depth
                           ~switch_depth ~code:"HCPARSE0070"
                           ~description:"a for update statement")
                  in
                  match parsed_update with
                  | None ->
                      recover_for_header cursor ~boundary:statement_boundary;
                      None
                  | Some update -> (
                      let closing_item = peek cursor in
                      if closing_item.token.kind <> Token_kind.Punctuation ')'
                      then (
                        report cursor closing_item ~code:"HCPARSE0070"
                          ~message:
                            (Printf.sprintf
                               "expected ')' after the for update, but found %s"
                               (token_description closing_item.token));
                        recover_for_header cursor ~boundary:statement_boundary;
                        None)
                      else
                        let closing_item = take cursor in
                        match
                          parse_required_statement cursor
                            ~boundary:statement_boundary ~block_depth
                            ~conditional_depth ~loop_depth:(loop_depth + 1)
                            ~lock_depth ~try_depth ~switch_depth
                            ~code:"HCPARSE0071"
                            ~description:"a statement after the for header"
                        with
                        | None -> None
                        | Some body ->
                            let update_tokens =
                              match update with
                              | None -> []
                              | Some (update : parsed_statement) ->
                                  update.tokens
                            in
                            let tokens =
                              keyword_item.token :: opening_item.token
                              :: initialization.tokens
                              @ condition.tokens
                              @ (condition_semicolon_item.token :: update_tokens)
                              @ (closing_item.token :: body.tokens)
                            in
                            let statement =
                              Ast.make_for_statement
                                ~keyword:(token_location keyword_item.token)
                                ~opening_parenthesis:
                                  (token_location opening_item.token)
                                ~initialization:initialization.node
                                ~condition:condition.node
                                ~condition_semicolon:
                                  (token_location condition_semicolon_item.token)
                                ~update:
                                  (Option.map
                                     (fun (update : parsed_statement) ->
                                       update.node)
                                     update)
                                ~closing_parenthesis:
                                  (token_location closing_item.token)
                                ~body:body.node
                                ~location:
                                  (location_from_expression_tokens tokens)
                            in
                            Some { node = Ast.For_statement statement; tokens })
                ))

and parse_if_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let keyword_item = peek cursor in
  if conditional_depth >= max_conditional_depth then (
    report cursor keyword_item ~code:"HCPARSE0057"
      ~message:
        (Printf.sprintf "if-statement nesting exceeds the hosted limit of %d"
           max_conditional_depth);
    recover_statement cursor ~boundary;
    None)
  else
    let keyword_item = take cursor in
    let opening_item = peek cursor in
    if opening_item.token.kind <> Token_kind.Punctuation '(' then (
      report cursor opening_item ~code:"HCPARSE0052"
        ~message:
          (Printf.sprintf "expected '(' after 'if', but found %s"
             (token_description opening_item.token));
      recover_statement cursor ~boundary;
      None)
    else
      let opening_item = take cursor in
      match
        parse_expression cursor ~context:If_condition_expression ~depth:0
          ~minimum_binding_power:0
      with
      | None ->
          recover_statement cursor ~boundary;
          None
      | Some (condition : parsed_expression) -> (
          let closing_item = peek cursor in
          if closing_item.token.kind <> Token_kind.Punctuation ')' then (
            report cursor closing_item ~code:"HCPARSE0053"
              ~message:
                (Printf.sprintf
                   "expected ')' after the if condition, but found %s"
                   (token_description closing_item.token));
            recover_statement cursor ~boundary;
            None)
          else
            let closing_item = take cursor in
            let branch_depth = conditional_depth + 1 in
            match
              parse_required_statement cursor
                ~boundary:(statement_body_boundary boundary)
                ~block_depth ~conditional_depth:branch_depth ~loop_depth
                ~lock_depth ~try_depth ~switch_depth ~code:"HCPARSE0054"
                ~description:"a statement after the if condition"
            with
            | None -> None
            | Some then_branch -> (
                let else_clause =
                  let else_item = peek cursor in
                  match else_item.token.kind with
                  | Token_kind.Keyword Keyword.Else ->
                      let else_item = take cursor in
                      Option.map
                        (fun (else_branch : parsed_statement) ->
                          let tokens = else_item.token :: else_branch.tokens in
                          let node =
                            Ast.make_else_clause
                              ~keyword:(token_location else_item.token)
                              ~branch:else_branch.node
                              ~location:(location_from_expression_tokens tokens)
                          in
                          (Some node, tokens))
                        (parse_required_statement cursor
                           ~boundary:(statement_body_boundary boundary)
                           ~block_depth ~conditional_depth:branch_depth
                           ~loop_depth ~lock_depth ~try_depth ~switch_depth
                           ~code:"HCPARSE0056"
                           ~description:"a statement after 'else'")
                  | _ -> Some (None, [])
                in
                match else_clause with
                | None -> None
                | Some (else_clause, else_tokens) ->
                    let tokens =
                      keyword_item.token :: opening_item.token
                      :: condition.tokens
                      @ (closing_item.token :: then_branch.tokens)
                      @ else_tokens
                    in
                    let statement =
                      Ast.make_if_statement
                        ~keyword:(token_location keyword_item.token)
                        ~opening_parenthesis:(token_location opening_item.token)
                        ~condition:condition.node
                        ~closing_parenthesis:(token_location closing_item.token)
                        ~then_branch:then_branch.node ~else_clause
                        ~location:(location_from_expression_tokens tokens)
                    in
                    Some { node = Ast.If_statement statement; tokens }))

and parse_lock_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let keyword_item = peek cursor in
  if lock_depth >= max_lock_depth then (
    report cursor keyword_item ~code:"HCPARSE0078"
      ~message:
        (Printf.sprintf "lock-statement nesting exceeds the hosted limit of %d"
           max_lock_depth);
    recover_statement cursor ~boundary;
    None)
  else
    let keyword_item = take cursor in
    match
      parse_required_statement cursor
        ~boundary:(statement_body_boundary boundary)
        ~block_depth ~conditional_depth ~loop_depth ~lock_depth:(lock_depth + 1)
        ~try_depth ~switch_depth ~code:"HCPARSE0077"
        ~description:"a statement after 'lock'"
    with
    | None -> None
    | Some body ->
        let tokens = keyword_item.token :: body.tokens in
        let statement =
          Ast.make_lock_statement
            ~keyword:(token_location keyword_item.token)
            ~body:body.node
            ~location:(location_from_expression_tokens tokens)
        in
        Some { node = Ast.Lock_statement statement; tokens }

and parse_switch_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let keyword_item = peek cursor in
  if switch_depth >= max_switch_depth then (
    report cursor keyword_item ~code:"HCPARSE0084"
      ~message:
        (Printf.sprintf
           "switch-statement nesting exceeds the hosted limit of %d"
           max_switch_depth);
    ignore (take cursor);
    recover_switch_tail cursor ~boundary;
    None)
  else
    let keyword_item = take cursor in
    let opening_item = peek cursor in
    let delimiter =
      match opening_item.token.kind with
      | Token_kind.Punctuation '(' ->
          Some (Ast.Bounded_switch, Token_kind.Punctuation ')')
      | Token_kind.Punctuation '[' ->
          Some (Ast.No_bound_switch, Token_kind.Punctuation ']')
      | _ -> None
    in
    match delimiter with
    | None ->
        report cursor opening_item ~code:"HCPARSE0085"
          ~message:
            (Printf.sprintf "expected '(' or '[' after 'switch', but found %s"
               (token_description opening_item.token));
        recover_switch_tail cursor ~boundary;
        None
    | Some (mode, closing_kind) -> (
        let opening_item = take cursor in
        match
          parse_expression cursor ~context:Switch_expression ~depth:0
            ~minimum_binding_power:0
        with
        | None ->
            recover_switch_tail cursor ~boundary;
            None
        | Some (expression : parsed_expression) -> (
            let closing_item = peek cursor in
            if closing_item.token.kind <> closing_kind then (
              report cursor closing_item ~code:"HCPARSE0086"
                ~message:
                  (Printf.sprintf
                     "expected %S after the switch expression, but found %s"
                     (match mode with
                     | Ast.Bounded_switch -> ")"
                     | Ast.No_bound_switch -> "]")
                     (token_description closing_item.token));
              recover_switch_tail cursor ~boundary;
              None)
            else
              let closing_item = take cursor in
              let opening_brace_item = peek cursor in
              if opening_brace_item.token.kind <> Token_kind.Punctuation '{'
              then (
                report cursor opening_brace_item ~code:"HCPARSE0087"
                  ~message:
                    (Printf.sprintf
                       "expected '{' after the switch header, but found %s"
                       (token_description opening_brace_item.token));
                recover_switch_tail cursor ~boundary;
                None)
              else
                let opening_brace_item = take cursor in
                match
                  parse_switch_region cursor ~expect_end:false
                    ~subswitch_depth:0 ~block_depth ~conditional_depth
                    ~loop_depth ~lock_depth ~try_depth
                    ~switch_depth:(switch_depth + 1)
                with
                | None -> None
                | Some region -> (
                    match region.region_end with
                    | Switch_region_end_label _ ->
                        invalid_arg
                          "switch body ended with a sub-switch terminator"
                    | Switch_region_brace closing_brace_item ->
                        if region.region_had_error then None
                        else
                          let tokens =
                            keyword_item.token :: opening_item.token
                            :: expression.tokens
                            @ closing_item.token :: opening_brace_item.token
                              :: region.region_tokens
                          in
                          let statement =
                            Ast.make_switch_statement
                              ~keyword:(token_location keyword_item.token)
                              ~mode
                              ~opening_delimiter:
                                (token_location opening_item.token)
                              ~expression:expression.node
                              ~closing_delimiter:
                                (token_location closing_item.token)
                              ~opening_brace:
                                (token_location opening_brace_item.token)
                              ~elements:region.region_elements
                              ~closing_brace:
                                (token_location closing_brace_item.token)
                              ~location:(location_from_expression_tokens tokens)
                          in
                          Some { node = Ast.Switch_statement statement; tokens }
                    )))

and parse_switch_region cursor ~expect_end ~subswitch_depth ~block_depth
    ~conditional_depth ~loop_depth ~lock_depth ~try_depth ~switch_depth :
    parsed_switch_region option =
  let finish elements_rev tokens_rev end_ ending_tokens had_error =
    Some
      {
        region_elements = List.rev elements_rev;
        region_tokens = List.rev tokens_rev @ ending_tokens;
        region_end = end_;
        region_had_error = had_error;
      }
  in
  let rec collect elements_rev tokens_rev had_error =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Punctuation '}' when expect_end ->
        report cursor item ~code:"HCPARSE0095"
          ~message:"expected 'end:' before the enclosing switch closes";
        None
    | Token_kind.Punctuation '}' ->
        let closing_item = take cursor in
        finish elements_rev tokens_rev (Switch_region_brace closing_item)
          [ closing_item.token ] had_error
    | Token_kind.Eof ->
        report cursor item
          ~code:(if expect_end then "HCPARSE0095" else "HCPARSE0088")
          ~message:
            (if expect_end then "expected 'end:' before the end of input"
             else "expected '}' to close the switch statement");
        None
    | Token_kind.Keyword Keyword.End when expect_end ->
        let end_item = take cursor in
        let colon_item = peek cursor in
        if colon_item.token.kind <> Token_kind.Punctuation ':' then (
          report cursor colon_item ~code:"HCPARSE0096"
            ~message:
              (Printf.sprintf "expected ':' after 'end', but found %s"
                 (token_description colon_item.token));
          recover_statement cursor ~boundary:Switch_boundary;
          None)
        else
          let colon_item = take cursor in
          finish elements_rev tokens_rev
            (Switch_region_end_label (end_item, colon_item))
            [ end_item.token; colon_item.token ]
            had_error
    | Token_kind.Keyword Keyword.End ->
        let end_item = take cursor in
        report cursor end_item ~code:"HCPARSE0094"
          ~message:"found 'end:' without a matching 'start:'";
        let consumed =
          let colon_item = peek cursor in
          if colon_item.token.kind = Token_kind.Punctuation ':' then
            [ end_item.token; (take cursor).token ]
          else [ end_item.token ]
        in
        collect elements_rev (List.rev_append consumed tokens_rev) true
    | Token_kind.Keyword Keyword.Case -> (
        match parse_switch_case_element cursor with
        | Some element ->
            collect
              (element.node :: elements_rev)
              (List.rev_append element.tokens tokens_rev)
              had_error
        | None -> collect elements_rev tokens_rev true)
    | Token_kind.Keyword Keyword.Default -> (
        match parse_switch_default_element cursor with
        | Some element ->
            collect
              (element.node :: elements_rev)
              (List.rev_append element.tokens tokens_rev)
              had_error
        | None -> collect elements_rev tokens_rev true)
    | Token_kind.Keyword Keyword.Start -> (
        match
          parse_switch_subswitch_element cursor ~subswitch_depth ~block_depth
            ~conditional_depth ~loop_depth ~lock_depth ~try_depth ~switch_depth
        with
        | Some element ->
            collect
              (element.node :: elements_rev)
              (List.rev_append element.tokens tokens_rev)
              had_error
        | None -> collect elements_rev tokens_rev true)
    | _ -> (
        match
          parse_statement_sequence cursor ~boundary:Switch_boundary ~block_depth
            ~conditional_depth ~loop_depth ~lock_depth ~try_depth ~switch_depth
        with
        | Some statement ->
            let element = Ast.Switch_statement_element statement.node in
            collect (element :: elements_rev)
              (List.rev_append statement.tokens tokens_rev)
              had_error
        | None -> collect elements_rev tokens_rev true)
  in
  collect [] [] false

and parse_switch_case_element cursor : parsed_switch_element option =
  let keyword_item = take cursor in
  let first_item = peek cursor in
  let parsed_pattern =
    if first_item.token.kind = Token_kind.Punctuation ':' then
      Some (Ast.Implicit_case, [])
    else
      match
        parse_expression cursor ~context:Switch_case_expression ~depth:0
          ~minimum_binding_power:0
      with
      | None -> None
      | Some (start_expression : parsed_expression) -> (
          let ellipsis_item = peek cursor in
          if ellipsis_item.token.kind <> Token_kind.Operator Operator.Ellipsis
          then
            Some (Ast.Single_case start_expression.node, start_expression.tokens)
          else
            let ellipsis_item = take cursor in
            let end_item = peek cursor in
            if
              end_item.token.kind = Token_kind.Punctuation ':'
              || end_item.token.kind = Token_kind.Punctuation '}'
              || end_item.token.kind = Token_kind.Eof
              || token_is_switch_boundary end_item.token
            then (
              report cursor end_item ~code:"HCPARSE0090"
                ~message:
                  (Printf.sprintf
                     "expected an expression after the case range ellipsis, \
                      but found %s"
                     (token_description end_item.token));
              None)
            else
              match
                parse_expression cursor ~context:Switch_case_expression ~depth:0
                  ~minimum_binding_power:0
              with
              | None -> None
              | Some (end_expression : parsed_expression) ->
                  let range_tokens =
                    start_expression.tokens
                    @ (ellipsis_item.token :: end_expression.tokens)
                  in
                  let range =
                    Ast.make_switch_case_range ~start:start_expression.node
                      ~ellipsis:(token_location ellipsis_item.token)
                      ~end_:end_expression.node
                      ~location:(location_from_expression_tokens range_tokens)
                  in
                  Some (Ast.Ranged_case range, range_tokens))
  in
  match parsed_pattern with
  | None ->
      recover_statement cursor ~boundary:Switch_boundary;
      None
  | Some (pattern, pattern_tokens) ->
      let colon_item = peek cursor in
      if colon_item.token.kind <> Token_kind.Punctuation ':' then (
        report cursor colon_item ~code:"HCPARSE0091"
          ~message:
            (Printf.sprintf "expected ':' after the case label, but found %s"
               (token_description colon_item.token));
        recover_statement cursor ~boundary:Switch_boundary;
        None)
      else
        let colon_item = take cursor in
        let tokens =
          (keyword_item.token :: pattern_tokens) @ [ colon_item.token ]
        in
        let label =
          Ast.make_switch_case_label
            ~keyword:(token_location keyword_item.token)
            ~pattern
            ~colon:(token_location colon_item.token)
            ~location:(location_from_expression_tokens tokens)
        in
        Some { node = Ast.Switch_case_element label; tokens }

and parse_switch_default_element cursor : parsed_switch_element option =
  let keyword_item = take cursor in
  let colon_item = peek cursor in
  if colon_item.token.kind <> Token_kind.Punctuation ':' then (
    report cursor colon_item ~code:"HCPARSE0092"
      ~message:
        (Printf.sprintf "expected ':' after 'default', but found %s"
           (token_description colon_item.token));
    recover_statement cursor ~boundary:Switch_boundary;
    None)
  else
    let colon_item = take cursor in
    let tokens = [ keyword_item.token; colon_item.token ] in
    let label =
      Ast.make_switch_default_label
        ~keyword:(token_location keyword_item.token)
        ~colon:(token_location colon_item.token)
        ~location:(location_from_expression_tokens tokens)
    in
    Some { node = Ast.Switch_default_element label; tokens }

and parse_switch_subswitch_element cursor ~subswitch_depth ~block_depth
    ~conditional_depth ~loop_depth ~lock_depth ~try_depth ~switch_depth :
    parsed_switch_element option =
  let start_item = peek cursor in
  if subswitch_depth >= max_switch_depth then (
    report cursor start_item ~code:"HCPARSE0097"
      ~message:
        (Printf.sprintf "sub-switch nesting exceeds the hosted limit of %d"
           max_switch_depth);
    ignore (take cursor);
    let rec skip nested =
      let item = peek cursor in
      match item.token.kind with
      | Token_kind.Eof | Token_kind.Punctuation '}' -> ()
      | Token_kind.Keyword Keyword.Start ->
          ignore (take cursor);
          skip (nested + 1)
      | Token_kind.Keyword Keyword.End ->
          ignore (take cursor);
          if (peek cursor).token.kind = Token_kind.Punctuation ':' then
            ignore (take cursor);
          if nested > 0 then skip (nested - 1)
      | _ ->
          ignore (take cursor);
          skip nested
    in
    skip 0;
    None)
  else
    let start_item = take cursor in
    let start_colon_item = peek cursor in
    if start_colon_item.token.kind <> Token_kind.Punctuation ':' then (
      report cursor start_colon_item ~code:"HCPARSE0093"
        ~message:
          (Printf.sprintf "expected ':' after 'start', but found %s"
             (token_description start_colon_item.token));
      recover_statement cursor ~boundary:Switch_boundary;
      None)
    else
      let start_colon_item = take cursor in
      match
        parse_switch_region cursor ~expect_end:true
          ~subswitch_depth:(subswitch_depth + 1) ~block_depth ~conditional_depth
          ~loop_depth ~lock_depth ~try_depth ~switch_depth
      with
      | None -> None
      | Some region -> (
          match region.region_end with
          | Switch_region_brace _ ->
              invalid_arg "sub-switch ended with a switch-body brace"
          | Switch_region_end_label (end_item, end_colon_item) ->
              if region.region_had_error then None
              else
                let tokens =
                  start_item.token :: start_colon_item.token
                  :: region.region_tokens
                in
                let subswitch =
                  Ast.make_switch_subswitch
                    ~start_keyword:(token_location start_item.token)
                    ~start_colon:(token_location start_colon_item.token)
                    ~elements:region.region_elements
                    ~end_keyword:(token_location end_item.token)
                    ~end_colon:(token_location end_colon_item.token)
                    ~location:(location_from_expression_tokens tokens)
                in
                Some { node = Ast.Switch_subswitch_element subswitch; tokens })

and parse_try_catch_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let try_item = peek cursor in
  if try_depth >= max_try_depth then (
    report cursor try_item ~code:"HCPARSE0083"
      ~message:
        (Printf.sprintf "try-statement nesting exceeds the hosted limit of %d"
           max_try_depth);
    recover_statement cursor ~boundary;
    None)
  else
    let try_item = take cursor in
    let body_boundary = statement_body_boundary boundary in
    let body_item = peek cursor in
    if body_item.token.kind = Token_kind.Keyword Keyword.Catch then (
      report cursor body_item ~code:"HCPARSE0079"
        ~message:"expected a statement after 'try', but found \"catch\"";
      recover_statement cursor ~boundary;
      None)
    else
      match
        parse_required_statement cursor ~boundary:body_boundary ~block_depth
          ~conditional_depth ~loop_depth ~lock_depth ~try_depth:(try_depth + 1)
          ~switch_depth ~code:"HCPARSE0079"
          ~description:"a statement after 'try'"
      with
      | None -> None
      | Some try_body -> (
          let catch_item = peek cursor in
          if catch_item.token.kind <> Token_kind.Keyword Keyword.Catch then (
            report cursor catch_item ~code:"HCPARSE0080"
              ~message:
                (Printf.sprintf
                   "expected 'catch' after the try body, but found %s"
                   (token_description catch_item.token));
            recover_statement cursor ~boundary;
            None)
          else
            let catch_item = take cursor in
            let handler_item = peek cursor in
            if handler_item.token.kind = Token_kind.Keyword Keyword.Catch then (
              report cursor handler_item ~code:"HCPARSE0081"
                ~message:
                  "expected a statement after 'catch', but found \"catch\"";
              recover_statement cursor ~boundary;
              None)
            else
              match
                parse_required_statement cursor ~boundary:body_boundary
                  ~block_depth ~conditional_depth ~loop_depth ~lock_depth
                  ~try_depth:(try_depth + 1) ~switch_depth ~code:"HCPARSE0081"
                  ~description:"a statement after 'catch'"
              with
              | None -> None
              | Some catch_body ->
                  let tokens =
                    (try_item.token :: try_body.tokens)
                    @ (catch_item.token :: catch_body.tokens)
                  in
                  let statement =
                    Ast.make_try_catch_statement
                      ~try_keyword:(token_location try_item.token)
                      ~try_body:try_body.node
                      ~catch_keyword:(token_location catch_item.token)
                      ~catch_body:catch_body.node
                      ~location:(location_from_expression_tokens tokens)
                  in
                  Some { node = Ast.Try_catch_statement statement; tokens })

and parse_while_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let keyword_item = peek cursor in
  if loop_depth >= max_loop_depth then (
    report cursor keyword_item ~code:"HCPARSE0061"
      ~message:
        (Printf.sprintf "loop-statement nesting exceeds the hosted limit of %d"
           max_loop_depth);
    recover_statement cursor ~boundary;
    None)
  else
    let keyword_item = take cursor in
    let opening_item = peek cursor in
    if opening_item.token.kind <> Token_kind.Punctuation '(' then (
      report cursor opening_item ~code:"HCPARSE0058"
        ~message:
          (Printf.sprintf "expected '(' after 'while', but found %s"
             (token_description opening_item.token));
      recover_statement cursor ~boundary;
      None)
    else
      let opening_item = take cursor in
      match
        parse_expression cursor ~context:While_condition_expression ~depth:0
          ~minimum_binding_power:0
      with
      | None ->
          recover_statement cursor ~boundary;
          None
      | Some (condition : parsed_expression) -> (
          let closing_item = peek cursor in
          if closing_item.token.kind <> Token_kind.Punctuation ')' then (
            report cursor closing_item ~code:"HCPARSE0059"
              ~message:
                (Printf.sprintf
                   "expected ')' after the while condition, but found %s"
                   (token_description closing_item.token));
            recover_statement cursor ~boundary;
            None)
          else
            let closing_item = take cursor in
            match
              parse_required_statement cursor
                ~boundary:(statement_body_boundary boundary)
                ~block_depth ~conditional_depth ~loop_depth:(loop_depth + 1)
                ~lock_depth ~try_depth ~switch_depth ~code:"HCPARSE0060"
                ~description:"a statement after the while condition"
            with
            | None -> None
            | Some body ->
                let tokens =
                  (keyword_item.token :: opening_item.token :: condition.tokens)
                  @ (closing_item.token :: body.tokens)
                in
                let statement =
                  Ast.make_while_statement
                    ~keyword:(token_location keyword_item.token)
                    ~opening_parenthesis:(token_location opening_item.token)
                    ~condition:condition.node
                    ~closing_parenthesis:(token_location closing_item.token)
                    ~body:body.node
                    ~location:(location_from_expression_tokens tokens)
                in
                Some { node = Ast.While_statement statement; tokens })

and parse_required_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth ~code ~description :
    parsed_statement option =
  let first_item = peek cursor in
  let missing =
    match first_item.token.kind with
    | Token_kind.Eof
    | Token_kind.Punctuation '}'
    | Token_kind.Keyword Keyword.Else -> true
    | _
      when statement_boundary_is_switch boundary
           && token_is_switch_boundary first_item.token -> true
    | _ -> false
  in
  if missing then (
    report cursor first_item ~code
      ~message:
        (Printf.sprintf "expected %s, but found %s" description
           (token_description first_item.token));
    recover_statement cursor ~boundary;
    None)
  else
    match
      parse_statement_sequence cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
    with
    | Some ({ node = Ast.Sequence_statement sequence; _ } : parsed_statement)
      when sequence.sequence_elements = [] ->
        report cursor first_item ~code
          ~message:
            (Printf.sprintf "expected %s, but found only statement commas"
               description);
        recover_statement cursor ~boundary;
        None
    | statement -> statement

and parse_block_statement cursor ~block_depth ~conditional_depth ~loop_depth
    ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let opening_item = peek cursor in
  if block_depth >= max_block_depth then (
    report cursor opening_item ~code:"HCPARSE0051"
      ~message:
        (Printf.sprintf
           "compound-statement nesting exceeds the hosted limit of %d"
           max_block_depth);
    recover_compound_statement cursor;
    None)
  else
    let opening_item = take cursor in
    let rec collect statements_rev tokens_rev had_error :
        parsed_statement option =
      let item = peek cursor in
      match item.token.kind with
      | Token_kind.Punctuation '}' ->
          let closing_item = take cursor in
          if had_error then None
          else
            let statements = List.rev statements_rev in
            let tokens =
              (opening_item.token :: List.rev tokens_rev)
              @ [ closing_item.token ]
            in
            let statement =
              Ast.make_block_statement
                ~opening_brace:(token_location opening_item.token)
                ~statements
                ~closing_brace:(token_location closing_item.token)
                ~location:(location_from_expression_tokens tokens)
            in
            Some { node = Ast.Block_statement statement; tokens }
      | Token_kind.Eof ->
          let secondary =
            [
              ({
                 Common.Diagnostic.span = opening_item.token.span;
                 message = "block starts here";
               }
                : Common.Diagnostic.related);
            ]
          in
          report ~secondary cursor item ~code:"HCPARSE0049"
            ~message:"expected '}' to close the compound statement";
          None
      | _ -> (
          match
            parse_statement_sequence cursor ~boundary:Block_boundary
              ~block_depth:(block_depth + 1) ~conditional_depth ~loop_depth
              ~lock_depth ~try_depth ~switch_depth
          with
          | Some statement ->
              collect
                (statement.node :: statements_rev)
                (List.rev_append statement.tokens tokens_rev)
                had_error
          | None -> collect statements_rev tokens_rev true)
    in
    collect [] [] false

and parse_statement_sequence cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let leading_items = take_statement_commas cursor [] in
  let leading_commas =
    List.map (fun item -> token_location item.token) leading_items
  in
  let leading_tokens = List.map (fun item -> item.token) leading_items in
  let rec collect elements_rev tokens_rev =
    let item = peek cursor in
    if item.token.kind = Token_kind.Eof then
      Some (List.rev elements_rev, List.rev tokens_rev)
    else
      match
        parse_statement_atom cursor ~boundary ~block_depth ~conditional_depth
          ~loop_depth ~lock_depth ~try_depth ~switch_depth
      with
      | None -> None
      | Some (statement : parsed_statement) ->
          let comma_items = take_statement_commas cursor [] in
          let following_commas =
            List.map (fun item -> token_location item.token) comma_items
          in
          let comma_tokens = List.map (fun item -> item.token) comma_items in
          let element_tokens = statement.tokens @ comma_tokens in
          let element =
            Ast.make_statement_sequence_element ~statement:statement.node
              ~following_commas
              ~location:(location_from_expression_tokens element_tokens)
          in
          let elements_rev = element :: elements_rev in
          let tokens_rev = List.rev_append element_tokens tokens_rev in
          let local_continues =
            match statement.node with
            | Ast.Local_declaration_statement _ -> true
            | _ -> false
          in
          let reaches_block_close =
            match (peek cursor).token.kind with
            | Token_kind.Punctuation '}' -> true
            | _ -> false
          in
          if comma_items = [] && ((not local_continues) || reaches_block_close)
          then Some (List.rev elements_rev, List.rev tokens_rev)
          else collect elements_rev tokens_rev
  in
  let next_item = peek cursor in
  if leading_items <> [] && next_item.token.kind = Token_kind.Eof then
    let location = location_from_expression_tokens leading_tokens in
    Some
      {
        node =
          Ast.Sequence_statement
            (Ast.make_statement_sequence ~leading_commas ~elements:[] ~location);
        tokens = leading_tokens;
      }
  else
    match collect [] (List.rev leading_tokens) with
    | None -> None
    | Some (elements, tokens) -> (
        let has_following_commas =
          List.exists
            (fun (element : Ast.statement_sequence_element) ->
              element.sequence_following_commas <> [])
            elements
        in
        match (leading_items, has_following_commas, elements) with
        | [], false, [ element ] ->
            Some { node = element.sequence_statement; tokens }
        | _ ->
            let location = location_from_expression_tokens tokens in
            Some
              {
                node =
                  Ast.Sequence_statement
                    (Ast.make_statement_sequence ~leading_commas ~elements
                       ~location);
                tokens;
              })

let parse_function_definition cursor ~modifier_tokens ~modifiers ~type_item
    ~return_type (prefix : parsed_declarator_prefix) =
  let provisional =
    declare_function cursor
      (declaration_header cursor ~modifiers ~binding:None
         ~type_specifier:return_type)
      prefix (peek cursor)
  in
  let opening = take cursor in
  let opening_parenthesis =
    match provisional with
    | Some publication -> publication.function_opening_parenthesis
    | None -> token_location opening.token
  in
  match
    parse_function_parameters
      ?default_owner:
        (Option.map (fun owner -> (owner, ref None, ref [])) provisional)
      cursor [] [] [] ~function_pointer_depth:0
  with
  | None -> None
  | Some parsed_parameters ->
      let header_tokens =
        modifier_tokens
        @ (type_item.token :: prefix.tokens)
        @ (opening.token :: parsed_parameters.tokens)
      in
      if not cursor.stop_on_error then
        publish_function cursor prefix.name parsed_parameters.parameters
          parsed_parameters.variadic;
      let completed_header = ref None in
      let parsed_body =
        with_function_local_context cursor parsed_parameters.parameters
          parsed_parameters.variadic (fun () ->
            let body_item = peek cursor in
            completed_header :=
              complete_function_header cursor body_item provisional
                parsed_parameters;
            match body_item.token.kind with
            | Token_kind.Eof -> Some (None, [])
            | _ ->
                parse_statement_sequence cursor ~boundary:Top_level_boundary
                  ~block_depth:0 ~conditional_depth:0 ~loop_depth:0
                  ~lock_depth:0 ~try_depth:0 ~switch_depth:0
                |> Option.map (fun (body : parsed_statement) ->
                    (Some body.node, body.tokens)))
      in
      Option.map
        (fun (body, body_tokens) ->
          let definition_tokens = header_tokens @ body_tokens in
          let definition =
            Ast.make_function_definition ~modifiers ~return_type
              ~return_pointer_layers:prefix.pointer_layers ~name:prefix.name
              ~opening_parenthesis ~parameters:parsed_parameters.parameters
              ~empty_parameter_entries:parsed_parameters.empty_parameter_entries
              ~variadic:parsed_parameters.variadic
              ~closing_parenthesis:parsed_parameters.closing_parenthesis ~body
              ~location:(location_from_expression_tokens definition_tokens)
          in
          Option.iter
            (fun header ->
              header.header_activity.function_body_active <- Some definition;
              Fun.protect
                ~finally:(fun () ->
                  header.header_activity.function_body_active <- None)
                (fun () ->
                  publish_declaration cursor opening
                    (Function_body_completed (header, definition))))
            !completed_header;
          Ast.Function_definition definition)
        parsed_body

let read_command cursor =
  let item = peek cursor in
  let statement () =
    parse_statement_sequence cursor ~boundary:Top_level_boundary ~block_depth:0
      ~conditional_depth:0 ~loop_depth:0 ~lock_depth:0 ~try_depth:0
      ~switch_depth:0
    |> Option.map (fun (statement : parsed_statement) ->
        Ast.Top_level_statement statement.node)
  in
  match item.token.Token.kind with
  | Token_kind.Identifier
    when token_starts_function_label cursor item.token
         || token_starts_inline_assembly cursor item.token -> statement ()
  | _ when token_starts_global_declaration cursor item.token ->
      parse_global cursor ~parse_function_definition
  | _ -> statement ()

let read_commands ?commands ?stream_opener cursor =
  let span =
    Common.Span.unsafe_make
      ~source:(Common.Source_file.id cursor.source)
      ~start:0
      ~stop:(Common.Source_file.length cursor.source)
  in
  let make_module items =
    Ast.make_module ~source:(Common.Source_file.id cursor.source) ~span ~items
  in
  let accept result =
    match result with
    | Ok () -> true
    | Error diagnostics ->
        cursor.diagnostics_rev <-
          List.rev_append diagnostics cursor.diagnostics_rev;
        if not (has_error diagnostics) then
          cursor.diagnostics_rev <-
            Common.Diagnostic.make ~code:"HCPARSE0161"
              ~severity:Common.Diagnostic.Error ~primary:span
              ~message:"command executor failed without an error diagnostic" ()
            :: cursor.diagnostics_rev;
        false
  in
  let consume_checkpoint event =
    match Option.bind commands (fun sink -> sink.checkpoint) with
    | None -> true
    | Some consume -> accept (consume event)
  in
  let saved_stack = !(cursor.command_stack) in
  let context =
    {
      context_sources = cursor.sources;
      context_source = cursor.source;
      context_environment = cursor.symbols;
      context_mode = cursor.compilation_mode;
      context_parent =
        (match saved_stack with
        | [] -> None
        | parent :: _ -> Some !parent);
      context_accepted_ast = None;
      context_active = true;
      context_event_count = 0;
      context_observation_id = fresh_observation_id ();
      context_stack = cursor.command_stack;
      context_position = None;
    }
  in
  if Option.is_some cursor.call then
    Context_observations.add context_observations context
      { events_rev = []; count = 0 };
  let checkpoint event =
    context.context_event_count <- context.context_event_count + 1;
    if Option.is_some (Option.bind commands (fun sink -> sink.checkpoint)) then
      record_observation context (Command event);
    consume_checkpoint event
  in
  let notify event = if not (checkpoint event) then raise Stop_command in
  let position = ref (Before_first_command context) in
  context.context_position <- Some position;
  cursor.command_stack := position :: saved_stack;
  let succeeded = ref false in
  Fun.protect
    ~finally:(fun () ->
      context.context_active <- false;
      context.context_position <- None;
      cursor.current_command <- None;
      cursor.command_stack := saved_stack;
      if not !succeeded then ignore (checkpoint (Sequence_aborted context)))
    (fun () ->
      notify (Sequence_started context);
      let items_rev = ref [] in
      let completed_rev = ref [] in
      let previous = ref None in
      let pending = ref None in
      let ordinal = ref 0 in
      let finished = ref false in
      while not !finished do
        let item = peek cursor in
        let proceed =
          match commands with
          | None -> true
          | Some commands ->
              (not (has_error cursor.diagnostics_rev))
              &&
              (Option.iter
                 (fun command -> notify (Command_resumed command))
                 !pending;
               pending := None;
               accept (commands.resume ()))
        in
        if not proceed then finished := true
        else
          match (item.token.Token.kind, stream_opener) with
          | Token_kind.Eof, opener ->
              Option.iter
                (fun span ->
                  report cursor item ~code:"HCPARSE0162"
                    ~secondary:
                      [
                        { Common.Diagnostic.span; message = "#exe starts here" };
                      ]
                    ~message:"expected '}' to close the #exe block")
                opener;
              ignore (take cursor);
              finished := true
          | Token_kind.Punctuation '}', Some _ ->
              ignore (take cursor);
              finished := true
          | _ -> (
              let start =
                {
                  command_context = context;
                  command_ordinal = !ordinal;
                  command_predecessor = !previous;
                }
              in
              incr ordinal;
              cursor.current_command <- Some start;
              position := Reading_command start;
              notify (Command_started start);
              match read_command cursor with
              | Some parsed ->
                  items_rev := parsed :: !items_rev;
                  let completed =
                    {
                      command_start = start;
                      command_ast = make_module [ parsed ];
                    }
                  in
                  completed_rev := completed :: !completed_rev;
                  previous := Some completed;
                  pending := Some completed;
                  cursor.current_command <- None;
                  position := Awaiting_resume completed;
                  Option.iter
                    (fun commands ->
                      if
                        has_error cursor.diagnostics_rev
                        ||
                        (notify (Command_completed completed);
                         not (accept (commands.command parsed)))
                      then finished := true)
                    commands
              | None -> if Option.is_some commands then finished := true)
      done;
      let ast = make_module (List.rev !items_rev) in
      if not (has_error cursor.diagnostics_rev) then (
        notify
          (Sequence_completed
             {
               sequence_context = context;
               sequence_commands = List.rev !completed_rev;
               sequence_ast = ast;
             });
        context.context_accepted_ast <- Some ast;
        succeeded := true);
      ast)

let make_cursor ?reference ?call ?implicit_output ?query ?declaration
    ?dimension_count ~command_stack ~stream ~sources ~source ~symbols
    ~compilation_mode ~stop_on_error () =
  if Option.is_some dimension_count && Option.is_none declaration then
    invalid_arg "an array count reader requires a declaration observer";
  {
    command_stack;
    current_command = None;
    stream;
    sources;
    source;
    symbols;
    compilation_mode;
    stop_on_error;
    reference;
    call;
    pending_calls = [];
    implicit_output;
    references = Identifier_table.create 32;
    query;
    declaration;
    dimension_count;
    dimension_counts = Dimension_table.create 16;
    lookahead = [];
    diagnostics_rev = [];
    local_context = None;
    local_publications = [];
  }

let parse_with_stack ~command_stack ?commands ?execute_stream ~sources
    ~definitions ~symbols ~config source =
  let execute_stream =
    Option.map
      (fun enter stream opener ->
        let opening_cursor =
          make_cursor ~command_stack ~stream ~sources ~source ~symbols
            ~stop_on_error:true
            ~compilation_mode:(Preprocessor.Config.compilation_mode config)
            ()
        in
        try
          let opening = take opening_cursor in
          if opening.token.kind <> Token_kind.Punctuation '{' then
            report opening_cursor opening ~code:"HCPARSE0163"
              ~message:"expected '{' after #exe";
          let entered =
            Result.map_error
              (fun diagnostics ->
                List.rev opening_cursor.diagnostics_rev @ diagnostics)
              (enter opener)
          in
          Result.bind entered (fun (execution : stream_execution) ->
              let completed = ref false in
              Fun.protect
                ~finally:(fun () -> if not !completed then execution.abort ())
                (fun () ->
                  Preprocessor.with_environment stream
                    ~definitions:execution.definitions
                    ~symbols:execution.symbols
                    ~compilation_mode:Preprocessor.Jit (fun () ->
                      Symbol_visibility.Environment.without_locals
                        execution.symbols (fun () ->
                          let cursor =
                            make_cursor ~command_stack ~stream ~sources ~source
                              ~symbols:execution.symbols
                              ?reference:execution.commands.reference
                              ?call:execution.commands.call
                              ?implicit_output:
                                execution.commands.implicit_output
                              ?query:execution.commands.query
                              ?declaration:execution.commands.declaration
                              ?dimension_count:
                                execution.commands.dimension_count
                              ~compilation_mode:Preprocessor.Jit
                              ~stop_on_error:true ()
                          in
                          cursor.diagnostics_rev <-
                            opening_cursor.diagnostics_rev;
                          (try
                             ignore
                               (read_commands ~commands:execution.commands
                                  ~stream_opener:opener cursor)
                           with Stop_command -> ());
                          let diagnostics = List.rev cursor.diagnostics_rev in
                          if has_error diagnostics then Error diagnostics
                          else
                            match execution.finish () with
                            | Error errors -> Error (diagnostics @ errors)
                            | Ok generated ->
                                completed := true;
                                Ok { Preprocessor.generated; diagnostics }))))
        with Stop_command -> Error (List.rev opening_cursor.diagnostics_rev))
      execute_stream
  in
  let stream =
    Preprocessor.create ?execute_stream ~sources ~definitions ~symbols ~config
      source
  in
  let cursor =
    make_cursor ~command_stack ~stream ~sources ~source ~symbols
      ?reference:
        (Option.bind commands (fun (commands : command_sink) ->
             commands.reference))
      ?call:
        (Option.bind commands (fun (commands : command_sink) -> commands.call))
      ?implicit_output:
        (Option.bind commands (fun (commands : command_sink) ->
             commands.implicit_output))
      ?query:
        (Option.bind commands (fun (commands : command_sink) -> commands.query))
      ?declaration:
        (Option.bind commands (fun (commands : command_sink) ->
             commands.declaration))
      ?dimension_count:
        (Option.bind commands (fun (commands : command_sink) ->
             commands.dimension_count))
      ~stop_on_error:(Option.is_some commands || Option.is_some execute_stream)
      ~compilation_mode:(Preprocessor.Config.compilation_mode config)
      ()
  in
  let ast =
    try Some (read_commands ?commands cursor) with Stop_command -> None
  in
  let diagnostics = List.rev cursor.diagnostics_rev in
  let ast = if has_error diagnostics then None else ast in
  { ast; diagnostics }

let parse ?commands ?execute_stream ~sources ~definitions ~symbols ~config
    source =
  parse_with_stack ~command_stack:(ref []) ?commands ?execute_stream ~sources
    ~definitions ~symbols ~config source

let parse_suspended suspension ?commands ?execute_stream ~sources ~definitions
    ~symbols ~config source =
  let context = suspension.suspended_context in
  let current =
    match !(context.context_stack) with
    | active :: _ -> active == suspension.suspended_ref
    | [] -> false
  in
  if
    suspension.suspension_consumed
    || (not (context.context_active && current))
    || context.context_sources != sources
    || context.context_environment != symbols
    || context.context_mode <> Preprocessor.Config.compilation_mode config
    || context.context_event_count <> suspension.suspended_events
    || !(suspension.suspended_ref) != suspension.suspended_position
  then Error "nested source requires its original live parser suspension"
  else (
    suspension.suspension_consumed <- true;
    let output =
      parse_with_stack ~command_stack:context.context_stack ?commands
        ?execute_stream ~sources ~definitions ~symbols ~config source
    in
    suspension.suspended_ast <- output.ast;
    Ok output)

let suspension_owns_sequence suspension sequence =
  suspension.suspension_consumed && sequence_accepted sequence
  && Option.fold ~none:false
       ~some:(( == ) sequence.sequence_ast)
       suspension.suspended_ast
  && Option.fold ~none:false
       ~some:(( == ) suspension.suspended_position)
       sequence.sequence_context.context_parent
