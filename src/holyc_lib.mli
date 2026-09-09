module Source_id = Common.Source_id
module Source_file = Common.Source_file
module Source_manager = Common.Source_manager
module Span = Common.Span
module Diagnostic = Common.Diagnostic
module Diagnostic_render = Common.Diagnostic_render
module Session = Driver.Session
module Integer_task = Driver.Integer_task
module Task_declarations = Driver.Task_declarations
module Version = Driver.Version
module Corpus = Driver.Corpus
module Primitive_type = Sema.Primitive_type
module Compiler_option = Sema.Compiler_option
module Function_flag = Sema.Function_flag
module Global_record_flag = Sema.Global_record_flag
module Member_flag = Sema.Member_flag
module Semantic_register_request = Sema.Register_request
module Semantic_symbol = Sema.Symbol
module Semantic_symbol_table = Sema.Symbol_table
module Ir_opcode = Ir.Opcode
module Ir_instruction_sequence = Ir.Instruction_sequence
module Ir_control_flow = Ir.Control_flow
module Ir_block_graph = Ir.Block_graph
module Ir_effects = Ir.Effects
module Ir_x87_stack = Ir.X87_stack

module Ir_integer_globals : sig
  type t = Ir.Integer_globals.t
  type slot = Ir.Integer_globals.slot
  type static_slot = Ir.Integer_globals.static_slot
  type storage_slot = Ir.Integer_globals.storage_slot

  val statics : t -> static_slot list
  val static_frame : static_slot -> Sema.Function_frame_layout.function_layout
  val static_location : static_slot -> Sema.Function_frame_layout.location

  val static_initializer :
    static_slot ->
    Sema.Function_call_expression_result.initializer_result option

  val static_compiler_options : static_slot -> int64
  val static_storage : static_slot -> storage_slot
  val global_storage : slot -> storage_slot
  val storage_slots : t -> storage_slot list
  val storage_index : storage_slot -> int
  val storage_symbol : storage_slot -> Sema.Symbol.t
  val storage_type : storage_slot -> Sema.Type.t
  val storage_opcode : storage_slot -> Ir.Opcode.t
  val storage_initial_bits : storage_slot -> int64 option
  val storage_preparation_steps : storage_slot -> int

  val storage_frame :
    storage_slot -> Sema.Function_frame_layout.function_layout option

  val find_static : t -> Sema.Symbol.t -> static_slot option
  val find_storage : t -> Sema.Symbol.t -> storage_slot option
  val has_initializers : t -> bool
  val has_unprepared_statics : t -> bool

  val create :
    ?initializers:Sema.Function_call_expression_result.top_level_t ->
    span:Common.Span.t ->
    Sema.Global_record_classification.t ->
    (t, Common.Diagnostic.t list) result

  val slots : t -> slot list

  val byte_size : t -> int
  (** Global declared widths plus eight-byte-padded static allocations,
      including unused declarations. Padding is not accessible object extent.
      The hosted quota excludes inter-object AOT alignment gaps and host
      bookkeeping. *)

  val find : t -> Sema.Symbol.t -> slot option
  val slot_index : slot -> int
  val slot_symbol : slot -> Sema.Symbol.t
  val slot_type : slot -> Sema.Type.t
  val slot_record : slot -> Sema.Global_record_classification.classified_record
  val slot_opcode : slot -> Ir.Opcode.t
  val slot_initial_bits : slot -> int64 option

  val slot_initializer :
    slot -> Sema.Function_call_expression_result.top_level_root_result option

  val slot_initializer_materialized : slot -> bool
  val slot_initializer_preparation_steps : slot -> int
  val requires_initializer_execution : t -> bool
  val human : t -> string
end

module Ir_global_address_lowering = Ir.Global_address_lowering
module Ir_integer_interpreter = Ir.Integer_interpreter
module Ir_runtime_call_context = Ir.Runtime_call_context
module Ir_integer_program_lowering = Ir.Integer_program_lowering
module Ir_global_initialization = Ir.Global_initialization

module Integer_initializer_preparation : sig
  type classification = Driver.Integer_initializers.classification =
    | Prepared_constant of int64
    | Scheduled

  type item = Driver.Integer_initializers.item
  type static_item = Driver.Integer_initializers.static_item
  type t = Driver.Integer_initializers.t

  val globals : t -> Ir.Integer_globals.t
  val items : t -> item list
  val static_items : t -> static_item list

  val static_root :
    static_item -> Sema.Function_call_expression_result.initializer_result

  val static_slot : static_item -> Ir.Integer_globals.static_slot
  val static_value_graph : static_item -> Ir.X87_stack.t
  val static_item_steps : static_item -> int
  val static_classification : static_item -> classification
  val executed_steps : t -> int
  val root : item -> Sema.Function_call_expression_result.top_level_root_result
  val value_graph : item -> Ir.X87_stack.t
  val classification : item -> classification
  val item_steps : item -> int
  val human : t -> string
end

module Ir_integer_unary_folding = Ir.Integer_unary_folding
module Ir_function_body = Ir.Function_body
module Ir_top_level_body = Ir.Top_level_body
module Ir_literal_lowering = Ir.Literal_lowering
module Ir_expression_lowering = Ir.Expression_lowering
module Ir_expression_statement_lowering = Ir.Expression_statement_lowering
module Ir_top_level_statement_lowering = Ir.Top_level_statement_lowering
module Ir_top_level_batch_lowering = Ir.Top_level_batch_lowering
module Ir_condition_lowering = Ir.Condition_lowering
module Ir_return_lowering = Ir.Return_lowering
module Ir_goto_label_lowering = Ir.Goto_label_lowering
module Ir_break_lowering = Ir.Break_lowering
module Ir_direct_call_lowering = Ir.Direct_call_lowering
module Ir_frame_address_lowering = Ir.Frame_address_lowering
module Templeos_bin_spec = Backend.Bin_spec
module Asm_directive = Asm.Directive
module Asm_opcode = Asm.Opcode
module Asm_register = Asm.Register
module Keyword = Frontend.Keyword
module Operator = Frontend.Operator
module Trivia = Frontend.Trivia
module Token_kind = Frontend.Token_kind
module Token = Frontend.Token
module Lexer = Frontend.Lexer
module Doldoc_binary = Frontend.Doldoc_binary
module Include_resolver = Frontend.Include_resolver
module Definition = Frontend.Definition
module Predefined = Frontend.Predefined
module Help_metadata = Frontend.Help_metadata
module Symbol_visibility = Frontend.Symbol_visibility
module Lexer_frame = Frontend.Lexer_frame
module Preprocessor = Frontend.Preprocessor
module Ast = Frontend.Ast
module Ast_dump = Frontend.Ast_dump
module Parser = Frontend.Parser
module Semantic_declaration_collection = Sema.Declaration_collection
module Semantic_member_collection = Sema.Member_collection
module Semantic_function_collection = Sema.Function_collection
module Semantic_label_resolution = Sema.Label_resolution
module Semantic_break_resolution = Sema.Break_resolution
module Semantic_aggregate_resolution = Sema.Aggregate_resolution
module Semantic_type = Sema.Type
module Semantic_type_reference = Sema.Type_reference
module Semantic_aggregate_header_resolution = Sema.Aggregate_header_resolution
module Semantic_member_type_resolution = Sema.Member_type_resolution
module Semantic_aggregate_layout = Sema.Aggregate_layout
module Semantic_aggregate_member_index = Sema.Aggregate_member_index
module Semantic_aggregate_layout_dump = Sema.Aggregate_layout_dump
module Semantic_function_type_resolution = Sema.Function_type_resolution
module Semantic_global_type_resolution = Sema.Global_type_resolution
module Semantic_local_type_resolution = Sema.Local_type_resolution
module Semantic_function_binding_index = Sema.Function_binding_index
module Semantic_function_frame_layout = Sema.Function_frame_layout
module Semantic_function_frame_layout_dump = Sema.Function_frame_layout_dump
module Semantic_function_expression_binding = Sema.Function_expression_binding
module Semantic_local_warning_analysis = Sema.Local_warning_analysis
module Semantic_module_expression_binding = Sema.Module_expression_binding
module Semantic_top_level_expression_binding = Sema.Top_level_expression_binding

module Semantic_top_level_outer_expression_binding =
  Sema.Top_level_outer_expression_binding

module Semantic_top_level_expression_tree = Sema.Top_level_expression_tree

module Semantic_top_level_statement_validation =
  Driver.Top_level_statement_validation

module Semantic_top_level_condition_result = Sema.Top_level_condition_result

module Semantic_top_level_switch_selector_result =
  Sema.Top_level_switch_selector_result

module Semantic_top_level_switch_case_result = Sema.Top_level_switch_case_result

module Semantic_top_level_identifier_resolution =
  Sema.Top_level_identifier_resolution

module Semantic_outer_environment = Sema.Outer_environment
module Semantic_reference_selection = Sema.Reference_selection
module Semantic_query_selection = Sema.Query_selection
module Semantic_outer_expression_binding = Sema.Outer_expression_binding
module Semantic_global_initializer_binding = Sema.Global_initializer_binding
module Semantic_global_dimension_binding = Sema.Global_dimension_binding
module Semantic_global_array_layout = Sema.Global_array_layout
module Semantic_initializer_source = Sema.Initializer_source
module Semantic_function_default_binding = Sema.Function_default_binding
module Semantic_function_resolution = Sema.Function_resolution
module Semantic_function_header_analysis = Sema.Function_header_analysis
module Semantic_function_call_resolution = Sema.Function_call_resolution

module Semantic_function_call_conversion_policy =
  Sema.Function_call_conversion_policy

module Semantic_function_call_expression_result =
  Sema.Function_call_expression_result

module Semantic_implicit_output_target_resolution =
  Sema.Implicit_output_target_resolution

module Semantic_top_level_implicit_output_target_resolution =
  Sema.Top_level_implicit_output_target_resolution

module Semantic_top_level_implicit_output_argument_binding =
  Sema.Top_level_implicit_output_argument_binding

module Semantic_implicit_output_argument_binding =
  Sema.Implicit_output_argument_binding

module Semantic_function_call_conversion_decision =
  Sema.Function_call_conversion_decision

module Semantic_function_record_classification =
  Sema.Function_record_classification

module Semantic_function_call_target_classification =
  Sema.Function_call_target_classification

module Semantic_top_level_function_call_target_classification =
  Sema.Top_level_function_call_target_classification

module Semantic_global_resolution = Sema.Global_resolution
module Semantic_global_record_classification = Sema.Global_record_classification

val lex :
  Session.t -> source:Source_file.t -> (Token.t list, Diagnostic.t list) result

val preprocess :
  Session.t ->
  config:Preprocessor.Config.t ->
  source:Source_file.t ->
  (Token.t list, Diagnostic.t list) result
(** The convenience entry point returns tokens when the stream has no errors.
    Use {!preprocess_detailed} when warnings must be retained. *)

val preprocess_detailed :
  Session.t ->
  config:Preprocessor.Config.t ->
  source:Source_file.t ->
  Preprocessor.output
(** Preprocess a source and retain tokens, warnings, notes, errors, and
    source-ordered help metadata in one result. *)

val parse :
  Session.t -> source:Source_file.t -> (Ast.module_, Diagnostic.t list) result

val parse_with_config :
  Session.t ->
  config:Preprocessor.Config.t ->
  source:Source_file.t ->
  (Ast.module_, Diagnostic.t list) result

val parse_detailed :
  Session.t ->
  config:Preprocessor.Config.t ->
  source:Source_file.t ->
  Parser.output

val collect_declarations :
  Session.t -> Ast.module_ -> (Semantic_declaration_collection.t, string) result
(** Collect accepted top-level declarations into a new semantic module scope.
    This entry point does not perform duplicate checks or reference resolution.
*)

val collect_members :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  Ast.module_ ->
  (Semantic_member_collection.t, string) result
(** Create aggregate scopes beneath [declarations] and collect direct members.
    Anonymous-union members share their containing aggregate scope. Layout,
    inheritance, duplicate checks, and member-reference resolution remain
    separate semantic passes. *)

val collect_functions :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  Ast.module_ ->
  (Semantic_function_collection.t, string) result
(** Create function scopes beneath [declarations] and collect named parameters,
    variadic [argc] and [argv], and function-wide locals. Type resolution,
    storage, duplicate checks, and reference resolution remain separate semantic
    passes. *)

val resolve_labels :
  Session.t ->
  functions:Semantic_function_collection.t ->
  Ast.module_ ->
  (Semantic_label_resolution.t, string) result
(** Bind function-local [goto] occurrences to language and assembly-block label
    definitions from the same AST. Assembly operand references and control-flow
    lowering remain separate passes. *)

val resolve_breaks :
  Session.t ->
  functions:Semantic_function_collection.t ->
  Ast.module_ ->
  (Semantic_break_resolution.t, string) result
(** Bind each function-body [break] to its nearest loop, switch, or subswitch
    region and reject a direct break without an active region. *)

val resolve_aggregates :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  Ast.module_ ->
  (Semantic_aggregate_resolution.t, string) result
(** Reconcile aggregate forwards and definitions from the same AST. Type
    resolution, inheritance, layout, and linkage remain separate passes. *)

val resolve_aggregate_headers :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  aggregates:Semantic_aggregate_resolution.t ->
  Ast.module_ ->
  (Semantic_aggregate_header_resolution.t, string) result
(** Resolve definition backing and base types at their source publication
    points. Member types, layout, inherited lookup, and linkage remain separate
    passes. *)

val resolve_member_types :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  aggregates:Semantic_aggregate_resolution.t ->
  headers:Semantic_aggregate_header_resolution.t ->
  members:Semantic_member_collection.t ->
  Ast.module_ ->
  (Semantic_member_type_resolution.t, string) result
(** Resolve aggregate member type references at their source publication points.
    Array extents, callback signatures, layout, inherited lookup, and linkage
    remain separate passes. *)

val layout_aggregates :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  aggregates:Semantic_aggregate_resolution.t ->
  headers:Semantic_aggregate_header_resolution.t ->
  members:Semantic_member_type_resolution.t ->
  Ast.module_ ->
  (Semantic_aggregate_layout.t, string) result
(** Calculate source-ordered layouts whose dimensions, offsets, bases, and
    by-value members have closed values. Unresolved semantic constants and later
    aggregate definitions remain explicit errors. *)

val index_aggregate_members :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  headers:Semantic_aggregate_header_resolution.t ->
  members:Semantic_member_type_resolution.t ->
  layouts:Semantic_aggregate_layout.t ->
  (Semantic_aggregate_member_index.t, string) result
(** Validate direct and inherited duplicate names, then build the immutable
    source-ordered member index used by later member, [sizeof], and [offset]
    resolution. Lookup itself does not update use counts. *)

val analyze_aggregate_layouts :
  Session.t -> Ast.module_ -> (Semantic_aggregate_member_index.t, string) result
(** Run the checked declaration, aggregate, header, member-type, closed-layout,
    and member-index passes needed by layout tooling. This does not resolve
    symbol-dependent layout expressions or allocate runtime storage. *)

val resolve_function_types :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  aggregates:Semantic_aggregate_resolution.t ->
  functions:Semantic_function_collection.t ->
  Ast.module_ ->
  (Semantic_function_type_resolution.t, string) result
(** Resolve function return and recursive parameter types at each declaration's
    source position. Default evaluation, declaration reconciliation, call
    checking, storage, and linkage remain separate passes. *)

val resolve_local_types :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  aggregates:Semantic_aggregate_resolution.t ->
  functions:Semantic_function_collection.t ->
  Ast.module_ ->
  (Semantic_local_type_resolution.t, string) result
(** Resolve local declaration and recursive callback types at each function's
    source position. Array extents, initializer evaluation, stack layout,
    register allocation, and ordinary expression binding remain separate passes.
*)

val index_function_bindings :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  functions:Semantic_function_collection.t ->
  function_types:Semantic_function_type_resolution.t ->
  local_types:Semantic_local_type_resolution.t ->
  (Semantic_function_binding_index.t, string) result
(** Validate the shared function namespace and build immutable lookup indexes.
    Expression publication timing, use counts, warnings, storage, and register
    allocation remain separate passes. *)

val layout_function_frames :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  bindings:Semantic_function_binding_index.t ->
  function_types:Semantic_function_type_resolution.t ->
  local_types:Semantic_local_type_resolution.t ->
  aggregate_layouts:Semantic_aggregate_layout.t ->
  Ast.module_ ->
  (Semantic_function_frame_layout.t, string) result
(** Retain checked parameter and local locations for every function definition.
    Named and synthetic parameters use eight-byte slots beginning at RBP+16.
    Automatic locals use downward-growing, size-aligned slots; static locals
    remain typed locations without frame slots. *)

val resolve_function_expressions :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  functions:Semantic_function_collection.t ->
  local_types:Semantic_local_type_resolution.t ->
  bindings:Semantic_function_binding_index.t ->
  Ast.module_ ->
  (Semantic_function_expression_binding.t, string) result
(** Bind ordinary function-body identifiers to parameters and locals at their
    source publication points. Nonlocal names remain explicit candidates for
    later global and type resolution. This pass does not update use counts. *)

val analyze_local_warnings :
  ?compiler_option_mask:int64 ->
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  function_types:Semantic_function_type_resolution.t ->
  local_types:Semantic_local_type_resolution.t ->
  bindings:Semantic_function_binding_index.t ->
  expressions:Semantic_function_expression_binding.t ->
  Ast.module_ ->
  (Semantic_local_warning_analysis.t, string) result
(** Derive effective member flags and source-compatible use counts, then
    classify unused bindings and unneeded [no_warn] suppressions. *)

val resolve_module_expressions :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  aggregates:Semantic_aggregate_resolution.t ->
  functions:Semantic_function_resolution.t ->
  globals:Semantic_global_resolution.t ->
  expressions:Semantic_function_expression_binding.t ->
  (Semantic_module_expression_binding.t, string) result
(** Bind nonlocal function expression candidates to source-visible aggregate,
    function, and global records. Names absent from the compilation unit remain
    explicit outer-environment candidates. This pass does not update use counts.
*)

val resolve_top_level_expressions :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  module_expressions:Semantic_module_expression_binding.t ->
  ?initializers:Semantic_global_initializer_binding.t ->
  Ast.module_ ->
  (Semantic_top_level_expression_binding.t, string) result
(** Bind ordinary names and retain specialized [defined] queries under
    executable top-level statements through the source-visible module
    publication prefix. Missing names remain explicit outer candidates. *)

val resolve_top_level_outer_expressions :
  Session.t ->
  environment:Semantic_outer_environment.t ->
  expressions:Semantic_top_level_expression_binding.t ->
  (Semantic_top_level_outer_expression_binding.t, string) result
(** Preserve source-visible top-level module bindings, then resolve every outer
    candidate through the complete JIT or AOT table chain. A missing ordinary
    identifier remains an error, while a missing [defined] query becomes a
    checked false result. *)

val validate_top_level_statements :
  Ast.module_ -> (unit, Semantic_top_level_statement_validation.error) result
(** Reject explicit [return] statements outside function definitions while
    retaining the source keyword origin. The implicit final value returned by
    TempleOS's top-level statement compiler is a separate lowering rule. *)

val build_top_level_expression_trees :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  compilation_mode:Preprocessor.compilation_mode ->
  expressions:Semantic_top_level_outer_expression_binding.t ->
  Ast.module_ ->
  (Semantic_top_level_expression_tree.t, string) result
(** Build immutable semantic expression trees for executable top-level
    statements while retaining their complete module and outer bindings. *)

val classify_top_level_identifiers :
  Session.t ->
  globals:Semantic_global_type_resolution.t ->
  functions:Semantic_function_resolution.t ->
  expressions:Semantic_top_level_expression_tree.t ->
  (Semantic_top_level_identifier_resolution.t, string) result
(** Classify every bound top-level identifier as a source-typed module value,
    aggregate offset base, or an outer record awaiting typed metadata. *)

val create_outer_environment :
  Session.t ->
  compilation_mode:Preprocessor.compilation_mode ->
  Semantic_outer_environment.table list ->
  (Semantic_outer_environment.t, string) result
(** Validate an immutable lookup chain for the selected compilation mode. JIT
    chains contain the current task and its parents; AOT chains contain the
    enclosing compilations. Both end at the assembler table. *)

val resolve_outer_expressions :
  Session.t ->
  environment:Semantic_outer_environment.t ->
  expressions:Semantic_module_expression_binding.t ->
  (Semantic_outer_expression_binding.t, string) result
(** Preserve local and compilation-unit expression bindings, then resolve every
    remaining ordinary identifier through the complete outer table chain. *)

val resolve_global_initializers :
  Session.t ->
  environment:Semantic_outer_environment.t ->
  expressions:Semantic_module_expression_binding.t ->
  globals:Semantic_global_resolution.t ->
  Ast.module_ ->
  (Semantic_global_initializer_binding.t, string) result
(** Bind ordinary identifier occurrences in scalar and recursively braced global
    initializers. The owning global is visible before its initializer, while
    later declarations remain unavailable. *)

val resolve_global_dimensions :
  Session.t ->
  environment:Semantic_outer_environment.t ->
  expressions:Semantic_module_expression_binding.t ->
  globals:Semantic_global_resolution.t ->
  Ast.module_ ->
  (Semantic_global_dimension_binding.t, string) result
(** Bind ordinary identifier occurrences in global array extents. Earlier
    declarations are visible, while the owning global and later declarations
    remain unavailable and can fall through to the outer environment. *)

val layout_global_arrays :
  Session.t ->
  bindings:Semantic_global_dimension_binding.t ->
  Ast.module_ ->
  (Semantic_global_array_layout.t, string) result
(** Evaluate fixed array extents from the original declaration expressions and
    their checked before-owner bindings. *)

val resolve_function_defaults :
  Session.t ->
  environment:Semantic_outer_environment.t ->
  expressions:Semantic_module_expression_binding.t ->
  functions:Semantic_function_resolution.t ->
  Ast.module_ ->
  (Semantic_function_default_binding.t, string) result
(** Bind ordinary identifier occurrences in defaults on top-level named function
    headers. The owning function is visible, parameters are not local bindings
    yet, and later declarations remain unavailable. *)

val resolve_global_types :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  aggregates:Semantic_aggregate_resolution.t ->
  Ast.module_ ->
  (Semantic_global_type_resolution.t, string) result
(** Resolve global type references at their source publication points. Array
    extents, initializers, identity reconciliation, storage, and linkage remain
    separate passes. *)

val resolve_function_identities :
  ?compiler_option_mask:int64 ->
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  functions:Semantic_function_type_resolution.t ->
  compilation_mode:Preprocessor.compilation_mode ->
  Ast.module_ ->
  (Semantic_function_resolution.t, string) result
(** Reconcile parsed function declarations using the pinned JIT/AOT join rules.
    The optional batch snapshot applies [OPTf_EXTERNS_TO_IMPORTS]; source-
    positioned option execution remains separate. Evaluated header analysis,
    task-parent lookup, alternate target resolution, and emitted linkage remain
    separate passes. *)

val analyze_function_headers :
  Session.t ->
  functions:Semantic_function_resolution.t ->
  Semantic_function_header_analysis.function_input list ->
  ( Semantic_function_header_analysis.t,
    Semantic_function_header_analysis.error )
  result
(** Compare joined function headers using evaluated default payloads. The
    compile-time VM remains responsible for producing those payloads. *)

val resolve_function_calls :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  ?members:Semantic_aggregate_member_index.t ->
  function_types:Semantic_function_type_resolution.t ->
  local_types:Semantic_local_type_resolution.t ->
  global_types:Semantic_global_type_resolution.t ->
  functions:Semantic_function_resolution.t ->
  expressions:Semantic_module_expression_binding.t ->
  ?outer:Semantic_outer_expression_binding.t ->
  Ast.module_ ->
  (Semantic_function_call_resolution.t, string) result
(** Bind calls in function bodies to the source-visible function or callback
    header. Supplying [members] also resolves direct and pointer member
    callbacks against the completed aggregate index. Fixed slots retain provided
    or declared-default origins; prefix and binary operands retain their
    recursive source views; bound identifier arguments retain their checked type
    and declarator shape; named aggregate cast targets retain the identity
    visible before the caller. Supplying [outer] proves ordinary [defined]
    operands against the complete mode-specific outer lookup chain; indirect
    call targets outside the compilation unit remain deferred. *)

val analyze_function_call_conversions :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  headers:Semantic_aggregate_header_resolution.t ->
  calls:Semantic_function_call_resolution.t ->
  ( Semantic_function_call_conversion_policy.t,
    Semantic_function_call_conversion_policy.error )
  result
(** Classify each provided fixed target through the aggregate backing relation
    visible before its caller. Defaults, variadic expressions, actual expression
    types, and deferred callees remain separate. *)

val type_function_call_expressions :
  Session.t ->
  members:Semantic_aggregate_member_index.t ->
  policies:Semantic_function_call_conversion_policy.t ->
  ( Semantic_function_call_expression_result.t,
    Semantic_function_call_expression_result.error )
  result
(** Derive stable, session-owned results for every provided fixed direct-call
    expression. Member expressions use the completed, immutable aggregate index.
    Known source types and value categories stay separate from the
    target-specific conversion intent selected by the next pass. *)

val type_function_call_expressions_with_outer :
  Session.t ->
  members:Semantic_aggregate_member_index.t ->
  outer:Semantic_outer_expression_binding.t ->
  policies:Semantic_function_call_conversion_policy.t ->
  ( Semantic_function_call_expression_result.t,
    Semantic_function_call_expression_result.error )
  result
(** Type function expressions with an exact outer-table binding batch. Entries
    without checked metadata remain unavailable. *)

val type_top_level_expressions :
  Session.t ->
  members:Semantic_aggregate_member_index.t ->
  policies:Semantic_function_call_conversion_policy.t ->
  identifiers:Semantic_top_level_identifier_resolution.t ->
  Semantic_top_level_expression_tree.t ->
  ( Semantic_function_call_expression_result.top_level_t,
    Semantic_function_call_expression_result.error )
  result
(** Type scalar roots in executable top-level statements through the function
    expression engine. Results retain their statement and root roles while
    unsupported aggregate, outer, member, and call boundaries stay explicit. *)

val collect_top_level_conditions :
  Session.t ->
  Semantic_function_call_expression_result.top_level_t ->
  ( Semantic_top_level_condition_result.t,
    Semantic_top_level_condition_result.error )
  result
(** Collect the checked roots used by executable top-level [if], [while],
    [do while], and [for] statements. Each record retains its source role and
    zero or nonzero branch sense; no Boolean conversion or IR is created. *)

val collect_top_level_switch_selectors :
  Session.t ->
  Semantic_function_call_expression_result.top_level_t ->
  ( Semantic_top_level_switch_selector_result.t,
    Semantic_top_level_switch_selector_result.error )
  result
(** Collect the checked roots used by bounded and no-bound executable top-level
    switch statements. Each record retains the source mode; no range arithmetic,
    jump table, or IR is created. *)

val collect_top_level_switch_cases :
  Session.t ->
  Semantic_function_call_expression_result.top_level_t ->
  ( Semantic_top_level_switch_case_result.t,
    Semantic_top_level_switch_case_result.error )
  result
(** Join implicit, single-value, and ranged executable top-level switch cases to
    their checked value roots. Explicit [F64] values record integer-conversion
    intent; no value is evaluated and no jump table or IR is created. *)

val resolve_implicit_output_targets :
  Session.t ->
  environment:Semantic_outer_environment.t ->
  module_expressions:Semantic_module_expression_binding.t ->
  function_types:Semantic_function_type_resolution.t ->
  functions:Semantic_function_resolution.t ->
  expressions:Semantic_function_call_expression_result.t ->
  ( Semantic_implicit_output_target_resolution.t,
    Semantic_implicit_output_target_resolution.error )
  result
(** Resolve implicit [Print] and [PutChars] statements through visible module
    function headers and the supplied outer hash-table snapshot. *)

val resolve_top_level_implicit_output_targets :
  Session.t ->
  function_types:Semantic_function_type_resolution.t ->
  functions:Semantic_function_resolution.t ->
  Semantic_function_call_expression_result.top_level_t ->
  ( Semantic_top_level_implicit_output_target_resolution.t,
    Semantic_top_level_implicit_output_target_resolution.error )
  result
(** Resolve executable top-level [Print] and [PutChars] targets through module
    functions visible before each statement and then through the exact outer
    environment retained by the typed expression batch. *)

val bind_top_level_implicit_output_arguments :
  Session.t ->
  policies:Semantic_function_call_conversion_policy.t ->
  ?outer_headers:Semantic_function_type_resolution.resolved_function list ->
  Semantic_top_level_implicit_output_target_resolution.t ->
  ( Semantic_top_level_implicit_output_argument_binding.t,
    Semantic_top_level_implicit_output_argument_binding.error )
  result
(** Bind executable top-level output values against the exact header selected by
    target resolution. An outer target remains deferred without a supplied
    checked header for the same symbol. *)

val bind_implicit_output_arguments :
  Session.t ->
  policies:Semantic_function_call_conversion_policy.t ->
  ?outer_headers:Semantic_function_type_resolution.resolved_function list ->
  Semantic_implicit_output_target_resolution.t ->
  ( Semantic_implicit_output_argument_binding.t,
    Semantic_implicit_output_argument_binding.error )
  result
(** Bind implicit output expressions to the fixed slots and variadic tail of
    each selected checked header. Untyped outer targets remain deferred. *)

val decide_function_call_conversions :
  Session.t ->
  policies:Semantic_function_call_conversion_policy.t ->
  expressions:Semantic_function_call_expression_result.t ->
  ( Semantic_function_call_conversion_decision.t,
    Semantic_function_call_conversion_decision.error )
  result
(** Select fixed-call conversion intent for audited argument classes, including
    source-visible named aggregate postfix casts and the checked prefix operator
    and binary operator paths. Unsupported expression classes remain explicit
    unresolved results. *)

val classify_function_records :
  ?compiler_option_mask:int64 ->
  Session.t ->
  resolution:Semantic_function_resolution.t ->
  Ast.module_ ->
  (Semantic_function_record_classification.t, string) result
(** Replay source-grounded function record mutations and expose raw flags, call
    access, lookup visibility, and AOT linkage intent. The optional option mask
    overrides the declaration snapshots retained by resolution and must agree on
    [OPTf_EXTERNS_TO_IMPORTS]. Source-positioned option execution, addresses,
    header comparison, and record emission remain separate. *)

val resolve_global_records :
  ?compiler_option_mask:int64 ->
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  globals:Semantic_global_type_resolution.t ->
  compilation_mode:Preprocessor.compilation_mode ->
  Ast.module_ ->
  (Semantic_global_resolution.t, string) result
(** Retain one semantic record per parsed global and attach immediate alias
    edges using the pinned JIT or AOT rule. The optional batch snapshot applies
    [OPTf_EXTERNS_TO_IMPORTS] and [OPTf_GLBLS_ON_DATA_HEAP]. AOT data-heap
    initializers are rejected here. Source-positioned option execution,
    target-address resolution, allocation, and emitted linkage remain separate
    passes. *)

val classify_global_records :
  ?compiler_option_mask:int64 ->
  Session.t ->
  resolution:Semantic_global_resolution.t ->
  Ast.module_ ->
  (Semantic_global_record_classification.t, string) result
(** Derive source-grounded hash and global-variable flags, import naming, value
    access, cleanup, map visibility, and AOT publication intent. The optional
    mask overrides the declaration snapshots retained by resolution and must
    agree on [OPTf_EXTERNS_TO_IMPORTS] and [OPTf_GLBLS_ON_DATA_HEAP].
    Source-positioned option execution, allocation, address resolution, and
    record emission remain separate. *)

type 'a integer_program_result = { value : 'a; diagnostics : Diagnostic.t list }
(** A successful program phase and its nonfatal diagnostics. Failures return all
    accumulated warnings and errors together in the result's error list. *)

val lower_integer_program :
  Session.t ->
  config:Preprocessor.Config.t ->
  source:Source_file.t ->
  (Ir_x87_stack.t integer_program_result, Diagnostic.t list) result
(** Lower a complete batch of top-level expressions and structured control
    statements into verified IR. Unsupported source shapes fail explicitly. VM
    opcode, type and flag restrictions are checked only during execution. *)

type integer_program

val compile_integer_task_ast :
  task:Ir_integer_interpreter.task_state ->
  ?declaration_command:Task_declarations.command ->
  Session.t ->
  config:Preprocessor.Config.t ->
  Ast.module_ ->
  (integer_program integer_program_result, Diagnostic.t list) result
(** Compile one checked unit against its owning JIT task snapshot, charging the
    task's cumulative preparation budget even on reached failure. Foreign
    semantic tables and AOT mode are rejected before collection. The resulting
    program still requires its owning task runtime for execution; compiling it
    does not admit storage, publish frontend entries or run source effects.
    Unlike [Integer_task.compile_ast], this low-level unit compiler does not
    cache or reject overlapping ASTs. VM replay checks own each compiled entry;
    parser command and source replay admission remain the caller's concern. *)

val compile_integer_ast :
  ?max_initializer_steps:int ->
  Session.t ->
  config:Preprocessor.Config.t ->
  Ast.module_ ->
  (integer_program integer_program_result, Diagnostic.t list) result
(** Compile an already parsed independent unit through the ordinary semantic and
    verified IR pipeline, without consuming or preprocessing source again. *)

val compile_integer_program :
  ?max_initializer_steps:int ->
  Session.t ->
  config:Preprocessor.Config.t ->
  source:Source_file.t ->
  (integer_program integer_program_result, Diagnostic.t list) result

val integer_program_entry : integer_program -> Ir_x87_stack.t
val integer_program_globals : integer_program -> Ir_integer_globals.t

val integer_program_initialization :
  integer_program -> Ir_global_initialization.t

val integer_program_initializer_preparation :
  integer_program -> Integer_initializer_preparation.t

val integer_program_functions :
  integer_program -> Ir_integer_interpreter.function_definition list

val integer_program_human : integer_program -> string
val integer_program_runtime_calls : integer_program -> Ir_runtime_call_context.t

val run_integer_program :
  ?max_initializer_steps:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  Session.t ->
  config:Preprocessor.Config.t ->
  source:Source_file.t ->
  max_steps:int ->
  (Ir_integer_interpreter.t integer_program_result, Diagnostic.t list) result
(** Execute integer source statements and checked nonzero integer/U0 function
    definitions with fixed parameters, automatic locals, direct call expressions
    and ordinary public integer globals and fixed arrays with supported
    declaration initializers. U0 calls complete without a word; an ordinary U0
    call expression's checked discard clears any preceding top-level final
    value. Bare return and fallthrough preserve the caller continuation. Numeric
    returns preserve full register bits as runtime I64/U64; missing required
    word returns retain a hosted diagnostic. Narrow storage and parameter entry
    normalize to declared width and signedness, with eight-byte ABI parameter
    slots. Plain assignment and compound results retain full register payloads;
    prefix/postfix return normalized stored values. One-level integer pointer
    locals and fixed parameters can alias scalar objects and fixed-array
    elements across direct calls. Exact declared types and native computation
    classes remain distinct, including unsigned calls, storage negation and
    complement. String literals own mutable bytes and a final initialized zero
    for one execution image; each source site persists across calls and
    initializers. Compatible public/internal U8 pointer forms preserve the same
    object. [max_literal_bytes] defaults to 1,048,576 and bounds all literal
    sites, including uncalled definitions, separately from frame and global
    bytes. Checked indexing retains source strides, grouping behavior and
    declared-object bounds through copies and recursion. Constant preparation,
    runtime instructions, active frame bytes, global bytes and call depth have
    separate positive bounds. Typed pointers can also alias global/static
    integer objects. Persistent cells are shared by calls in one execution.
    Conditions short-circuit AND and OR; ordinary values and XOR remain eager.
    Scheduled arithmetic uses runtime IR semantics; initializers and their
    transitive callees retain explicit shift/divisor and narrow read/range
    optimizer boundaries. Supported pure constants supply initial-image bits.
    General memory, arbitrary indirect/external calls and native code remain
    unsupported. Checked Print/PutChars calls execute under separate positive
    output/work limits (both default 1,048,576). This convenience entrypoint
    projects the outcome; use [run_integer_program_report] to retain captured
    bytes on both success and failure. Implicit output preserves the last
    ordinary expression. *)

type integer_program_report

val run_integer_program_report :
  ?max_initializer_steps:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  Session.t ->
  config:Preprocessor.Config.t ->
  source:Source_file.t ->
  max_steps:int ->
  integer_program_report
(** Execute with fresh captured bytes and formatting work, including on failure.
    Configuration and preflight failures have empty capture. Output/work limits
    are positive and default independently to 1,048,576. *)

val integer_program_report_outcome :
  integer_program_report ->
  (Ir_integer_interpreter.t integer_program_result, Diagnostic.t list) result

val integer_program_report_output_bytes : integer_program_report -> string
val integer_program_report_output_work : integer_program_report -> int

val lower_integer_expression :
  Session.t ->
  config:Preprocessor.Config.t ->
  source:Source_file.t ->
  (Ir_x87_stack.t, Diagnostic.t list) result
(** Lower exactly one ordinary [EXPR;] statement into a verified return harness.
    Preprocessing and semantic checking use the supplied session and config. VM
    opcode, type and flag restrictions are checked only during evaluation. *)

val evaluate_integer_expression :
  Session.t ->
  config:Preprocessor.Config.t ->
  source:Source_file.t ->
  max_steps:int ->
  (Ir_integer_interpreter.t, Diagnostic.t list) result
(** Evaluate one source expression through the bounded integer interpreter.
    [max_steps] must be positive; validation precedes parsing. Every harness
    instruction, including return preparation and return, consumes one step. *)
