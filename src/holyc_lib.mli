module Source_id = Common.Source_id
module Source_file = Common.Source_file
module Source_manager = Common.Source_manager
module Span = Common.Span
module Diagnostic = Common.Diagnostic
module Diagnostic_render = Common.Diagnostic_render
module Session = Driver.Session
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
module Ir_function_body = Ir.Function_body
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
module Semantic_outer_expression_binding = Sema.Outer_expression_binding
module Semantic_global_initializer_binding = Sema.Global_initializer_binding
module Semantic_global_dimension_binding = Sema.Global_dimension_binding
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
  Ast.module_ ->
  (Semantic_top_level_expression_binding.t, string) result
(** Bind ordinary names under executable top-level statements through the
    source-visible module publication prefix. Names absent from that prefix
    remain explicit outer candidates. *)

val resolve_top_level_outer_expressions :
  Session.t ->
  environment:Semantic_outer_environment.t ->
  expressions:Semantic_top_level_expression_binding.t ->
  (Semantic_top_level_outer_expression_binding.t, string) result
(** Preserve source-visible top-level module bindings, then resolve every outer
    candidate through the complete JIT or AOT table chain. *)

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
  Ast.module_ ->
  (Semantic_function_call_resolution.t, string) result
(** Bind calls in function bodies to the source-visible function or callback
    header. Supplying [members] also resolves direct and pointer member
    callbacks against the completed aggregate index. Fixed slots retain provided
    or declared-default origins; prefix and binary operands retain their
    recursive source views; bound identifier arguments retain their checked type
    and declarator shape; named aggregate cast targets retain the identity
    visible before the caller; indirect and outer targets remain explicit
    deferred results. *)

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
