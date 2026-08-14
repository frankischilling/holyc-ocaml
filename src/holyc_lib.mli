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
  function_types:Semantic_function_type_resolution.t ->
  functions:Semantic_function_resolution.t ->
  expressions:Semantic_module_expression_binding.t ->
  Ast.module_ ->
  (Semantic_function_call_resolution.t, string) result
(** Bind syntactically direct calls in function bodies to the source-visible
    function header. Fixed slots retain provided or declared-default origins;
    indirect and outer targets remain explicit deferred results. *)

val analyze_function_call_conversions :
  Session.t ->
  declarations:Semantic_declaration_collection.t ->
  headers:Semantic_aggregate_header_resolution.t ->
  calls:Semantic_function_call_resolution.t ->
  ( Semantic_function_call_conversion_policy.t,
    Semantic_function_call_conversion_policy.error )
  result
(** Classify the forwarded target path for each provided fixed argument.
    Defaults, variadic expressions, actual expression types, and deferred
    callees remain separate. *)

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
