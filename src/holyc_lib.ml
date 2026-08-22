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
module Semantic_top_level_expression_binding = Sema.Top_level_expression_binding

module Semantic_top_level_outer_expression_binding =
  Sema.Top_level_outer_expression_binding

module Semantic_top_level_expression_tree = Sema.Top_level_expression_tree

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

module Semantic_implicit_output_argument_binding =
  Sema.Implicit_output_argument_binding

module Semantic_function_call_conversion_decision =
  Sema.Function_call_conversion_decision

module Semantic_function_record_classification =
  Sema.Function_record_classification

module Semantic_global_resolution = Sema.Global_resolution
module Semantic_global_record_classification = Sema.Global_record_classification

let lex _session ~source = Frontend.Lexer.lex_all source

let preprocess_detailed session ~config ~source =
  Frontend.Preprocessor.collect_all ~sources:(Session.sources session)
    ~definitions:(Session.definitions session)
    ~symbols:(Session.symbols session) ~config source

let preprocess session ~config ~source =
  let output = preprocess_detailed session ~config ~source in
  if Frontend.Preprocessor.has_errors output then Error output.diagnostics
  else Ok output.tokens

let parse_detailed session ~config ~source =
  Frontend.Parser.parse ~sources:(Session.sources session)
    ~definitions:(Session.definitions session)
    ~symbols:(Session.symbols session) ~config source

let parse_with_config session ~config ~source =
  let output = parse_detailed session ~config ~source in
  match output.ast with
  | Some ast -> Ok ast
  | None -> Error output.diagnostics

let parse session ~source =
  let config =
    match Frontend.Preprocessor.Config.create () with
    | Ok config -> config
    | Error message -> invalid_arg message
  in
  parse_with_config session ~config ~source

let collect_declarations session module_ =
  Driver.Semantic_collection.collect ~sources:(Session.sources session)
    ~table:(Session.semantic_symbols session)
    module_

let collect_members session ~declarations module_ =
  Driver.Member_collection.collect
    ~table:(Session.semantic_symbols session)
    ~declarations module_

let collect_functions session ~declarations module_ =
  Driver.Function_collection.collect
    ~table:(Session.semantic_symbols session)
    ~declarations module_

let resolve_labels session ~functions module_ =
  Driver.Label_resolution.resolve
    ~table:(Session.semantic_symbols session)
    ~functions module_

let resolve_aggregates session ~declarations module_ =
  Driver.Aggregate_resolution.resolve
    ~table:(Session.semantic_symbols session)
    ~declarations module_

let resolve_aggregate_headers session ~declarations ~aggregates module_ =
  Driver.Aggregate_header_resolution.resolve
    ~table:(Session.semantic_symbols session)
    ~declarations ~aggregates module_

let resolve_member_types session ~declarations ~aggregates ~headers ~members
    module_ =
  Driver.Member_type_resolution.resolve
    ~table:(Session.semantic_symbols session)
    ~declarations ~aggregates ~headers ~members module_

let layout_aggregates session ~declarations ~aggregates ~headers ~members
    module_ =
  Driver.Aggregate_layout.layout
    ~table:(Session.semantic_symbols session)
    ~declarations ~aggregates ~headers ~members module_

let index_aggregate_members session ~declarations ~headers ~members ~layouts =
  Driver.Aggregate_member_index.build
    ~table:(Session.semantic_symbols session)
    ~declarations ~headers ~members ~layouts

let analyze_aggregate_layouts session module_ =
  Result.bind (collect_declarations session module_) (fun declarations ->
      Result.bind (resolve_aggregates session ~declarations module_)
        (fun aggregates ->
          Result.bind
            (resolve_aggregate_headers session ~declarations ~aggregates module_)
            (fun headers ->
              Result.bind (collect_members session ~declarations module_)
                (fun collected_members ->
                  Result.bind
                    (resolve_member_types session ~declarations ~aggregates
                       ~headers ~members:collected_members module_)
                    (fun members ->
                      Result.bind
                        (layout_aggregates session ~declarations ~aggregates
                           ~headers ~members module_) (fun layouts ->
                          index_aggregate_members session ~declarations ~headers
                            ~members ~layouts))))))

let resolve_function_types session ~declarations ~aggregates ~functions module_
    =
  Driver.Function_type_resolution.resolve
    ~table:(Session.semantic_symbols session)
    ~declarations ~aggregates ~functions module_

let resolve_local_types session ~declarations ~aggregates ~functions module_ =
  Driver.Local_type_resolution.resolve
    ~table:(Session.semantic_symbols session)
    ~declarations ~aggregates ~functions module_

let index_function_bindings session ~declarations ~functions ~function_types
    ~local_types =
  Driver.Function_binding_index.build
    ~table:(Session.semantic_symbols session)
    ~declarations ~functions ~function_types ~local_types

let resolve_function_expressions session ~declarations ~functions ~local_types
    ~bindings module_ =
  Driver.Function_expression_binding.resolve
    ~table:(Session.semantic_symbols session)
    ~declarations ~functions ~local_types ~bindings module_

let analyze_local_warnings ?compiler_option_mask session ~declarations
    ~function_types ~local_types ~bindings ~expressions module_ =
  Driver.Local_warning_analysis.analyze ?compiler_option_mask
    ~table:(Session.semantic_symbols session)
    ~declarations ~function_types ~local_types ~bindings ~expressions module_

let resolve_module_expressions session ~declarations ~aggregates ~functions
    ~globals ~expressions =
  Driver.Module_expression_binding.resolve
    ~table:(Session.semantic_symbols session)
    ~declarations ~aggregates ~functions ~globals ~expressions

let resolve_top_level_expressions session ~declarations ~module_expressions
    module_ =
  Driver.Top_level_expression_binding.resolve
    ~table:(Session.semantic_symbols session)
    ~declarations ~module_expressions module_

let resolve_top_level_outer_expressions session ~environment ~expressions =
  Sema.Top_level_outer_expression_binding.resolve
    ~table:(Session.semantic_symbols session)
    ~environment ~expressions
  |> Result.map_error Sema.Top_level_outer_expression_binding.error_to_string

let build_top_level_expression_trees session ~declarations ~compilation_mode
    ~expressions module_ =
  let compilation_mode =
    match compilation_mode with
    | Preprocessor.Jit -> Sema.Outer_environment.Jit
    | Preprocessor.Aot -> Sema.Outer_environment.Aot
  in
  Driver.Top_level_expression_tree.build
    ~table:(Session.semantic_symbols session)
    ~declarations ~compilation_mode ~expressions module_

let classify_top_level_identifiers session ~globals ~functions ~expressions =
  Driver.Top_level_identifier_resolution.classify
    ~table:(Session.semantic_symbols session)
    ~globals ~functions ~expressions

let create_outer_environment session ~compilation_mode tables =
  Driver.Outer_expression_binding.create_environment
    ~table:(Session.semantic_symbols session)
    ~compilation_mode tables

let resolve_outer_expressions session ~environment ~expressions =
  Driver.Outer_expression_binding.resolve
    ~table:(Session.semantic_symbols session)
    ~environment ~expressions

let resolve_global_initializers session ~environment ~expressions ~globals
    module_ =
  Driver.Global_initializer_binding.resolve
    ~table:(Session.semantic_symbols session)
    ~environment ~expressions ~globals module_

let resolve_global_dimensions session ~environment ~expressions ~globals module_
    =
  Driver.Global_dimension_binding.resolve
    ~table:(Session.semantic_symbols session)
    ~environment ~expressions ~globals module_

let resolve_function_defaults session ~environment ~expressions ~functions
    module_ =
  Driver.Function_default_binding.resolve
    ~table:(Session.semantic_symbols session)
    ~environment ~expressions ~functions module_

let resolve_global_types session ~declarations ~aggregates module_ =
  Driver.Global_type_resolution.resolve
    ~table:(Session.semantic_symbols session)
    ~declarations ~aggregates module_

let resolve_function_identities ?compiler_option_mask session ~declarations
    ~functions ~compilation_mode module_ =
  Driver.Function_resolution.resolve ?compiler_option_mask
    ~table:(Session.semantic_symbols session)
    ~declarations ~functions ~compilation_mode module_

let analyze_function_headers session ~functions inputs =
  Sema.Function_header_analysis.analyze
    ~table:(Session.semantic_symbols session)
    ~functions inputs

let resolve_function_calls session ~declarations ?members ~function_types
    ~local_types ~global_types ~functions ~expressions module_ =
  Driver.Function_call_resolution.resolve
    ~table:(Session.semantic_symbols session)
    ~declarations ?members ~function_types ~local_types ~global_types ~functions
    ~expressions module_

let analyze_function_call_conversions session ~declarations ~headers ~calls =
  Sema.Function_call_conversion_policy.analyze
    ~table:(Session.semantic_symbols session)
    ~parent:(Sema.Declaration_collection.scope declarations)
    ~headers ~calls

let type_function_call_expressions session ~members ~policies =
  Sema.Function_call_expression_result.analyze
    ~table:(Session.semantic_symbols session)
    ~members policies

let resolve_implicit_output_targets session ~environment ~module_expressions
    ~function_types ~functions ~expressions =
  Sema.Implicit_output_target_resolution.resolve
    ~table:(Session.semantic_symbols session)
    ~environment ~module_expressions ~function_types ~functions ~expressions

let bind_implicit_output_arguments session ~policies ?outer_headers targets =
  Sema.Implicit_output_argument_binding.bind
    ~table:(Session.semantic_symbols session)
    ~policies ?outer_headers targets

let decide_function_call_conversions session ~policies ~expressions =
  Sema.Function_call_conversion_decision.decide
    ~table:(Session.semantic_symbols session)
    ~policies expressions

let classify_function_records ?compiler_option_mask _session ~resolution module_
    =
  Driver.Function_record_classification.classify ?compiler_option_mask
    ~resolution module_

let resolve_global_records ?compiler_option_mask session ~declarations ~globals
    ~compilation_mode module_ =
  Driver.Global_resolution.resolve ?compiler_option_mask
    ~table:(Session.semantic_symbols session)
    ~declarations ~globals ~compilation_mode module_

let classify_global_records ?compiler_option_mask _session ~resolution module_ =
  Driver.Global_record_classification.classify ?compiler_option_mask ~resolution
    module_
