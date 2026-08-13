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
module Semantic_aggregate_header_resolution = Sema.Aggregate_header_resolution

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
