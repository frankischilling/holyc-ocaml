module Source_id = Common.Source_id
module Source_file = Common.Source_file
module Source_manager = Common.Source_manager
module Span = Common.Span
module Diagnostic = Common.Diagnostic
module Diagnostic_render = Common.Diagnostic_render
module Session = Driver.Session
module Version = Driver.Version
module Primitive_type = Sema.Primitive_type
module Compiler_option = Sema.Compiler_option
module Function_flag = Sema.Function_flag
module Ir_opcode = Ir.Opcode
module Templeos_bin_spec = Backend.Bin_spec
module Asm_directive = Asm.Directive
module Keyword = Frontend.Keyword
module Operator = Frontend.Operator
module Trivia = Frontend.Trivia
module Token_kind = Frontend.Token_kind
module Token = Frontend.Token
module Lexer = Frontend.Lexer
module Include_resolver = Frontend.Include_resolver
module Definition = Frontend.Definition
module Symbol_visibility = Frontend.Symbol_visibility
module Lexer_frame = Frontend.Lexer_frame
module Preprocessor = Frontend.Preprocessor

let lex _session ~source = Frontend.Lexer.lex_all source

let preprocess session ~config ~source =
  Frontend.Preprocessor.lex_all ~sources:(Session.sources session)
    ~definitions:(Session.definitions session)
    ~symbols:(Session.symbols session)
    ~config source
