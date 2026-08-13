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
