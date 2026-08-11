type value
type directive = If | Assert

type problem = {
  code : string;
  message : string;
  primary : Common.Span.t;
  secondary : Common.Diagnostic.related list;
  notes : string list;
  help : string option;
}

type failure =
  | Lexer_diagnostic of Common.Diagnostic.t
  | Problem of { problem : problem; lookahead : Token.t option }

type parsed

val parse :
  directive:directive ->
  opener:Common.Span.t ->
  max_nodes:int ->
  next:(unit -> Lexer.item) ->
  symbol_defined:(Token.t -> bool) ->
  unit ->
  (parsed, failure) result

val lookahead : parsed -> Token.t
val evaluate : parsed -> (value, problem) result
val truthy : value -> bool
