type severity = Error | Warning | Note
type related = { span : Span.t; message : string }

type t = {
  code : string;
  severity : severity;
  message : string;
  primary : Span.t;
  secondary : related list;
  include_stack : related list;
  notes : string list;
  help : string option;
}

val make :
  ?secondary:related list ->
  ?include_stack:related list ->
  ?notes:string list ->
  ?help:string ->
  code:string ->
  severity:severity ->
  message:string ->
  primary:Span.t ->
  unit ->
  t

val with_include_stack : t -> related list -> t
val severity_name : severity -> string
val to_yojson : Source_manager.t -> t -> Yojson.Safe.t
