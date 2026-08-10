type severity = Error | Warning | Note
type related = { span : Span.t; message : string }

type t = {
  code : string;
  severity : severity;
  message : string;
  primary : Span.t;
  secondary : related list;
  notes : string list;
  help : string option;
}

val make :
  ?secondary:related list ->
  ?notes:string list ->
  ?help:string ->
  code:string ->
  severity:severity ->
  message:string ->
  primary:Span.t ->
  unit ->
  t

val severity_name : severity -> string
val to_yojson : Source_manager.t -> t -> Yojson.Safe.t
