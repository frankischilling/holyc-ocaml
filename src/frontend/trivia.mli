type kind =
  | Whitespace
  | Line_continuation
  | Line_comment
  | Block_comment
  | Dollar_comment

type t = {
  kind : kind;
  raw : string;
  span : Common.Span.t;
  source_segments : Common.Span.t list;
}

val kind_name : kind -> string

val to_yojson :
  ?containing_source:Common.Source_id.t ->
  Common.Source_manager.t ->
  t ->
  Yojson.Safe.t
