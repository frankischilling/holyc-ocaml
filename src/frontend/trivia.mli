type kind =
  | Whitespace
  | Line_continuation
  | Line_comment
  | Block_comment
  | Dollar_comment

type t = { kind : kind; raw : string; span : Common.Span.t }

val kind_name : kind -> string
val to_yojson : t -> Yojson.Safe.t
