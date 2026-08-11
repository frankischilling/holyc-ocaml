type kind =
  | Whitespace
  | Line_continuation
  | Line_comment
  | Block_comment
  | Dollar_comment

type t = { kind : kind; raw : string; span : Common.Span.t }

let kind_name = function
  | Whitespace -> "whitespace"
  | Line_continuation -> "line-continuation"
  | Line_comment -> "line-comment"
  | Block_comment -> "block-comment"
  | Dollar_comment -> "dollar-comment"

let to_yojson trivia =
  `Assoc
    [
      ("kind", `String (kind_name trivia.kind));
      ("raw", `String trivia.raw);
      ("start", `Int trivia.span.start);
      ("stop", `Int trivia.span.stop);
    ]
