type t =
  | Eof
  | Identifier
  | Keyword of Keyword.t
  | Integer
  | Float
  | String
  | Inserted_binary
  | Inserted_binary_size
  | Character
  | Operator of Operator.t
  | Punctuation of char
  | Newline

val name : t -> string
val templeos_token_id : t -> int option
