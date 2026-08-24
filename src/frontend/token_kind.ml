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

let name = function
  | Eof -> "eof"
  | Identifier -> "identifier"
  | Keyword keyword -> "keyword(" ^ Keyword.spelling keyword ^ ")"
  | Integer -> "integer"
  | Float -> "float"
  | String -> "string"
  | Inserted_binary -> "inserted-binary"
  | Inserted_binary_size -> "inserted-binary-size"
  | Character -> "character"
  | Operator operator -> "operator(" ^ Operator.spelling operator ^ ")"
  | Punctuation punctuation -> Printf.sprintf "punctuation(%C)" punctuation
  | Newline -> "newline"

let templeos_token_id = function
  | Eof -> Some 0
  | Identifier | Keyword _ -> Some 0x100
  | String -> Some 0x101
  | Inserted_binary -> Some 0x125
  | Inserted_binary_size -> Some 0x126
  | Integer -> Some 0x102
  | Character -> Some 0x103
  | Float -> Some 0x104
  | Operator operator -> Operator.templeos_token_id operator
  | Punctuation punctuation -> Some (Char.code punctuation)
  | Newline -> Some (Char.code '\n')
