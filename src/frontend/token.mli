type mode = Holyc | Assembler | Raw

type value =
  | No_value
  | Text of string
  | Int64 of int64
  | Float64 of float
  | Bytes of string

type origin = {
  frame : Common.Source_id.t;
  generated_from : Common.Span.t option;
}

type t = {
  kind : Token_kind.t;
  raw : string;
  value : value;
  span : Common.Span.t;
  origin : origin;
  leading_trivia : Trivia.t list;
  mode : mode;
}

val mode_name : mode -> string
val value_text : value -> string option
val human : Common.Source_manager.t -> t -> string
val to_yojson : Common.Source_manager.t -> t -> Yojson.Safe.t
val json : Common.Source_manager.t -> t list -> string
