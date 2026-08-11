type kind = Language | Assembly

type entry = {
  kind : kind;
  spelling : string;
  templeos_id : int;
  source_line : int;
}

type tables = {
  language : entry list;
  assembly : entry list;
}

type error = {
  line : int option;
  message : string;
}

val parse : string -> (tables, error) result
val verify_sha256 : expected:string -> string -> (unit, error) result
val error_to_string : error -> string
