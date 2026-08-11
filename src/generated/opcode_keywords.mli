type kind = Language | Assembly

type entry = private {
  kind : kind;
  spelling : string;
  templeos_id : int;
  source_line : int;
}

val reference_commit : string
val source_path : string
val source_sha256 : string
val language : entry list
val assembly : entry list
