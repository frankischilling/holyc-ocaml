type error =
  | Empty_path
  | Path_contains_nul
  | Home_path_requires_mapping of string
  | Templeos_root_requires_mapping of string
  | Drive_path_unsupported of string
  | Not_found of { spelling : string; searched : string list }
  | Outside_allowed_roots of {
      spelling : string;
      resolved : string;
      allowed_roots : string list;
    }
  | Is_directory of string
  | Not_regular_file of string
  | Io_error of { path : string; message : string }

type resolution = { canonical_path : string; source_path : string }
type metadata_resolution = { resolved_path : string }
type t

val create :
  ?working_directory:string ->
  ?include_roots:string list ->
  ?templeos_root:string ->
  unit ->
  (t, string) result

val resolve : t -> spelling:string -> (resolution, error) result

val resolve_metadata :
  t -> spelling:string -> (metadata_resolution, error) result
(** Resolve a metadata path inside the configured roots without requiring the
    target to exist or reading it. *)

val equal_path : string -> string -> bool
val working_directory : t -> string
val include_roots : t -> string list
val templeos_root : t -> string option
val allowed_roots : t -> string list
