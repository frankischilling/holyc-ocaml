(** Source-ordered metadata produced by HolyC help directives. *)

type provenance = {
  directive_span : Common.Span.t;
  value_spans : Common.Span.t list;
  include_stack : Common.Diagnostic.related list;
  definition_trace : Common.Diagnostic.related list;
}

type index_entry = { value : string; provenance : provenance }

type file_entry = {
  declared_path : string;
  effective_path : string;
  resolved_path : string;
  source_link : string;
  index : index_entry option;
  provenance : provenance;
}

type t

val empty : t
val record_index : t -> index_entry -> t
val record_file : t -> file_entry -> t

val current_index : t -> index_entry option
(** The final nonempty index, or [None] after an empty index directive. *)

val index_events : t -> index_entry list
val help_files : t -> file_entry list

val with_default_extension : string -> string
(** Apply the pinned [FileExtDot] and [ExtDft] rule for [DD.Z]. *)

val sanitize_file_name : string -> string
(** Apply the file-name sanitation used by the pinned [FileNameAbs] path. *)

val source_link : Common.Source_manager.t -> Common.Span.t -> string
val human : Common.Source_manager.t -> t -> string
val to_yojson : Common.Source_manager.t -> t -> Yojson.Safe.t
val json : Common.Source_manager.t -> t -> string
