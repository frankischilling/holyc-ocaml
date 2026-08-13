val schema : string

val human : Common.Source_manager.t -> Aggregate_member_index.t -> string
(** Render completed aggregate layouts in source order. Target offsets and sizes
    remain signed 64-bit values. *)

val to_yojson :
  Common.Source_manager.t -> Aggregate_member_index.t -> Yojson.Safe.t

val json : Common.Source_manager.t -> Aggregate_member_index.t -> string
