type 'program t
(** Opaque result of an actual native parser-time entry. Completion requires its
    exact original program, actual work and live task arena. *)

val consume :
  ?scope:Native_source_suspension.t ->
  'program t ->
  program:'program ->
  work:int ->
  (int64, string) result
(** Require an actual word; an executed U0 result cannot satisfy this consumer.
*)

val consume_value :
  ?scope:Native_source_suspension.t ->
  'program t ->
  program:'program ->
  work:int ->
  (int64 option, string) result
(** Consume the actual result, retaining U0 as absence rather than inventing a
    word. *)
