type 'target t
(** Bytes committed by an actual native stream entry, tied to its exact original
    generation target and live arena. No public constructor exists. *)

val bounds : 'target t -> target:'target -> (int * int, string) result
(** Actual shared byte frontiers recorded by C. Observing them does not grant
    permission to consume the capture. *)

val consume :
  ?scope:Native_source_suspension.t ->
  'target t ->
  target:'target ->
  (string, string) result
