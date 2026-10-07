type 'target t
(** Bytes committed by an actual native stream entry, tied to its exact original
    generation target and live arena. No public constructor exists. *)

val consume : 'target t -> target:'target -> (string, string) result
