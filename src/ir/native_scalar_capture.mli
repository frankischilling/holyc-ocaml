type 'program t
(** Opaque result of an actual native parser-time scalar entry. Completion
    requires its exact original program, actual work and live task arena. *)

val consume :
  'program t -> program:'program -> work:int -> (int64, string) result
