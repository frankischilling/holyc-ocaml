type candidate = {
  member : int;
  score : int64;
  offset : int64;
  lea_balance : int64;
  pointer_depth : int;
  has_dimensions : bool;
  name : string;
}

val sort : candidate list -> candidate list
(** TempleOS Kernel/QSort.HC QSort2b with OptLib.HC's descending score
    comparator. Equal scores deliberately have no stable tie breaker. *)

val allocate : reserved:int list -> candidate list -> (int * int) list
(** Sort the complete original candidate list before trimming zero scores and
    filtering nonzero-LEA/zero-offset members. Return member/register pairs in
    allocation order, including R10/R11. Callers authenticate source membership,
    score bounds, register requests and NO_REG_VAR before using this result. *)
