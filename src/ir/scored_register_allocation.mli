type candidate = {
  member : int;
  score : int64;
  offset : int64;
  lea_balance : int64;
  pointer_depth : int;
  has_dimensions : bool;
  name : string;
}

type explicit_conflict = {
  previous_member : int;
  replacement_member : int;
  register : int;
}

type explicit_resolution = {
  owners : (int * int) list;
  shadowed : (int * int) list;
  conflicts : explicit_conflict list;
}

val resolve_explicit : (int * int) list -> explicit_resolution
(** Fold original ordered member/register requests as OptPass3 does. Every
    subsequent request replaces that register's owner and retains a conflict;
    displaced members never become automatic candidates. Final owners are in
    their last-occurrence source order. This pure algorithm grants no source
    authority: callers must prove complete ordered membership, distinct frame
    extents, supported requests and source ownership before using its result. *)

val sort : ?charge:(unit -> unit) -> candidate list -> candidate list
(** TempleOS Kernel/QSort.HC QSort2b with OptLib.HC's descending score
    comparator. Equal scores deliberately have no stable tie breaker. The
    optional work hook charges partition visits, comparisons, scan steps and
    swaps; raising from it aborts without returning a partial result. *)

val allocate :
  ?charge:(unit -> unit) ->
  reserved:int list ->
  candidate list ->
  (int * int) list
(** Sort the complete original candidate list before trimming zero scores and
    filtering nonzero-LEA/zero-offset members. Return member/register pairs in
    allocation order, including R10/R11. Callers authenticate source membership,
    score bounds, register requests and NO_REG_VAR before using this result. The
    optional work hook also charges trimming and bank/member scans. *)
