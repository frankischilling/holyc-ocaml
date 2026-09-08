val forward : Type.t -> Type.t
(** Native forwarded class for a nonzero scalar integer computation. Exact
    declared storage/call identity remains separate. Other types are retained.
*)

val declared : Type.t -> Type.t
(** Native unforwarded class at a call end or explicit cast. Generated
    declaration metadata accounts for public-spelled internal types such as U8.
*)

val negate : Type.t -> Type.t
(** OptFixupUnaryOp forwards the result; OptPass012 chooses the signed partner
    only when the original operand class is internal and unsigned. *)
