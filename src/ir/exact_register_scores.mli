type source_stamp

val stamp_source : Function_body.t -> source_stamp option

val equal_source_stamp : source_stamp -> source_stamp -> bool
(** Independent original statement identities/order and primitive spans, so
    mutations to untyped statement structure revoke a previously saved proof. *)

val permits_automatic_analysis : Function_body.t -> bool
(** Reject changed original syntax and source try/catch before any exact or
    conservative fallback proof. Try sets CCF_NO_REG_OPT even when empty or
    unreachable; absence of an IR operation cannot clear that source flag. *)

val source_requests_match :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  bool
(** Authenticate every exclusion, forced request and explicit reservation
    against the independently sealed original AST, before any fallback. *)

val analyze :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  (Sema.Function_frame_layout.location * int) list option
(** Authenticate a complete straight-line integer/F64 source and canonical IR.
    Return original member order and source occurrence counts, including zero
    counts. Each occurrence has a distinct canonical frame address and exactly
    one read/write consumer. Floating expressions admit homogeneous direct
    reads, literals, parentheses, unary +/-, +, -, *, /, discarded plain
    assignments, initialized locals and a final scalar return. Conversions,
    floating updates, nested writes, calls and control flow are not admitted.
    Float payloads are compared as raw int64 bits. This is score evidence only:
    callers still check requests, compiler options and value semantics. *)
