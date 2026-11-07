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

val analyze_whole :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  statements:Integer_program_lowering.statement list ->
  (Sema.Function_frame_layout.location * int) list option
(** Phase-surviving direct access ledger for a complete original statement tree.
    The caller must independently reconstruct this tree from the sealed source
    and compare its complete canonical graph and call receipts with [body]. This
    function proves the separate OptPass012 access-conservation obligation,
    original member order and one-to-one canonical address/consumer ownership.
    Every proved occurrence contributes score 2 and net LEA balance 0. All arms,
    loop components and returns count once, even when unreachable. Arithmetic,
    implicit numeric conversions, provided named-call arguments and discarded
    direct assignments/integer updates are admitted. Short circuit, chained
    comparisons, consumed writes, decrement conditions and address escapes fail
    closed. This ledger alone is not an allocation witness. *)

type member_score = {
  location : Sema.Function_frame_layout.location;
  score : int64;
  lea_balance : int64;
}

val analyze_members :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  statements:Integer_program_lowering.statement list ->
  member_score list option
(** Independently authenticate original primitive member shapes/order and
    phase-surviving accesses, including owned pointer copies, dereferences and
    indexing. Direct accesses score twice and cancel their LEA; explicit address
    formation scores once and remains unbalanced even in [*(&x)]. Uninitialized
    declarations remain members without granting readable storage. Positive,
    source-authenticated primitive local arrays remain in original member order
    and candidate pressure even though they cannot receive physical homes.
    Complete multidimensional chains retain the original dimensions and
    per-dimension strides. Pure integer indices admit literals, owned scalar
    reads, grouping, unary +/- and binary +,-,*. A bounded two-pass source tree
    models parser SIZEOF removal, constant folding and access-preserving
    rewrites, including the surviving read in multiplication by zero. Each
    enclosing authenticated provided fixed named-call argument contributes its
    original pass-0/pass-1 preprocessing before whole-function passes 1 and 2.
    Its actual RBP ADD and final consumer prove zero/interior/dynamic LEA
    events; a flattened algebraic index cannot replace this receipt. Every
    constant dimension must remain in bounds, and unproved constant overflow
    rejects. Empty, formal, pointer, callback, aggregate and explicitly
    registered arrays, array initializers, grouped array bases, partial array
    escapes, casts and side effects in indices remain outside this proof. The
    caller must independently reconstruct and authenticate the complete
    canonical graph. *)

val permits_automatic_analysis_with_budget :
  budget:Register_proof_budget.t -> Function_body.t -> bool
(** Shared-budget entry points. Exhaustion is deliberately not caught: callers
    must revoke the entire attempt rather than try another proof after it. *)

val source_requests_match_with_budget :
  budget:Register_proof_budget.t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  bool

val analyze_with_budget :
  budget:Register_proof_budget.t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  (Sema.Function_frame_layout.location * int) list option

val analyze_whole_with_budget :
  budget:Register_proof_budget.t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  statements:Integer_program_lowering.statement list ->
  (Sema.Function_frame_layout.location * int) list option

val analyze_members_with_budget :
  budget:Register_proof_budget.t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  statements:Integer_program_lowering.statement list ->
  member_score list option
