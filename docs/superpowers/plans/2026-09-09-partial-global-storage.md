# Partial global storage in an active source task

Continue the approved #635 design from `295f484`. The original parser publication
and checked dimensions must authorize one JIT allocation while its declaration
is still open. A nested command must use that allocation through the existing
retained binding and address path. Completing the original declaration must reuse
it without another allocation, publication, or byte charge.

1. Add failing integration coverage for a nested write/read inside an open array
   initializer, preservation of a write before declaration resume, and an
   uninitialized scalar read. Exercise the actual task interpreter and stream
   executor, with an exact cumulative allocation limit.
2. Retain a checked declaration-specific storage certificate. Validate original
   namespace, publication, type children, ordered dimensions, supported storage
   policy, and source position. Do not construct a completed semantic global or
   AST wrapper from partial syntax.
3. Admit an unknown JIT allocation and one retained reference without admitting a
   command. Carry this storage through task snapshots and retained addresses.
4. Join completed semantic storage only through its exact original declaration
   and matching checked type/extent. Keep allocation ownership separate from
   references, resolve the original arena index, and apply completed initializer
   publications to that arena. Ordered live initializer execution remains a
   separate required integration; this step must not claim it is implemented.
5. Check replay, foreign-owner, unsupported/invalid shape, resource failure, and
   completed-command behavior. Obtain an independent read-only review under the
   existing #635 review workflow, then run relevant tests and repository checks.

The public stateful JIT facade, function timing, effectful dimensions, and the
remaining full compiler/native/BIN/loader/bootstrap requirements stay open.

## Local validation

The twelve partial-storage groups pass. The full local suite passes 2,375 of
2,389 groups in 80.673 seconds; exactly fourteen public stateful-exe groups retain
HCPP0008. Complete CLI checks pass. Formatting, generated source and build/install
checks pass, as do all 82 pinned checksums. Complete lexer/parser reports retain
528/528 lexical acceptance and 25 standalone / 126 prelude parser successes.
The reference is c26482bb6ad3f80106d28504ec5db3c6a360732c.

Independent review exposed a raw admission path that lacked the driver's
namespace and lifetime checks. The task now binds its declaration namespace once;
opaque storage certificates retain that namespace and original predecessors.
VM admission checks current parser observation before allocating. A reproduced
foreign-namespace regression and matching-namespace lifetime controls pass after
the correction. Re-review found no remaining blocker in this increment; this is
separate from GitHub approval.

Next, use the original initializer start/leaf witnesses for fragment typing and
ordered execution during parsing. Completing a command must reuse those effects
as well as this increment's existing allocations. Public JIT source orchestration,
partial function timing, effectful dimensions, implicit providers/defaults/later
reads and extern joins remain required. No new native execution is claimed.
