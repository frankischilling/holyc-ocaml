# Allocation pressure and graph width

Automatic register selection now follows the complete authenticated member and
instruction evidence instead of stopping at 32 members, 64 blocks, or 512
instructions. Those former hosted proof cutoffs were not TempleOS allocation
rules. Source authenticity, expression shape, supported types and register
ownership still apply. A finite checked proof-work budget and the explicit
nesting guard are separate from native frame, code, instruction and execution
quotas. The traversal allowance does not claim to bound total compiler CPU
work; other parsing, authentication and code-generation work has its own costs.

The source authority is the pinned TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/OptPass3.HC:21-49` traverses the entire member list. Unused and
  duplicate-name members retain their own source positions. Plain `reg` seeds
  a candidate with `I64_MAX/2`, while ordinary F64 is excluded before sorting.
- `OptPass3.HC:119-135,187-199,238-245` scores the surviving direct accesses.
  Lines 536-612 sort before zero-score trimming, filter address escapes and
  assign registers. More than six surviving candidates enables the two
  nonpointer registers. Arrays still contribute pressure without receiving
  automatic register homes.
- `Compiler/CInit.HC:244-257` defines R12/R13 as the nonpointer bank and
  RSI/RDI/R10/R11/R14/R15 as the general bank. `OptPass4.HC:196-219` replaces
  selected frame accesses with register accesses. `OptPass789A.HC:698-719`
  normalizes incoming narrow parameters before preserving their register value.
- `Compiler/LexLib.HC:67-84,103-147` resolves the first duplicate member name.
  `OptPass3.HC:29-36` makes the last explicit request the register owner.

`test/test_allocation_pressure_cli.ml` contains source-derived expectations,
not new TempleOS runtime captures or values copied from the allocator:

- 31, 32, 33 and 128 initialized I8 members compete. The first eight have
  distinct final-read counts of 9 through 2; the others have one. The winners
  are therefore unambiguous without depending on tie ordering. Updating 127
  produces 128 in a selected home and -128 in a spill. The independently
  calculated result is `128 * (52 - member_count)`.
- Equal-score 31/32/33-member cases keep a middle zero-score declaration in
  the independently traced TempleOS quicksort input before trimming.
- 128-member functions retain a late unused forced owner, duplicate-name lookup
  and the final explicit-register owner. Mixed integer, forced F64 and pointer
  members survive named calls and recursion alongside large automatic arrays.
- Graph assertions require exactly 64/65 blocks and 512/513 instructions.
  Balanced arithmetic trees keep these width regressions separate from nesting
  depth. The tests check every member at the small boundaries and every selected
  home plus representative spills at 128 members, alongside complete decoded
  parameter entry maps and physical save/restore ownership.
- A shallow function performs 1,500 updates without crossing the separate
  nesting limit. A 300-level ordinary I8 expression must reject proof exhaustion
  rather than quietly fall back to narrow stack storage.
- A 128-member large-frame owner exercises exact and one-below semantic frame,
  private native frame, active native stack, code and instruction quotas.
  Retained runs check exact step charging, repeat success after quota faults,
  and recover from division by zero in the same image.

The portable `runtest` lane checks proof admission, emitted instructions and
integer CLI execution in JIT and AOT modes. The `native-tests` lane additionally
executes both `host-jit` and `host-jit-task`, floating cases and retained recovery.
The 1,500-update stress case executes its already-emitted retained native image
instead of redundantly compiling it through both CLI routes.
Native execution is tested on the current host; a Linux pass does not establish
Windows runtime behavior. Existing unsupported syntax, allocation domains and
explicit resource limits remain separate boundaries. This does not claim a
complete general register allocator or close issue #703.
