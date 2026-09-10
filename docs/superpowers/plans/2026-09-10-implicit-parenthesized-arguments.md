# Parenthesized implicit arguments

Continue #635 from cf79b7df730d56254e817228c834e2f3d89c779a. The pinned
TempleOS reference remains c26482bb6ad3f80106d28504ec5db3c6a360732c.
PrsExp.HC:385-535 selects the implicit target before consuming an empty marker,
then treats a following opening parenthesis as call syntax. It is not an
ordinary grouped fixed expression.

Retain both original parentheses on the implicit statement. Parse fixed
parameters and any variadic tail using the frozen selected header. Keep Print's
comma/semicolon separator rule distinct from PutChars' comma/right-parenthesis
rule. Supplied values override defaults inside parentheses; omitted slots keep
their original lookahead and use the selected declaration's saved values.
Both semantic paths keep the same original statement and expressions.

- [x] Reproduce parenthesized Print and PutChars calls, supplied default slots,
      and omitted trailing/interior slots through JIT/AOT and function bodies.
- [x] Retain parentheses and parse the native argument boundaries without
      wrapping or reparsing the source expressions.
- [x] Admit source-backed PutChars fixed arguments through existing call
      binding while preserving the legacy semantic constructor contract.
- [x] Cover native separator differences, source locations, target selection
      during lookahead, nested expression parentheses and failure ordering.
- [ ] Verify focused/full/CLI, build/install, corpus, reference, provenance and
      exact-revision consumers; review and publish the checked result.

Omitted initial arguments and zero-value implicit calls still need a genuine
absent fixed-value representation. General defaults/conversions, native
linkage and the remaining compiler/native/BIN/loader/bootstrap requirements
remain active; this plan does not redefine completion.

Local verification before publication: the four initial execution regressions
failed before implementation. A separate regression reproduced the legacy
constructor error-order change and passes with the original validation order
restored. All 229 focused groups pass. The final full run passes 2,529 compiler
tests in 53.934 seconds, fourteen standalone authority tests and both CLI
suites. Format, generated sources, build and install pass. All 104 provenance
scenarios and 82 reference checksums pass. Complete lexer/parser reports match
the pinned baselines (528 lexical files, 25 standalone parser acceptances and
126 with the prelude). Independent read-only review found no remaining blocker.
Exact-revision consumers and publication follow the source commit.
