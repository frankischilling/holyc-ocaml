# Compiler warnings

The bounded integer source pipeline emits `HCSEMA0034` for an unused parameter
or local when `OPTf_WARN_UNUSED_VAR` is enabled. Each original completed function
body keeps the mask reached at its closing boundary. A call to `Option(16,...)`
while preparing a local array bound therefore affects that function's warning.
Later changes cannot rewrite an earlier warning or its saved mask.

`no_warn` updates the source use count and the effective
`MLF_NO_UNUSED_WARN` flag. It emits no runtime instructions. An ordinary use
plus a suppression, or two suppressions, can produce `HCSEMA0035` for an
unneeded `no_warn`, even when option 16 is disabled. The spelling `_anon_`
remains exempt from this unneeded-suppression warning. Initializer resets and
specialized name queries retain the existing source counting rules.

Warnings are structured diagnostics with their source location and warning
severity. They do not enter captured program output or make a successful
command fail. An active source task emits a function's warnings once after its
checked integer body compiles. Nested directives and synchronous source children
deliver them to the enclosing diagnostic stream in reached order. An ordinary
child starts with its caller's current options and keeps its changes separate.
A later parser, execution or budget failure retains warnings already emitted.
A later AOT native module failure also keeps warnings from its completed
directive task.

[The example](../examples/compiler-warning-options.hc) prints `42;42;42;42;`,
returns 42 and emits three diagnostics: an unused parameter in `Loud`, an
unneeded suppression in `Suppression`, and an unused parameter in `Child`.
`Quiet` and `Parent` remain silent. The CLI fixture checks IR and native task
execution in both outer modes, exact and one-below instruction limits, failed
bodies and the absence of executable work for `no_warn`.

The standalone local-warning analysis API still accepts one batch mask. The
source driver supplies the original per-function snapshots through checked
symbol and command identities. A saved snapshot is evidence; it grants no
authority to mutate a closed compiler context. Copies of a completed header or
body cannot substitute for their original receipts.

The pinned rules are in `Compiler/PrsStmt.HC:193-207`. These `PrintWarn` calls do
not increment `cc->warning_cnt`; header mismatch warnings do. The hosted
diagnostic list does not claim to reproduce that native counter or terminal
formatting.

Full warning timing remains unfinished. TempleOS tests unused locals after
`COCCompile`; this pipeline emits after checked integer lowering, before native
code generation. An ordinary isolated module emits its warnings after the
whole module compiles, so an earlier function's warning is not retained when
later source prevents module compilation. Header mismatch, parentheses,
duplicate-type and return-warning consumers still need their original phase
integration. The remaining compiler options, typed compiler exceptions, wider
execution, exported ABI, object/BIN loader, bootstrap and release requirements
remain open.
