# U0 function calls

Issue [#619](https://github.com/frankischilling/holyc-ocaml/issues/619) connects
ordinary U0 source functions to the existing call interpreter. The source gate
uses a procedure to write a global before the caller reads it:

```c
I64 G;
U0 Set(I64 n)
{
    G=n;
    return;
}
Set(42);
G;
```

```text
holyc run --target=ir --mode=jit --step-limit=19 --frame-byte-limit=8 --global-byte-limit=8 --call-depth-limit=1 examples/integer-u0.hc
holyc run --target=ir --mode=aot --step-limit=19 --frame-byte-limit=8 --global-byte-limit=8 --call-depth-limit=1 examples/integer-u0.hc
```

Both modes return I64 42 in 19 runtime instructions and zero preparation
instructions. The parameter occupies eight active frame bytes; the global
occupies eight separately bounded bytes. Call depth is one. At baseline
`5b6d3f1c4005cfac99c66801d3070ea5ccf05c6f`, all four initial maintained source
groups fail with HCIRVM0011 at the old word-return requirement.

## Completion and reporting

The existing lowerers retain the selected function, U0 call type, cleanup,
call-end result identity and expression discard. The VM distinguishes a call
that has not completed, successful completion without a word, and a returned
word. Only an exact checked U0 call-end produces the private no-value marker;
its discard verifies that the reached result exists. No integer zero or memory
slot represents a void result.

U0 functions support ordinary fallthrough and early bare returns through their
shared leave block. Each activation retains its own pending word return,
temporary values, automatic storage and caller continuation. An ignored word
call cannot satisfy the containing function's required return value. Frames
are invalidated on return; referenced caller objects and persistent storage
keep their existing lifetimes.

The public types remain unchanged. `execute_function` reports successful U0
completion as `Returned None`. A program still reports `Stream_end`. Its final
value is the last reached ordinary expression: `42;Set(1);` reports none,
while `Set(1);42;` reports 42. Human output uses `final-value=none`; JSON uses
`"final_value": null`. A preceding word does not survive a later U0 discard.
Function-internal expressions and declaration initializer regions do not
replace the top-level final value.

## Checked boundaries

Admission covers scalar primitive U0 returns, with exact function/frame/scope
and call-target identity. A pointer to U0 is a different type. No-value markers
cannot serve as arithmetic operands, conditions, stored words, fixed arguments
or word-returning expressions. Malformed call order, cleanup, result types,
owners and flags fail preflight. Graph-only `eval` and `execute` retain their
existing domains.

The hosted VM retains HCIRVM0013 when a reached I64/U64 function returns without
a word. U0 value-return instructions remain unsupported. These are explicit
hosted restrictions: native HolyC warns about these forms rather than rejecting
them. Full compatibility for warning-bearing returns remains unfinished.

U0 calls use the existing shared instruction, active frame and depth limits;
their globals, literals and initializer preparation retain separate bounds.
An empty procedure needs no return slot. Every reached canonical instruction,
including call-end, discard and return, consumes its ordinary step. Reached
faults retain the active function, source, instruction, block, phase and count;
a failed execution exposes no successful final value.

## Source evidence and tests

All source evidence uses TempleOS
`c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/PrsExp.HC:530-588` emits call setup, right-to-left argument
  evaluation, selected calls, cleanup and call-end results.
- `Compiler/PrsStmt.HC:150-169,1110-1119` supplies the common leave path,
  fallthrough, bare return and warning distinctions.
- `Kernel/KExts.HC:83-84` declares U0 Print and PutChars. Their execution is a
  subsequent runtime connection.

`test/test_integer_u0.ml` covers source calls, control flow, parameter order,
pointer writes, recursion, no-value reporting, initializers, fresh executions,
resource limits and malformed IR. `test/test_integer_program_cli.ml` maintains
the example in both modes, one-below limits, deterministic IR, reporting and
fault provenance. Issue #619 records completed local and final-source CI
evidence. These hosted tests and source audits do not claim a new native capture.

Local verification passes all 1,838 tests in 58.668 seconds, including thirteen
U0 groups, plus the CLI checks. Exact/one-below literal and static preparation
limits cover called and uncalled U0 bodies; recursive saved locals exercise
frame restoration. Formatting, generated source, build/install, 82 reference
checksums and all 11 incremental provenance scenarios pass. Lexer acceptance
remains 528/528; parser acceptance remains 25/528 standalone and 126/528 with
the prelude, with the complete JSON matching the reviewed baseline after
Windows newline normalization. Independent review approved the implementation
and expanded resource coverage. Final revision/CI/integration evidence is
recorded separately in #619 and its PR.

Runtime prototypes/output bindings, variadic execution, HolyC formatting and
captured byte output remain unfinished. Reuse the existing output typing,
target and argument-binding passes when connecting them. Broader memory,
stateful compilation/#exe, optimizer parity, native backends, TempleOS BIN and
actual-loader acceptance, whole-tree compatibility and bootstrap remain required.
