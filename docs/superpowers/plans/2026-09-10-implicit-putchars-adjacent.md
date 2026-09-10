# Parenthesis-free PutChars arguments

Continue #635 from 027c8ea089cdb82d7cc74a3d7330c4e7fb2ef6b1 against
TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c.

PrsFunCall consumes required fixed PutChars expressions without separators when
there are no call parentheses. Defaults leave the current token untouched. A
nonempty marker may therefore supply a later required formal after an omitted
initial default. Preserve that original literal/expression once, with no fake
comma location or reparsing. Completed calls leave commas to PrsStmt sequences.

- [x] Reproduce adjacent required arguments, intervening defaults and an
      omitted initial default followed by a supplied marker expression.
- [x] Retain optional original argument separators and native token ownership
      through parsing, both semantic paths and retained runtime lowering.
- [x] Test source ownership, legacy constructors, lookahead, retained inputs,
      comma statement boundaries and exact/one-below resource accounting.
- [ ] Verify full/build/install/CLI/reference/corpus/provenance checks, obtain
      review, verify committed consumers and publish the checked checkpoint.

General defaults/conversions, source-defined variadic bodies, remaining native
argument boundaries and all compiler/native/BIN/loader/bootstrap work remain
active. Do not redefine task completion around this change.

Local verification before publication: three initial execution regressions failed
before implementation (CA3L617W). A separate regression reproduced legacy
admission of adjacent arguments without original source (FIYFEMQP); the guard
preserves earlier validation order. All 267 focused groups pass, and additional
bare-value/variadic terminator assertions pass in the 67 stream-parser groups.
The final full run passes 2,557 compiler tests in 55.050 seconds, fourteen
standalone authority tests and both CLI suites. Format, generated sources,
build and install pass. All 104 provenance scenarios and 82 reference checksums
pass. Complete corpus reports match pinned baselines: 528 lexical files, 25
standalone parser acceptances and 126 with the prelude. Independent review found
no remaining blocker. Exact-revision consumer checks and publication follow
the source commit.
