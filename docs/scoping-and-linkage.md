# Scoping and linkage

This document records the behavior established from TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`. The main references are `Compiler/PrsStmt.HC`, `Compiler/CompilerA.HH`, `Doc/ScopingLinkage.DD`, and `Doc/Options.DD`.

## Source behavior

JIT compilation searches the current task's symbol table and then its parent tasks. New declarations enter the current task's table and may shadow older names where the source permits duplicates. A plain `extern` function or global variable binds to the same name when it already exists and can also create a forward declaration.

A plain `import` names a symbol that the TempleOS loader resolves when an AOT module is loaded. If the symbol is unavailable then, uses remain incomplete until a later definition can resolve them. `PrsGlblVarLst` rejects `PRS0_IMPORT` when `CCF_AOT_COMPILE` is clear.

`_extern` and `_import` bind a HolyC declaration to a separately named system or imported symbol. `_intern` binds through a compile-time address expression. Those forms have more syntax and state than plain `extern` and `import`.

`OPTf_EXTERNS_TO_IMPORTS` changes ordinary and underscored extern declarations into their import forms. The pinned compiler uses this while compiling shared headers in different JIT and AOT contexts.

## Implemented parser boundary

The AST keeps `public`, `static`, `interrupt`, `haserrcode`, `argpop`, and `noargpop` as ordered staged modifiers. It keeps one ordinary or alternate-name binding separate from the declared type. An `_extern` or `_import` node also retains the target identifier that appears before the type. Function parameters retain ordered `reg` and `noreg` qualifiers, their position around the type, and an optional canonical U64 register name. Those separations follow `PrsStmt` and `PrsVarLst`. Calling modifiers and parameter register requests remain syntax here; no linkage, allocation, or ABI rule is applied during parsing.

The default parser mode is JIT. It accepts `extern` and `_extern`, rejects `import` and `_import` with `HCPARSE0006`, and does not publish the rejected name. An explicit AOT parser configuration accepts all four bindings. Each accepted binding and alternate target retains its spelling, source segments, generated frame, invocation site, and definition site. Singleton and comma-separated primitive globals share the same binding representation. Bound primitive function prototypes retain that binding alongside their return type, parameters, and punctuation. A missing alternate target receives `HCPARSE0007`.

This is syntax handling plus the source-backed AOT gate. An accepted prototype is published to the streaming environment with function identity, but the parser does not look up an ordinary or alternate target, compare headers, resolve a forward reference or alias, create an import or export record, assign an address, apply `OPTf_EXTERNS_TO_IMPORTS`, or model task-parent lookup. Those behaviors belong to semantic linkage, AOT emission, and the loader model.

`_intern` remains unavailable because its target is a compile-time expression. Parameter defaults now retain their source expressions, including defaults before required parameters, but semantic evaluation, propagation between matching declarations, call-site argument filling, and string-default storage are not implemented. Primitive global arrays retain their ordered dimensions, but the parser does not evaluate sizes, calculate storage, or apply linkage to array objects. Unbound function definitions, function-flag validation and ABI effects, register allocation, class declarations, initializers, non-global arrays, local scope, and duplicate-declaration rules also remain unavailable. [Issue #57](https://github.com/frankischilling/holyc-ocaml/issues/57) records ordinary bindings, [issue #59](https://github.com/frankischilling/holyc-ocaml/issues/59) records alternate-name bindings, [issue #61](https://github.com/frankischilling/holyc-ocaml/issues/61) records the first function prototype slice, [issue #63](https://github.com/frankischilling/holyc-ocaml/issues/63) records calling modifier syntax, [issue #65](https://github.com/frankischilling/holyc-ocaml/issues/65) records parameter register qualifiers, [issue #67](https://github.com/frankischilling/holyc-ocaml/issues/67) records function-pointer parameters, [issue #69](https://github.com/frankischilling/holyc-ocaml/issues/69) records core default expressions, and [issue #71](https://github.com/frankischilling/holyc-ocaml/issues/71) records global array declarators. The declaration, symbol, ABI, and BIN milestones track the remaining work.

## Reproducible checks

`test/test_parser.ml` checks the exact keyword, mode, staging, function-argument, calling-modifier, register-definition, array-dimension, and variadic behavior; target and punctuation provenance; the AOT gate; streaming symbol visibility; and rejected syntax. `test/cli/parse-bindings.hc`, `test/cli/parse-alternate-bindings.hc`, `test/cli/parse-function-prototypes.hc`, `test/cli/parse-function-modifiers.hc`, `test/cli/parse-register-parameters.hc`, and `test/cli/parse-arrays.hc` exercise the public CLI. The JIT import, array, and function error fixtures record diagnostics and exit status.
