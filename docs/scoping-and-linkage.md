# Scoping and linkage

This document records the behavior established from TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`. The main references are `Compiler/PrsStmt.HC`, `Compiler/CompilerA.HH`, `Doc/ScopingLinkage.DD`, and `Doc/Options.DD`.

## Source behavior

JIT compilation searches the current task's symbol table and then its parent tasks. New declarations enter the current task's table and may shadow older names where the source permits duplicates. A plain `extern` function or global variable binds to the same name when it already exists and can also create a forward declaration.

A plain `import` names a symbol that the TempleOS loader resolves when an AOT module is loaded. If the symbol is unavailable then, uses remain incomplete until a later definition can resolve them. `PrsGlblVarLst` rejects `PRS0_IMPORT` when `CCF_AOT_COMPILE` is clear.

`_extern` and `_import` bind a HolyC declaration to a separately named system or imported symbol. `_intern` binds through a compile-time address expression. Those forms have more syntax and state than plain `extern` and `import`.

`OPTf_EXTERNS_TO_IMPORTS` changes ordinary and underscored extern declarations into their import forms. The pinned compiler uses this while compiling shared headers in different JIT and AOT contexts.

## Implemented parser boundary

The AST keeps `public` and `static` as ordered staged modifiers. It keeps one plain `extern` or `import` token in a separate optional binding node. That separation follows `PrsStmt`, which consumes staged modifiers before it selects a declaration mode.

The default parser mode is JIT. It accepts `extern`, rejects `import` with `HCPARSE0006`, and does not publish the rejected name. An explicit AOT parser configuration accepts both bindings. Each accepted binding retains its spelling, source segments, generated frame, invocation site, and definition site. Singleton and comma-separated primitive declarations use the same representation.

This is syntax handling plus the source-backed AOT gate. It does not look up an extern, resolve a forward reference, create an import or export record, assign an address, apply `OPTf_EXTERNS_TO_IMPORTS`, or model task-parent lookup. Those behaviors belong to semantic linkage, AOT emission, and the loader model.

The underscored forms, function declarations, class declarations, arrays, initializers, local scope, and duplicate-declaration rules remain unavailable. [Issue #57](https://github.com/frankischilling/holyc-ocaml/issues/57) records the implemented slice, while the declaration, symbol, and BIN milestones track the remaining work.

## Reproducible checks

`test/test_parser.ml` checks the exact keyword and parser-mode definitions, the AOT gate, generated-token provenance, streaming symbol visibility, and rejected syntax. `test/cli/parse-bindings.hc` exercises the public CLI in AOT mode. `test/cli/parse-import-jit.hc` records the JIT diagnostic and exit status.
