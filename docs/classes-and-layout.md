# Classes and layout

All compatibility findings in this document use TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Retained syntax

The current AST distinguishes class and union definitions, an optional backing type, one optional base class, grouped member declarators, pointer and array suffixes, function-pointer members, recursive anonymous unions, explicit `$$ = expression;` offset directives, empty separators, member metadata, and globals attached after the closing brace. Parsing keeps source segments and generated-input origins. It does not assign a semantic type or storage layout.

## Direct-member identity

`Compiler/PrsVar.HC:PrsVarLst` creates a `CMemberLst` entry for each named declarator and calls `Compiler/LexLib.HC:MemberAdd` in source order. Grouped declarations therefore enter left to right. When `PrsVarLst` encounters an anonymous union, it recurses with the same `CHashClass`; the nested fields use the containing aggregate's member list and lookup namespace.

`Sema.Member_collection` models that boundary. `holyc_lib.collect_members` consumes the same AST and top-level declaration collection, creates one aggregate scope per definition, and gives every named direct member a stable `Member` symbol. Each entry retains its grouped-declarator index and a path of member-list indexes through any anonymous unions. Offset directives, empty separators, and metadata names do not create member symbols.

Collection preserves repeated names. The later duplicate pass must reproduce `MemberAdd`, which permits repeated `pad`, `reserved`, and `_anon_` names but rejects other direct or inherited duplicates. Keeping every occurrence here avoids deciding that rule before types and bases are resolved.

`Compiler/LexLib.HC:MemberFind` searches the current class and then follows `base_class`. The current aggregate scope has the module scope as its parent; it does not point at a base aggregate scope. Inherited lookup remains unimplemented rather than being approximated through ordinary lexical parents.

## Layout status

No class or union layout is calculated yet. The implementation does not assign member types, offsets, storage sizes, alignment, array extents, union overlap, base offsets, negative offsets, backing conversions, subinteger access, or metadata storage. It also does not evaluate aggregate offset expressions or expose `MemberMetaData` and `MemberMetaFind` behavior.

The layout pass will use `Compiler/PrsVar.HC:PrsVarLst`, `Compiler/LexLib.HC`, `Compiler/CInit.HC`, the `CMemberLst` and `CHashClass` definitions in `Kernel/KernelA.HH`, and their backend consumers. Compatibility claims will require golden fixtures for total size, alignment, every member offset and storage size, signedness, dimensions, inheritance, and union overlap.

## Reproducible checks

`test/test_member_collection.ml` covers direct and grouped members, nested anonymous unions, repeated names, source provenance, ownership validation, the inheritance boundary, and deterministic symbol dumps. These tests prove semantic identity and ordering, not layout compatibility.
