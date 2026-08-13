# Classes and layout

All compatibility findings in this document use TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Retained syntax

The current AST distinguishes class and union definitions, an optional backing type, one optional base class, grouped member declarators, pointer and array suffixes, function-pointer members, recursive anonymous unions, explicit `$$ = expression;` offset directives, empty separators, member metadata, and globals attached after the closing brace. Parsing keeps source segments and generated-input origins. Semantic passes assign source-ordered identities to aggregate declarations, bind definition backings and bases at their source lookup points, and resolve the base and pointer portion of each direct member type. Type uses outside aggregate member heads and storage layout remain unresolved.

## Forward and definition identity

`Compiler/PrsStmt.HC:PrsClass` adds a fresh `CHashClass` for every `extern class` or `extern union`. An ordinary body reuses only the newest visible class-category record when that record still has `Cf_EXTERN`; otherwise it adds a new definition and shadows the older record. Because class and union share `HTT_CLASS`, the completing body does not reject a different aggregate keyword.

`Sema.Aggregate_resolution` models this module-local rule without rewriting the declaration collection. Each forward starts a separate identity. A definition completes only the newest same-name unresolved forward, and a definition after a completed identity starts another identity. The result retains both sites and both aggregate spellings. A completed identity uses the definition symbol as its canonical symbol because the existing direct-member scope is already associated with that definition entry. Older unresolved forwards remain available as separate identities.

The result is deterministic and pure after validation. `Driver.Aggregate_resolution` checks the declaration kind, module-item index, spelling, origin, table ownership, and module scope before returning it. Parent-task lookup differs between the JIT `HashSingleTableFind` and AOT `HashFind` paths and is not modeled by this module-local pass.

## Backing and base types

`Compiler/PrsVar.HC:PrsType` reads a definition's optional backing before it calls `PrsClass`. A named backing therefore sees the previously published identity, even when the new definition reuses the same spelling. Primitive and intrinsic backings remain distinct because `Compiler/CInit.HC:internal_types_table` creates separate source records. `PrsClassNew` provides the base record plus four adjacent pointer records, and a fifth star is rejected through `PTR_STARS_NUM`.

`Compiler/PrsStmt.HC:PrsClass` publishes or completes the current identity before it reads `: Base`. The base lookup can consequently select the current definition, while an ordinary prior base selects the newest visible aggregate identity. The implementation records this timing directly: `Driver.Aggregate_header_resolution` resolves a backing, publishes the canonical symbol from aggregate reconciliation, and then resolves the base. `Sema.Type` distinguishes a public primitive spelling, an intrinsic storage spelling, and an aggregate identity at pointer depths zero through four. `Sema.Aggregate_header_resolution` retains the full definition, keyword, backing, pointer, colon, and base-name origins and validates module ownership without changing the symbol table.

A header that selected a forward keeps the canonical identity chosen by aggregate reconciliation. If a later body completes that forward, existing header references already point to the definition symbol. This is separate from the `fwd_class` relation followed by `Compiler/OptLib.HC:OptClassFwd`; the implementation records that relation as a backing type and does not merge declarations because of it.

This pass does not follow backing chains, reject backing or inheritance cycles, calculate offsets, or search inherited members. It also does not resolve member, global, parameter, local, cast, `sizeof`, or `offset` type uses.

## Direct-member identity

`Compiler/PrsVar.HC:PrsVarLst` creates a `CMemberLst` entry for each named declarator and calls `Compiler/LexLib.HC:MemberAdd` in source order. Grouped declarations therefore enter left to right. When `PrsVarLst` encounters an anonymous union, it recurses with the same `CHashClass`; the nested fields use the containing aggregate's member list and lookup namespace.

`Sema.Member_collection` models that boundary. `holyc_lib.collect_members` consumes the same AST and top-level declaration collection, creates one aggregate scope per definition, and gives every named direct member a stable `Member` symbol. Each entry retains its grouped-declarator index and a path of member-list indexes through any anonymous unions. Offset directives, empty separators, and metadata names do not create member symbols. For a completed forward, the reconciliation result deliberately selects this definition symbol as the canonical aggregate identity.

Collection preserves repeated names. The later duplicate pass must reproduce `MemberAdd`, which permits repeated `pad`, `reserved`, and `_anon_` names but rejects other direct or inherited duplicates. Keeping every occurrence here avoids deciding that rule before types and bases are resolved.

`Compiler/LexLib.HC:MemberFind` searches the current class and then follows `base_class`. The current aggregate scope has the module scope as its parent; it does not point at the semantic base identity recorded by the header pass. Inherited lookup remains unimplemented rather than being approximated through ordinary lexical parents.

## Member type references

`Compiler/PrsVar.HC:PrsVarLst` selects one class or intrinsic entry for a declaration group, resets `tmpc1` to that entry for each comma declarator, and lets `PrsType` consume that declarator's pointer stars. A callback takes a separate path: pointer stars before its opening parenthesis belong to the return type, while the stars inside `(*name)` select the callback indirection. `PrsArrayDims` runs afterward, so array shape is distinct from both.

`Driver.Member_type_resolution` reproduces those distinctions over the checked semantic inputs. It publishes the current canonical aggregate identity before reading its members, which makes self references bind to the current definition while a same-name backing remains bound to the older prepublication identity. Earlier definitions, unresolved forwards, repeated forwards, and shadowing definitions use the source-order map established by aggregate reconciliation rather than the completed module table.

`Sema.Member_type_resolution` gives every collected direct member one checked type-reference result. Public primitive spellings, intrinsic storage spellings, aggregate identities, and zero through four ordinary pointer layers remain distinct. Callback members retain the resolved return type, one through four callback indirection origins, and callback-head origin without claiming a complete recursive function type. Array rank and each dimension origin survive unchanged; extent expressions are not evaluated. The pass also validates the declaration, reconciliation, header, member-scope, AST, source-order, and session associations before returning, and it does not add or renumber symbols.

## Layout status

No class or union layout is calculated yet. Members have source-resolved base and pointer references, but they do not yet have evaluated array extents, complete callback signatures, storage sizes, alignment, offsets, union overlap, base offsets, negative offsets, backing conversions, subinteger access, or metadata storage. The implementation also does not evaluate aggregate offset expressions or expose `MemberMetaData` and `MemberMetaFind` behavior.

The layout pass will use `Compiler/PrsVar.HC:PrsVarLst`, `Compiler/LexLib.HC`, `Compiler/CInit.HC`, the `CMemberLst` and `CHashClass` definitions in `Kernel/KernelA.HH`, and their backend consumers. Compatibility claims will require golden fixtures for total size, alignment, every member offset and storage size, signedness, dimensions, inheritance, and union overlap.

## Reproducible checks

`test/test_aggregate_resolution.ml` covers simple completion, repeated forwards, repeated definitions, a late forward, class/union spelling changes, unresolved identities, JIT and AOT inputs, generated and included provenance, invalid ownership or ordering, and deterministic repeated resolution without table mutation. `test/test_aggregate_header_resolution.ml` covers public and intrinsic backings, pointer depths zero through four, prepublication shadowing, postpublication bases, repeated forwards, later completion, source provenance, both compilation modes, and rejected foreign facts. `test/test_member_collection.ml` separately covers direct and grouped members, nested anonymous unions, repeated names, source provenance, ownership validation, the inheritance boundary, and deterministic symbol dumps. `test/test_member_type_resolution.ml` adds public, intrinsic, named, self, forward, repeated-forward, shadowed, callback, array, anonymous-union, generated, included, JIT, AOT, deterministic, pure, and rejected-association cases. These tests prove declaration, header, direct-member identity, and member type-reference binding, not layout compatibility.
