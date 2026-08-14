# HolyC ABI source notes

These notes describe the ABI facts audited so far from TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`. They are not a claim that `holyc-ocaml` can emit or call HolyC machine code yet.

## Function flag storage

`CHashFun` inherits the `U16 flags` field from `CHashClass`. Bits 0 and 1 are the shared `Cf_EXTERN` and `Cf_INTERNAL_TYPE` positions. Function-specific positions occupy the upper byte:

| Bit | Source name | Observed role |
| ---: | --- | --- |
| 8 | `Ff_INTERRUPT` | Selects interrupt register save/restore and `IRETQ` |
| 9 | `Ff_HASERRCODE` | Discards the processor error-code slot before `IRETQ` |
| 10 | `Ff_ARGPOP` | Requests callee argument cleanup |
| 11 | `Ff_NOARGPOP` | Overrides `RET1` and `ARGPOP` cleanup |
| 12 | `Ff_INTERNAL` | Treats the function address as an internal IC operation |
| 13 | `Ff__EXTERN` | Records the underscore form of `_extern` |
| 14 | `Ff_DOT_DOT_DOT` | Marks a varargs function and its synthetic `argc`/`argv` members |
| 15 | `Ff_RET1` | Allows the emitted return to pop the fixed argument block |

`Compiler/CompilerA.HH` defines a separate parser mask. Its low bits represent `public`, assembly state, `static`, and the underscore-name form. Only `FSF_INTERRUPT`, `FSF_HASERRCODE`, `FSF_ARGPOP`, and `FSF_NOARGPOP` are copied into `CHashFun.flags` through `FSG_FUN_FLAGS1`. `public` updates the hash entry's type flags instead. Assembly, `static`, and underscore-name state remain parser concerns.

`PrsStmt` preserves existing function flags, public state, and assembly state while reading `interrupt`, `haserrcode`, `argpop`, `noargpop`, or `public`. Reading `interrupt` also sets `FSF_NOARGPOP`. Reading `static` clears other staged declaration flags and retains only assembly state. The generated `Function_flag.apply_modifier` function reproduces these assignments. Bound prototype AST nodes now retain the modifier tokens in order, and parser tests fold them through this function. No stored flag or ABI effect is applied yet.

## Parameter register requests

`Compiler/PrsVar.HC:PrsVarLst` accepts `reg` and `noreg` before a function parameter type or immediately after it. `reg` sets the working member value to `REG_ALLOC`; `noreg` sets it to `REG_NONE`. Repeated qualifiers overwrite that working value in source order. A following identifier is an explicit register only when it belongs to `Compiler/CInit.HC:ST_U64_REGS`, whose pinned value is `RAX`, `RCX`, `RDX`, `RBX`, `RSP`, `RBP`, `RSI`, `RDI`, and `R8` through `R15`. The parser and semantic signature retain all qualifiers, their positions, and their source origins. An explicit register also carries its canonical number.

The accepted name set is not the complete assembler register set. For example, `EAX` and `R8u64` do not match `ST_U64_REGS`, so `reg EAX` or `reg R8u64` leaves that identifier available as the parameter name. The semantic final state uses `REG_UNDEF` (`-128`) when no request appears, `REG_NONE` (`32`) for `noreg`, `REG_ALLOC` (`33`) for plain `reg`, and `0` through `15` for an explicit register. The last request wins. A terminal variadic marker gives the same ordered requests to both synthetic `argc` and `argv`, following the `_reg` value passed to `PrsDotDotDot`. `MemberLstCmp` does not compare `reg`, so joined headers remain one identity when only these requests differ.

This state is input to later compiler work, not an allocation result. `OPTf_NO_REG_VAR`, register availability, conflicts, spills, parameter moves, and ABI placement are not implemented, and the backend does not yet promise to honor an explicit request.

## Function-pointer parameters

`Compiler/PrsVar.HC:PrsType` parses a parenthesized function-pointer declarator and calls `PrsFunJoin` with a null name for its signature metadata. One through four stars inside the declarator determine the function-pointer type, while any stars between the primitive type and the declarator belong to the callback return type. `PrsVarLst` stores the returned function metadata in `CMemberLst.fun_ptr` and sets `MLF_FUN`. The parser keeps these parts separate in a recursive AST and accepts empty, fixed, variadic, and nested callback signatures. Semantic aggregate members retain the complete recursive signature and the checked `MLF_FUN` mask `0x8`; ordinary aggregate members carry zero.

This is syntax and metadata capture, not ABI implementation. Function type compatibility, indirect-call lowering, calling flags inside callback types, register assignment, and native invocation remain unavailable.

## Resolved signature facts

`Compiler/PrsStmt.HC:PrsGlblVarLst` passes the selected return class to `PrsFunJoin`. `PrsVarLst` then creates one fixed slot per source parameter; an unnamed slot remains in the signature even though it receives no `CMemberLst` name. `PrsType` can recurse through callback parameters, preserving zero through four return-pointer layers and requiring one through four callback-indirection stars. Defaults are per-slot flags, so an ordinary expression default, `lastclass`, and no default remain different even when a required slot follows a defaulted one.

`Sema.Function_type_resolution` and its driver bind those facts for top-level prototypes and definitions. Return and fixed-parameter types resolve to public primitives, intrinsic storage types, or the canonical aggregate identity visible at the declaration. Callback parameter signatures resolve recursively. Named top-level slots map to the parameter identities already collected in the function scope; unnamed slots remain positional only. Every slot retains its ordered register requests and derived final state through top-level function, aggregate-member, global, and local callback type passes. Each fixed slot also carries the mask assigned by `PrsVarLst`: `MLF_NO_UNUSED_WARN` for an unnamed slot, `MLF_FUN` for a callback declarator, and `MLF_DFT_AVAILABLE` plus the applicable `MLF_LASTCLASS` or `MLF_STR_DFT_AVAILABLE` bit for a default. Each callback level derives its own state, so nested metadata does not alter its owning parameter. The pass preserves default kind and source provenance but does not evaluate or substitute a default.

`PrsDotDotDot` appends `argc` and then `argv` after the fixed slots. Both use the compiler's internal `I64` class, carry exactly `MLF_DOT_DOT_DOT`, and retain the ellipsis request list. `argc` is scalar. `argv` is array-shaped: its source declaration has no extent, while the compiler stores 127 as an internal placeholder dimension. This is not a C-style `char **argv`, and 127 is not reported as a source-written bound. Nested callback ellipses retain their marker and request list but do not create top-level synthetic bindings.

These are checked semantic signature facts, not a calling-convention implementation. Prototype and definition identities and top-level function record flags are modeled by separate passes. Direct function-body calls now select fixed and variadic slots from the visible header, but default evaluation, conversions, register allocation, argument placement, cleanup emission, storage, indirect calls, and native entry points remain separate work. [Issue #181](https://github.com/frankischilling/holyc-ocaml/issues/181) records the signature boundary, [issue #220](https://github.com/frankischilling/holyc-ocaml/issues/220) records the parameter masks, [issue #234](https://github.com/frankischilling/holyc-ocaml/issues/234) records register-request retention, [issue #238](https://github.com/frankischilling/holyc-ocaml/issues/238) records direct call-slot selection, and [issue #191](https://github.com/frankischilling/holyc-ocaml/issues/191) records the function-record boundary.

## Local variable member flags

`Compiler/PrsVar.HC:490-524` allocates a zeroed `CMemberLst` for each local declarator. Static-local mode adds `MLF_STATIC` before `PrsType`; a callback declarator adds `MLF_FUN` afterward. The two decisions are independent, so an ordinary automatic object has mask `0x0`, an automatic callback has `0x8`, an ordinary static object has `0x40`, and a static callback has `0x48`. A nested callback parameter keeps its own parameter mask and does not add bits to the owning local.

`Sema.Local_type_resolution` derives this declaration-time mask from its checked storage and declarator kind. The public constructor accepts neither a raw mask nor individual flags, while accessors expose the exact mask and typed queries. Comma-group declarators are classified independently. The pinned corpus supplies ordinary static locals, including the three-variable group at `Kernel/KMisc.HC:87`; no static callback local was found there, so the combined mask is direct source-path coverage rather than a corpus-frequency claim.

`Compiler/PrsExp.HC:763-804` consumes `MLF_FUN` to recover callback metadata and `MLF_STATIC` to choose an AOT absolute address or JIT immediate address instead of an RBP-relative local. Those address, allocation, initializer, cleanup, and execution paths are not implemented yet. [Issue #222](https://github.com/frankischilling/holyc-ocaml/issues/222) records the retained metadata boundary.

## Classified function records

`PrsFunJoin` copies staged calling bits only when it creates a `CHashFun`. A later header that joins the same record does not replace those stored bits. The record can still accumulate `Ff_DOT_DOT_DOT` and `Ff_RET1`; `HTF_PUBLIC` is replaced on every header, and `HTF_PRIVATE` may accumulate from `OPTf_KEEP_PRIVATE`. Binding paths then set or clear `Cf_EXTERN`, `Ff_INTERNAL`, `Ff__EXTERN`, `HTF_IMPORT`, `HTF_EXPORT`, and `HTF_RESOLVE` in source order.

`Sema.Function_record_classification` retains those raw domains separately and reports which source consumer would select an internal operation, direct call, JIT extern address slot, AOT import, or AOT extern. It also reports the cleanup predicate through the checked function-flag API. This is ABI input, not ABI implementation: no call sequence, stack layout, prologue, epilogue, register save, interrupt entry, or machine instruction is emitted.

## Argument cleanup

`PrsFunJoin` derives `Ff_RET1` when a function has at least one fixed argument, is not variadic, and its argument byte count fits the signed 16-bit immediate used by `RET`. With eight-byte argument slots, the accepted count is 1 through 4,095.

Both call lowering in `PrsExp.HC` and the function epilogue in `OptPass789A.HC` use the same choice:

```text
(RET1 or ARGPOP) and not NOARGPOP
```

When the choice is true, the callee emits `RET imm16` and the caller uses the matching stack-adjustment form. Otherwise the callee emits an ordinary return and the caller removes the argument block. The generated API exposes the predicate, but there is no call lowerer or epilogue emitter in this project yet.

## Interrupt functions

The final TempleOS backend saves the clobbered register set on interrupt entry and restores it on exit. It emits `IRETQ` instead of `RET`. If `Ff_HASERRCODE` is set, it removes one eight-byte error-code slot first. The parser's automatic `NOARGPOP` on `interrupt` prevents the ordinary function cleanup path from being selected accidentally.

## Work still requiring source audit

Argument order, the complete saved-register contract, floating returns, frame layout, register-variable allocation, indirect-call details, exception unwinding, and hosted ABI bridging are not yet specified here. Those findings require the remaining backend and runtime audit before implementation. Until executable tests cover them, this document should be read as a checked function-flag and parameter-request specification, not a complete HolyC ABI description.
