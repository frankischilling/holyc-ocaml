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

`PrsStmt` preserves existing function flags, public state, and assembly state while reading `interrupt`, `haserrcode`, `argpop`, `noargpop`, or `public`. Reading `interrupt` also sets `FSF_NOARGPOP`. Reading `static` clears other staged declaration flags and retains only assembly state. The generated `Function_flag.apply_modifier` function reproduces these assignments for later parser work.

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

Argument order, the complete saved-register contract, floating returns, frame layout, register-variable interactions, indirect-call details, exception unwinding, and hosted ABI bridging are not yet specified here. Those findings require the remaining backend and runtime audit before implementation. Until executable tests cover them, this document should be read as a checked function-flag specification, not a complete HolyC ABI description.
