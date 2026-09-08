# Owned string-literal storage

Issue [#617](https://github.com/frankischilling/holyc-ocaml/issues/617) connects
the canonical string IR from #490 to the existing [byte storage](integer-bytes.md)
interpreter. Run the source example through the ordinary program path:

```text
holyc run --target=ir --mode=jit --literal-byte-limit=2 examples/integer-strings.hc
holyc run --target=ir --mode=aot --literal-byte-limit=2 examples/integer-strings.hc
```

```c
I64 Read(U8 *p) { return p[0] + p[1]; }
I64 F()
{
    U8 *s="*";
    return Read(s);
}
F();
```

The final I64 value is 42: the byte for `*` plus the initialized zero
terminator. Both modes use 45 runtime instructions, zero preparation
instructions, two literal bytes, 16 active frame bytes and call depth two.
At baseline `4af56c7db43b12dfb9b6fae659f4eaaa1e49be49`, both modes
reject the pointer initializer with HCRUN0003. Six maintained source groups
first reproduced the missing connection before implementation.

## Byte images and source types

Each IC_STR_CONST owns its exact byte payload plus one initialized zero. Empty
strings occupy one byte; `"A\0B"` occupies four. Concatenated literals have one
final terminator. Embedded zeros and high bytes remain ordinary stored bytes;
the runtime does not scan or reinterpret the payload as host Unicode text.

One literal site retains its object across calls, recursion and initializer
execution. Different sites remain distinct even when their text is identical
or their graph-local instruction/value numbers coincide. Each hosted execution
allocates a fresh image. Literal storage survives an automatic frame's return.
Writes through U8 pointers retain low eight bits, subsequent reads zero-extend
them, and plain assignment expressions keep their complete RHS payload.

The canonical producer retains its exact Internal_storage U8* type. Local
initializers, assignments and fixed arguments admit the checked public/internal
forms of one-level U8 pointers. This follows the pinned U8 internal class and
does not identify public I64/U64 unions with their storage classes. Conversion
retains the actual object and offset, while adopting the destination pointee
type for later strict load/index checks. General casts remain unsupported.

References retain each literal's original base, extent, byte offset and live
storage. Negative indexing from an interior pointer may reach earlier bytes of
that object. One-past references may be materialized, but loads and stores must
remain within the original literal, including its terminator. An adjacent
literal never enlarges those bounds. Access faults retain HCIRVM0019; checked
offset overflow retains HCIRVM0020.

## Limits and entry points

`run_integer_program`, `Ir_integer_interpreter.execute_program` and
`execute_function` accept optional `max_literal_bytes`, default 1,048,576.
The CLI exposes `--literal-byte-limit`. Program JSON reports
`literal_byte_limit`; human output reports `literal-byte-limit`. These fields
extend the existing program report without changing its result meaning.

Preflight counts the payload and terminator of every supplied site, including
unreachable blocks and uncalled functions. Checked arithmetic and host
container limits precede allocation. HCIRVM0021 reports literal capacity
exhaustion with zero executed steps and the offending site's available owner
and source information. Nonpositive limits report HCIRVM0001 before source
compilation. Literal storage has its own budget; it consumes neither active
frame bytes nor global storage bytes. Repeated calls allocate no second copy.
Image preparation consumes no IR instructions, while each reached IC_STR_CONST
consumes its ordinary execution step.

The graph-only `execute` and bounded `eval` contracts remain unchanged. String
execution requires the function/program context. Pointer returns and arbitrary
addresses remain unsupported. Runtime output, U0 external calls, varargs,
formatting, broader storage, stateful compilation, native code and bootstrap
are subsequent connections.

HolyC treats a leading string in statement position as implicit output.
Use `("*"[0]+0);` for an ordinary entry expression; it returns 42 in ten
instructions through `run`. The same expression remains outside `eval`'s VM
domain. Parentheses select the existing expression grammar.

## Source evidence

Local verification passes all 1,825 tests (117.064 seconds), including fourteen
string groups and CLI checks. Exact example limits are 45 steps, two literal
bytes, 16 frame bytes and depth two; one-below limits fail with their distinct
diagnostics. Formatting, generated source, build/install, 82 pinned-reference
checksums and all 11 incremental provenance scenarios pass. Lexer acceptance
is 528/528; parser acceptance is 25/528 standalone and 126/528 with the prelude.
The complete local parser JSON matches the committed baseline after Windows
newline normalization. Independent code review approved the final type, owner,
flag and resource checks. #617 and PR #618 record final revision/CI/integration
evidence separately from this local source snapshot.

Global declaration initializers retain their I64/U64 result domain. The
word-valued `I64 G="*"[0]+0;G;` is covered, including literal-capacity and
reached-access faults with JIT compile/AOT load phase labels. A direct U8-valued
global initializer remains outside that existing domain.

All source references use TempleOS
`c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/CInit.HC:9`, `AsmInit.HC:197-206` and `PrsExp.HC:693-694` establish
  the final native U8 class and string pointer type.
- `Compiler/LexLib.HC:248-274` retains embedded bytes and adjacent-string
  concatenation. `PrsLib.HC:143` creates each miscellaneous record;
  `OptPass789A.HC:1098-1105` emits its exact bytes independently.
- `Compiler/PrsLib.HC:298-308` and `PrsStmt.HC:150-189` put function literals
  in compiled storage. Mutability is inferred from ordinary U8 references to
  allocated compiled storage and native byte-store instructions. No new native
  execution capture is claimed.

The next integration reuses implicit-output typing, target resolution and
argument binding from #302, #304, #306, #346 and #348. It must connect checked
runtime declaration identity, U0 results, right-to-left arguments, source
formatting, bounded byte output and explicit CLI reporting.
