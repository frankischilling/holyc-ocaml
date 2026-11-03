# Narrow register variable analysis

TempleOS can keep a narrow integer variable in a register without narrowing the result of a compound shift back to the declared type. With a signed byte parameter initialized to 127, `x <<= 1` followed by `return x` produces 254 when the parameter is selected for a register. An explicit `noreg` parameter or taking its address produces -2 instead. The unsigned byte counterparts produce 510 and 254. The reference captures in `test/oracle/register-variable-analysis` record both JIT and compiled observations.

Selection depends on more than the declared type. The original `Compiler/OptPass3.HC` scores frame members and assigns a limited set of registers; escape and register pressure can prevent selection. The captures include a larger pressure case where the signed result is -2. An option experiment enabled an option in the surrounding compiler context but still returned 254 from a compiled function. That experiment does not establish the function's own option snapshot, so the analysis declines non-default option states.

For parameters, the analysis covers a default-option, straight-line function with two scalar integer parameters, one compound shift, addition, subtraction, multiplication, division or bitwise update on an 8-, 16- or 32-bit parameter, and a direct return. It ties the result to the original typed source, frame and instruction graph and rejects changes to that evidence. Explicit `noreg` is classified as stack storage. Address escapes, extra statements, control flow, explicit register assignments and larger graphs remain unproved.

For this bounded class, the interpreter and native backends preserve the full register value through the update and return. Arguments are first converted to their declared narrow type, as in the entry-conversion captures. Explicit `noreg` continues to use narrow memory storage. The native entry path normalizes the incoming value before preserving its full-width register value.

The analysis also covers a function with one narrow local initialized from an integer literal and returned directly, optionally after a literal shift, compound addition, subtraction or multiplication. This does not implement general register allocation. Other local patterns, more parameters, aliases and other control flow remain outside the proof. Both the interpreter and native source-task paths handle this bounded case in retained initializers. The interpreter initializer guard still rejects unproved cases. The native path may still produce a narrow result for other register-selected updates. Issue #703 remains open.

The repeated 16- and 32-bit reference captures cover register results, `noreg` storage, parameter conversion and retained default initializers for each signed and unsigned width.

Local initialization differs from parameter entry: the reference can retain an out-of-range literal initializer in the register, whereas parameter entry first converts to the declared type. The local reference captures record this distinction and the corresponding `noreg` behavior.

For this parameter shape, original JIT and compiled captures also cover compound addition across all six signed and unsigned widths, with `noreg` controls.

Local compound addition and subtraction are limited to nonnegative signed-32-bit literals. Larger literals reached invalid-opcode faults in reference probes for both operations, so that optimizer edge remains outside this proof. Partial output and debugger screenshots are kept as incomplete evidence in `test/oracle/register-variable-analysis/local-add-high-fault.*` and `local-sub-high-fault.*`; they do not establish AOT results.

Original JIT and compiled captures cover the same bounded parameter and local shapes for compound subtraction, including `noreg`, retained defaults and the signed-32-bit immediate boundary.

Original JIT and compiled captures cover the bounded parameter and local shapes for compound multiplication, including high-bit operands and retained defaults.

Original JIT and compiled captures cover parameter compound AND, OR and XOR using a second parameter, including `noreg` and retained defaults. Literal bitwise updates on locals remain outside this proof because native bit operations can affect neighboring memory.

Compound division is supported for the two-parameter shape only. Original JIT and compiled observations cover signed and unsigned widths, negative operands, truncation toward zero, `noreg` and retained defaults. A zero divisor reaches the original `DivZero` exception; native exception handling remains a separate boundary.

A narrow local initialized from one signed or unsigned integer parameter and returned directly is also covered. It may be updated first with a literal shift, addition, subtraction or multiplication; addition and subtraction are limited to nonnegative signed-32-bit literals. Original JIT and compiled observations cover signed and unsigned locals, a high-bit unsigned parameter, `noreg` and retained defaults. A second signed or unsigned 8-, 16-, 32- or 64-bit parameter may supply the operand for shifts, addition, subtraction, multiplication, division, remainder or bitwise updates. Division by zero and signed quotient overflow raise the original `DivZero` exception. Additional locals and other control flow remain outside this case; native exception unwinding remains a separate boundary.

A one-parameter function can also return a literal-initialized narrow local, with or without a literal update. The same local may be updated using its full-word parameter with shifts, arithmetic or bitwise operations. Division by zero and signed quotient overflow retain their original fault behavior. Other uses of that parameter remain outside this proof.

The analysis also covers a narrow parameter or literal-initialized local incremented or decremented as a standalone statement before direct return. The reference captures cover prefix and postfix forms, explicit `noreg` storage and signed and unsigned widths. Other uses of the update expression remain outside the proof.

The same bounded parameter and local shapes may directly return a prefix increment or decrement. Repeated original JIT and compiled captures cover each narrow width and `noreg` controls. Postfix expressions returned directly remain outside this proof.
