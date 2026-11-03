# Narrow register variable analysis

TempleOS can keep a narrow integer variable in a register without narrowing the result of a compound shift back to the declared type. With a signed byte parameter initialized to 127, `x <<= 1` followed by `return x` produces 254 when the parameter is selected for a register. An explicit `noreg` parameter or taking its address produces -2 instead. The unsigned byte counterparts produce 510 and 254. The reference captures in `test/oracle/register-variable-analysis` record both JIT and compiled observations.

Selection depends on more than the declared type. The original `Compiler/OptPass3.HC` scores frame members and assigns a limited set of registers; escape and register pressure can prevent selection. The captures include a larger pressure case where the signed result is -2. An option experiment enabled an option in the surrounding compiler context but still returned 254 from a compiled function. That experiment does not establish the function's own option snapshot, so the analysis declines non-default option states.

The current analysis covers only a default-option, straight-line function with two scalar integer parameters, one compound left shift, right shift or addition on an 8-, 16- or 32-bit parameter, and a direct return of that parameter. It declines address escapes, additional statements, control flow, locals, explicit register assignments, and larger graphs. It ties the result to the original typed source, frame, and instruction graph and rejects subsequent changes to the evidence. Explicit `noreg` is classified as stack storage; other unproved cases remain unknown.

For this bounded class, the interpreter and native backends preserve the full register value through the update and return. Arguments are first converted to their declared narrow type, as in the entry-conversion captures. Explicit `noreg` continues to use narrow memory storage. The native entry path normalizes the incoming value before preserving its full-width register value.

The analysis also covers a function with one narrow local initialized from an integer literal and returned directly, optionally after a literal shift or compound addition. This does not implement general register allocation. Other local patterns, more parameters, aliases and other control flow remain outside the proof. Both the interpreter and native source-task paths handle this bounded case in retained initializers. The interpreter initializer guard still rejects unproved cases. The native path may still produce a narrow result for other register-selected updates. Issue #703 remains open.

The repeated 16- and 32-bit reference captures cover register results, `noreg` storage, parameter conversion and retained default initializers for each signed and unsigned width.

Local initialization differs from parameter entry: the reference can retain an out-of-range literal initializer in the register, whereas parameter entry first converts to the declared type. The local reference captures record this distinction and the corresponding `noreg` behavior.

For this parameter shape, original JIT and compiled captures also cover compound addition across all six signed and unsigned widths, with `noreg` controls.

Local compound addition is limited to nonnegative signed-32-bit literals. A larger literal reached an invalid-opcode fault in a reference probe, so that optimizer edge remains outside this proof. The partial output and debugger screenshot are kept as incomplete evidence in `test/oracle/register-variable-analysis/local-add-high-fault.*`; they do not establish an AOT result.
