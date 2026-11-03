# Narrow register variable analysis

TempleOS can keep a narrow integer variable in a register without narrowing the result of a compound shift back to the declared type. With a signed byte parameter initialized to 127, `x <<= 1` followed by `return x` produces 254 when the parameter is selected for a register. An explicit `noreg` parameter or taking its address produces -2 instead. The unsigned byte counterparts produce 510 and 254. The reference captures in `test/oracle/register-variable-analysis` record both JIT and compiled observations.

Selection depends on more than the declared type. The original `Compiler/OptPass3.HC` scores frame members and assigns a limited set of registers; escape and register pressure can prevent selection. The captures include a larger pressure case where the signed result is -2. An option experiment enabled an option in the surrounding compiler context but still returned 254 from a compiled function. That experiment does not establish the function's own option snapshot, so the analysis declines non-default option states.

The current analysis covers only a default-option, straight-line function with two scalar integer parameters, one compound left or right shift on a byte parameter, and a direct return of that parameter. It declines address escapes, additional statements, control flow, locals, explicit register assignments, and larger graphs. It ties the result to the original typed source, frame, and instruction graph and rejects subsequent changes to the evidence. Explicit `noreg` is classified as stack storage; other unproved cases remain unknown.

This is groundwork only. It does not change interpreter or native execution. The registered narrow-variable result described in issue #703 remains an execution gap.
