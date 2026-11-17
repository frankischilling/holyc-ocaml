// Pinned TempleOS source deduction, not a new runtime capture.
// The runtime integer converts to an extended x87 value before addition.
// No binary64 store occurs before the final truncating integer conversion.
I64 Cancel(F64 offset,I64 value)
{
  return value+offset;
}

// Returns 1. Replacing value in the function with the integer literal
// 9007199254740993 would round that literal to binary64 and return 0.
Cancel(-9007199254740992.0,9007199254740993);
