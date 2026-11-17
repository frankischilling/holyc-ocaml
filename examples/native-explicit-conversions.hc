// Pinned TempleOS source deduction, not a new runtime capture.
_intern 27 I64 Round(F64 value);

I64 Explicit(F64 a,F64 b)
{
  return Round(a+b);
}

// The explicit intrinsic receives a materialized binary64 argument.
// This returns 9007199254740992. An implicit I64 return of a+b can keep
// the extended x87 value and instead return 9007199254740993.
Explicit(9007199254740992.0,1.0);
