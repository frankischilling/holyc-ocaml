// Each activation preserves its own binary64 variable-register words.
// Echo uses R10 without overwriting Compute's a; total stays live in R11.
// The final result is F64 9.0 (bits 0x4022000000000000).
F64 Echo(F64 reg R10 value)
{
  return value;
}
F64 Compute(F64 reg R10 a,F64 b)
{
  F64 reg R11 total=a+b;
  F64 returned=Echo(total);
  return total+returned+a;
}
Compute(1.5,2.25);
