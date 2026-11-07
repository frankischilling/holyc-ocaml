// The linked addition stays in x87 extended precision until subtraction.
// At a=2^53 and b=1, this returns F64 1.0 (bits 0x3ff0000000000000).
// Storing a+b in an F64 local first would round it and return positive zero.
F64 LinkedDifference(F64 a,F64 b)
{
  return (a+b)-a;
}
LinkedDifference(9007199254740992.0,1.0);
