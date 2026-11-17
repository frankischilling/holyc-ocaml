// The sum stays extended until the ordered comparison.
// Materializing it in an F64 local before comparing would return 0.
I64 AboveRounded(F64 a,F64 b)
{
  return (a+b)>a;
}
AboveRounded(9007199254740992.0,1.0);
