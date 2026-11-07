// Bounded scalar F64 native example: exact binary64 result 3.75.
// Run with --target=host-jit or --target=host-jit-task.
// The integer interpreter intentionally does not execute F64 arithmetic.
F64 Add(F64 left,F64 right)
{
  F64 result=left+right;
  return result;
}
Add(1.5,2.25);
