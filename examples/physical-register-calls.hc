// The nested call reuses R12 without changing the caller's value.
I64 Inner(I64 reg R12 x)
{
  x+=1;
  return x;
}

I64 Outer(I64 reg R12 x,I64 reg R13 y)
{
  Inner(y);
  return x+y+1;
}

Outer(20,21);
