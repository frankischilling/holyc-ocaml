// RSI/RDI survive the unpinned callee and its hosted bookkeeping calls.
I64 Inner(I64 x)
{
  return x+1;
}

I64 Outer(I64 reg RSI x,I64 reg RDI y)
{
  Inner(y);
  return x+y+1;
}

Outer(20,21);
