class Pair {I64 a; I64 b;};

I64 Compute()
{
  static Pair (*p)()[2]={370,42};
  I64 (*q)();
  I64 answer=(p[0]+1)-p[1];
  return (q=answer)|0;
}

class Pair {U8 later;};
Compute();
Compute();
