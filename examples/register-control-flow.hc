I64 Selected(I8 x,I64 n,I64 step,I64 one)
{
  while (n) {
    x+=step;
    n-=one;
  }
  return x;
}

I64 Stored(I8 noreg x,I64 n,I64 step,I64 one)
{
  while (n) {
    x+=step;
    n-=one;
  }
  return x;
}

Selected(127,3,2,1)-Stored(127,3,2,1);
