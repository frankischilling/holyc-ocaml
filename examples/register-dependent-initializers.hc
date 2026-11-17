I64 Selected()
{
  I8 reg x=127;
  I8 y=x+1;
  I8 z=y+x;
  return z;
}
I64 Stacked()
{
  I8 noreg x=127;
  I8 noreg y=x+1;
  I8 noreg z=y+x;
  return z;
}
Selected-Stacked;
