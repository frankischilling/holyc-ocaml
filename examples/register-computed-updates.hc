I64 Selected(I8 reg x,I64 a,I64 b)
{
  if(a)x+=a+b;
  return x;
}
I64 Stacked(I8 noreg x,I64 a,I64 b)
{
  if(a)x+=a+b;
  return x;
}
Selected(127,1,1)-Stacked(127,1,1);
