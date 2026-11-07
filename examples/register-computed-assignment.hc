I64 Selected(I64 a,I64 b)
{
  I8 reg x=0;
  if(b)x=(a+1)*(b-1);
  return x;
}
I64 Stacked(I64 a,I64 b)
{
  I8 noreg x=0;
  if(b)x=(a+1)*(b-1);
  return x;
}
Selected(127,3)-Stacked(127,3);
