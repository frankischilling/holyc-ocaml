I64 Selected(I64 a,I64 b)
{
  I8 reg x=-(a/b);
  if(b)x-=1;
  return x;
}
I64 Stacked(I64 a,I64 b)
{
  I8 noreg x=-(a/b);
  if(b)x-=1;
  return x;
}
Selected(1024,2)-Stacked(1024,2);
