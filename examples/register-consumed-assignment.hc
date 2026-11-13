I64 Parameter(I8 x,I64 a)
{
  if(x=a+1)
    return x;
  return -1;
}
I64 Local(I64 a)
{
  I8 reg x=0;
  while(x=a+1)
    return x;
  return -1;
}
Parameter(0,255)+Local(255);
