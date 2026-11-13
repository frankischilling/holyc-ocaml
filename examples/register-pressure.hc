I64 Selected(I8 reg x,I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 h)
{
  if(a)x+=1;
  return x+a+b+c+d+e+f+g+h;
}
I64 Stacked(I8 noreg x,I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 h)
{
  if(a)x+=1;
  return x+a+b+c+d+e+f+g+h;
}
Selected(127,1,2,3,4,5,6,7,8)-Stacked(127,1,2,3,4,5,6,7,8);
