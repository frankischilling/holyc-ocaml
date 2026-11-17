I64 SumCallers(I8 x,I64 depth,I64 step,I64 one)
{
  x+=step;
  if (depth) {
    depth-=one;
    return SumCallers(x,depth,step,one)+x;
  }
  return x;
}

SumCallers(127,3,2,1);
