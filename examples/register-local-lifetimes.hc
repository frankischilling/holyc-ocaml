I64 Recur(I64 initial,I64 depth,I64 step,I64 one)
{
  I8 value=initial;
  value+=step;
  if (depth) {
    depth-=one;
    return Recur(value,depth,step,one)+value;
  }
  return value;
}
Recur(127,3,2,1);
