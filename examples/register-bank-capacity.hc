I64 Recur(I8 reg RSI value,I64 reg RDI depth,I64 reg R10 step,
          I64 reg R11 one,I64 reg R12 a,I64 reg R13 b,
          I64 reg R14 c,I64 reg R15 d)
{
  value+=step;
  if (depth) {
    depth-=one;
    return Recur(value,depth,step,one,a,b,c,d)+value;
  }
  return value;
}
Recur(127,3,2,1,0,0,0,0);
