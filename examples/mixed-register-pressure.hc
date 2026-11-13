// Pinned TempleOS source deduction, not a new runtime capture.
// Every forced member has two source accesses, including F64 self-assignments.
// QSort2b orders the tied candidates f,e,h,g,d,b,a,c,x, so x spills.
I64 MixedPressure(F64 reg a,F64 reg b,I8 reg x,
                  F64 reg c,F64 reg d,F64 reg e,
                  F64 reg f,F64 reg g,F64 reg h)
{
  a=a;
  b=b;
  c=c;
  d=d;
  e=e;
  f=f;
  g=g;
  h=h;
  x+=1;
  // Add a discarded x; here to raise its score and select R12 instead.
  // With that extra read this returns 128; without it the I8 spill is -128.
  return x;
}

MixedPressure(1.5,1.5,127,1.5,1.5,1.5,1.5,1.5,1.5);
