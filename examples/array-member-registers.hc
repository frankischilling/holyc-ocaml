// Six unequal scalar scores. A noreg array does not enter the candidate list.
// Homes: a=RSI, b=RDI, c=R10, d=R11, e=R14, f=R15.
I64 Ordinary(I64 a,I64 b,I64 c,I64 d,I64 e,I64 f)
{
  I64 noreg scratch[5000];
  return a+a+a+a+a+a+b+b+b+b+b+c+c+c+c+d+d+d+e+e+f;
}

// A plain-reg array survives with its forced score, but stays in memory.
// Seven surviving candidates enable R12/R13 for the eligible scalar members.
// Homes: a=R12, b=R13, c=RSI, d=RDI, e=R10, f=R11.
I64 Forced(I64 a,I64 b,I64 c,I64 d,I64 e,I64 f)
{
  I64 reg scratch[5000];
  return a+a+a+a+a+a+b+b+b+b+b+c+c+c+c+d+d+d+e+e+f;
}

Ordinary(1,1,1,1,1,1)+Forced(1,1,1,1,1,1); // 42
