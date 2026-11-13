// Pinned-source deduction: a scores six; all other parameters score two.
// QSort2b order: a,h,g,flag,i,f,c,b,e,d. a gets R12; e and d spill.
// Each source branch/call occurrence counts once, not once per execution.
I64 KeepWord(I64 x) { return x; }
I64 Pressure(I8 a,I8 b,I8 c,I8 d,I8 e,I8 f,I8 g,I8 h,I8 i,I64 flag)
{
  if (flag)
    a=KeepWord(a+1);
  return a+b+c+d+e+f+g+h+i;
}
Pressure(127,0,0,0,0,0,0,0,0,1); // 128: selected a retains its full word.
