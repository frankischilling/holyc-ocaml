// Each original parameter occurs twice: its discarded update and final read.
// The pinned unstable sort orders equal scores g,f,i,h,e,b,a,d,c.
// g/f use R12/R13; i/h use RSI/RDI; e/b keep full-word private homes for
// R10/R11; a/d use R14/R15. Only c spills and wraps at its declared I8 width.
I64 F(I8 a,I8 b,I8 c,I8 d,I8 e,I8 f,I8 g,I8 h,I8 i)
{
  a+=1;
  b+=1;
  c+=1;
  d+=1;
  e+=1;
  f+=1;
  g+=1;
  h+=1;
  i+=1;
  return a+b*2+c*3+d*4+e*5+f*6+g*7+h*8+i*9;
}

// 128*45 - 256*3 = 4992.
F(127,127,127,127,127,127,127,127,127);
