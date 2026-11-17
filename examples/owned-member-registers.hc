// Pointer members count toward pressure, but R12/R13 only hold nonpointers.
// Equal scores: a=R12, b=R13, q=RSI, r=RDI, s=R10, p=R11, c=R14.
I64 F(I8 a,I8 b,I8 c,I64 *p,I64 *q,I64 *r,I64 *s)
{
  a+=1;b+=1;c+=1;
  *p+=1;*q+=1;*r+=1;*s+=1;
  return a+b+c+*p+*q+*r+*s;
}
I64 P=1,Q=2,R=3,S=4;
// Each byte register retains 128. The four pointed-to values sum to 14.
F(127,127,127,&P,&Q,&R,&S); // 398
