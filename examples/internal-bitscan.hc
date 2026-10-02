public _intern 0x7E I64 Bsf(I64 bits);
public _intern 0x7F I64 Bsr(I64 bits);
extern U0 Print(U8 *fmt,...);
I64 Compute() {
  return Bsf(1<<32)+Bsr(1<<10);
}
Print("%d:%d;",Compute(),Bsf(0));
Compute();
