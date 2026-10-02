// Operand snapshots survive the Print argument order and right-side rebinding.
extern U0 Print(U8 *fmt,...);

I64 Difference(I64 *left,I64 *right) {return left-right;}

I64 Entry() {
  I64 q[3];
  I64 *p=q,*alias=q+2;
  Print("%d:%d:%d:%d:%d;",Difference(alias,p),p-alias,
        (q+3)-(q+3),p-(p=q+1),Difference(q+1,q));
  return 42;
}
Entry();
