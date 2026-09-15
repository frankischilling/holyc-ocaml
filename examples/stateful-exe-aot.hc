// holyc run --mode=aot: returns 42 and captures AB.
#exe {I64 N=20+20;Print("A");StreamPrint("%d;",N+2);}
extern U0 Print(U8 *fmt,...);
Print("B");
I64 G=1+1;
G+40;
