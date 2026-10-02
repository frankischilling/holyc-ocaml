public _intern 0xAB I64 MinI64(I64 a,I64 b);
public _intern 0xAC U64 MinU64(U64 a,U64 b);
public _intern 0xAD I64 MaxI64(I64 a,I64 b);
public _intern 0xAE U64 MaxU64(U64 a,U64 b);
extern U0 Print(U8 *fmt,...);
I64 Compute() {
  return MinI64(-3,4)+MaxI64(40,42)+MinU64(3,5)+MaxU64(0,0);
}
Print("%d:%X;",Compute(),MaxU64(1,0x8000000000000000));
Compute();
