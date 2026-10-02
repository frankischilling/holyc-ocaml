public _intern 0xA9 I64 AbsI64(I64 i);
public _intern 0xAA I64 SignI64(I64 i);
public _intern 0xB0 I64 SqrI64(I64 i);
public _intern 0xB1 U64 SqrU64(U64 i);
public _intern 0x1D U8 ToBool(I64 i);
extern U0 Print(U8 *fmt,...);
I64 Compute() {
  return AbsI64(-32)+SignI64(-1)+SqrI64(-3)+SqrU64(1)+ToBool(7);
}
Print("%d:%d;",Compute(),ToBool(0x100));
Compute();
