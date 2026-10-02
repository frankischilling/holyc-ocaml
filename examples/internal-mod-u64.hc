public _intern 0xAF U64 ModU64(U64 *q,U64 d);
extern U0 Print(U8 *fmt,...);
I64 Compute()
{
  I64 q=442;
  U64 remainder;
  remainder=ModU64(&q,10);
  Print("%d:%d;",q,remainder);
  return q-remainder;
}
Compute();
