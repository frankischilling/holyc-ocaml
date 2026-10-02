// Each swap reads both owned cells before storing either, then completes U0.
public _intern 0xa5 U0 SwapU8(U8 *a,U8 *b);
public _intern 0xa6 U0 SwapU16(U16 *a,U16 *b);
public _intern 0xa7 U0 SwapU32(U32 *a,U32 *b);
public _intern 0xa8 U0 SwapI64(I64 *a,I64 *b);
extern U0 Print(U8 *fmt,...);
I64 Result()
{
  I64 wide[2];wide[0]=8;wide[1]=42;
  U32 d=8,e=42;
  U16 w=8,z=42;
  Bool b=-128;
  U8 c=42;
  U8 *letters="AB";
  SwapI64(&wide[0],&wide[1]);
  SwapU32(&d,&e);
  SwapU16(&w,&z);
  SwapU8(&b,&c);
  SwapU8(&letters[0],&letters[1]);
  Print("%d:%d:%d:%d:%d:%s;",wide[0],d,w,b,c,letters);
  return wide[0]+(wide[1]!=8)+(e!=8)+(z!=8)+(b!=42)+(c!=128)
    +(letters[0]!='B')+(letters[1]!='A');
}
Result();
