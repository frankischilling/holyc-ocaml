// Bool uses signed byte storage; ToBool tests the complete computed word.
public _intern 0x1d Bool ToBool(I64 i);
extern U0 Print(U8 *fmt,...);
Bool Computed(){return 0x180;}
I64 Read(Bool value){return value;}
I64 Result()
{
  Bool stored=0x180;
  Print("%d:%d:%d;",Computed(),stored,ToBool(0x100));
  if(Read(Computed())!=-128)return 0;
  return ToBool(0x100)*42;
}
Result();
