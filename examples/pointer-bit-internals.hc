// Plain bit calls retain the original object's extent and declared storage.
public _intern 0x77 Bool Bt(U8 *p,I64 n);
public _intern 0x78 Bool Bts(U8 *p,I64 n);
public _intern 0x79 Bool Btr(U8 *p,I64 n);
public _intern 0x7a Bool Btc(U8 *p,I64 n);
extern U0 Print(U8 *fmt,...);
I64 Result()
{
  I64 flags[2];
  flags[0]=0;flags[1]=0;
  Bool small=0;
  I64 old=Bts(flags,70);
  I64 tested=Bt(flags,70);
  I64 reset=Btr(flags,70);
  Btc(&small,7);
  Print("%d:%d:%d:%d:%d;",old,tested,reset,flags[1],small);
  return tested*reset*42;
}
Result();
