// The original mutating default runs once in its retained source task.
#exe {
  public _intern 0x77 Bool Bt(U8 *p,I64 n);
  public _intern 0x78 Bool Bts(U8 *p,I64 n);
  public _intern 0x79 Bool Btr(U8 *p,I64 n);
  public _intern 0x7a Bool Btc(U8 *p,I64 n);
  I64 Q=2;
  I64 N=0;
  I64 Index(){N++;return 1;}
  Bool Saved(Bool value=Btc(&Q,Index())){return value;}
  if(Q!=0||N!=1||Bt(&Q,1))Print("bad");
  Bts(&Q,1);
  if(Btr(&Q,1)!=1||Q)Print("bad");
  Q=999;N=0;
  if(Saved()!=1||Q!=999||N)Print("bad");
  StreamPrint("%d;",Saved()*42);
}
