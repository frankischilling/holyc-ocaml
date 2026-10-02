// The original ToBool call prepares a Bool default once in its source task.
#exe {
  public _intern 0x1d Bool ToBool(I64 i);
  I64 N=0;
  I64 Input(){N++;return 0x100;}
  Bool Saved(Bool value=ToBool(Input())){return value;}
  N=0;
  if(Saved()!=1||N)Print("bad");
  StreamPrint("%d;",Saved()*42);
}
