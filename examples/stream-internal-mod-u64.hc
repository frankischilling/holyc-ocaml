#exe {
  public _intern 0xAF U64 ModU64(U64 *q,U64 d);
  U64 Q=442;
  I64 N=0;
  U64 Divisor(){N++;return 10;}
  U64 Saved(U64 x=ModU64(&Q,Divisor())){return x;}
  if(Q!=44||N!=1)Print("bad");
  Q=999;N=0;
  if(Saved()!=2||Q!=999||N)Print("bad");
  StreamPrint("%d;",Saved()+40);
}
