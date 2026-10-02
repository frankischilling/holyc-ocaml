#exe {
  public _intern 0xAB I64 MinI64(I64 a,I64 b);
  public _intern 0xAC U64 MinU64(U64 a,U64 b);
  public _intern 0xAD I64 MaxI64(I64 a,I64 b);
  public _intern 0xAE U64 MaxU64(U64 a,U64 b);
  I64 N=0;
  I64 Input(I64 n){N=N*10+n;return n;}
  I64 Saved(I64 x=MinI64(Input(4),Input(2))){return x;}
  N=0;
  if(Saved()!=2||N) Print("bad");
  StreamPrint("%d;",MaxI64(Saved(),MinI64(50,42)));
}
