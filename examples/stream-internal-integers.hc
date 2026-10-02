#exe {
  public _intern 0xA9 I64 AbsI64(I64 i);
  public _intern 0xAA I64 SignI64(I64 i);
  public _intern 0xB0 I64 SqrI64(I64 i);
  public _intern 0xB1 U64 SqrU64(U64 i);
  public _intern 0x1D U8 ToBool(I64 i);
  I64 N=0;
  I64 Input(){N++;return -6;}
  I64 Saved(I64 x=AbsI64(Input())){return x;}
  N=0;
  if(SqrI64(Saved())+ToBool(N)!=36) Print("bad");
  StreamPrint("%d;",AbsI64(-32)+SignI64(-1)+SqrI64(-3)+SqrU64(1)+ToBool(7));
}
