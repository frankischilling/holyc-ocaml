#exe {
  public _intern 0x7E I64 Bsf(I64 bits);
  public _intern 0x7F I64 Bsr(I64 bits);
  I64 N=0;
  I64 Input(){N++;return 0x40000000000;}
  I64 Saved(I64 x=Bsf(Input())){return x;}
  N=0;
  if(Saved()!=42||N) Print("bad");
  StreamPrint("%d;",Saved()+Bsr(0)+1);
}
