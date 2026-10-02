// The original void mutation runs once while its retained default is prepared.
#exe {
  public _intern 0xa8 U0 SwapI64(I64 *a,I64 *b);
  I64 A=8,B=42,N=0;
  I64 Init(){N++;SwapI64(&A,&B);return A;}
  I64 Saved(I64 x=Init()){return x;}
  if(A!=42||B!=8||N!=1)Print("bad");
  A=99;B=77;N=0;
  if(Saved()!=42||A!=99||B!=77||N)Print("bad");
  StreamPrint("%d;",Saved());
}
