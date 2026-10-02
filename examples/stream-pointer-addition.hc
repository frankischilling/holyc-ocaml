// Preparing the default reads the original cell once and saves its word.
#exe {
  I64 Q[2];Q[1]=42;I64 N=0;
  I64 Init(){N++;I64 *p=Q;return *(p+1);}
  I64 Saved(I64 x=Init()){return x;}
  if(N!=1||Saved()!=42)Print("bad");
  Q[1]=99;N=0;
  if(Saved()!=42||Q[1]!=99||N)Print("bad");
  StreamPrint("%d;",Saved());
}
