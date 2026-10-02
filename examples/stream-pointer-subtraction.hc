// Preparing the default reads the original offset cell once.
#exe {
  I64 Q[2];Q[0]=42;I64 N=0;
  I64 Init(){N++;I64 *p=&Q[1];return *(p-1);}
  I64 Saved(I64 x=Init()){return x;}
  if(N!=1||Saved()!=42)Print("bad");
  Q[0]=99;N=0;
  if(Saved()!=42||Q[0]!=99||N)Print("bad");
  StreamPrint("%d;",Saved());
}
