// Prepare the original comparison once, then change its offset operand.
#exe {
  I64 Q[2];
  I64 Offset=0,N=0;
  I64 Init() {N++;I64 *p=Q+Offset;return 41+(p==Q);}
  I64 Saved(I64 x=Init()) {return x;}
  if(N!=1||Saved()!=42)Print("bad");
  Offset=1;N=0;
  if(Saved()!=42||Offset!=1||N)Print("bad");
  StreamPrint("%d;",Saved());
}
