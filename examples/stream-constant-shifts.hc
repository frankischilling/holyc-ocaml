#exe {
  I64 N=0;
  I64 Init() {
    N++;
    return 1<<2;
  }
  I64 Saved(I64 n=Init()) {
    return n;
  }
  if(N!=1||Saved()!=4)Print("bad");
  N=0;
  if(Saved()!=4||N)Print("bad");
  StreamPrint("%d;",Saved()+38);
}
