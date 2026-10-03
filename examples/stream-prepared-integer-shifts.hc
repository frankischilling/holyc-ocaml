#exe {
  I64 N=0,X=-7;
  I64 Divide(I64 x) { N++; return x/2; }
  I64 Saved(I64 n=Divide(X)) { return n; }
  I64 G=Divide(X);
  I64 Persistent() { static I64 n=Divide(X); return n; }
  if(N!=3||Saved()!=-4||G!=-4||Persistent()!=-4) Print("bad");
  X=100; N=0;
  if(Saved()!=-4||G!=-4||Persistent()!=-4||N) Print("replay");
  StreamPrint("%d;",Saved()+G+Persistent()+54);
}
