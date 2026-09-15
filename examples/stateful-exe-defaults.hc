#exe {
  I64 N=20;
  I64 Next(){return ++N;};
  I64 Saved(I64 n=Next()){return n;};
  N=0;
  StreamPrint("%d;",Saved()+Saved());
}
