#exe {
  I64 F(I64 n=40,... { I64 v=n+argc+argv[0]; return v; }
  #exe { I64 Saved() { return F(,1); } }
  StreamPrint("%d;",Saved());
}
