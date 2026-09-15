#exe {
  I64 F(I64 n=40) { return n+2; }
  #exe {
    I64 SavedBefore() { return F(); }
    I64 F(I64 n=99) { return 7; }
    I64 SavedInner() { return F(); }
  }
  StreamPrint("%d;",SavedBefore()+SavedInner()+F()-108);
}
