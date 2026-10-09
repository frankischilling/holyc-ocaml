extern U0 Print(U8 *fmt,...);
I64 Seed=1;
#exe {
  I64 Seed=40;
  I64 Read(I64 add=2) { return Seed+add; }
  StreamPrint("I64 Answer=%d;",Read);
}
#exe {
  Seed=41;
  Print("parse%d;",Read);
}
Print("load%d;",Seed);
Answer;
