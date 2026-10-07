#exe {
I64 Emit(U0 (*sink)(U8 *fmt,...)=&StreamPrint) {
  sink("I64 Answer=%d;",40);
  return 0;
}
Emit;
Print("made");
}
Answer+2;
