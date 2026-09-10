#exe {
  I64 N=40;
  I64 Out=0;
  U0 Print(U8 *text,I64 n=++N) {Out=n;}
  U0 Saved() {"saved";}
  N=0;
  "pending" #exe {U0 Print(U8 *text,I64 n=7) {Out=n;}};
  I64 Before=Out;
  "replacement";
  I64 After=Out;
  Saved;
  StreamPrint("%d;",Before+Out-After-33+N);
}
