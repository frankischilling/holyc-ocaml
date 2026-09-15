#exe {
  "A";
  'B';
  I64 N=0;
  U0 Print(U8 *text) {N=42;}
  U0 Saved() {"original";}
  "pending" #exe {U0 Print(U8 *text) {N=7;}};
  I64 Before=N;
  "replacement";
  I64 After=N;
  Saved;
  StreamPrint("%d;",Before+N-After-35);
}
