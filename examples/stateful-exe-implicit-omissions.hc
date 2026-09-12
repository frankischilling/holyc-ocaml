// Print defaults retain their header-time values around supplied arguments.
#exe {
  I64 N=38;
  I64 Out=0;
  U0 Print(U8 *s,I64 saved=++N,I64 required,I64 tail=1) {
    Out=saved+required+tail;
  }
  N=0;
  "top",,2,;
  I64 Top=Out;
  U0 Saved() { "body",,2,; }
  Out=0;
  Saved;
  StreamPrint("%d;",Top+Out+N-42);
}
