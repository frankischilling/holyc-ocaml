// Implicit call parentheses preserve supplied values and saved defaults.
#exe {
  I64 N=38;
  I64 Out=0;
  U0 Print(U8 *s,I64 saved=++N,I64 required,I64 tail=1) {
    Out=saved+required+tail;
  }
  N=0;
  ""("top",,2,);
  I64 Top=Out;
  U0 Saved() { ""("body",20,21,1); }
  Out=0;
  Saved;
  I64 Body=Out;
  U0 PutChars(I64 a,I64 b=2) { Out=a+b; }
  ''(40);
  StreamPrint("%d;",Top+Body+Out+N-84);
}
