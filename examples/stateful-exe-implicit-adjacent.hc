#exe {
 I64 N=39; I64 Out=0;
 U0 PutChars(I64 saved=++N,I64 required) { Out=saved+required; }
 N=0;
 ''2;
 I64 Top=Out;
 U0 Saved() { 'A'-63; }
 Out=0; Saved;
 I64 Body=Out;
 U0 PutChars(I64 a,I64 b=1,I64 c) { Out=a+b+c; }
 ''39 2;
 StreamPrint("%d;",Top+Body+Out+N-84);
}
