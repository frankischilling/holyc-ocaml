#exe {
 I64 N=39; I64 Out=0;
 U0 Print(I64 a=++N,I64 b) { Out=a+b; }
 N=0;
 ""(,2);
 I64 Top=Out;
 U0 Saved() { ""(,2); }
 Out=0; Saved;
 I64 Body=Out;
 U0 Print() { Out+=20; }
 U0 PutChars(I64 a=22) { Out+=a; }
 Out=0; ""(); ''();
 StreamPrint("%d;",Top+Body+Out+N-84);
}
