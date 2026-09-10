#exe {
 I64 Out=0;
 U0 Print(I64 a=40,...){Out=a+argc+argv[0];}
 ""(,1);
 I64 Top=Out;
 U0 Saved(){""(39,1,7);}
 Out=0;Saved;
 StreamPrint("%d;",Top+Out-42);
}
