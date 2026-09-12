#exe {
 extern I64 F(I64 n=40,...);
 I64 Use(){return F(,1);}
 I64 F(I64 n=99,...){return n+argc+argv[0];}
 StreamPrint("%d;",Use());
}
