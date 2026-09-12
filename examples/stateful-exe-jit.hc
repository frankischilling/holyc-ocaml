extern U0 Print(U8 *fmt,...);
I64 N=20;
I64 Next(){return ++N;};
I64 Saved(I64 n=Next()){return n;};
Print("A");
N=0;
#exe {
  Print("B");
  StreamPrint("Saved()+Saved();");
}
