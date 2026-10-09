class Outer { I64 n; };
#exe {
  Print("before;");
  I64 N=StreamExePrint("class Child:Outer{I64 b;};I64 Values[2]={20,22};I64 ChildFn(){return Values[0]+Values[1];}Print(\"child;\");ChildFn();");
  Print("after;");
  StreamPrint("%d;",N);
}
#exe {
  I64 N=StreamExePrint("ChildFn();");
  StreamPrint("%d;",N);
}
sizeof(Child)+26;
