#exe {
  Print("before;");
  I64 N=StreamExePrint("I64 Count=2;I64 Values[Count]={20,22};I64 ChildFn(){return Values[0]+Values[1];}Print(\"child;\");ChildFn();");
  Print("after;");
  StreamPrint("%d;",N);
}
