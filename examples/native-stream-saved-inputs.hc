#exe {
  I64 N=40;
  I64 Parent()
  #exe {
    Print("%d;",StreamExePrint("Print(\"child;\");N+2;"));
    StreamExePrint("I64 Child(){return N+2;}class Made{I64 n;};");
    Print("%d;",StreamExePrint("Child();"));
  }
  {return Child();}
  Print("%d;%d;",Parent(),sizeof(Made)+34);
  Print("%d;",StreamExePrint("I64 GrandParent()#exe {Print(\"inner;\");Print(\"%%d;\",StreamExePrint(\"Print(\\\"grand;\\\");42;\"));}{return 42;}GrandParent();"));
}
42;
