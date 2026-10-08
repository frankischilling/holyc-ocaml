extern U0 Print(U8 *fmt,...);
#exe {
  I64 result=StreamExePrint("class Child {I64 n;};");
  Print("child%d;",result);
}
sizeof(Child)+34;
