extern U0 Print(U8 *fmt,...);
#exe {
  I64 Calls=0;
  I64 Target() {
    Calls++;
    Print("target:%d;",Calls);
    return 0x10+0xe;
  }
  _intern Target() I64 Convert(U8 ch);
  StreamPrint("Print(\"%%c:%%d\",%d,%d);",Convert('a'),Calls);
}
42;
