extern U0 Print(U8 *fmt,...);
#exe {
  _intern 0x1e I64 ToUpper(U8 ch);
  _intern 0x84 I64 StrLen(U8 *text);
  I64 Convert(I64 ch) { return ToUpper(ch); }
  StreamPrint("Print(\"%%c:%%d\",%d,%d);",Convert('a'),StrLen("abc"));
}
42;
