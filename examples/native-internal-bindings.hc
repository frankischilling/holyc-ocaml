extern U0 Print(U8 *format,...);
I64 Bindings=0;
I64 Target() {
  Print("bind");
  Bindings++;
  return 0x84;
}
_intern Target() I64 ByteCount(U8 *str);
I64 F(U8 *p="AB") { return ByteCount(p); }
F();
F()+Bindings+39;
