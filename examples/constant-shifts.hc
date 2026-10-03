extern U0 Print(U8 *fmt,...);

I64 Nested(I64 x) {
  return (x<<63)<<1;
}
I64 Signed(I64 x) {
  return x>>0x8000000000000001;
}
I64 EarlierUnsigned(I64 x) {
  return (x>>0x8000000000000001)<0;
}
I64 Literal() {
  return (-7<<63)<<1;
}

Print("%d:%d:%d:%d;",Nested(-7),Signed(-7),EarlierUnsigned(-7),Literal());
49+Nested(-7);
