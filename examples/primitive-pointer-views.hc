extern U0 Print(U8 *fmt,...);

U0 Write(U16 *p) {
  *p=0x4241;
}

I64 Demo() {
  U64 word;
  U8 *bytes=(&word)(U8*);
  Write(bytes(U16*));
  bytes[2]=0;
  Print("%s",bytes);
  I64 i;
  for(i=3;i<8;i++)bytes[i]=0;
  U64 *again=bytes(U64*);
  return *again-0x4241+42;
}

Demo();
