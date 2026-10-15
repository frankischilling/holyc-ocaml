class Box { U64 word; };
I64 F() {
  Box o;
  o.word = 0x1002a;
  if (o(U8) != 42) return -1;
  if ((o)(U8) != 0x1002a) return -2;
  o(U8) = 40;
  o(U8) += 2;
  return o(U8);
}
F();
