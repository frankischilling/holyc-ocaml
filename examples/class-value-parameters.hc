U16 class Packet { U16 low; U8 guard; };
I64 Take(Packet o, I64 n) {
  if (sizeof(Packet)!=3 || o.low!=33 || o.guard!=7 || n!=9) return -1;
  o.guard=11;
  o+=0x10000;
  if (o.guard!=11) return -2;
  return o+n;
}
Take(0x070021,9);
