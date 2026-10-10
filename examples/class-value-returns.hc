U16 class Packet { U16 low; U8 guard; };
Packet Make() { return 0x07002a; }
Packet Read(Packet o) { return o; }
I64 Take(Packet o) {
  if (o.guard!=7 || Read(o)!=42) return -1;
  return o;
}
Take(Make());
