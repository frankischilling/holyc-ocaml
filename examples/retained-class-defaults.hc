I64 Preparations=0;

U16 class Packet {
  U16 low;
  U8 guard;
};

Packet Make() {
  Preparations++;
  return 0x07002a;
}

I64 Take(Packet packet=Make()) {
  if (packet.guard!=7) return -1;
  return packet;
}

Take();
Take()+Preparations;
