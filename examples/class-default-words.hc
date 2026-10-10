U16 class Packet {
  U16 low;
  U8 guard;
};

I64 Take(Packet packet=0x07002a,I64 tail=0)
{
  if (packet.guard!=7)
    return -1;
  return packet+tail;
}

Take();
