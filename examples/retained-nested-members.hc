I64 Dimensions=0;
I64 Next() {
  Dimensions++;
  return 2;
}
class Element { U16 word[Next()]; };
class Packet { Element items[2]; };
class Element { U8 replacement; };
I64 Read(Packet packet=0x0029000000000000) {
  return packet.items[1].word[1]+Dimensions;
}
Read();
