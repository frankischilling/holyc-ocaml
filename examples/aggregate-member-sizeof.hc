extern U0 Print(U8 *fmt,...);

class Payload { U16 words[3]; U8 tag; };
class Packet { U8 first; Payload payloads[2]; U32 tail; };

I64 Measure()
{
  Packet packet;
  Print("%d %d %d %d %d\n", sizeof(packet.first), sizeof(packet.payloads),
        sizeof(packet.payloads.words), sizeof(Packet.tail),
        sizeof(packet.payloads.words *));
  return sizeof(packet.first)+sizeof(packet.payloads)
        +sizeof(packet.payloads.words)+sizeof(Packet.tail)
        +sizeof(packet.payloads.words *);
}

Measure();
