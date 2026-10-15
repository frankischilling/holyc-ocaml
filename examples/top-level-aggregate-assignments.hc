#exe {
extern class Item;
class Box{Item items[2][3];U8 bytes[16];};
Box object;
object.bytes[2]=7;
object.items[1][2]=#exe{U16 class Item{U16 value;};}40;
object.items[0][0]+=2;
object.items[0][1]=44;

extern U16 class Saved;
class Envelope{Saved word;U8 bytes[8];};
Envelope original;
I64 captured=(original.word=0x12a#exe{U8 class Saved{U8 value;};});

Print("A");
StreamPrint("%d;",object.bytes[0]
  +(object.bytes[2]!=44)
  +(original.bytes[0]!=42)
  +(original.bytes[1]!=1)
  +(captured!=0x12a));
}
