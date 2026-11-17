#exe {
extern class Item;
class Box{Item items[2];U8 bytes[8];};
extern U16 class Saved;
class Envelope{Saved word;U8 bytes[8];};

I64 F(){
  Box box;
  box.bytes[2]=7;
  box.items[1]=#exe{U16 class Item{U16 value;};}40;
  box.items[0]+=2;
  if(box.bytes[0]!=42 || box.bytes[2]!=7)return -1;
  box.items[1]=44;
  if(box.bytes[0]!=42 || box.bytes[2]!=44)return -3;

  Envelope original;
  original.bytes[1]=0;
  original.word=0x12a#exe{U8 class Saved{U8 value;};};
  if(original.bytes[0]!=42 || original.bytes[1]!=1)return -2;
  return box.bytes[0];
}
Print("A");
StreamPrint("%d;",F());
}
