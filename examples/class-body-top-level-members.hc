extern U0 Print(U8 *fmt,...);

#exe {
  I64 answer;
  class Base { U8 value; };
  class Item:Base {
    I8 signed_value;
    union { U16 word; U8 bytes[2]; };
    U16 values[2];
    #exe {
      Print("A");
      class Box { Item item; };
      Box object;
      object.item.value=40;
      object.item.signed_value=-1;
      object.item.word=0x12a;
      object.item.values[1]=40;
      if (object.item.value!=40 || object.item.signed_value!=-1 ||
          object.item.bytes[0]!=42) answer=-1;
      else answer=object.item.values[1]+2;
    }
    U8 later;
  };
  StreamPrint("%d;",answer);
}
