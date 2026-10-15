extern U0 Print(U8 *fmt,...);

#exe {
  extern class Item;
  class Box { Item item; U8 tail; $$=8; };

  I64 Complete() {
    Print("A");
    StreamExePrint("class Item{U16 value;};");
    return 42;
  }

  I64 F() {
    static Box object={Complete(),7};
    return object.tail++;
  }

  F();
  StreamPrint("%d;",F()-1);
}
