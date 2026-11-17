extern U0 Print(U8 *fmt,...);

#exe {
  extern U16 class Item;
  class Box { Item items[2]; $$=8; };

  I64 F() {
    static Box object={{
      #exe { Print("A"); U8 class Item { U8 value; }; }
      0x12a,7
    }};
    U8 *p=(&object)(U8*);
    if (p[0]!=42 || p[1]!=1 || p[2]!=7 || p[3]!=0) return -1;
    return 42;
  }

  StreamPrint("%d;",F());
}
