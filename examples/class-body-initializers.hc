extern U0 Print(U8 *fmt,...);

#exe {
  U16 class Item
    #exe {
      Print("A");
      class Box { Item item; U8 tail; $$=8; };
      Box before={9,42};
    }
  { U8 value; };

  Box after={0x12a,7};
  I64 Check() {
    U8 *p=(&after)(U8*);
    if (before.tail!=42 || p[0]!=42 || p[1]!=1 || p[2]!=7) return -1;
    return 42;
  }
  StreamPrint("%d;",Check());
}
