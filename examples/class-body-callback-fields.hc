extern U0 Print(U8 *fmt,...);

#exe {
  I64 answer;
  class Item {
    I64 (*callbacks)(I64 n=40)[2];
    #exe {
      Print("A");
      class Box { Item item; };
      Box object;
      I64 Old(I64 n) { return n+2; }
      I64 New(I64 n) { return n+100; }
      I64 Change() {
        object.item.callbacks[1]=&New;
        return 40;
      }
      I64 Read(Item *p) { return p->callbacks[0](); }
      object.item.callbacks[0]=&Old;
      object.item.callbacks[1]=&Old;
      answer=object.item.callbacks[1](Change());
      if (Read(&object.item)!=42 || object.item.callbacks[1]()!=140)
        answer=-1;
    }
    U8 later;
  };
  StreamPrint("%d;",answer);
}
