extern U0 Print(U8 *fmt,...);

I64 calls=0;
extern class Item;
class Box {
  Item item;
  U8 tail;
  $$=8;
};

I64 Next() {
  calls++;
  Print("A");
  return 7;
}

Box object={Next(),42};

I64 Check() {
  if (calls!=1) return -1;
  return object.tail;
}

Check();
