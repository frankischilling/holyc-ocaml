extern U0 Print(U8 *fmt,...);

extern class Item;
class Box {
  Item item;
  U8 tail;
  $$=8;
};

Box object={7,40};

I64 Next() {
  static Box saved={9,1};
  return object.tail+saved.tail++;
}

class Item { U16 value; };
class Item { U8 replacement; };

#exe { Print("A"); }

I64 Check() {
  Next();
  return Next();
}

Check();
