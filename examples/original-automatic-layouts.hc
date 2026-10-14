extern U0 Print(U8 *fmt,...);

extern class Item;
I64 Before() { Item object; return sizeof(object); }
class Item { U16 value; };
I64 After() { Item object; object.value=2; return sizeof(object)+object.value; }

extern class Base;
class Middle:Base { U8 tag; };
class Leaf:Middle { U8 tail; };
class Base { U8 head; };
class Base { U64 replacement; };

I64 Read(Leaf *p) { return p->head+p->tail; }
I64 Check() {
  Leaf objects[2];
  objects[1].tag=36;
  objects[1].tail=2;
  if (sizeof(objects)!=4) return -1;
  return Read(&objects[1])+Before()+After();
}

Print("A");
Check();
