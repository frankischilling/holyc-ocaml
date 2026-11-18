extern U0 Print(U8 *fmt,...);
class Item { U16 value; };
class Box { U8 *text; Item *items[2]; Item **selected; };
I64 Bump(Item *p) { return ++p->value; }
Item **ItemSlot(Box *box, I64 index) { return &box->items[index]; }
I64 F() {
  Box object;
  U8 text[3];
  Item item;
  text[0]=65;
  text[1]=66;
  text[2]=0;
  item.value=40;
  object.text=text;
  object.selected=ItemSlot(&object,1);
  *object.selected=&item;
  Print("%s",object.text);
  Bump(*object.selected);
  return Bump(object.items[1]);
}
F();
