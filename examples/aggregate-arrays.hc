extern U0 Print(U8 *fmt,...);

class Item {
  U8 text[3];
  U16 value;
};

I64 Bump(Item *p) {
  return ++p->value;
}

I64 F() {
  Item items[2][3];
  items[1][2].text[0]=65;
  items[1][2].text[1]=66;
  items[1][2].text[2]=0;
  Print("%s",items[1][2].text);
  items[1][2].value=40;
  Bump(&items[1][2]);
  return Bump(&items[1][2]);
}

F();
