extern U0 Print(U8 *fmt,...);

class Packed {
  U8 text[3];
  U16 value;
};

I64 Bump(Packed *p) {
  return ++p->value;
}

I64 F() {
  Packed object;
  Packed *p=&object;
  p->text[0]=65;
  p->text[1]=66;
  p->text[2]=0;
  Print("%s",p->text);
  object.value=40;
  Bump(p);
  return Bump(&object);
}

F();
