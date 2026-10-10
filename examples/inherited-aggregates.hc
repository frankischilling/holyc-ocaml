extern U0 Print(U8 *fmt,...);
class Base { U8 text[3]; };
class Item:Base { U16 value; };
I64 Sum(Item *p) {
  I64 n=p->value;
  p++;
  return n+p->value;
}
I64 F() {
  Item a[2];
  Item *p=a;
  a[0].value=20;
  a[1].value=22;
  p[1].text[0]=65;
  p[1].text[1]=66;
  p[1].text[2]=0;
  Print("%s",(p+1)->text);
  return Sum(p);
}
F();
