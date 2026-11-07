extern U0 Print(U8 *fmt,...);
class Item { U8 text[3]; U16 value; };
I64 F() {
  U8 bytes[10];
  Item *p=bytes(Item*);
  p[1].text[0]=65;
  p[1].text[1]=66;
  p[1].text[2]=0;
  p[1].value=42;
  U8 *raw=p(U8*);
  Item *q=(raw+5)(Item*);
  if (q-p!=1 || q!=p+1) return -1;
  Print("%s",q->text);
  return q->value;
}
F();
