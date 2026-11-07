extern U0 Print(U8 *fmt,...);

I64 calls=0;
I64 Next(I64 n) {
  calls++;
  if(calls==1)Print("A");
  return n;
}
I64 first=Next(0);

extern class Item;
class Box {
  Item zero;
  U8 values[2];
  $$=8;
};
I64 F() {
  static Box saved={Next(7),{Next(40),2}};
  static U8 tail=saved.values[1];
  return saved.values[0]+tail++;
}
class Item { U16 value; };
class Item { U8 replacement; };
I64 Next(I64 n) { return n+99; }

I64 Check() {
  if(calls!=3)return -1;
  F();
  return F()-1;
}
Check();
