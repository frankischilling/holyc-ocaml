extern U0 Print(U8 *fmt,...);

U16 class Word {
  U16 value;
  U8 guard;
};

class Base {
  U64 value;
};

U64 class Item:Base {
  U8 guard;
};

I64 Next(I64 value) {
  if (value==20)
    Print("A");
  return value;
}

I64 Main() {
  Word first=Next(20),second=first+2;
  first.guard=7;
  second.guard=9;
  Item item=second.value;
  item.guard=11;
  if (first.guard!=7 || second.guard!=9 || item.guard!=11)
    return -1;
  return first.value+item.value;
}

Main();
