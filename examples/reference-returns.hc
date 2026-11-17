extern U0 PutChars(U64 ch);

class Box {
  U8 pad;
  U16 value;
};

Box *Second(Box *items) {
  PutChars('A');
  return items+1;
}

I64 Run() {
  Box items[2];
  items[1].value=42;
  Box *selected=Second(items);
  PutChars('B');
  return selected->value;
}

Run();
