extern U0 Print(U8 *fmt,...);

I64 calls=0;
I64 Next(I64 value) {
  if (!calls) Print("A");
  calls++;
  return value;
}

class Empty {};
Empty object=Next(7);
Empty objects[2][2]={{Next(8),Next(9)},{Next(10),Next(11)}};
U8 guard=33;

I64 F() {
  static Empty saved=Next(12);
  static Empty saved_array[2]={Next(13),Next(14)};
  return sizeof(object)+sizeof(objects)+sizeof(saved)+sizeof(saved_array)
    +guard+calls;
}

I64 Unused() {
  static Empty saved=Next(15);
  return sizeof(saved);
}

class Empty { U16 replacement; };
I64 Check() {
  if (calls!=9) return -1;
  F();
  return F();
}
Check();
