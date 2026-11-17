U8 text[]="AB";
I64 words[]={};
I64 neighbor=42;

I64 CheckEmptyArrays() {
  static I64 saved[2][0]={{},{}};
  U8 local[];
  return neighbor+sizeof(text)+sizeof(words)+sizeof(saved)+sizeof(local);
}

CheckEmptyArrays();
