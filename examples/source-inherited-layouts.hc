extern U0 Print(U8 *fmt,...);

I64 Count() { Print("dim"); return 4; }
I64 Offset() { Print("off"); return 34; }

class Base {
  U8 bytes[Count()];
  $$=Offset();
};
class Child:Base { I64 last; };
class Copy:Child {};

I64 Total(I64 n=sizeof(Copy)) {
  I64 values[sizeof(Copy)];
  return sizeof(values)/8+n-42;
}

class Base { U8 replacement; };
I64 answer=Total;
Print("%d",answer);
answer;
