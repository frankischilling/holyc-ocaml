extern U0 Print(U8 *fmt,...);

I64 calls=0;
I64 Seed(I64 value) {
  if (!calls) Print("A");
  calls++;
  return value;
}

I64 before=40
#exe { Option(35,1); }
;

class Box { U8 values[2]; };
I64 F() {
  static Box object={{Seed(40)
#exe { Option(35,0); }
    ,2
#exe { Option(35,1); }
  }};
  static I64 marker=7;
  if (marker!=7) return -1;
  return object.values[0]+object.values[1];
}

#exe { Option(35,0); }
I64 Check() {
  if (before!=40 || calls!=1) return -1;
  F();
  return F();
}
Check();
