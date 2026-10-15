extern U0 Print(U8 *fmt,...);

class Empty {};
union EmptyUnion { U0 value; };

Empty *Keep(Empty *p) { return p; }

I64 Check() {
  U8 first=40;
  Empty object,other,objects[2][3];
  EmptyUnion empty_union;
  U8 tail=2;
  U8 *p=(Keep(&object))(U8*);
  if (p!=(&other)(U8*) || p!=(&first)(U8*)) return -1;
  if (p!=(&objects[1][2])(U8*)) return -2;
  if (sizeof(object)+sizeof(other)+sizeof(objects)+sizeof(empty_union)) return -3;
  return first+tail;
}

Print("A");
Check();
