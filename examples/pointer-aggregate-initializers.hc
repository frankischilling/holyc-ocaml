I64 target=40,initializations=0;
I64 *Reference() {initializations++;return &target;}
class Box {U8 tag;I64 *value;};
Box object={7,Reference()};
I64 Step() {
  static Box saved={9,Reference()};
  return ++(*saved.value);
}
I64 Check() {
  if (Step()!=41 || Step()!=42 || initializations!=2) return -1;
  return *object.value;
}
Check();
