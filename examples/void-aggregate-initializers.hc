extern U0 Print(U8 *fmt,...);
I64 effects=0;
U0 Note() {
  effects++;
  Print("A");
}
class Box { U0 marker; U8 value; };
Box first={Note(),20};
I64 Sum() {
  static Box second={Note(),22};
  if (effects!=2) return -1;
  return first.value+second.value;
}
Sum();
