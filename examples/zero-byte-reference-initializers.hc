I64 target=7,calls=0;

I64 *Observe() {
  calls++;
  return &target;
}

class Box { U0 marker; I64 value; };
Box first={Observe(),20};

I64 Sum() {
  static Box second={Observe(),22};
  if (calls!=2 || target!=7) return -1;
  return first.value+second.value;
}

Sum();
