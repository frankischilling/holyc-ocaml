argpop I64 Add(I64 n) { return n + 2; }
noargpop I64 Twice(I64 n) { return n * 2; }
haserrcode argpop noargpop U8 Answer(U8 n = 554) { return n; }
noargpop argpop U0 Done() { return; }

I64 Outer() {
  argpop static I64 seen;
  seen = Add(38);
  Done();
  return Twice(seen) / 2 + Answer() - 40;
}

Outer();
