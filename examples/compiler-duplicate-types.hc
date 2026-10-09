#exe {
  Option(16,0);
  Option(18,1);
  I64 Sum() {
    I64 a=40;
    I64 b=2;
    return a+b;
  }
  I64 CallbackBase() {
    I64 word;
    I64 (*first)(I64 n);
    U8 (*second)(U8 n);
    return 42;
  }
  Print("%d;",Sum());
}
42;
