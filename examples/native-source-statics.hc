extern U0 PutChars(U64 ch);
I64 N=40;
I64 Next()
{
  PutChars('I');
  return ++N;
}
I64 Counter()
{
  static I64 A=Next(),B=A;
  return ++B;
}
Counter();
Counter();
