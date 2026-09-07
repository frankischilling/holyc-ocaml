I64 Seed()
{
  return 40;
}
I64 Next()
{
  static I64 n=Seed();
  return ++n;
}
Next();
Next();
