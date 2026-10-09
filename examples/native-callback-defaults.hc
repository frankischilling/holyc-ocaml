I64 Echo(I64 n=17)
{
  return n;
}

I64 (*Saved)(I64 n=7)[2];

I64 Apply(I64 (*p)(I64 n=42))
{
  return p();
}

I64 Run()
{
  Saved[1]=&Echo;
  Saved[1]();
  return Apply(Saved[1]);
}

Run();
