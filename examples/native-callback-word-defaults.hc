I64 Check(I64 (*cb)(I64 n)=12)
{
  if (cb==17)
    return 42;
  return 0;
}

I64 Run()
{
  I64 (*invoke)(I64 (*cb)(I64 n)=17);
  invoke=&Check;
  return invoke();
}

Run();
