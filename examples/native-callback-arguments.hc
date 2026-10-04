I64 Add(I64 n)
{
  return n+2;
}

I64 Apply(I64 (*cb)(I64 n=40))
{
  return cb();
}

I64 Run()
{
  I64 (*invoke)(I64 (*cb)(I64 n=12));
  I64 (*saved)(I64 n=10);
  invoke=&Apply;
  saved=&Add;
  return invoke(saved);
}

Run();
