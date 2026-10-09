I64 Add(I64 n)
{
  return n+2;
}

I64 Apply(I64 (*p)(I64 n),I64 n)
{
  I64 (*saved)(I64 n);
  saved=p;
  p=123;
  return saved(n);
}

I64 Forward(I64 (*p)(I64 n),I64 n)
{
  return Apply(p,n);
}

Forward(&Add,40);
