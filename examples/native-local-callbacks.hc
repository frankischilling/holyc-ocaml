I64 Add(I8 n,U16 extra)
{
  return n+extra;
}

U0 Visit(U64 n)
{
  return;
}

I64 Run()
{
  I64 (*p)(I8 n,U16 extra),(*saved)(I8 n,U16 extra);
  U0 (*visit)(U64 n);
  p=&Add;
  saved=p;
  p=0;
  visit=&Visit;
  visit(0);
  return saved(40,2);
}

Run();
