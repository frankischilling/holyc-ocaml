I64 Add(I64 value)
{
  return value+2;
}

I64 Apply(I64 (*callback)(I64 value=40))
{
  return (*callback)();
}

I64 (*selected)(I64 value);
selected=&Add;
Apply(*selected);
