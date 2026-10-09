extern U0 PutChars(U64 value);

I64 Target(I64 value=7)
{
  return value+2;
}

I64 Select()
{
  PutChars('I');
  return 1;
}

I64 Invoke()
{
  I64 (*callbacks)(I64 value=40)[2];
  *callbacks[1]=&Target;
  return (*callbacks[Select()])();
}

Invoke;
