I64 Set(I64 *p)
{
  *p+=2;
  return *p;
}

I64 F()
{
  I64 n=40;
  Set(&n);
  return n;
}

F();
