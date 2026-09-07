I64 Set(I64 *p)
{
  *p+=2;
  return 0;
}

I64 F()
{
  I64 a[2];
  a[0]=40;
  Set(&a[0]);
  return a[0];
}

F();
