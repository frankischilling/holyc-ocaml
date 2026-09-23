I64 F()
{
  I64 a[2];
  I64 *p,*q;
  I64 i=0;

  while (i<2) {
    I64 *r=&a[i];
    if (i)
      q=r;
    else
      p=r;
    i++;
  }

  *p=40;
  *q=2;
  return a[0]+a[1];
}

F();
