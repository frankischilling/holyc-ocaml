public _intern 0x7b Bool LBts(U8 *p,I64 n);

I64 Total=40;

U0 Add(I64 *p,I64 amount)
{
  lock *p+=amount;
}

I64 LockExample()
{
  U8 count=255;
  I64 old=0;
  lock old=count++;
  if(old!=255||count!=0)
    return 0;

  U8 bits[2];
  bits[0]=0;
  bits[1]=0;
  if(LBts(bits,8)!=0||bits[0]!=0||bits[1]!=1)
    return 0;

  Add(&Total,1);
  lock {lock ++Total;}
  return Total;
}

LockExample();
