I64 total=0;
I64 values[2]={0,0};

I64 Accumulate(I64 (*amount)())
{
  U8 local=14;
  I64 *pointer=&total;
  local+=amount;
  total+=amount;
  values[1]+=amount;
  *pointer+=amount;
  return local+values[1]+total;
}

I64 (*amount)()[2]={0,7};
Accumulate(amount[1]);
