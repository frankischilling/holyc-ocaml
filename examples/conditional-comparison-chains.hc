I64 Count=0;

I64 Touch(I64 value)
{
  Count++;
  return value;
}

I64 SharedChain()
{
  if(Touch(3)<Touch(2)<Touch(1))
    return -1;
  return Count+40;
}

SharedChain();
