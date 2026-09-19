I8 Base=40;

I64 Next(I64 reset=0)
{
  static I8 count;
  if(reset)
    count=Base;
  else
    count++;
  return count;
}

Next(1);
Next;
Next;
