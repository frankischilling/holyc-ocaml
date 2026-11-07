// Direct frame assignments reload their destination when the result is used.
// Stack bytes narrow before a condition or call; selected registers keep a word.
I64 Echo(I64 value)
{
  return value;
}
I64 StackCondition(I8 noreg x,I64 a)
{
  if(x=a+1)
    return x;
  return -1;
}
I64 RegisterCondition(I8 reg x,I64 a)
{
  if(x=a+1)
    return x;
  return -1;
}
I64 StackArgument(I64 a)
{
  I8 noreg x=0;
  return Echo(x=a+1);
}
I64 RegisterArgument(I64 a)
{
  I8 reg R12 x=0;
  return Echo(x=a+1);
}
RegisterCondition(0,255)-StackCondition(0,255)
  +RegisterArgument(127)-StackArgument(127)+1;
