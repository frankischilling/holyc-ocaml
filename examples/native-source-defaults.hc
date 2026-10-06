I64 Counter=40;
I64 Seed()
{
  return ++Counter;
}
I64 Answer(I64 value=Seed())
{
  return value+1;
}
Answer();
Counter=100;
Answer();
