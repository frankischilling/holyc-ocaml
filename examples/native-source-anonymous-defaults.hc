I64 Counter=40;
I64 Seed()
{
  return ++Counter;
}
I64 Answer(I64 value)
{
  return value+1;
}
I64 (*answer)(I64 value=Seed())=&Answer;

I64 Original()
{
  return 42;
}
I64 Call(I64 (*callback)())
{
  return callback();
}
I64 (*invoke)(I64 (*callback)()=&Original)=&Call;

Counter=100;
I64 Original()
{
  return 17;
}
answer();
invoke();
