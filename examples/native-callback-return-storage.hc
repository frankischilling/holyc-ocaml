extern U0 PutChars(U64 ch);

I64 Answer(I64 n)
{
  PutChars('A');
  return n+2;
}

I64 Forward(F64 **(*incoming)(I64 n))
{
  F64 **(*local)(I64 n);
  static F64 **(*saved)(I64 n)[2];
  I64 (*invoke)(I64 n);
  local=incoming;
  saved[1]=local;
  invoke=saved[1];
  return invoke(40);
}

I64 Run()
{
  F64 (*cells)(I64 n)[2][3];
  cells[1][2]=&Answer;
  return Forward(cells[1][2]);
}

Run();
