extern U0 Print(U8 *fmt,...);

I64 Add(I64 n)
{
  return n+2;
}

I64 (*Callbacks)(I64 n=40)[2]={&Add,Callbacks[0]};

I64 Seed()
{
  Print("A");
  return 17;
}

I64 (*Word)()=Seed();

I64 Check()
{
  if (Word==17)
    return Callbacks[1]();
  return 0;
}

Callbacks[0]=0;
Check();
