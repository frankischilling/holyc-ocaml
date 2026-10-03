noargpop I64 Add(I64 n)
{
  return n+2;
}

noargpop I64 (*Selected)(I64 n=40);
noargpop I64 (*Slots)(I64 n=20)[2][3];

I64 Run()
{
  Selected=&Add;
  Slots[1][2]=Selected;
  return Selected()+Slots[1][2]()-22;
}

Run();
