I64 (*Saved)(I64 n),(*Slots)(I64 n)[2][3];

I64 Add(I64 n)
{
  return n+2;
}

I64 Apply(I64 (*p)(I64 n),I64 n)
{
  I64 (*local)(I64 n)[2];
  local[1]=p;
  return local[1](n);
}

I64 Run(I64 seed)
{
  static I64 (*cache)(I64 n)[2];
  if(seed) {
    Saved=&Add;
    Slots[1][2]=Saved;
    cache[1]=Slots[1][2];
  }
  Saved=0;
  Slots[1][2]=123;
  return Apply(cache[1],40);
}

Run(1);
Run(0);
