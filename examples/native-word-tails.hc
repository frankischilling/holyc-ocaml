I64 Sum(I64 base,...)
{
  I64 *tail=argv;
  I64 i;
  for(i=0;i<argc;i++)
    base+=tail[i];
  return base;
}

I64 (*Saved)(I64 base=10,...)[2];

I64 Apply(I64 (*call)(I64 base=10,...),...)
{
  return call(,argv[0],argv[1]);
}

I64 Run()
{
  Saved[1]=&Sum;
  return Apply(Saved[1],20,12);
}

Run();
