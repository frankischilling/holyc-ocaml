extern I64 Answer();

I64 Read()
{
  I64 (*p)();
  p=&Answer;
  return p();
}

I64 Same(I64 (*p)()=&Answer)
{
  return p==&Answer;
}

I64 Answer(){return 42;}
I64 Answer(){return 17;}
Read()+Same();
