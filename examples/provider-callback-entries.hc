extern U0 PutChars(U64 ch);
I64 Last;
I64 Apply(U0 (*print)(U64 ch)=&PutChars)
{
  print('AB');
  return 42;
}
U0 PutChars(U64 ch)
{
  Last=ch;
}
Apply();
