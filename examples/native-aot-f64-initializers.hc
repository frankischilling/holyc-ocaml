extern U0 Print(U8 *format,...);

F64 First()
{
  Print("A");
  return 1.5;
}

F64 Second()
{
  return 2.25;
}

F64 values[2]={First(),Second()};

F64 Read()
{
  static F64 saved[2]={values[0],values[1]};
  return saved[0]+saved[1];
}

// AOT load initialization prints A once. Both calls return 3.75.
Read();
Read();
