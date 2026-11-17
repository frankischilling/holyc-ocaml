extern U0 Print(U8 *format,...);

F64 seed=1.5;

F64 Value()
{
  Print("A");
  return seed;
}

F64 Read()
{
  static F64 values[2]={Value(),2.25};
  return values[0]+values[1];
}

// JIT declaration preparation prints A once. Both calls return 3.75.
Read();
Read();
