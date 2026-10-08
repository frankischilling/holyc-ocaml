#exe {
I64 Local()
{
  I64 local=42;
#define local 7
  return local;
}
I64 Parameter(I64 parameter)
{
#define parameter 7
  return parameter;
}
Print("%d;%d;",Local(),Parameter(42));
}
42;
