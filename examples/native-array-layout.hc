I64 ArraySize()
{
  I16 values[3][7];
  I8 marker=42;
  return sizeof(values)+marker-42;
}
ArraySize();
