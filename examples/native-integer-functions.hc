I64 Add(I64 left, I64 right)
{
  I64 sum = left + right;
  return sum;
}

I64 Wrap(I64 value)
{
  I64 keep = 2;
  return keep + Add(value, 20);
}

Wrap(20);
