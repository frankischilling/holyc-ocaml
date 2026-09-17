// TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c:
// PrsVar.HC:631-656 and PrsExp.HC:455-468.
// Four declaration-time scalar defaults are prepared once and reused by native calls.
U64 AllBits(U64 value = 0xffffffffffffffff)
{
  return value;
}

I64 Pair(I64 left = 20, I64 right = 22)
{
  return left + right;
}

I64 Twice(I64 value = 21)
{
  return value + value;
}

(AllBits() == 0xffffffffffffffff) + Pair() + Pair() + Twice() - 85;
