// Each unnamed argument keeps its own slot. Lookup finds the first _anon_.
I64 First(I64,I64)
{
  return _anon_;
}

// This declaration adds a local, but its initializer assigns the earlier
// formal named _anon_. The formal's I64 type controls the assignment.
I64 AssignFirst(I64)
{
  I8 _anon_=37;
  return _anon_;
}

// TempleOS warns about the repeated request and gives R12 to the last owner.
// The first member uses byte storage (-128); the last retains 128 in R12.
I64 LastRegister(reg R12 I8 first,reg R12 I8 last)
{
  first=128;
  last=128;
  return first+last;
}

First(11,99)+AssignFirst(10)+LastRegister(0,0); // 48
