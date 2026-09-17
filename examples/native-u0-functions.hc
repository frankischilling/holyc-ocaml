// TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c:
// Compiler/PrsExp.HC:530-588 and Compiler/PrsStmt.HC:150-169,1110-1119.
// A reached U0 call completes without a word and clears a preceding top-level
// value. Fallthrough, bare early return and recursive calls share that contract.
U0 Fallthrough(I8 n)
{
  I8 saved=n;
  if(saved<0)return;
}

U0 Early(I16 n)
{
  if(n)return;
  I16 saved=42;
  saved;
}

U0 Recur(I32 n)
{
  if(n)
  {
    Recur(n-1);
    return;
  }
  Fallthrough(0);
}

U0 Outer(U64 n)
{
  Early(n);
  Recur(3);
}

42;
Outer(1);
