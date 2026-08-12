// Sources: Compiler/PrsStmt.HC:PrsGlblVarLst, PrsFun, and PrsStmt.
public U0 Empty();
I64 Recursive(I64 value=1,...)
{
  if (value)
    return Recursive();
  return 0;
}
U0 End()
