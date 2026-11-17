F64 Scale(F64 value=1.5)
{
  return value;
}

I64 Offset(I8 value=257.75)
{
  return value;
}

F64 Target(F64 value=2.25)
{
  return value;
}

F64 (*Selected)(F64 value=2.75);

Selected=&Target;
Scale()+Selected()+Offset();
