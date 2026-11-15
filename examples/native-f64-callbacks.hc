F64 Add(F64 a,F64 b)
{
  return a+b;
}

F64 Apply(F64 (*operation)(F64 a,F64 b),F64 a,F64 b)
{
  return operation(a,b);
}

F64 (*operation)(F64 a,F64 b);
operation=&Add;
Apply(operation,1.5,2.25);
