// Named and anonymous F64 defaults execute during original JIT declarations.
// The named default converts I64 1 to F64, then the anonymous default saves 3.75.
I64 Seed()
{
  return 1;
}

F64 Named(F64 value=Seed())
{
  return value+2.75;
}

F64 Echo(F64 value)
{
  return value;
}

F64 Run()
{
  F64 (*callback)(F64 value=Named());
  callback=&Echo;
  return callback();
}

Run();
