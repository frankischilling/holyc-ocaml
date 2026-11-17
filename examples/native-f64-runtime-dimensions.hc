// Each bound runs while its declaration is parsed, before the function is called.
I64 Visits=0;

F64 Bound()
{
  ++Visits;
  return 2.875;
}

I64 Global[Bound()]={20,22};

I64 Unused()
{
  I64 Automatic[Bound()];
  static I64 Persistent[Bound()]={1,2};
  return sizeof(Automatic)+sizeof(Persistent);
}

class Box
{
  I64 Members[Bound()];
};

Global[0]+Global[1]+Visits-4+sizeof(Box)-16;
