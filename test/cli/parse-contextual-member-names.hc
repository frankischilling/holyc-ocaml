class ContextualNames
{
  I64 start,end;
  U8 *if[3];
  I64 (*switch)(I64 value);
  union
  {
    I16 class;
  };
};
