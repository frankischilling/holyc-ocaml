extern U0 Print(U8 *fmt,...);

U0 Publish()
{
  extern class Earlier,extern union Later;
  class Earlier { U8 bytes[20]; };
  union Later { U8 bytes[22]; I64 word; };
}

I64 Result(I64 saved=sizeof(Earlier))
{
  return saved+sizeof(Later);
}

I64 result=Result;
Print("%d",result);
result;
