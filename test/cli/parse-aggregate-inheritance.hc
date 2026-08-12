class Root
{
  I64 root;
};
public class Derived : Root
{
  I64 child;
};
I64i union Raw
{
  I64 value;
};
I64i union Overlay : Raw
{
  I8 byte;
};
